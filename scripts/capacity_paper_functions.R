
# This script assumes the working directory is the paper repo.
source("scripts/capacity_paper_variables.R")


# Get the start dates for weeks in the 2019 period.
get_2019_week_start_dates <- function() {
  as.character(
    seq(ymd(period_2019_start_week_date), ymd(period_2019_end_week_date), by="weeks")
  )
}


# Get the start dates for weeks in the 2020 period.
get_2020_week_start_dates <- function() {
  as.character(
    seq(ymd(period_2020_start_week_date), ymd(period_2020_end_week_date), by="weeks")
  )
}


# Get the start dates for weeks in baseline occupancy period.
get_baseline_week_start_dates <- function() {
  as.character(
    seq(ymd(period_baseline_start_week_date), ymd(period_baseline_end_week_date), by="weeks")
  )
}


# Filter SafeGraph Core Places for specific sectors.
filter_sector_df <- function(sg_core_df) {
  big_box_ca_df <- 
    sg_core_df %>% 
    filter(
      grepl(big_box_regex, location_name) 
      | sub_category %in% c("All Other General Merchandise Stores")
    ) %>% 
    mutate(type = "big_box")
  
  pharmacy_ca_df <- 
    sg_core_df %>% 
    filter(
      sub_category == "Pharmacies and Drug Stores"
    ) %>% 
    mutate(type = "pharmacy")
  
  grocery_ca_df <- 
    sg_core_df %>% 
    filter(
      sub_category == "Supermarkets and Other Grocery (except Convenience) Stores"
    ) %>% 
    mutate(type = "grocery")
  
  sector_df <- 
    big_box_ca_df %>% 
    rbind(pharmacy_ca_df) %>% 
    rbind(grocery_ca_df)
  
  sector_df
}


# Load SafeGraph Weekly Patterns data for given week start dates and place ids
load_hourly_scc_df <- function(week_start_dates, sector_df) { 
  hourly_df <- 
    week_start_dates %>% 
    paste0(
      data_path, "hourly-patterns-scc/", ., "-hourly-patterns-bay.rds"
    ) %>% 
    map_dfr(readRDS) %>% 
    inner_join(
      sector_df %>% select(safegraph_place_id), 
      by="safegraph_place_id"
    ) %>% 
    select(-sqft_per_visitor)
  
  hourly_df <- 
    hourly_df %>% 
    right_join(
      merge(
        hourly_df %>% select(date, hour) %>% unique(), 
        hourly_df %>% select(safegraph_place_id) %>% unique()
      ),
      by = c("date", "hour", "safegraph_place_id")
    ) %>% 
    mutate(
      hourly_visits = ifelse(is.na(hourly_visits), 0, hourly_visits),
      median_dwell = ifelse(is.na(median_dwell), 0, median_dwell)
    )
  
  hourly_df
}


# Explodes SafeGraph open hours field per POI
explode_open_hours <- function(sg_core_df) {
  open_hours_df <-
    sg_core_df %>%
    select(safegraph_place_id, location_name, open_hours) %>% 
    filter(!is.na(open_hours)) %>%
    mutate(
      open_hours = map(open_hours, ~ fromJSON(., simplifyMatrix = FALSE))
    ) %>%
    unnest_wider(open_hours) %>%
    pivot_longer(names(.)[3:9]) %>% 
    left_join(
      list(
        safegraph_place_id = unique(.$safegraph_place_id),
        hour = 1:24
      ) %>%
        expand.grid(),
      by = "safegraph_place_id"
    ) %>%
    as.data.frame() %>%
    mutate(
      is_open = apply(.[,c('value','hour')], 1, function(x) {
        if (is.na(x[1])) return(FALSE)
        for (interval in x[1][[1]]) {
          start_time <- hour(hm(interval[1]))
          end_time <- hour(hm(interval[2])) + trunc((minute(hm(interval[2])) + 59) / 60)
          if ((start_time < x[2]) & (x[2] <= end_time)) return(TRUE)
        }
        return(FALSE)
      })
    ) %>% 
    mutate(weekday = name) %>% 
    select(safegraph_place_id, weekday, hour, is_open)
  
  open_hours_df
}


# Computes sector open hours if at least 50% of sector places are open for given hour
get_sector_open_hours <- function(places_hourly_open_raw_df, places_df) {
  places_hourly_open_raw_df %>% 
    inner_join(
      places_df %>% select(safegraph_place_id, type),
      by = "safegraph_place_id"
    ) %>% 
    group_by(weekday, hour, type) %>% 
    summarize(count_open = sum(is_open), count_total=n()) %>% 
    mutate(
      frac_open = count_open / count_total,
      is_open = frac_open > 0.5
    )
}


# Imputes missing open hours using sector open hours
impute_open_hours <- function(places_hourly_open_raw_df, sector_hourly_open_df, places_df) {
  rbind(
    places_hourly_open_raw_df %>% mutate(imputed = FALSE),
    places_df %>% 
      filter(!(safegraph_place_id %in% places_hourly_open_raw_df$safegraph_place_id)) %>% 
      select(safegraph_place_id, type) %>% 
      merge(
        ., 
        sector_hourly_open_df %>% select(type, weekday, hour, is_open),
        all=TRUE
      ) %>% 
      select(-type) %>% 
      mutate(imputed = TRUE)
  ) %>% 
    mutate(
      weekday = case_when(
        weekday == "Mon" ~ 2,
        weekday == "Tue" ~ 3,
        weekday == "Wed" ~ 4,
        weekday == "Thu" ~ 5,
        weekday == "Fri" ~ 6,
        weekday == "Sat" ~ 7,
        weekday == "Sun" ~ 1
      )
    ) 
}


# Filters hourly data to remove places without continuous 
filter_continuous_hourly <- function(hourly_df) {
  max_num_hours <- 
    as.integer(
      ymd(max(hourly_df$date)) - ymd(min(hourly_df$date)) + 1
    ) * 24
  
  continuous_place_ids <- hourly_df %>% 
    group_by(safegraph_place_id) %>% 
    summarize(num_hours = n()) %>% 
    filter(num_hours == max_num_hours) %>% 
    .$safegraph_place_id
  
  hourly_df %>% 
    filter(safegraph_place_id %in% continuous_place_ids)
}


# Join hourly, place, and open hour data
join_hourly_with_place_open <- function(hourly_df, places_df, places_hourly_open_df) {
  hourly_df %>% 
    inner_join(
      places_df %>% select(-open_hours), 
      by = "safegraph_place_id"
    ) %>%
    mutate(weekday = lubridate::wday(lubridate::ymd(date))) %>% 
    left_join(
      places_hourly_open_df, 
      by = c("safegraph_place_id", "weekday", "hour")
    )
}


# Truncates square footage, visitor arrivals, and median dwell outliers.
truncate_hourly_outliers <- function(hourly_df) {
  # area 5th, 95th percentile by sector
  # group_by(type)
  area_percentile_df <- 
    hourly_df %>% 
    select(safegraph_place_id, type, area_square_feet) %>% 
    unique() %>% 
    select(-safegraph_place_id) %>% 
    group_by(type) %>% 
    summarize(
      area_5_percentile = quantile(area_square_feet, 0.05),
      area_95_percentile = quantile(area_square_feet, 0.95)
    )
  
  # hour visitors 95th percentile for visitors in that date, hour
  # group_by(type, date, hour)
  visitor_percentile_df <- 
    hourly_df %>% 
    select(type, date, hour, hourly_visits) %>% 
    group_by(type, date, hour) %>% 
    summarize(
      visitor_95_percentile = quantile(hourly_visits, 0.95)
    )
  
  # median_dwell 90th percentile by sector for that period
  # group_by(type, date, hour)
  dwell_percentile_df <- 
    hourly_df %>% 
    select(type, date, hour, median_dwell) %>% 
    group_by(type, date, hour) %>% 
    summarize(
      dwell_90_percentile = quantile(median_dwell, 0.90)
    )
  
  truncate_df <- 
    hourly_df %>% 
    left_join(
      area_percentile_df, 
      by = "type"
    ) %>% 
    left_join(
      visitor_percentile_df,
      by = c("type", "date", "hour")
    ) %>% 
    left_join(
      dwell_percentile_df,
      by = c("type", "date", "hour")
    ) %>% 
    mutate(
      area_square_feet = case_when(
        area_square_feet < area_5_percentile ~ area_5_percentile,
        area_square_feet > area_95_percentile ~ area_95_percentile,
        TRUE ~ area_square_feet
      ),
      hourly_visits = ifelse(
        hourly_visits > visitor_95_percentile,
        visitor_95_percentile,
        hourly_visits
      ),
      median_dwell = ifelse(
        median_dwell > dwell_90_percentile,
        dwell_90_percentile,
        median_dwell
      )
    ) %>%
    select(
      -c(
        "area_5_percentile",
        "area_95_percentile",
        "visitor_95_percentile",
        "dwell_90_percentile"
      )
    )
}


# Addds utc hour column for specified time zone given date and hour.
add_utc_hour <- function(hourly_df, tz = "America/Los_Angeles") {
  hourly_df  %>% 
    mutate(
      utc_hour = hour(
        with_tz(
          ymd_h(paste0(date, " ", hour - 1), tz=tz), 
          tz="UTC"
        )
      ) + 1
    )
}


# Lags median dwell time for each POI until the first occurence of midnight UTC.
# Warning: overwrites median dwell time.
lag_median_dwell_to_utc_boundary <- function(hourly_df) {
  lag_n <- 
    HOURS_PER_DAY - (
      hourly_df %>% 
        arrange(date, hour) %>% 
        .[1, ] %>% 
        .$utc_hour
    ) + 1
  
  hourly_df %>% 
    group_by(safegraph_place_id) %>% 
    mutate(median_dwell = lag(median_dwell, lag_n))
}


# Smears hourly visitor arrivals assuming each stays for median dwell
# time. Does not smear across the UTC day boundary.
smear_hourly_visits <- function(hourly_df) {
  # precompute number of smears for each POI-date-hour.
  to_smear_df <- 
    hourly_df %>% 
    filter(!is.na(median_dwell)) %>% 
    mutate(
      # don't smear any hour across UTC day boundary.
      num_smears = ifelse(
        HOURS_PER_DAY - utc_hour < ceiling(median_dwell / MINUTES_PER_HOUR), 
        HOURS_PER_DAY - utc_hour, 
        ceiling(median_dwell / MINUTES_PER_HOUR)
      )
    )

  # smear hourly visits
  max_num_smears <- max(to_smear_df$num_smears)
  smear_add_df <- NULL
  # group all smearing operations by how many hours they shift into the future.
  # reduces the number of data frames being merged by rbind. maximum number of 
  # loop iterations is 24, since we don't roll over UTC day boundaries.
  for (idx in 1:max_num_smears) {
    tmp <- 
      to_smear_df %>% 
      filter(idx <= num_smears) %>% 
      select(safegraph_place_id, date, hour, hourly_visits, median_dwell, num_smears) %>% 
      mutate(
        date = as_date(ifelse(hour >= HOURS_PER_DAY - (idx - 1), date + 1, date)),
        hour = ((hour + idx - 1) %% HOURS_PER_DAY) + 1,
        # compute fraction of hourly visitors to smear to this hour, capped at 1.
        dwell_modifier = ifelse(
          (median_dwell - ((idx - 1) * MINUTES_PER_HOUR)) / MINUTES_PER_HOUR < 1, 
          (median_dwell - ((idx - 1) * MINUTES_PER_HOUR)) / MINUTES_PER_HOUR, 
          1
        ),
        hourly_visits_add = hourly_visits * dwell_modifier
      ) %>% 
      select(safegraph_place_id, date, hour, hourly_visits_add)
    smear_add_df <- rbind(smear_add_df, tmp)
  }
  smear_add_df <- 
    smear_add_df %>% 
    arrange(safegraph_place_id, date, hour, hourly_visits_add) %>% 
    group_by(safegraph_place_id, date, hour) %>% 
    summarize(total_visits_add = sum(hourly_visits_add))
  
  smeared_hourly_df <- 
    hourly_df %>% 
    left_join(smear_add_df, by = c("safegraph_place_id", "date", "hour")) %>% 
    mutate(
      total_visits_add = ifelse(is.na(total_visits_add), 0, total_visits_add),
      hourly_visits_smeared = hourly_visits + total_visits_add
    )

  smeared_hourly_df
}


# Applies dwell time correction factor.
# See Chang et al. Supplementary Materials 3.1.
apply_dwell_correction_factor <- function(smeared_hourly_df) {
  smeared_hourly_df %>% 
    mutate(
      correction_factor = ifelse(is.na(median_dwell), 0, (median_dwell / 60) / ((median_dwell / 60) + 1)),
      avg_hourly_visits = hourly_visits_smeared * correction_factor
    ) %>% 
    select(-correction_factor)
}
