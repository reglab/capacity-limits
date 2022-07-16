
pre_2019_start_date <- mdy("10-28-2019")
pre_2019_end_date <- mdy("12-08-2019")

post_2019_start_date <- mdy("12-09-2019")
post_2019_end_date <- mdy("01-19-2020")

pre_2020_start_date <- mdy("10-26-2020")
pre_2020_end_date <- mdy("12-06-2020")

post_2020_start_date <- mdy("12-07-2020")
post_2020_end_date <- mdy("01-17-2021")

# Baseline occupancy period
academic_baseline_start_date <- ymd("2020-03-01")
academic_baseline_end_date <- ymd("2020-05-02")

period_baseline_start_week_date <- ymd("2020-02-24")
period_baseline_end_week_date <- ymd("2020-04-27")

gross_to_net_scale <- 1 # 0.85

period_2019_start_week_date <- "2019-09-30"
period_2019_end_week_date <- "2020-01-20"

period_2020_start_week_date <- "2020-09-28"
period_2020_end_week_date <- "2021-01-18"

dst_days <- c(
  "2021-03-14",
  "2020-11-01",
  "2020-03-08",
  "2019-11-03"
)

HOURS_PER_DAY <- 24
MINUTES_PER_HOUR <- 60
MERCANTILE_OCCUPANCY_LOAD <- 60

big_box_regex <- "Target|The Home Depot|Kmart|Walmart|Best Buy|Lowe's|IKEA"

sector_factor_levels = c(
  "grocery",
  "pharmacy",
  "big_box"
)

sector_factor_labels = c(
  "Grocery Stores",
  "Pharmacies",
  "General Merchandise Stores"
)

sector_colors = c(
  "Grocery Stores" = '#d95f02',
  "Pharmacies" = '#1b9e77',
  "General Merchandise Stores" = '#7570b3'
)

sector_period_colors = c(
  "Before Dec. 6_General Merchandise Stores" = '#b8b0e0',
  "Before Dec. 6_Pharmacies"= '#85cdb0',
  "Before Dec. 6_Grocery Stores" = '#f5aa7b',
  "After Dec. 7_General Merchandise Stores" = '#7570b3',
  "After Dec. 7_Pharmacies" = '#1b9e77',
  "After Dec. 7_Grocery Stores" = '#d95f02'
)

county_factor_levels = c(
  "Alameda",
  "Contra Costa",
  "Marin",
  "Napa",
  "San Francisco",
  "San Mateo",
  "Santa Clara",
  "Solano",
  "Sonoma"
)

county_colors = c(
  "#2010A7",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#DE3232",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#AB170C"
)

# Paths and directories
root_path <- # Root path for input data.
sg_hourly_dir <- # Set this path to point to the directory containing the normalized hourly visit datasets.
sg_poi_ca_dir <- # Set this path to point to the directory containing the SafeGraph Core POI/Places datasets for October 2020, filtered to CA.

social_distancing_path_20 <- # Set this path to point to the SafeGraph 2020 social distancing metrics for bay area counties.
social_distancing_path_21 <- # Set this path to point to the SafeGraph 2021 social distancing metrics for bay area counties.

repo_path <- # Set this to be the absolute path to the code release repo.
data_dir <- "data/"
data_path <- paste0(repo_path, data_dir)

nytimes_case_path <- # Set this path to point to the NYTimes COVID case count data.