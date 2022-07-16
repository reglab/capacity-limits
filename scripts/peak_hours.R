library(tidyverse)

scc_2019_path <- # Set this path to point to `scc_preprocessed_2019_df.rds` output by the preprocessing notebook.
scc_2020_path <- # Set this path to point to `scc_preprocessed_2020_df.rds` output by the preprocessing notebook.

scc_2019 <- readRDS(scc_2019_path)
scc_2020 <- readRDS(scc_2020_path)

hourly_capped <- scc_2019 %>%
  rbind(scc_2020)

DiD_sectors <- hourly_capped %>%
  filter(type %in% c("big_box","grocery","pharmacy"))

relevant_dates <- DiD_sectors %>%
  filter((date > '2019-10-05' & date < '2020-01-20') | (date > '2020-10-03' & date < '2021-01-18'))

distribution <- relevant_dates %>%
  mutate(treatment=ifelse((date>'2020-01-19'),"2020","2019"),
         time=ifelse(date>'2020-12-6'|(date > '2019-12-8'&date<'2020-01-20') , "December/January", "October/November")) %>%
  group_by(type,
           hour,
           treatment,
           time) %>%
  summarize(avg_hourly_visits=mean(hourly_visits_smeared)) %>%
  transform(time=factor(time,levels=c("October/November", "December/January")))

sector_period_colors = c(
  "Before December 6th_General Merchandise Stores" = '#b8b0e0',
  "Before December 6th_Pharmacies"= '#85cdb0',
  "Before December 6th_Grocery Stores" = '#f5aa7b',
  "After December 6th_General Merchandise Stores" = '#7570b3',
  "After December 6th_Pharmacies" = '#1b9e77',
  "After December 6th_Grocery Stores" = '#d95f02'
)

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

or_hist_df <- distribution %>%
  mutate(sector=type,
         sector_name=ifelse(type=="big_box","General Merchandise Stores",ifelse(type=="grocery","Grocery Stores", "Pharmacies")),
         year=treatment,
         period_f=ifelse(time=="December/January","After December 6th","Before December 6th")) %>%
  transform(sector=factor(sector,levels=sector_factor_levels, labels=sector_factor_labels)) %>%
  unite("period_sector",period_f,sector_name,remove=FALSE) 

hist_period_labels <- data.frame(
  period_f = rep(c("Before December 6th", "After December 6th"), 3),
  x = rep(c(1, 3), 3),
  y = c(120,140,20,23,20,23),
  year = rep("2019", 6),
  sector = c("big_box", "big_box", "pharmacy", "pharmacy", "grocery", "grocery"),
  period_sector = c("Before December 6th_General Merchandise Stores", "After December 6th_General Merchandise Stores", "Before December 6th_Pharmacies", "After December 6th_Pharmacies", "Before December 6th_Grocery Stores", "After December 6th_Grocery Stores")
)

hist_period_labels$sector <-
  factor(
    hist_period_labels$sector,
    levels = sector_factor_levels,
    labels = sector_factor_labels
  )

hourly_visits <- 
  ggplot() +
  geom_bar(
    data = or_hist_df %>% 
      filter(period_f == "Before December 6th" & year == "2020" & type == "pharmacy"),
    aes(x = hour, y = avg_hourly_visits), stat="identity", fill = "#85cdb0"
  ) +
  geom_bar(
    data = or_hist_df %>% 
      filter(period_f == "Before December 6th" & year == "2020" & type == "pharmacy" & hour %in% c(17, 18)),
    aes(x = hour, y = avg_hourly_visits), stat="identity",  fill = "#1b9e77"
  ) +
  xlab("Hour of the Day") +
  ylab("Average Hourly Visits") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3), labels = scales::comma) +
  theme_classic() +
  theme(
    text = element_text(size=20),
    legend.position = "none",
    strip.text.y = element_text(angle = 0),
    strip.background = element_blank()
  ) +
  ggtitle("Peak Hours for Pharmacies in October and November 2020")

hourly_visits
