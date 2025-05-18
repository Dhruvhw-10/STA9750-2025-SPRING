# 03_data_aggregation.R

# =============================
# 📊 Data Aggregation & Summary Tables
# =============================

# Load libraries
library(tidyverse)
library(lubridate)

# Load cleaned hourly ridership data
mta_hourly <- readRDS("data/cleaned/mta_hourly_cleaned.rds")

# =============================
# 1. Daily totals by station and COVID era
# =============================
daily_ridership <- mta_hourly |> 
  group_by(station_complex, borough, date, covid_era, day_type) |> 
  summarise(daily_ridership = sum(ridership, na.rm = TRUE), .groups = "drop")

# =============================
# 2. Average weekday vs weekend ridership by COVID era
# =============================
weekday_summary <- daily_ridership |> 
  group_by(covid_era, day_type) |> 
  summarise(avg_daily_riders = mean(daily_ridership), .groups = "drop")

# =============================
# 3. Station-level change from Pre-COVID to WFH
# =============================
station_era_summary <- daily_ridership |> 
  group_by(station_complex, covid_era) |> 
  summarise(mean_ridership = mean(daily_ridership), .groups = "drop") |> 
  pivot_wider(names_from = covid_era, values_from = mean_ridership) |> 
  filter(!is.na(`Core COVID`) & !is.na(`WFH Era`)) |> 
  mutate(
    pct_change_covid_to_wfh = (`WFH Era` - `Core COVID`) / `Core COVID`
  )

# =============================
# 4. Hourly patterns across COVID eras
# =============================
hourly_summary <- mta_hourly |> 
  group_by(covid_era, day_type, hour) |> 
  summarise(avg_hourly_riders = mean(ridership), .groups = "drop")

# =============================
# 5. Save aggregated summaries
# =============================
write_csv(daily_ridership, "data/cleaned/daily_ridership_by_station.csv")
write_csv(weekday_summary, "data/cleaned/weekday_vs_weekend_summary.csv")
write_csv(station_era_summary, "data/cleaned/station_ridership_era_comparison.csv")
write_csv(hourly_summary, "data/cleaned/hourly_patterns_by_era.csv")
write_csv(mta_zip_summary, "data/cleaned/mta_zip_summary.csv")
write_csv(mta_with_zip, "data/cleaned/mta_with_zip.csv")

message("\n✅ Aggregated summaries saved! Ready for visualization and spatial joining.")
