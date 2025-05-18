# 04_visualizations.R

# =============================
# 📈 Visualization Script for All 4 Subquestions
# =============================

library(tidyverse)
library(gganimate)
library(ggthemes)
library(scales)
library(sf)
library(viridis)

# Load data
mta_hourly <- readRDS("data/cleaned/mta_hourly_cleaned.rds")
daily_ridership <- read_csv("data/cleaned/daily_ridership_by_station.csv")
hourly_summary <- read_csv("data/cleaned/hourly_patterns_by_era.csv")
station_era_summary <- read_csv("data/cleaned/station_ridership_era_comparison.csv")
acs_joined <- read_csv("data/cleaned/acs_joined.csv")
mta_zip_summary <- read_csv("data/cleaned/mta_zip_summary.csv")

# =============================
# Load Census ZCTA shapefile (already downloaded)
# =============================
zip_shapefile <- st_read("data/tl_2020_us_zcta510.shp")

library(sf)
library(dplyr)
library(lubridate)

# 1. Load shapefile
zip_shapes <- st_read("data/tl_2020_us_zcta510.shp", quiet = TRUE)

# 2. Transform to WGS84 CRS
zip_shapes <- st_transform(zip_shapes, crs = 4326)

# 3. Convert station coordinates to spatial layer
station_coords <- mta_hourly_filtered %>%
  select(station_complex_id, latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 4. Spatial join: assign ZIP codes
station_with_zip <- st_join(station_coords, zip_shapes, join = st_within) %>%
  st_drop_geometry() %>%
  select(station_complex_id, zip_code = ZCTA5CE10)

station_with_zip <- read_csv("data/cleaned/station_with_zip.csv")

# 5. Convert hourly → daily ridership
mta_daily <- mta_hourly_filtered %>%
  mutate(date = as_date(transit_timestamp)) %>%
  group_by(station_complex_id, station_complex, borough, date) %>%
  summarise(daily_ridership = sum(ridership, na.rm = TRUE), .groups = "drop")

# 6. Join with ZIPs + assign COVID era
mta_daily_with_zip <- mta_daily %>%
  left_join(station_with_zip, by = "station_complex_id") %>%
  filter(!is.na(zip_code)) %>%
  mutate(
    year = year(date),
    covid_era = case_when(
      year %in% c(2020, 2021) ~ "Core COVID",
      year %in% c(2022, 2023) ~ "WFH Era",
      TRUE ~ "Other"
    )
  )

# 7. Save cleaned output
write_csv(mta_daily_with_zip, "data/cleaned/daily_ridership_by_station_zip.csv")

library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(scales)
library(lubridate)

# Load cleaned daily ridership by station ZIP
daily_ridership <- read_csv("data/cleaned/daily_ridership_by_station_zip.csv")

# Map ZIPs to area labels
zip_labels <- c(
  "10004" = "FiDi",
  "10022" = "Midtown East"
)
office_zips <- names(zip_labels)

# Filter and smooth data
office_daily <- daily_ridership %>%
  filter(zip_code %in% office_zips, covid_era %in% c("Core COVID", "WFH Era")) %>%
  mutate(
    area = recode(as.character(zip_code), !!!zip_labels),
    date = as_date(date)
  ) %>%
  arrange(area, date) %>%
  group_by(area) %>%
  mutate(smoothed_riders = rollmean(daily_ridership, k = 7, fill = NA)) %>%
  ungroup()

# Create faceted line plot
final_plot <- ggplot(office_daily, aes(x = date, y = smoothed_riders)) +
  geom_line(color = "#1f77b4", size = 1) +
  facet_wrap(~ area, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = as.Date("2021-06-15"), linetype = "dashed", color = "gray50") +
  geom_text(data = data.frame(
    area = c("FiDi", "Midtown East"),
    date = rep(as.Date("2021-06-15"), 2),
    label = rep("Reopening", 2),
    y = c(19000, 36000)
  ), aes(x = date, y = y, label = label), inherit.aes = FALSE,
  angle = 90, size = 3, hjust = -0.2, color = "gray40") +
  labs(
    title = "Remote Work Flattened Subway Usage in Manhattan's Business Districts",
    subtitle = "7-day average subway ridership in FiDi and Midtown East (2020–2023)",
    x = "Date", y = "7-Day Avg. Subway Riders"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13)

# Save plot as PNG
ggsave("plots/subq2_office_faceted_final.png", final_plot, width = 10, height = 6)

# =============================
# 2. Hourly Ridership Pattern Animation
# =============================
# Drop rows with NA or non-numeric hour values
hourly_summary_clean <- hourly_summary %>%
  filter(!is.na(hour), !is.na(avg_hourly_riders)) %>%
  arrange(hour)

# Create animated plot with cleaned data
animated_hourly <- ggplot(hourly_summary_clean, 
                          aes(x = hour, y = avg_hourly_riders, 
                              color = interaction(covid_era, day_type), 
                              group = interaction(covid_era, day_type))) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Subway Ridership Patterns by COVID Era and Day Type",
    x = "Hour of Day", y = "Average Riders", color = "Era + Day Type"
  ) +
  theme_minimal() +
  transition_reveal(hour)
# Render and save
library(gifski)
animated_hourly_rendered <- animate(animated_hourly,
                                    width = 800, height = 500, fps = 15,
                                    renderer = gifski_renderer("plots/hourly_pattern_animation.gif"))

# =============================
# 3. Top 10 Stations with Ridership Drop
# =============================
top_drops <- station_era_summary %>%
  arrange(pct_change_covid_to_wfh) %>%
  slice(1:10) %>%
  mutate(station_complex = fct_reorder(station_complex, pct_change_covid_to_wfh))

drop_plot <- ggplot(top_drops, aes(x = station_complex, y = pct_change_covid_to_wfh)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Top 10 Stations by % Drop in Ridership",
       x = "Station", y = "% Change from Core COVID to WFH Era") +
  theme_minimal()

# =============================
# 4. Map: Remote Work Shift by ZIP Code
# =============================
# Step 1: Prepare ZIP shapefile for joining
zip_shapefile <- zip_shapefile %>%
  mutate(zip_code = as.character(ZCTA5CE10))  # rename and ensure character

# Step 2: Join with ACS data
zip_combined_data <- left_join(zip_shapefile, acs_joined, by = "zip_code") %>%
  filter(!is.na(wfh_shift))

install.packages("scico")
library(scico)

# ✅ Assign map to a variable so you can save it
zip_map <- ggplot(data = zip_combined_data) +
  geom_sf(aes(fill = wfh_shift), color = "black", size = 0.15) +
  coord_sf(xlim = c(-74.3, -73.6), ylim = c(40.45, 40.95)) +
  scale_fill_scico(
    palette = "vik",
    midpoint = 0,
    limits = c(-0.1, 0.1),
    oob = scales::squish,
    name = "WFH Shift"
  ) +
  labs(
    title = "NYC ZIP Code Remote Work Shift (2019–2023)",
    subtitle = "WFH shifts exaggerated for clarity",
    fill = "WFH Shift"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey95", color = NA)
  )

# =============================
# 5. Scatter Plot: WFH Shift vs Ridership Decline
# =============================
mta_zip_summary <- mta_zip_summary %>%
  mutate(zip_code = as.character(zip_code))

zip_combined_data <- left_join(mta_zip_summary, acs_joined, by = "zip_code") %>%
  filter(!is.na(wfh_shift) & !is.na(ridership_pct_change))

scatter_plot <- ggplot(zip_combined_data, aes(x = wfh_shift, y = ridership_pct_change)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Remote Work Growth vs. Subway Ridership Decline",
       x = "Change in % Working from Home (2019–2023)",
       y = "% Change in Subway Ridership (2019–2023)") +
  theme_minimal()

# =============================
# Save all outputs
# =============================
if (!dir.exists("plots")) dir.create("plots")

ggsave("plots/subq2_office_faceted_final.png", plot = final_plot, width = 8, height = 5)
ggsave("plots/top_station_drops.png", plot = drop_plot, width = 8, height = 5)
ggsave("plots/zip_map_wfh_shift.png", plot = zip_map, width = 8, height = 6)
ggsave("plots/wfh_vs_ridership_scatter.png", plot = scatter_plot, width = 8, height = 5)

anim_save("plots/hourly_pattern_animation.gif", animated_hourly)

message("\n✅ Visualizations for all 4 subquestions — including map + scatter — have been saved!")
