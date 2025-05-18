# 01_data_loading.R

# ======================
# 🚀 Data Loading Script (Web-Filtered: 2020–2023 only)
# ======================

# Install required packages (only once)
install.packages(c("tidyverse", "readr", "janitor", "lubridate", "readxl", "sf"))


# Load Libraries
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(readxl)
library(sf)

# Create folder structure
if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)
if (!dir.exists("data/cleaned")) dir.create("data/cleaned", recursive = TRUE)

# =============================
# 1. Load MTA Subway Hourly Ridership (2020–2023 only, via API with encoded URL)
# =============================
library(readr)
library(janitor)

# Direct CSV API call with $limit
url <- URLencode("https://data.ny.gov/resource/wujg-7c2s.csv?$where=transit_timestamp between '2020-01-01T00:00:00' and '2023-12-31T23:59:59'&$limit=100000000")

mta_hourly_filtered <- read_csv(url) |>
  clean_names()

# Save filtered dataset for future use
saveRDS(mta_hourly_filtered, "data/cleaned/mta_hourly_filtered_2020_2023.rds")

# =============================
# 2. Load ACS Remote Work Data (2019 & 2023)
# =============================
acs_2019 <- read_csv("data/acs_b08128_2019.csv") |> clean_names()
acs_2023 <- read_csv("data/acs_b08128_2023.csv") |> clean_names()

# Clean & Join ACS Data
acs_2019_clean <- acs_2019 |> 
  filter(str_detect(name, "\\d{5}")) |>  # Keep rows with ZIPs
  select(zip_code = name, total = b08128_001e, wfh = b08128_009e) |> 
  mutate(
    zip_code = str_extract(zip_code, "\\d{5}"),
    total = parse_number(total),
    wfh = parse_number(wfh)
  ) |> 
  rename(total_2019 = total, wfh_2019 = wfh)

acs_2023_clean <- acs_2023 |>
  filter(str_detect(name, "\\d{5}")) |>  # Keep rows with ZIPs
  select(zip_code = name, total = b08128_001e, wfh = b08128_009e) |> 
  mutate(zip_code = str_extract(zip_code, "\\d{5}"),
         total = parse_number(total),
         wfh = parse_number(wfh)) |> 
  rename(total_2023 = total, wfh_2023 = wfh)

acs_joined <- left_join(acs_2019_clean, acs_2023_clean, by = "zip_code") |> 
  mutate(wfh_rate_2019 = wfh_2019 / total_2019,
         wfh_rate_2023 = wfh_2023 / total_2023,
         wfh_shift = wfh_rate_2023 - wfh_rate_2019)
write_csv(acs_joined, "data/cleaned/acs_joined.csv")

# =============================
# 3. Load MTA 2023 Ridership Report (from Excel)
# =============================
mta_2023 <- read_excel("data/mta_2023_ridership.xlsx", sheet = "Annual Total", skip = 1) |> 
  clean_names()

mta_2023_clean <- mta_2023 |> 
  select(station = station_alphabetical_by_borough,
         borough = s,
         `2019` = x2019, `2020` = x2020,
         `2021` = x2021, `2022` = x2022, `2023` = x2023) |> 
  filter(!is.na(station)) |> 
  filter(!str_detect(station, "(?i)weekday|weekend|total|average")) |> 
  mutate(borough = case_when(
    borough == "Bx" ~ "Bronx",
    borough == "M"  ~ "Manhattan",
    borough == "B"  ~ "Brooklyn",
    borough == "Q"  ~ "Queens",
    borough == "SI" ~ "Staten Island",
    TRUE ~ borough
  ))
station_coords <- mta_hourly_filtered %>%
  select(station = station_complex, latitude, longitude) %>%
  group_by(station) %>%
  summarise(across(c(latitude, longitude), ~mean(.x, na.rm = TRUE)), .groups = "drop")

# Join coordinates to 2023 ridership data
mta_2023_with_coords <- mta_2023_clean %>%
  left_join(station_coords, by = "station")

# Now convert to spatial object
mta_2023_with_coords <- mta_2023_with_coords |> 
  filter(!is.na(latitude), !is.na(longitude))

mta_sf <- st_as_sf(mta_2023_with_coords, coords = c("longitude", "latitude"), crs = 4326)

# =============================
# 4. Load Census ZCTA shapefile
# =============================
zip_shapefile <- st_read("data/tl_2020_us_zcta510.shp")

# Reproject MTA stations to match ZIP shapefile CRS
mta_sf <- st_transform(mta_sf, crs = st_crs(zip_shapefile))

# Now spatial join
mta_with_zip <- st_join(mta_sf, zip_shapefile, join = st_within) |> 
  mutate(zip_code = ZCTA5CE10) |> 
  st_drop_geometry()


# =============================
# 6. Aggregate by ZIP code
# =============================
mta_zip_summary <- mta_with_zip |> 
  group_by(zip_code) |> 
  summarise(
    ridership_2019 = sum(`2019`, na.rm = TRUE),
    ridership_2023 = sum(`2023`, na.rm = TRUE),
    ridership_pct_change = (ridership_2023 - ridership_2019) / ridership_2019
  )

# =============================
# 7. Save ZIP-level summary
# =============================
write_csv(mta_zip_summary, "data/cleaned/mta_zip_summary.csv")

# ✅ Summary Message
message("\n✅ All datasets loaded (2020–2023 subset), cleaned, and ready for analysis!")
