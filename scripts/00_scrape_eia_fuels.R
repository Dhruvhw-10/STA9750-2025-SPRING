# ── 📦 Load Libraries ──
library(rvest)
library(dplyr)
library(stringr)
library(readr)

# ── 🔗 EIA CO2 emissions page ──
url <- "https://www.eia.gov/environment/emissions/co2_vol_mass.php"

# ── 📥 Scrape and Parse Table ──
co2_fuel_factors <- read_html(url) %>%
  html_elements("table") %>%
  .[[1]] %>%
  html_table() %>%
  select(Fuel = 1, kg_per_unit = 2) %>%
  mutate(
    Fuel = str_trim(Fuel),
    kg_per_unit = parse_number(kg_per_unit),
    CO2_lb_per_unit = kg_per_unit * 2.20462  # Convert kg → lbs
  ) %>%
  filter(!is.na(kg_per_unit))  # Remove non-numeric rows

# ── 📂 Create Output Directory ──
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ── 💾 Save Clean CSV ──
write_csv(co2_fuel_factors, "data/processed/eia_co2_fuel_factors.csv")

# ── 👀 Preview ──
print(co2_fuel_factors)
