# 05_modeling.R

# =============================
# 📊 Modeling the Link Between Remote Work and Subway Ridership Decline
# =============================

library(tidyverse)
library(broom)
library(scales)
library(readr)

# Load merged ZIP-level dataset
acs <- read_csv("data/cleaned/acs_joined.csv") %>%
  mutate(zip_code = as.character(zip_code))
ridership <- read_csv("data/cleaned/mta_zip_summary.csv") %>%
  mutate(zip_code = as.character(zip_code))
mta_with_zip <- read_csv("data/cleaned/mta_with_zip.csv") %>%
  mutate(zip_code = as.character(zip_code))

# 1. Get borough info by ZIP
borough_by_zip <- mta_with_zip %>%
  select(zip_code, borough) %>%
  distinct()

# 2. Merge all modeling data
model_data <- left_join(ridership, acs, by = "zip_code") %>%
  left_join(borough_by_zip, by = "zip_code") %>%
  filter(!is.na(wfh_shift) & !is.na(ridership_pct_change))

# =============================
# 3. Simple Linear Regression
# =============================
model1 <- lm(ridership_pct_change ~ wfh_shift, data = model_data)
summary(model1)

# =============================
# 4. Add Borough Fixed Effects
# =============================
model2 <- lm(ridership_pct_change ~ wfh_shift + borough, data = model_data)
summary(model2)

# =============================
# 5. Visualization: Fit Plot
# =============================
ggplot(model_data, aes(x = wfh_shift, y = ridership_pct_change)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Remote Work vs Subway Ridership Decline (with Borough Control)",
       x = "% Change in Work From Home (2019–2023)",
       y = "% Change in Subway Ridership (2019–2023)") +
  theme_minimal()

# =============================
# 6. Save model summary as CSV
# =============================
write_csv(tidy(model1), "data/cleaned/model1_summary.csv")
write_csv(tidy(model2), "data/cleaned/model2_with_borough_summary.csv")

message("\n✅ Linear models (with and without borough) fitted and saved.")
