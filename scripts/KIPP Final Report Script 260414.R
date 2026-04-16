library(tidyverse)

# Load cleaned long-format data
data <- read_csv("cleaneddata.csv")

# Fix county name inconsistency: Insurance/income files use "Adams County", immunization uses "Adams"
# Cleaning the cleaned data lol
data <- data %>%
  mutate(County = str_remove(County, " County$"))

# Pivot wide for analysis (one row per county)
data_wide <- data %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  rename(
    uninsured = `Children without Health Insurance`,
    income = `Household Income ($)`,
    hpv_rate = `Up-To-Date Percent`
  )

# Summary Stats
summary(data_wide %>% select(uninsured, income, hpv_rate))

# Correlation Tests
# HPV immunization rate vs. median household income
cor.test(data_wide$hpv_rate, data_wide$income, method = "pearson")

# HPV immunization rate vs. uninsured rate
cor.test(data_wide$hpv_rate, data_wide$uninsured, method = "pearson")

# Multiple Linear Regression
model <- lm(hpv_rate ~ income + uninsured, data = data_wide)
summary(model)

# Checking model assumptions
par(mfrow = c(2, 2))
plot(model)

# Figure 1: HPV Rate vs. Median Household Income
ggplot(data_wide, aes(x = income, y = hpv_rate)) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Teen HPV Immunization Rate vs. Median Household Income",
    subtitle = "Colorado Counties",
    x = "Median Household Income ($)",
    y = "Up-to-Date HPV Immunization Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figure1_income_vs_hpv.png", width = 8, height = 6, dpi = 300)

# Figure 2: HPV Rate vs. Uninsured Rate
ggplot(data_wide, aes(x = uninsured, y = hpv_rate)) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Teen HPV Immunization Rate vs. Children Without Health Insurance",
    subtitle = "Colorado Counties",
    x = "Proportion of Children Without Health Insurance",
    y = "Up-to-Date HPV Immunization Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figure2_uninsured_vs_hpv.png", width = 8, height = 6, dpi = 300)

