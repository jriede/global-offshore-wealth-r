library(dplyr)
library(readr)

countries <- read_work_data("countries")

names(countries)
head(countries)


global_offshore_R <- countries %>%
  filter(indicator == "total", year %in% 2001:2021) %>%
  group_by(year) %>%
  summarise(
    offshore_wealth = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

print(global_offshore_R, n = 21)


options(digits = 16)

print(global_offshore_R, n = 21, width = Inf)

readr::write_csv(
  global_offshore_R,
  "global_offshore_R.csv"
)




global_offshore_R <- countries %>%
  filter(
    indicator == "total",
    year >= 2001,
    year <= 2021
  ) %>%
  group_by(year) %>%
  summarise(
    offshore_wealth_R = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  global_offshore_R,
  "global_offshore_R.csv"
)

print(global_offshore_R, n = 21)
