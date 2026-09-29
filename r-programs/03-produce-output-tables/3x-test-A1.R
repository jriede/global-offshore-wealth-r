library(readr)
library(dplyr)
data_full_matrices <- read_work_data("data_full_matrices")


g_by_country <- data_full_matrices %>%
  filter(cpis == 1) %>%
  group_by(year, source, sourcename) %>%
  summarise(
    eq_raw = sum(eqasset, na.rm = TRUE),
    eq_aug = sum(augmeqasset, na.rm = TRUE),
    debt_raw = sum(debtasset, na.rm = TRUE),
    debt_aug = sum(augmdebtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    eq_corr = (eq_aug - eq_raw) / 1000,
    debt_corr = (debt_aug - debt_raw) / 1000
  ) %>%
  filter(
    !source %in% c(377, 924),
    year %in% 2018:2021
  )

g_by_country %>%
  group_by(source, sourcename) %>%
  summarise(
    eq_corr = sum(eq_corr),
    debt_corr = sum(debt_corr),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(eq_corr))) %>%
  print(n = 30)


g_by_country %>%
  filter(
    source %in% c(1012, 316, 313, 283, 299)
  ) %>%
  select(
    year, source, sourcename,
    eq_raw, eq_aug, eq_corr,
    debt_raw, debt_aug, debt_corr
  ) %>%
  arrange(year, source) %>%
  print(n = Inf, width = Inf, digits = 12)




gravity_saved %>%
  filter(
    source == 1012,
    year %in% 2018:2021
  ) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    eqasset = sum(eqasset, na.rm = TRUE),
    debtasset = sum(debtasset, na.rm = TRUE),
    n_eq_nonmissing = sum(!is.na(eqasset)),
    n_debt_nonmissing = sum(!is.na(debtasset)),
    .groups = "drop"
  ) %>%
  print(n = Inf)



r_A1_G <- g_by_country %>%
  select(year, source, eq_corr, debt_corr)

write.csv(
  r_A1_G,
  "r_A1_G_by_country.csv",
  row.names = FALSE
)
