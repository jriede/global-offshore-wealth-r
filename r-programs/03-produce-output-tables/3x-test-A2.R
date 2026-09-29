
library(dplyr)
library(tidyr)

small_ofcs_by_country_r <- host_year %>%
  filter(host %in% c(815, 1003, 1100)) %>%
  group_by(year, host, hostname) %>%
  summarise(
    equity = sum(eqliab_host, na.rm = TRUE) / 1000,
    debt   = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year, host)

# Auffällige Jahre
small_ofcs_by_country_r %>%
  filter(year %in% c(2001, 2005, 2014, 2021)) %>%
  print(n = Inf)

# Export für den Vergleich
write.csv(
  small_ofcs_by_country_r,
  "small_ofcs_by_country_r.csv",
  row.names = FALSE
)

