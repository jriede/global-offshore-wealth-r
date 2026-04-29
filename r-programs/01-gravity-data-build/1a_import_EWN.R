# ------------------------------------------------------------------------------
# Project: Offshore financial wealth database - update 2023
# jlenke, 2026-04-14
# Purpose: import EWN-dataset_12-2022
# This version: translated from Stata
# ------------------------------------------------------------------------------

library(readxl)
library(dplyr)

# Import external wealth of nations dataset
ewn <- read_excel(
  path = file.path(raw, "EWN", "EWN-dataset_12-2022.xlsx"),
  sheet = "Dataset"
)

# Rename variables
ewn <- ewn %>%
  rename(
    year = Year,
    source = IFS_Code,
    aequity = "Portfolio equity assets",
    lequity = "Portfolio equity liabilities",
    adebt = "Debt assets",
    ldebt = "Debt liabilities",
    aportif_debt = "Portfolio debt assets",
    lportif_debt = "Portfolio debt liabilities",
    country = Country,
    gdp_us = "GDP (US$)"
  ) %>%
  mutate(
    ewn22 = 1
  ) %>%
  filter(year >= 2001) %>%
  select(
    year, source, country, aequity, adebt, aportif_debt,
    ewn22, lequity, lportif_debt, gdp_us
  )

# Harmonise IFS code
ewn <- ewn %>%
  mutate(
    source = if_else(source == 379, 371, source)
  ) %>%
  filter(country != "ECCU", country != "Euro Area")

# Preserve equivalent: create temporary Curacao/Sint Maarten aggregate
curacao <- ewn %>%
  filter(source %in% c(354, 352)) %>%
  mutate(
    source = 355
  ) %>%
  group_by(source, year) %>%
  summarise(
    country = first(country),
    ewn22 = first(ewn22),
    aequity = sum(aequity, na.rm = TRUE),
    lequity = sum(lequity, na.rm = TRUE),
    adebt = sum(adebt, na.rm = TRUE),
    gdp_us = sum(gdp_us, na.rm = TRUE),
    aportif_debt = sum(aportif_debt, na.rm = TRUE),
    lportif_debt = sum(lportif_debt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(aequity, lequity, adebt, gdp_us, aportif_debt, lportif_debt),
      ~ na_if(., 0)
    ),
    country = "Curacao and Sint Maarten"
  )

# Drop original Curacao and Sint Maarten rows and append combined row
ewn <- ewn %>%
  filter(!source %in% c(354, 352)) %>%
  bind_rows(curacao)

# Save main dataset
saveRDS(ewn, file = file.path(work, "data_ewn_update.rds"))
write_dta(ewn, file.path(work, "data_ewn_update.dta"))

# Keep GDP subset and save
ewn_gdp <- ewn %>%
  select(country, source, year, gdp_us)

saveRDS(ewn_gdp, file = file.path(work, "ewn_gdp.rds"))
write_dta(ewn_gdp, file.path(work, "ewn_gdp.dta"))
# ------------------------------------------------------------------------------
