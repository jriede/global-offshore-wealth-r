# -----------------------------------------------------------------------------#
# Project: Offshore financial wealth database - update 2023
# Title: 3_do_table_A1.R
# Purpose: produce TABLE A1 "Global Cross-Border Securities Assets"
#          (total assets and corrections)
# Translation of 3_do_table_A1.do from Stata to R
# -----------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(haven)
library(openxlsx)

# Assumes that work_path and tables_path are already defined, e.g.
# work_path   <- "work-data"
# tables_path <- "tables"

myexcel <- file.path(tables, "FGZ_R_Appendix_Tables_A1-A3.xlsx")
sheet_name <- "Table A1_update"

# Helper: write a vector vertically into an existing Excel workbook.
# start_col can be either a column letter ("C") or a number.
write_vertical <- function(wb, sheet, x, start_col, start_row) {
  if (is.character(start_col)) {
    start_col <- openxlsx::convertFromExcelRef(start_col)
  }
  openxlsx::writeData(
    wb,
    sheet = sheet,
    x = data.frame(value = x),
    startCol = start_col,
    startRow = start_row,
    colNames = FALSE,
    rowNames = FALSE
  )
}

if (file.exists(myexcel)) {
  
  # Existing workbook
  wb <- loadWorkbook(myexcel)
  
  # Create sheet if it does not yet exist
  if (!(sheet_name %in% names(wb))) {
    addWorksheet(wb, sheet_name)
  }
  
} else {
  
  # Create new workbook
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  
}

# -----------------------------------------------------------------------------#
# Col. (1)
# -----------------------------------------------------------------------------#

#data_toteq_update <- read_dta(file.path(work2, "data_toteq_update.dta"))
data_toteq_update <- read_work_data("data_toteq_update")

sumeqasset <- data_toteq_update %>%
  mutate(sumeqasset = sumeqasset / 1000) %>%
  group_by(year) %>%
  summarise(sumeqasset = sum(sumeqasset, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

write_vertical(
  wb, sheet_name, sumeqasset$sumeqasset,
  start_col = "C", start_row = 34
)

#data_totdebt_update <- read_dta(file.path(work2, "data_totdebt_update.dta"))
data_totdebt_update <- read_work_data("data_totdebt_update")

sumdebtasset <- data_totdebt_update %>%
  group_by(year) %>%
  summarise(sumdebtasset = sum(sumdebtasset, na.rm = TRUE), .groups = "drop") %>%
  mutate(sumdebtasset = sumdebtasset / 1000) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, sumdebtasset$sumdebtasset,
  start_col = "C", start_row = 57
)

# -----------------------------------------------------------------------------#
# Col. (2), (3), (3b), (5), (6b), (7), (9), (10), (12)
# -----------------------------------------------------------------------------#

#data_full_matrices <- read_dta(file.path(work2, "data_full_matrices.dta"))
data_full_matrices <- read_work_data("data_full_matrices")

selected_sources <- c(9999, 377, 924, 456, 419, 443, 453, 9994)


full_by_source <- data_full_matrices %>%
  group_by(year, source) %>%
  summarise(
    eqasset       = sum(eqasset, na.rm = TRUE),
    debtasset     = sum(debtasset, na.rm = TRUE),
    augmeqasset   = sum(augmeqasset, na.rm = TRUE),
    augmdebtasset = sum(augmdebtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(source %in% selected_sources) %>%
  pivot_wider(
    names_from = source,
    values_from = c(eqasset, debtasset, augmeqasset, augmdebtasset),
    names_sep = ""
  ) %>%
  arrange(year) %>%
  mutate(
    across(
      matches("^(eqasset|debtasset|augmeqasset|augmdebtasset)"),
      ~ .x / 1000
    )
  )

# SEFER & SSIO
write_vertical(wb, sheet_name, full_by_source$eqasset9999,   "D", 34)
write_vertical(wb, sheet_name, full_by_source$debtasset9999, "D", 57)

# Cayman Islands
write_vertical(wb, sheet_name, full_by_source$augmeqasset377,   "E", 34)
write_vertical(wb, sheet_name, full_by_source$augmdebtasset377, "E", 57)

write_vertical(wb, sheet_name, full_by_source$eqasset377,   "F", 34)
write_vertical(wb, sheet_name, full_by_source$debtasset377, "F", 57)

# China
write_vertical(wb, sheet_name, full_by_source$augmeqasset924,   "H", 34)
write_vertical(wb, sheet_name, full_by_source$augmdebtasset924, "H", 57)

write_vertical(wb, sheet_name, full_by_source$eqasset924,   "J", 34)
write_vertical(wb, sheet_name, full_by_source$debtasset924, "J", 57)

# Middle East
# Of which in CPIS: Bahrain + Kuwait + Saudi Arabia
full_by_source <- full_by_source %>%
  mutate(
    eq_ME_cpis   = eqasset419 + eqasset443 + eqasset456,
    debt_ME_cpis = debtasset419 + debtasset443 + debtasset456,

    # Correction including CPIS.
    # Qatar (453) is the parking slot for estimated Middle East assets.
    eq_ME   = eq_ME_cpis + augmeqasset453,
    debt_ME = debt_ME_cpis + augmdebtasset453
  )

write_vertical(wb, sheet_name, full_by_source$eq_ME_cpis,   "M", 34)
write_vertical(wb, sheet_name, full_by_source$debt_ME_cpis, "M", 57)

write_vertical(wb, sheet_name, full_by_source$eq_ME,   "K", 34)
write_vertical(wb, sheet_name, full_by_source$debt_ME, "K", 57)

write_vertical(wb, sheet_name, full_by_source$augmeqasset9994,   "P", 34)
write_vertical(wb, sheet_name, full_by_source$augmdebtasset9994, "P", 57)

# -----------------------------------------------------------------------------#
# Col. (4): Correction for CPIS-reporting countries other than
# Cayman, China, Bahrain, Kuwait, Saudi Arabia
# -----------------------------------------------------------------------------#

# Original CPIS minus augmented assets for countries reporting to CPIS
cpis_correction <- data_full_matrices %>%
  filter(cpis == 1) %>%
  group_by(year) %>%
  summarise(
    eqasset       = sum(eqasset, na.rm = TRUE),
    debtasset     = sum(debtasset, na.rm = TRUE),
    augmeqasset   = sum(augmeqasset, na.rm = TRUE),
    augmdebtasset = sum(augmdebtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    eq_corr_cpis   = augmeqasset - eqasset,
    debt_corr_cpis = augmdebtasset - debtasset
  ) %>%
  select(year, eq_corr_cpis, debt_corr_cpis)

# Equivalent of:
# save "$work/cpis_correction.dta", replace
saveRDS(cpis_correction, file = file.path(work, "cpis_correction.rds"))
#write_dta(cpis_correction, file.path(work2, "cpis_correction.dta"))

# Minus augmented assets for CPIS-reporting countries whose corrections
# are reported individually (Cayman and China)
individual_corrections <- data_full_matrices %>%
  filter(source %in% c(377, 924)) %>%
  group_by(year) %>%
  summarise(
    augmeqasset   = sum(augmeqasset, na.rm = TRUE),
    eqasset       = sum(eqasset, na.rm = TRUE),
    augmdebtasset = sum(augmdebtasset, na.rm = TRUE),
    debtasset     = sum(debtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    corr_eq   = augmeqasset - eqasset,
    corr_debt = augmdebtasset - debtasset
  )

other_cpis <- individual_corrections %>%
  left_join(cpis_correction, by = "year") %>%
  mutate(
    eq_othercpis   = (eq_corr_cpis - corr_eq) / 1000,
    debt_othercpis = (debt_corr_cpis - corr_debt) / 1000
  ) %>%
  arrange(year)

write_vertical(wb, sheet_name, other_cpis$eq_othercpis,   "G", 34)
write_vertical(wb, sheet_name, other_cpis$debt_othercpis, "G", 57)

# -----------------------------------------------------------------------------#
# Col. (6): China reserves
# -----------------------------------------------------------------------------#

china_reserves <- data_full_matrices %>%
  select(year, totaleq_China_public, totaldebt_China_public) %>%
  group_by(year) %>%
  summarise(
    totaleq_China_public = mean(totaleq_China_public, na.rm = TRUE),
    totaldebt_China_public = mean(totaldebt_China_public, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    totaleq_China_public = totaleq_China_public / 1000,
    totaldebt_China_public = totaldebt_China_public / 1000
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, china_reserves$totaleq_China_public,
  "I", 34
)
write_vertical(
  wb, sheet_name, china_reserves$totaldebt_China_public,
  "I", 57
)

# -----------------------------------------------------------------------------#
# Col. (11): Private assets of non-CPIS countries
# -----------------------------------------------------------------------------#

excluded_sources <- c(924, 9994, 449, 453, 456, 466, 429, 433)

# Private equity assets EWN
# Private equity assets of non-CPIS countries
other_private_eq <- data_full_matrices %>%
  filter(
    (is.na(cpis) | cpis != 1),
    !source %in% excluded_sources
  ) %>%
  group_by(year) %>%
  summarise(
    other_private_eq = sum(augmeqasset, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)


write_vertical(
  wb, sheet_name, other_private_eq$other_private_eq,
  "O", 34
)

# Private debt assets EWN
# Private debt assets of non-CPIS countries
other_private_debt <- data_full_matrices %>%
  filter(
    (is.na(cpis) | cpis != 1),
    !source %in% excluded_sources
  ) %>%
  group_by(year) %>%
  summarise(
    other_private_debt = sum(augmdebtasset, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

# Write to Table A1
write_vertical(
  wb,
  sheet_name,
  other_private_debt$other_private_debt,
  "O",
  57
)

# Save all modifications to the workbook.
# overwrite = TRUE replaces the workbook file itself, while preserving all
# sheets and formatting loaded into `wb`.
saveWorkbook(wb, myexcel, overwrite = TRUE)

# -----------------------------------------------------------------------------#
# End
# -----------------------------------------------------------------------------#
