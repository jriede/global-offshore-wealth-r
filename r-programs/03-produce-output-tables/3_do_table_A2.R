# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# Purpose: produce TABLE A2 "Global Cross-Border Securities Liabilities"
#          (total liabilities and corrections)
# -----------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(haven)
library(openxlsx)

# Assumes that work, work2 and tables are already defined.

myexcel <- file.path(tables, "FGZ_R_Appendix_Tables_A1-A3.xlsx")
sheet_name <- "Table A2_update"

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
# Prepare host-year dataset
# Stata:
# sort host year source
# collapse (first) ..., by(host year)
# -----------------------------------------------------------------------------#

#data_full_matrices <- read_dta(file.path(work2, "data_full_matrices.dta"))
data_full_matrices <- read_work_data("data_full_matrices")

host_year <- data_full_matrices %>%
  arrange(host, year, source) %>%
  group_by(host, year) %>%
  summarise(
    hostname             = first(hostname),
    ewn22_host           = first(ewn22_host),
    lequity_host         = first(lequity_host),
    lportif_debt_host    = first(lportif_debt_host),
    eqliab_host          = first(eqliab_host),
    derivedeqliab_host   = first(derivedeqliab_host),
    deriveddebtliab_host = first(deriveddebtliab_host),
    debtliab_host        = first(debtliab_host),
    gapeq_host           = first(gapeq_host),
    gapdebt_host         = first(gapdebt_host),
    missingliabeq        = first(missingliabeq),
    missingliabdebt      = first(missingliabdebt),
    ofc_host             = first(ofc_host),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------#
# Col. (1): EWN22 reported liabilities
# -----------------------------------------------------------------------------#

reported_liabilities <- host_year %>%
  group_by(year) %>%
  summarise(
    lequity_host      = sum(lequity_host, na.rm = TRUE) / 1000,
    lportif_debt_host = sum(lportif_debt_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, reported_liabilities$lequity_host,
  start_col = "C", start_row = 33
)

write_vertical(
  wb, sheet_name, reported_liabilities$lportif_debt_host,
  start_col = "C", start_row = 56
)

# -----------------------------------------------------------------------------#
# Col. (3): No portfolio data in EWN22, but data in IMF IIP
#           or creditor-derived liabilities
# -----------------------------------------------------------------------------#

missing_ewn_debt <- host_year %>%
  filter(ewn22_host == 1, is.na(lportif_debt_host)) %>%
  group_by(year) %>%
  summarise(
    debtliab_host = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, missing_ewn_debt$debtliab_host,
  start_col = "E", start_row = 56
)

missing_ewn_equity <- host_year %>%
  filter(ewn22_host == 1, is.na(lequity_host)) %>%
  group_by(year) %>%
  summarise(
    eqliab_host = sum(eqliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, missing_ewn_equity$eqliab_host,
  start_col = "E", start_row = 33
)

# -----------------------------------------------------------------------------#
# Col. (4): Dutch SFIs
# -----------------------------------------------------------------------------#


# -----------------------------------------------------------------------------#
# Col. (4): Dutch SFIs
# SFIs are included in CPIS from 2003 onwards.
# Additional corrections are therefore restricted to 2001 and 2002.
# -----------------------------------------------------------------------------#

dutch_sfi <- data_full_matrices %>%
  filter(
    host == 138,
    year %in% c(2001, 2002)
  ) %>%
  group_by(year) %>%
  summarise(
    lequity_SFI = mean(lequity_SFI, na.rm = TRUE) / 1000,
    ldebt_SFI   = mean(ldebt_SFI, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

# Panel B: Equity
write_vertical(
  wb, sheet_name, dutch_sfi$lequity_SFI,
  start_col = "F", start_row = 33
)

# Panel C: Debt
write_vertical(
  wb, sheet_name, dutch_sfi$ldebt_SFI,
  start_col = "F", start_row = 56
)


# -----------------------------------------------------------------------------#
# Col. (5): Raw CPIS-derived liabilities > reported liabilities
# Cayman Islands are excluded because their correction is reported separately.
# -----------------------------------------------------------------------------#

# Equivalent to the two Stata table commands used as checks.
cpis_liability_check_all <- host_year %>%
  group_by(year) %>%
  summarise(
    missingliabeq   = sum(missingliabeq, na.rm = TRUE),
    missingliabdebt = sum(missingliabdebt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

cpis_liability_check_excl_cayman <- host_year %>%
  filter(host != 377) %>%
  group_by(year) %>%
  summarise(
    missingliabeq   = sum(missingliabeq, na.rm = TRUE),
    missingliabdebt = sum(missingliabdebt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

print(cpis_liability_check_all)
print(cpis_liability_check_excl_cayman)

cpis_liability_correction <- host_year %>%
  filter(host != 377) %>%
  group_by(year) %>%
  summarise(
    missingliabeq   = sum(missingliabeq, na.rm = TRUE) / 1000,
    missingliabdebt = sum(missingliabdebt, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, cpis_liability_correction$missingliabeq,
  start_col = "G", start_row = 33
)

write_vertical(
  wb, sheet_name, cpis_liability_correction$missingliabdebt,
  start_col = "G", start_row = 56
)

# -----------------------------------------------------------------------------#
# Col. (6), (6b): Cayman Islands
# -----------------------------------------------------------------------------#

cayman_liabilities <- host_year %>%
  filter(host == 377) %>%
  group_by(year) %>%
  summarise(
    eqliab_host       = sum(eqliab_host, na.rm = TRUE) / 1000,
    debtliab_host     = sum(debtliab_host, na.rm = TRUE) / 1000,
    lequity_host      = sum(lequity_host, na.rm = TRUE) / 1000,
    lportif_debt_host = sum(lportif_debt_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(wb, sheet_name, cayman_liabilities$eqliab_host,       "H", 33)
write_vertical(wb, sheet_name, cayman_liabilities$debtliab_host,     "H", 56)
write_vertical(wb, sheet_name, cayman_liabilities$lequity_host,      "I", 33)
write_vertical(wb, sheet_name, cayman_liabilities$lportif_debt_host, "I", 56)

# -----------------------------------------------------------------------------#
# Col. (7): Small OFCs
# -----------------------------------------------------------------------------#

small_ofcs <- host_year %>%
  filter(
    (is.na(ewn22_host) | ewn22_host != 1),
    host != 377,
    ofc_host == 1
  )%>%
  group_by(year) %>%
  summarise(
    eqliab_host   = sum(eqliab_host, na.rm = TRUE) / 1000,
    debtliab_host = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(wb, sheet_name, small_ofcs$eqliab_host,   "J", 33)
write_vertical(wb, sheet_name, small_ofcs$debtliab_host, "J", 56)

# -----------------------------------------------------------------------------#
# Col. (9): Other non-EWN22 countries
# -----------------------------------------------------------------------------#

other_non_ewn <- host_year %>%
  filter(
    is.na(ewn22_host) | ewn22_host != 1,
    host != 9998,
    is.na(ofc_host) | ofc_host != 1
  ) %>%
  group_by(year) %>%
  summarise(
    eqliab_host   = sum(eqliab_host, na.rm = TRUE) / 1000,
    debtliab_host = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(wb, sheet_name, other_non_ewn$eqliab_host,   "L", 33)
write_vertical(wb, sheet_name, other_non_ewn$debtliab_host, "L", 56)

# -----------------------------------------------------------------------------#
# Col. (10): International Organizations
# -----------------------------------------------------------------------------#

international_organizations <- host_year %>%
  filter(host == 9998) %>%
  group_by(year) %>%
  summarise(
    eqliab_host   = sum(eqliab_host, na.rm = TRUE) / 1000,
    debtliab_host = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, international_organizations$eqliab_host,
  "M", 33
)

write_vertical(
  wb, sheet_name, international_organizations$debtliab_host,
  "M", 56
)

# -----------------------------------------------------------------------------#
# Col. (11): Total
# -----------------------------------------------------------------------------#

total_liabilities <- host_year %>%
  group_by(year) %>%
  summarise(
    eqliab_host   = sum(eqliab_host, na.rm = TRUE) / 1000,
    debtliab_host = sum(debtliab_host, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, total_liabilities$eqliab_host,
  "N", 33
)

write_vertical(
  wb, sheet_name, total_liabilities$debtliab_host,
  "N", 56
)

# -----------------------------------------------------------------------------#
# Col. (2): IMF IIPs
# -----------------------------------------------------------------------------#

#IIP_eqliab <- read_dta(file.path(work2, "IIP_eqliab.dta")) %>%
#  mutate(eqliab_IIP = eqliab_IIP / 1000)
IIP_eqliab <- read_work_data("IIP_eqliab") %>%
  mutate(eqliab_IIP = eqliab_IIP / 1000)


write_vertical(
  wb, sheet_name, IIP_eqliab$eqliab_IIP,
  "D", 33
)

#IIP_debtliab <- read_dta(file.path(work2, "IIP_debtliab.dta")) %>%
#  mutate(debtliab_IIP = debtliab_IIP / 1000)
IIP_debtliab <- read_work_data("IIP_debtliab") %>%
  mutate(debtliab_IIP = debtliab_IIP / 1000)

write_vertical(
  wb, sheet_name, IIP_debtliab$debtliab_IIP,
  "D", 56
)

# Save all modifications to the workbook.
# overwrite = TRUE replaces the workbook file itself, while preserving all
# sheets and formatting loaded into `wb`.
saveWorkbook(wb, myexcel, overwrite = TRUE)

# -----------------------------------------------------------------------------#
# End
# -----------------------------------------------------------------------------#
