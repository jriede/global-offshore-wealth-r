# -----------------------------------------------------------------------------#
# Project: Offshore financial wealth database - update 2023
# Title: 3_do_table_A3.R
# Purpose: produce TABLE A3
# Translation of 3_do_table_A3.do from Stata to R
# -----------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(haven)
library(openxlsx)

# Assumes that work, work2 and tables are already defined.

myexcel <- file.path(tables, "FGZ_R_Appendix_Tables_A1-A3.xlsx")
sheet_name <- "Table A3_update"

# Helper: write a vector vertically into an existing Excel workbook.
# start_col can be either a column letter ("F") or a number.
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

# -----------------------------------------------------------------------------#
# Open / create workbook and worksheet
# -----------------------------------------------------------------------------#

if (file.exists(myexcel)) {

  wb <- loadWorkbook(myexcel)

  if (!(sheet_name %in% names(wb))) {
    addWorksheet(wb, sheet_name)
  }

} else {

  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)

}

# -----------------------------------------------------------------------------#
# Load data
# -----------------------------------------------------------------------------#

#data_full_matrices <- read_dta(file.path(work2, "data_full_matrices.dta"))
data_full_matrices <- read_work_data("data_full_matrices")

# Stata:
# sort host year source
# collapse (first) hostname eqliab_host derivedeqliab_host gapeq_host
#                  debtliab_host deriveddebtliab_host gapdebt_host,
#                  by(host year)
#
# Sorting before first() preserves the Stata logic of selecting the first
# source observation within each host-year group.

host_year <- data_full_matrices %>%
  arrange(host, year, source) %>%
  group_by(host, year) %>%
  summarise(
    hostname             = first(hostname),
    eqliab_host          = first(eqliab_host),
    derivedeqliab_host   = first(derivedeqliab_host),
    gapeq_host           = first(gapeq_host),
    debtliab_host        = first(debtliab_host),
    deriveddebtliab_host = first(deriveddebtliab_host),
    gapdebt_host         = first(gapdebt_host),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------#
# Discrepancy control column
#
# This block is commented out in the original Stata file as well.
# -----------------------------------------------------------------------------#

# discrepancy_control <- host_year %>%
#   group_by(year) %>%
#   summarise(
#     gapeq_host   = sum(gapeq_host, na.rm = TRUE) / 1000,
#     gapdebt_host = sum(gapdebt_host, na.rm = TRUE) / 1000,
#     .groups = "drop"
#   ) %>%
#   arrange(year)
#
# write_vertical(
#   wb, sheet_name, discrepancy_control$gapeq_host,
#   start_col = "R", start_row = 33
# )
#
# write_vertical(
#   wb, sheet_name, discrepancy_control$gapdebt_host,
#   start_col = "R", start_row = 56
# )

# -----------------------------------------------------------------------------#
# Col. (4) - (11)
#
# Stata:
# collapse (first) gapeq_host gapdebt_host, by(host year)
# replace ... = ... / 1000
# reshape wide gapeq_host gapdebt_host, i(year) j(host)
# -----------------------------------------------------------------------------#

gap_by_host <- host_year %>%
  select(host, year, gapeq_host, gapdebt_host) %>%
  mutate(
    gapeq_host   = gapeq_host / 1000,
    gapdebt_host = gapdebt_host / 1000
  ) %>%
  arrange(year, host)

# The Stata file reshapes the data to wide format. Keeping an equivalent
# wide object is useful for direct inspection and comparison with Stata.
gap_by_host_wide <- gap_by_host %>%
  pivot_wider(
    id_cols = year,
    names_from = host,
    values_from = c(gapeq_host, gapdebt_host),
    names_glue = "{.value}{host}"
  ) %>%
  arrange(year)

# -----------------------------------------------------------------------------#
# Luxembourg, host == 137
# Stata: F33 / F56
# -----------------------------------------------------------------------------#

luxembourg <- gap_by_host %>%
  filter(host == 137) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, luxembourg$gapeq_host,
  start_col = "F", start_row = 33
)

write_vertical(
  wb, sheet_name, luxembourg$gapdebt_host,
  start_col = "F", start_row = 56
)

# -----------------------------------------------------------------------------#
# Cayman Islands, host == 377
# Stata: G33 / G56
# -----------------------------------------------------------------------------#

cayman <- gap_by_host %>%
  filter(host == 377) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, cayman$gapeq_host,
  start_col = "G", start_row = 33
)

write_vertical(
  wb, sheet_name, cayman$gapdebt_host,
  start_col = "G", start_row = 56
)

# -----------------------------------------------------------------------------#
# Ireland, host == 178
# Stata: H33 / H56
# -----------------------------------------------------------------------------#

ireland <- gap_by_host %>%
  filter(host == 178) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, ireland$gapeq_host,
  start_col = "H", start_row = 33
)

write_vertical(
  wb, sheet_name, ireland$gapdebt_host,
  start_col = "H", start_row = 56
)

# -----------------------------------------------------------------------------#
# USA, host == 111
# Stata: I33 / I56
# -----------------------------------------------------------------------------#

usa <- gap_by_host %>%
  filter(host == 111) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, usa$gapeq_host,
  start_col = "I", start_row = 33
)

write_vertical(
  wb, sheet_name, usa$gapdebt_host,
  start_col = "I", start_row = 56
)

# -----------------------------------------------------------------------------#
# Japan, host == 158
# Stata: J33 / J56
# -----------------------------------------------------------------------------#

japan <- gap_by_host %>%
  filter(host == 158) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, japan$gapeq_host,
  start_col = "J", start_row = 33
)

write_vertical(
  wb, sheet_name, japan$gapdebt_host,
  start_col = "J", start_row = 56
)

# -----------------------------------------------------------------------------#
# Switzerland, host == 146
# Stata: K33 / K56
# -----------------------------------------------------------------------------#

switzerland <- gap_by_host %>%
  filter(host == 146) %>%
  arrange(year)

write_vertical(
  wb, sheet_name, switzerland$gapeq_host,
  start_col = "K", start_row = 33
)

write_vertical(
  wb, sheet_name, switzerland$gapdebt_host,
  start_col = "K", start_row = 56
)

# -----------------------------------------------------------------------------#
# Save workbook
# -----------------------------------------------------------------------------#

saveWorkbook(wb, myexcel, overwrite = TRUE)

# -----------------------------------------------------------------------------#
# End
# -----------------------------------------------------------------------------#
