# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026

# Purpose:
#   Reconstruct Swiss offshore wealth from the underlying SNB source sheets
#   contained in FGZ2023Data.xlsx, rather than importing the finished T.A10
#   or T.A2 series.
#
# FGZ Appendix B.1:
#   Swiss offshore wealth =
#       foreign securities belonging to foreigners
#     + Swiss securities belonging to foreigners
#     + foreign securities wrongly attributed to Switzerland
#     + fiduciary deposits
#
# Output:
#   work/switzerland_offshore_wealth.rds
#   work/switzerland_offshore_wealth.csv
#
# Units: USD billion.
# ==============================================================================

library(dplyr)
library(readxl)
library(readr)
library(lubridate)

# ------------------------------------------------------------------------------
# 0. Locate FGZ2023Data.xlsx
# ------------------------------------------------------------------------------

candidates <- c(
  file.path(raw, "FGZ2023Data.xlsx"),
  file.path(root, "FGZ2023Data.xlsx"),
  "FGZ2023Data.xlsx"
)

xlsx_file <- candidates[file.exists(candidates)][1]

if (is.na(xlsx_file)) {
  stop(
    "FGZ2023Data.xlsx not found. Put the workbook in raw/, root/, ",
    "or the current working directory."
  )
}

message("Using source workbook: ", xlsx_file)

# Read raw sheets without interpreting their first rows as headers.
read_raw_sheet <- function(sheet) {
  read_excel(
    xlsx_file,
    sheet = sheet,
    col_names = FALSE
  )
}

sec_raw  <- read_raw_sheet("SNB_D5_1a_M33")
fid1_raw <- read_raw_sheet("SNB_D4_1a")
fid2_raw <- read_raw_sheet("SNB_2E")
mona_raw <- read_raw_sheet("SNB.MONA")
fx_raw   <- read_raw_sheet("SNB_FX")

# ------------------------------------------------------------------------------
# 1. Reconstruct the monthly calendar used in T.A10
#
# T.A10 begins in Jan-1999. Its source formulas reveal the following mapping:
#
#   T.A10 row 6  (1999-01):
#       securities <- SNB_D5_1a_M33 row 32
#       fiduciary1 <- SNB_D4_1a row 157
#       fiduciary2 <- SNB_2E row 158
#       FX          <- SNB_FX row 25
#
# Each subsequent month advances one row in each source sheet.
# From 2014 onward fiduciary deposits use SNB.MONA instead.
# ------------------------------------------------------------------------------

dates <- seq(
  as.Date("1999-01-01"),
  as.Date("2022-12-01"),
  by = "month"
)

n <- length(dates)

source_map <- tibble(
  date = dates,
  year = year(date),
  month = month(date),
  sec_row  = 32L  + 0:(n - 1),
  fid1_row = 157L + 0:(n - 1),
  fid2_row = 158L + 0:(n - 1),
  fx_row   = 25L  + 0:(n - 1)
)

# SNB.MONA starts with June 2014 at workbook row 25.
# This matches the T.A10 formula: Dec-2014 -> SNB.MONA row 31.
source_map <- source_map %>%
  mutate(
    mona_row = if_else(
      date >= as.Date("2014-06-01"),
      25L + interval(as.Date("2014-06-01"), date) %/% months(1),
      NA_integer_
    )
  )

# ------------------------------------------------------------------------------
# 2. Helper to pull numeric values from exact workbook cells
# ------------------------------------------------------------------------------

cell_num <- function(tbl, row, col) {
  if (is.na(row) || row < 1 || row > nrow(tbl) || col > ncol(tbl)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(tbl[[col]][row]))
}

# Source columns are taken directly from the T.A10 formulas:
#
#   securities: SNB_D5_1a_M33 column B
#   fiduciary foreign: SNB_D4_1a column I
#   fiduciary Swiss: SNB_2E column H
#   post-2014 fiduciary: SNB.MONA column B
#   USD/CHF conversion used by T.A10: SNB_FX column L
# ------------------------------------------------------------------------------

monthly <- source_map %>%
  rowwise() %>%
  mutate(
    securities_raw = cell_num(sec_raw, sec_row, 2),
    fid_foreign_raw = cell_num(fid1_raw, fid1_row, 9),
    fid_swiss_raw   = cell_num(fid2_raw, fid2_row, 8),
    fid_mona_raw    = cell_num(mona_raw, mona_row, 2),
    fx_usd           = cell_num(fx_raw, fx_row, 12)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 3. Foreign securities belonging to foreigners
#
# T.A10 applies:
#   - /0.968 for incomplete SNB survey coverage
#   - /0.94 (= /(1 - .06)) before Dec-2012 because the reporting population
#     was expanded at end-2012 by about 6%
#   - division by the SNB FX series to convert CHF to current USD
# ------------------------------------------------------------------------------

monthly <- monthly %>%
  mutate(
    foreign_securities_foreigners =
      securities_raw / 0.968 / fx_usd,

    foreign_securities_foreigners = if_else(
      date < as.Date("2012-12-01"),
      foreign_securities_foreigners / 0.94,
      foreign_securities_foreigners
    )
  )

# ------------------------------------------------------------------------------
# 4. Swiss securities belonging to foreigners
#
# FGZ/T.A10 anchors this correction at USD 77bn in Dec-2011 and lets it follow
# the evolution of foreign securities belonging to foreigners in all months.
#
# Thus:
#   D_t = 77 * C_t / C_(2011-12)
# ------------------------------------------------------------------------------

c_anchor_77 <- monthly %>%
  filter(date == as.Date("2011-12-01")) %>%
  pull(foreign_securities_foreigners)

if (length(c_anchor_77) != 1 || is.na(c_anchor_77)) {
  stop("Could not construct the Dec-2011 securities anchor.")
}

monthly <- monthly %>%
  mutate(
    swiss_securities_foreigners =
      77 * foreign_securities_foreigners / c_anchor_77
  )

# ------------------------------------------------------------------------------
# 5. Foreign securities wrongly attributed to Switzerland
#
# FGZ/T.A10 anchors this correction at USD 100bn in May-2013 and lets it follow
# the same securities series:
#
#   E_t = 100 * C_t / C_(2013-05)
# ------------------------------------------------------------------------------

c_anchor_100 <- monthly %>%
  filter(date == as.Date("2013-05-01")) %>%
  pull(foreign_securities_foreigners)

if (length(c_anchor_100) != 1 || is.na(c_anchor_100)) {
  stop("Could not construct the May-2013 securities anchor.")
}

monthly <- monthly %>%
  mutate(
    foreign_securities_misattributed =
      100 * foreign_securities_foreigners / c_anchor_100
  )

# ------------------------------------------------------------------------------
# 6. Fiduciary deposits
#
# T.A10 uses the old monthly SNB tables through 2013:
#
#   (foreign fiduciary + Swiss-owned fiduciary) / FX
#
# From 2014 onward it uses the newer SNB.MONA series.
#
# The raw tables are in CHF million, hence /1000 before FX conversion.
# ------------------------------------------------------------------------------

monthly <- monthly %>%
  mutate(
    fiduciary_deposits_old =
      (fid_foreign_raw / 1000 + fid_swiss_raw / 1000) / fx_usd,

    fiduciary_deposits_new =
      (fid_mona_raw / 1000) / fx_usd,

    fiduciary_deposits = if_else(
      date < as.Date("2014-01-01"),
      fiduciary_deposits_old,
      fiduciary_deposits_new
    )
  )

# ------------------------------------------------------------------------------
# 7. Total Swiss offshore wealth
# ------------------------------------------------------------------------------

monthly <- monthly %>%
  mutate(
    switzerland_offshore_wealth =
      foreign_securities_foreigners +
      swiss_securities_foreigners +
      foreign_securities_misattributed +
      fiduciary_deposits
  )

# Figure 2 uses year-end values.
switzerland_offshore_wealth <- monthly %>%
  filter(
    month == 12,
    year %in% 2001:2021
  ) %>%
  transmute(
    year = as.integer(year),
    foreign_securities_foreigners,
    swiss_securities_foreigners,
    foreign_securities_misattributed,
    fiduciary_deposits,
    switzerland_offshore_wealth
  ) %>%
  arrange(year)

# ------------------------------------------------------------------------------
# 8. Checks
# ------------------------------------------------------------------------------

stopifnot(
  nrow(switzerland_offshore_wealth) == 21,
  identical(switzerland_offshore_wealth$year, 2001:2021),
  all(is.finite(switzerland_offshore_wealth$switzerland_offshore_wealth))
)

print(switzerland_offshore_wealth, n = Inf)

# ------------------------------------------------------------------------------
# 9. Save
# ------------------------------------------------------------------------------

saveRDS(
  switzerland_offshore_wealth,
  file.path(work, "switzerland_offshore_wealth.rds")
)

write_csv(
  switzerland_offshore_wealth,
  file.path(work, "switzerland_offshore_wealth.csv"),
  na = ""
)

# Also save monthly reconstruction for auditability.
saveRDS(
  monthly,
  file.path(work, "switzerland_offshore_wealth_monthly.rds")
)

write_csv(
  monthly,
  file.path(work, "switzerland_offshore_wealth_monthly.csv"),
  na = ""
)

message(
  "Saved Swiss offshore wealth to: ",
  file.path(work, "switzerland_offshore_wealth.rds")
)

# ==============================================================================
# End
# ==============================================================================
