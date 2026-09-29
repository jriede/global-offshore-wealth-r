# ==============================================================================
# REPL: Global Offshore Wealth, 2001-2021
#
# R translation of: 4a-import-bis.do
#
# This program imports and cleans BIS Locational Banking Statistics
#
# databases used:
#   - WS_LBS_D_PUB_csv_col.csv
#   - isocodes.xlsx
#
# output:
#   - locational.dta
# ==============================================================================


# ------------------------------ PACKAGES --------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(data.table)

# ==============================================================================
# I ---- Import and cleaning of BIS Locational Banking Statistics
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------

locational <- readr::read_csv(
  file.path(raw, "WS_LBS_D_PUB_csv_col.csv"),
  show_col_types = FALSE
)

locational <- locational %>%
  select(
    -any_of(c(
      "Frequency",
      "Measure",
      "Currency denomination",
      "Currency type of reporting country",
      "Type of reporting institutions",
      "Position type"
    ))
  )

# Example BIS series:
#
# Q:S:C:D:USD:F:GB:A:DE:N:FR:N
#
# Q   = quarterly
# S   = outstanding
# C   = claims
# D   = debt securities
# USD = denominated in USD
# F   = foreign currency
# GB  = British banks
# A   = all reporting banks
# DE  = Germany
# N   = non-banks
# FR  = France
# N   = cross-border positions


# ----------------------------- FILTER DATA ------------------------------------

# Equivalent to:
#
# keep if freq == "Q"
# keep if l_measure == "S"
# keep if l_denom == "TO1"
# keep if l_curr_type == "A"
# keep if l_rep_bank_type == "A"
# keep if l_pos_type == "N"

locational <- locational %>%
  filter(
    FREQ == "Q",
    L_MEASURE == "S",
    L_DENOM == "TO1",
    L_CURR_TYPE == "A",
    L_REP_BANK_TYPE == "A",
    L_POS_TYPE == "N"
  ) %>%
  select(
    -FREQ,
    -L_MEASURE,
    -L_DENOM,
    -L_CURR_TYPE,
    -L_REP_BANK_TYPE,
    -L_POS_TYPE
  )


# ------------------------------ RESHAPE ---------------------------------------

# The original Stata program first renames the quarterly value columns and
# subsequently uses fastreshape.
#
# readr preserves the original BIS column names, e.g.
#
# 1977-Q4
# 1978-Q1
# 1978-Q2
# ...
#
# Therefore, we can reshape these columns directly.

# --- FIXME speicherintensiv, dauert
#locational <- locational %>%
#  pivot_longer(
#    cols = matches("^\\d{4}-Q[1-4]$"),
#    names_to = "period",
#    values_to = "value"
#  ) %>%
#  separate(
#    period,
#    into = c("year", "quarter"),
#    sep = "-Q",
#    convert = TRUE
#  )
# ---

# --- alternativ: 
# Quartalsspalten identifizieren
quarter_cols <- grep(
  "^\\d{4}-Q[1-4]$",
  names(locational),
  value = TRUE
)

# Nur tatsächlich benötigte Variablen behalten
keep_cols <- c(
  "L_POSITION",
  "L_INSTR",
  "L_PARENT_CTY",
  "L_REP_CTY",
  "L_CP_SECTOR",
  "L_CP_COUNTRY",
  "Series",
  quarter_cols
)

locational <- as.data.table(
  locational[, keep_cols]
)

# Wide -> Long
locational <- melt(
  locational,
  id.vars = setdiff(keep_cols, quarter_cols),
  measure.vars = quarter_cols,
  variable.name = "period",
  value.name = "value",
  variable.factor = FALSE
)

# Jahr und Quartal effizient extrahieren
locational[, year := as.integer(substr(period, 1, 4))]
locational[, quarter := as.integer(substr(period, 7, 7))]

# Hilfsspalte entfernen
locational[, period := NULL]

#---

# ---------------------------- CLEAN VALUES ------------------------------------

# Equivalent to:
# destring(value), replace i(NaN)

locational <- locational %>%
  mutate(
    value = na_if(as.character(value), "NaN"),
    value = as.numeric(value)
  )


# -------------------------- RENAME VARIABLES ----------------------------------

# Equivalent to Stata:
#
# rename l_rep_cty bank
# rename l_cp_country counter
# rename series code
# rename l_cp_sector sector
# rename l_parent_cty parent
# rename l_position position
# rename l_instr instrument

locational <- locational %>%
  rename(
    bank       = L_REP_CTY,
    counter    = L_CP_COUNTRY,
    code       = Series,
    sector     = L_CP_SECTOR,
    parent     = L_PARENT_CTY,
    position   = L_POSITION,
    instrument = L_INSTR
  )


# ------------------------- ORDER AND SORT -------------------------------------

locational <- locational %>%
  arrange(
    year,
    quarter,
    position,
    bank,
    counter
  ) %>%
  relocate(
    quarter,
    year,
    instrument,
    position,
    parent,
    bank,
    sector,
    counter,
    value,
    code
  )


# ------------------------- SAVE INTERMEDIATE ----------------------------------

saveRDS(locational, file = file.path(work, "locational.rds"))

#haven::write_dta(locational,file.path(work, "locational.dta"))


# ==============================================================================
# II ---- Add ISO-3 codes to counterparty countries
# ==============================================================================

iso <- readxl::read_excel(
  file.path(raw, "isocodes.xlsx"),
  sheet = "iso"
)


# Check actual ISO variable names
names(iso)


# Prepare ISO lookup table for counterparty countries

iso_counter <- iso %>%
  rename(
    counter = iso2,
    namecounter = isoname,
    iso3counter = iso3
  ) %>%
  select(
    counter,
    namecounter,
    iso3counter
  )


# Equivalent to:
#
# merge 1:m counter using "$work/locational.dta",
#     nogenerate keep(2 3)
#
# We keep every BIS observation, including observations without ISO match.

locational <- locational %>%
  left_join(
    iso_counter,
    by = "counter"
  ) %>%
  relocate(
    namecounter,
    counter,
    iso3counter
  )


# Save intermediate result
saveRDS(locational, file = file.path(work, "locational.rds"))
#haven::write_dta(locational,file.path(work, "locational.dta"))


# ==============================================================================
# III ---- Add ISO-3 codes to BIS reporting countries
# ==============================================================================

iso_bank <- iso %>%
  rename(
    bank = iso2,
    namebank = isoname,
    iso3bank = iso3
  ) %>%
  select(
    bank,
    namebank,
    iso3bank
  )


locational <- locational %>%
  left_join(
    iso_bank,
    by = "bank"
  ) %>%
  relocate(
    namebank,
    bank,
    iso3bank
  )


# -------------------------- SPECIAL BIS CODES ---------------------------------

# Equivalent to:
#
# replace namebank = "All BIS-reporting banks" if bank == "5A"
# replace namecounter = "All" if counter == "5J"

locational <- locational %>%
  mutate(
    namebank = if_else(
      bank == "5A",
      "All BIS-reporting banks",
      namebank
    ),
    namecounter = if_else(
      counter == "5J",
      "All",
      namecounter
    )
  )

# ---------------------------- SAVE OUTPUT -------------------------------------
saveRDS(locational, file = file.path(work, "locational.rds"))
saveRDS(locational, file = file.path(work, "locational-after-4a-import.rds"))
#haven::write_dta(locational,file.path(work, "locational.dta"))