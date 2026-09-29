# ==============================================================================
# REPL: Global Offshore Wealth, 2001-2021
#
# R translation of: 4b-build-bis-01-22.do
#
# This program constructs bilateral non-bank and interbank deposits
# spanning 2001 to 2023.
#
# databases used:
#   - locational.dta
#   - FGZ-raw-data.xlsx
#
# outputs:
#   - bis-deposits-all-01-22.dta
#   - bis-interbank-all-01-22.dta
# ==============================================================================


# ------------------------------ PACKAGES --------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(haven)


# --------------------------- HELPER OBJECTS -----------------------------------

# BIS aggregate country codes to be excluded
bis_aggregates <- c(
  "5R", "4W", "4Y", "3C", "4U", "4T", "2D", "2C",
  "2T", "2S", "5M", "2R", "5C", "5K", "4L", "2B",
  "2H", "2O", "2W", "2N", "1C", "2U", "2Z"
)

# Reporting countries used to construct the residual aggregate 1R
bilateral_banks <- c(
  "AT", "AU", "BE", "BR", "CA", "CH", "CL", "DE",
  "DK", "ES", "FI", "FR", "GB", "GG", "GR", "HK",
  "IE", "IM", "IT", "JE", "JP", "KR", "LU", "MO",
  "MX", "NL", "PH", "SE", "TW", "US", "ZA"
)

# Havens without bilateral data
residual_havens <- c(
  "AN", "PA", "MY", "BH", "BM", "BS", "CW", "SG"
)

# Offshore financial centres
ofc_codes <- c(
  "1Z", "AD", "1W", "AN", "AW", "BB", "BH", "BM",
  "BQ", "BS", "BZ", "CH", "CR", "CW", "CY", "DM",
  "GD", "GG", "GI", "HK", "IE", "IM", "JE", "KN",
  "KY", "LB", "LC", "LI", "LR", "LU", "MH", "MO",
  "MS", "MT", "MU", "MY", "NR", "PA", "PW", "SC",
  "SG", "SX", "TC", "VC", "VU", "WS"
)


# ==============================================================================
# I ---- BIS vis-a-vis non-bank counterparties
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------
#locational <- read_work_data("locational")
locational <- read_work_data("locational-after-4a-import")
#locational <- haven::read_dta(file.path(work2, "locational.dta"))

bis_deposits <- locational %>%
  rename(
    saver = counter,
    dep = value
  )


# ----------------------------- FILTER DATA ------------------------------------

# Keep non-bank liabilities, all instruments, all parent countries

bis_deposits <- bis_deposits %>%
  filter(
    sector == "N",
    position == "L",
    instrument == "A",
    parent == "5J",
    year >= 2001
  )


# ------------------------- ANNUAL MEAN -----------------------------------------

# Mean amount outstanding over quarters
# Stata: collapse (mean) dep, by(bank year saver)

bis_deposits <- bis_deposits %>%
  group_by(bank, year, saver) %>%
  summarise(
    dep = mean(dep, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------ DROP BIS AGGREGATES ---------------------------------

bis_deposits <- bis_deposits %>%
  filter(!saver %in% bis_aggregates)


# ==============================================================================
# Geographic adjustments
# ==============================================================================

# French Southern Territories -> France
# Anguilla and Montserrat -> British Overseas Territories
# Greenland -> Denmark

bis_deposits <- bis_deposits %>%
  pivot_wider(
    names_from = saver,
    values_from = dep,
    names_prefix = "dep"
  )

# Zeilensumme: NA, falls sämtliche Komponenten fehlen
stata_rowsum <- function(x) {
  result <- rowSums(x, na.rm = TRUE)
  result[rowSums(!is.na(x)) == 0] <- NA_real_
  result
}

bis_deposits <- bis_deposits %>%
  mutate(
    depFR = stata_rowsum(cbind(depFR, depTF)),
    dep1W = stata_rowsum(cbind(depAI, dep1W, depMS)),
    depDK = stata_rowsum(cbind(depGL, depDK))
  ) %>%
  select(-depTF, -depAI, -depMS, -depGL)

bis_deposits <- bis_deposits %>%
  pivot_longer(
    cols = starts_with("dep"),
    names_to = "saver",
    names_prefix = "dep",
    values_to = "dep"
  )


# ==============================================================================
# Construct residual reporting-country aggregate 1R
# ==============================================================================

bis_deposits <- bis_deposits %>%
  pivot_wider(
    names_from = bank,
    values_from = dep,
    names_prefix = "dep"
  )


# Sum bilateral reporting countries

bilateral_dep_vars <- paste0(
  "dep",
  bilateral_banks
)

# --- FIXME debug
bis_deposits %>%
  filter(year == 2019, saver == "VG") %>%
  select(
    year, saver, dep5A,
    all_of(bilateral_dep_vars)
  ) %>%
  print(width = Inf)

bis_deposits %>%
  filter(year == 2019, saver == "VG") %>%
  summarise(
    dep5A = first(dep5A),
    n_bilateral_available = sum(
      !is.na(across(all_of(bilateral_dep_vars)))
    ),
    bilateral_sum = sum(
      across(all_of(bilateral_dep_vars)),
      na.rm = TRUE
    )
  )
# ---

bis_deposits <- bis_deposits %>%
  mutate(
    negative1R = rowSums(
      across(all_of(bilateral_dep_vars)),
      na.rm = TRUE
    ),
    
    # Keine Residualschätzung, wenn sämtliche bilateralen Werte fehlen
    negative1R = if_else(
      rowSums(!is.na(across(all_of(bilateral_dep_vars)))) == 0,
      NA_real_,
      negative1R
    ),
    
    dep1R = dep5A - negative1R
  )

# Some countries do not disclose bilateral deposits for some years

bis_deposits <- bis_deposits %>%
  mutate(
    dep1R = if_else(
      year <= 2014 & saver == "5J",
      dep1R + coalesce(depHK, 0),
      dep1R
    ),
    dep1R = if_else(
      year == 2013 & saver == "5J",
      dep1R + coalesce(depES, 0),
      dep1R
    ),
    dep1R = if_else(
      year <= 2009 & saver == "5J",
      dep1R + coalesce(depIT, 0),
      dep1R
    ),
    dep1R = if_else(
      year <= 2006 & saver == "5J",
      dep1R + coalesce(depAT, 0) + coalesce(depCA, 0),
      dep1R
    ),
    dep1R = if_else(
      dep1R < 0,
      0,
      dep1R
    )
  ) %>%
  select(-negative1R)


# ==============================================================================
# Compute shares in residual aggregate
# ==============================================================================

# Stata computes, for each year:
#
# share = dep1R / sum(dep1R)
#
# excluding saver == "5J" from the denominator.
# The aggregate observation 5J receives share = 1.

bis_deposits <- bis_deposits %>%
  group_by(year) %>%
  mutate(
    dep_sum1R = sum(
      dep1R[saver != "5J"],
      na.rm = TRUE
    ),
    share = case_when(
      saver == "5J" ~ 1,
      TRUE ~ dep1R / dep_sum1R
    )
  ) %>%
  ungroup() %>%
  select(-dep_sum1R)


# ==============================================================================
# Allocate havens without bilateral data
# ==============================================================================

# For each haven, take the total deposits reported against saver == "5J"
# and distribute them according to the residual shares.

for (ctry in residual_havens) {
  
  dep_var <- paste0("dep", ctry)
  
  totals <- bis_deposits %>%
    filter(saver == "5J") %>%
    select(
      year,
      total_dep = all_of(dep_var)
    )
  
  bis_deposits <- bis_deposits %>%
    left_join(
      totals,
      by = "year"
    ) %>%
    mutate(
      "{dep_var}" := if_else(
        saver != "5J",
        total_dep * share,
        .data[[dep_var]]
      )
    ) %>%
    select(-total_dep)
}


# Hong Kong: allocate 2001-2014

totals_HK <- bis_deposits %>%
  filter(
    saver == "5J",
    year <= 2014
  ) %>%
  select(
    year,
    total_dep = depHK
  )

bis_deposits <- bis_deposits %>%
  left_join(
    totals_HK,
    by = "year"
  ) %>%
  mutate(
    depHK = if_else(
      saver != "5J" & year <= 2014,
      total_dep * share,
      depHK
    )
  ) %>%
  select(-total_dep)


# Macao: allocate 2001-2002

totals_MO <- bis_deposits %>%
  filter(
    saver == "5J",
    year <= 2002
  ) %>%
  select(
    year,
    total_dep = depMO
  )

bis_deposits <- bis_deposits %>%
  left_join(
    totals_MO,
    by = "year"
  ) %>%
  mutate(
    depMO = if_else(
      saver != "5J" & year <= 2002,
      total_dep * share,
      depMO
    )
  ) %>%
  select(-total_dep)


# Return to long format

bis_deposits <- bis_deposits %>%
  pivot_longer(
    cols = starts_with("dep"),
    names_to = "bank",
    names_prefix = "dep",
    values_to = "dep"
  ) %>%
  select(
    year,
    bank,
    saver,
    dep,
    share
  ) %>%
  arrange(
    bank,
    saver,
    year
  ) %>%
  select(-share)


# ==============================================================================
# Cayman Islands
# ==============================================================================

# Cayman Islands deposits are assumed to be held 100% by US savers.

KY_totals <- bis_deposits %>%
  filter(
    bank == "KY",
    saver == "5J"
  ) %>%
  select(
    year,
    total_KY = dep
  )

bis_deposits <- bis_deposits %>%
  left_join(
    KY_totals,
    by = "year"
  ) %>%
  mutate(
    dep = case_when(
      bank == "KY" &
        saver == "US" &
        saver != "5J" ~ total_KY,
      
      bank == "KY" &
        saver != "US" &
        saver != "5J" ~ 0,
      
      TRUE ~ dep
    )
  ) %>%
  select(-total_KY)


# ==============================================================================
# Bermuda, Chile and Panama
# ==============================================================================

# Assume 2001 deposits equal 2002 deposits.

for (b in c("CL", "PA", "BM")) {
  
  copy_2002 <- bis_deposits %>%
    filter(
      bank == b,
      year == 2002
    ) %>%
    mutate(
      year = 2001
    )
  
  bis_deposits <- bis_deposits %>%
    filter(
      !(bank == b & year == 2001)
    ) %>%
    bind_rows(copy_2002)
}


# ==============================================================================
# Construct haven aggregate 1N
# ==============================================================================

bis_deposits <- bis_deposits %>%
  pivot_wider(
    names_from = bank,
    values_from = dep,
    names_prefix = "dep"
  )


bis_deposits <- bis_deposits %>%
  mutate(
    dep1N = rowSums(
      across(
        all_of(
          paste0(
            "dep",
            c(
              "AN", "BH", "BM", "BS", "CW", "KY", "GG",
              "HK", "IM", "JE", "MO", "PA", "SG"
            )
          )
        )
      ),
      na.rm = TRUE
    )
  )


# ==============================================================================
# Household deposit shares
# ==============================================================================

share_household_dep <- readxl::read_excel(
  file.path(raw, "FGZ-raw-data.xlsx"),
  sheet = "sharehouseholddep",
  range = "A3:W25"
)


bis_deposits <- bis_deposits %>%
  left_join(
    share_household_dep,
    by = "year"
  )


# Adjust deposits using household shares

for (bank_code in c(
  "GG", "IM", "JE", "LU", "AT", "BE", "GB", "AN", "CW"
)) {
  
  dep_var <- paste0("dep", bank_code)
  adjusted_var <- paste0("adjusted_dep", bank_code)
  
  bis_deposits[[adjusted_var]] <-
    bis_deposits[[bank_code]] *
    bis_deposits[[dep_var]]
  
  bis_deposits[[bank_code]] <- NULL
}


# EU haven aggregate

adjusted_vars <- paste0(
  "adjusted_dep",
  c("GG", "IM", "JE", "LU", "AT", "BE", "GB")
)

bis_deposits <- bis_deposits %>%
  mutate(
    depEU = rowSums(
      across(
        all_of(c(adjusted_vars, "depCY"))
      ),
      na.rm = TRUE
    )
  ) %>%
  select(
    -starts_with("adjusted_dep")
  )


# ==============================================================================
# Cyprus backcasting
# ==============================================================================

# Cyprus started reporting in 2008.
# Backcast using the development of EU haven deposits.

for (y in 2007:2001) {
  
  y_1 <- y + 1
  
  EUdeposits <- bis_deposits %>%
    filter(
      saver == "5J",
      year == y_1
    ) %>%
    pull(depEU)
  
  CYdeposits <- bis_deposits %>%
    filter(
      saver == "5J",
      year == y_1
    ) %>%
    pull(depCY)
  
  bis_deposits <- bis_deposits %>%
    mutate(
      depCY = if_else(
        saver == "5J" & year == y,
        (CYdeposits * depEU) / EUdeposits,
        depCY
      )
    )
}


bis_deposits <- bis_deposits %>%
  select(
    -depEU,
    -BH,
    -BM,
    -CH,
    -CL,
    -BS,
    -CY,
    -HK,
    -KY,
    -MO,
    -MY,
    -PA,
    -SG,
    -US
  )


# Return to long format

bis_deposits <- bis_deposits %>%
  pivot_longer(
    cols = starts_with("dep"),
    names_to = "bank",
    names_prefix = "dep",
    values_to = "dep"
  )


# ==============================================================================
# Allocate Cyprus
# ==============================================================================

# Russia = 90%
# Greece = 10%

CY_totals <- bis_deposits %>%
  filter(
    bank == "CY",
    saver == "5J"
  ) %>%
  select(
    year,
    total_CY = dep
  )


bis_deposits <- bis_deposits %>%
  left_join(
    CY_totals,
    by = "year"
  ) %>%
  mutate(
    dep = case_when(
      bank == "CY" & saver == "RU" ~ 0.9 * total_CY,
      bank == "CY" & saver == "GR" ~ 0.1 * total_CY,
      bank == "CY" & saver != "5J" ~ 0,
      TRUE ~ dep
    )
  ) %>%
  select(-total_CY)


# ==============================================================================
# Offshore financial centre indicator
# ==============================================================================

bis_deposits <- bis_deposits %>%
  mutate(
    OFC = if_else(
      saver %in% ofc_codes,
      1,
      0
    )
  )


# Drop deposits held in the same country

bis_deposits <- bis_deposits %>%
  filter(
    bank != saver
  )


# ---------------------------- FINAL ORDER -------------------------------------

bis_deposits <- bis_deposits %>%
  select(
    year,
    bank,
    saver,
    dep,
    OFC
  ) %>%
  arrange(
    bank,
    saver,
    year
  )


# ---------------------------- SAVE OUTPUT -------------------------------------
saveRDS(bis_deposits, file = file.path(work, "bis-deposits-all-01-22.rds"))
#haven::write_dta(bis_deposits, file.path(work, "bis-deposits-all-01-22.dta"))



# ==============================================================================
# II ---- BIS vis-a-vis all counterparty sectors
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------
#locational <- read_work_data("locational")
locational <- read_work_data("locational-after-4a-import")
#locational <- haven::read_dta(file.path(work2, "locational.dta"))

bis_interbank <- locational %>%
  rename(
    saver = counter,
    totdep = value
  )


# ----------------------------- FILTER DATA ------------------------------------

bis_interbank <- bis_interbank %>%
  filter(
    sector == "A",
    position == "L",
    instrument == "A",
    parent == "5J",
    year >= 2001
  )


# ------------------------- ANNUAL MEAN -----------------------------------------

bis_interbank <- bis_interbank %>%
  group_by(bank, year, saver) %>%
  summarise(
    totdep = mean(totdep, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------ DROP BIS AGGREGATES ---------------------------------

bis_interbank <- bis_interbank %>%
  filter(
    !saver %in% bis_aggregates
  )


# ==============================================================================
# Geographic adjustments
# ==============================================================================

bis_interbank <- bis_interbank %>%
  pivot_wider(
    names_from = saver,
    values_from = totdep,
    names_prefix = "totdep"
  )


bis_interbank <- bis_interbank %>%
  mutate(
    totdepFR = stata_rowsum(cbind(totdepFR, totdepTF)),
    totdep1W = stata_rowsum(cbind(totdepAI, totdep1W, totdepMS)),
    totdepDK = stata_rowsum(cbind(totdepGL, totdepDK))
  ) %>%
  select(-totdepTF, -totdepAI, -totdepMS, -totdepGL)


bis_interbank <- bis_interbank %>%
  pivot_longer(
    cols = starts_with("totdep"),
    names_to = "saver",
    names_prefix = "totdep",
    values_to = "totdep"
  )


# ==============================================================================
# Construct residual reporting-country aggregate 1R
# ==============================================================================

bis_interbank <- bis_interbank %>%
  pivot_wider(
    names_from = bank,
    values_from = totdep,
    names_prefix = "totdep"
  )


bilateral_totdep_vars <- paste0(
  "totdep",
  setdiff(bilateral_banks, "MX")
)

"totdepMX" %in% bilateral_totdep_vars

bis_interbank <- bis_interbank %>%
  mutate(
    negative1R = rowSums(
      across(all_of(bilateral_totdep_vars)),
      na.rm = TRUE
    ),
    
    negative1R = if_else(
      rowSums(!is.na(across(all_of(bilateral_totdep_vars)))) == 0,
      NA_real_,
      negative1R
    ),
    
    totdep1R = totdep5A - negative1R
  )


# Some countries do not disclose bilateral deposits for some years

bis_interbank <- bis_interbank %>%
  mutate(
    totdep1R = if_else(
      year <= 2014 & saver == "5J",
      totdep1R + coalesce(totdepHK, 0),
      totdep1R
    ),
    totdep1R = if_else(
      year == 2013 & saver == "5J",
      totdep1R + coalesce(totdepES, 0),
      totdep1R
    ),
    totdep1R = if_else(
      year <= 2009 & saver == "5J",
      totdep1R + coalesce(totdepIT, 0),
      totdep1R
    ),
    totdep1R = if_else(
      year <= 2006 & saver == "5J",
      totdep1R +
        coalesce(totdepAT, 0) +
        coalesce(totdepCA, 0),
      totdep1R
    ),
    totdep1R = if_else(
      totdep1R < 0,
      0,
      totdep1R
    )
  ) %>%
  select(-negative1R)


# ==============================================================================
# Compute shares in residual aggregate
# ==============================================================================

bis_interbank <- bis_interbank %>%
  group_by(year) %>%
  mutate(
    totdep_sum1R = sum(
      totdep1R[saver != "5J"],
      na.rm = TRUE
    ),
    share = case_when(
      saver == "5J" ~ 1,
      TRUE ~ totdep1R / totdep_sum1R
    )
  ) %>%
  ungroup() %>%
  select(-totdep_sum1R)


# ==============================================================================
# Allocate havens without bilateral data
# ==============================================================================

for (ctry in residual_havens) {
  
  dep_var <- paste0("totdep", ctry)
  
  totals <- bis_interbank %>%
    filter(saver == "5J") %>%
    select(
      year,
      total_dep = all_of(dep_var)
    )
  
  bis_interbank <- bis_interbank %>%
    left_join(
      totals,
      by = "year"
    ) %>%
    mutate(
      "{dep_var}" := if_else(
        saver != "5J",
        total_dep * share,
        .data[[dep_var]]
      )
    ) %>%
    select(-total_dep)
}


# Hong Kong: 2001-2014

totals_HK <- bis_interbank %>%
  filter(
    saver == "5J",
    year <= 2014
  ) %>%
  select(
    year,
    total_dep = totdepHK
  )

bis_interbank <- bis_interbank %>%
  left_join(
    totals_HK,
    by = "year"
  ) %>%
  mutate(
    totdepHK = if_else(
      saver != "5J" & year <= 2014,
      total_dep * share,
      totdepHK
    )
  ) %>%
  select(-total_dep)


# Macao: 2001-2002

totals_MO <- bis_interbank %>%
  filter(
    saver == "5J",
    year <= 2002
  ) %>%
  select(
    year,
    total_dep = totdepMO
  )

bis_interbank <- bis_interbank %>%
  left_join(
    totals_MO,
    by = "year"
  ) %>%
  mutate(
    totdepMO = if_else(
      saver != "5J" & year <= 2002,
      total_dep * share,
      totdepMO
    )
  ) %>%
  select(-total_dep)


# Return to long format

bis_interbank <- bis_interbank %>%
  pivot_longer(
    cols = starts_with("totdep"),
    names_to = "bank",
    names_prefix = "totdep",
    values_to = "totdep"
  ) %>%
  select(
    year,
    bank,
    saver,
    totdep,
    share
  ) %>%
  arrange(
    bank,
    saver,
    year
  ) %>%
  select(-share)


# ==============================================================================
# Cayman Islands
# ==============================================================================

KY_totals <- bis_interbank %>%
  filter(
    bank == "KY",
    saver == "5J"
  ) %>%
  select(
    year,
    total_KY = totdep
  )


bis_interbank <- bis_interbank %>%
  left_join(
    KY_totals,
    by = "year"
  ) %>%
  mutate(
    totdep = case_when(
      bank == "KY" &
        saver == "US" &
        saver != "5J" ~ total_KY,
      
      bank == "KY" &
        saver != "US" &
        saver != "5J" ~ 0,
      
      TRUE ~ totdep
    )
  ) %>%
  select(-total_KY)


# ==============================================================================
# Bermuda, Chile and Panama
# ==============================================================================

for (b in c("CL", "PA", "BM")) {
  
  copy_2002 <- bis_interbank %>%
    filter(
      bank == b,
      year == 2002
    ) %>%
    mutate(
      year = 2001
    )
  
  bis_interbank <- bis_interbank %>%
    filter(
      !(bank == b & year == 2001)
    ) %>%
    bind_rows(copy_2002)
}


# ==============================================================================
# Construct haven aggregate 1N
# ==============================================================================

bis_interbank <- bis_interbank %>%
  pivot_wider(
    names_from = bank,
    values_from = totdep,
    names_prefix = "totdep"
  )


bis_interbank <- bis_interbank %>%
  mutate(
    totdep1N = rowSums(
      across(
        all_of(
          paste0(
            "totdep",
            c(
              "AN", "BH", "BM", "BS", "CW", "KY", "GG",
              "HK", "IM", "JE", "MO", "PA", "SG"
            )
          )
        )
      ),
      na.rm = TRUE
    ),
    
    # EU haven aggregate
    totdepEU = rowSums(
      across(
        all_of(
          paste0(
            "totdep",
            c(
              "GG", "IM", "JE", "LU",
              "AT", "BE", "GB", "CY"
            )
          )
        )
      ),
      na.rm = TRUE
    )
  )


# ==============================================================================
# Cyprus backcasting
# ==============================================================================

for (y in 2007:2001) {
  
  y_1 <- y + 1
  
  EUdeposits <- bis_interbank %>%
    filter(
      saver == "5J",
      year == y_1
    ) %>%
    pull(totdepEU)
  
  CYdeposits <- bis_interbank %>%
    filter(
      saver == "5J",
      year == y_1
    ) %>%
    pull(totdepCY)
  
  bis_interbank <- bis_interbank %>%
    mutate(
      totdepCY = if_else(
        saver == "5J" & year == y,
        (CYdeposits * totdepEU) / EUdeposits,
        totdepCY
      )
    )
}


bis_interbank <- bis_interbank %>%
  select(-totdepEU)


# Return to long format

bis_interbank <- bis_interbank %>%
  pivot_longer(
    cols = starts_with("totdep"),
    names_to = "bank",
    names_prefix = "totdep",
    values_to = "totdep"
  )


# ==============================================================================
# Allocate Cyprus
# ==============================================================================

CY_totals <- bis_interbank %>%
  filter(
    bank == "CY",
    saver == "5J"
  ) %>%
  select(
    year,
    total_CY = totdep
  )


bis_interbank <- bis_interbank %>%
  left_join(
    CY_totals,
    by = "year"
  ) %>%
  mutate(
    totdep = case_when(
      bank == "CY" & saver == "RU" ~ 0.9 * total_CY,
      bank == "CY" & saver == "GR" ~ 0.1 * total_CY,
      bank == "CY" & saver != "5J" ~ 0,
      TRUE ~ totdep
    )
  ) %>%
  select(-total_CY)


# ==============================================================================
# Offshore financial centre indicator
# ==============================================================================

bis_interbank <- bis_interbank %>%
  mutate(
    OFC = if_else(
      saver %in% ofc_codes,
      1,
      0
    )
  )


# Drop deposits held in the same country

bis_interbank <- bis_interbank %>%
  filter(
    bank != saver
  )


# ---------------------------- FINAL ORDER -------------------------------------

bis_interbank <- bis_interbank %>%
  select(
    year,
    bank,
    saver,
    totdep,
    OFC
  ) %>%
  arrange(
    bank,
    saver,
    year
  )


# ---------------------------- SAVE OUTPUT -------------------------------------
saveRDS(bis_interbank, file = file.path(work, "bis-interbank-all-01-22.rds"))
#haven::write_dta(bis_interbank,file.path(work, "bis-interbank-all-01-22.dta"))