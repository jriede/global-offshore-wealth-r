# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# This program builds a simpler dataset of each country's offshore wealth
# in total, in haven groups (American, European, Asian and Swiss), and the
# total wealth attracted by each haven.
#
# databases used: - offshore[i] (2001-2022)
#                 - FGZ-raw-data.xlsx
#
# outputs:        - countries.dta
#                 - sheet "ctrybyctry01-22" in FGZ-raw-data.xlsx
# ==============================================================================

library(tidyverse)
library(readxl)
library(haven)
library(openxlsx)


# ==============================================================================
# Helper function: Stata egen rowtotal(...), missing
# ==============================================================================

# Stata's:
#
# egen offshore_total = rowtotal(...), missing
#
# sums available values but returns missing if ALL values are missing.

stata_rowtotal <- function(...) {
  
  x <- cbind(...)
  
  result <- rowSums(
    x,
    na.rm = TRUE
  )
  
  result[
    rowSums(!is.na(x)) == 0
  ] <- NA_real_
  
  result
}


# ==============================================================================
# I ---- Countries
# ==============================================================================

countries_all <- vector(
  "list",
  length = 22
)

names(countries_all) <- as.character(
  2001:2022
)


for (i in 2001:2022) {
  
  message(
    "Processing year ",
    i,
    " ..."
  )
  
  
  # ============================================================================
  # I.1 ---- Global offshore wealth by haven group
  # ============================================================================
  
  # Stata:
  #
  # import excel "$raw/FGZ-raw-data.xlsx", clear firstrow
  # cellrange(H4:N26) sheet(T.A2)
  
  offshore_totals <- read_excel(
    file.path(
      raw,
      "FGZ-raw-data.xlsx"
    ),
    sheet = "T.A2",
    range = "H4:N26"
  )
  
  
  # The Stata import produces:
  #
  # H
  # Totaloffshorewealth
  # Switzerland
  # TaxhavensotherthanSwitzerlan
  # OfwhichAmericantaxhavens
  # OfwhichAsiantaxhavens
  # OfwhichEuropeantaxhavens
  
  names(offshore_totals)[1:7] <- c(
    "year",
    "global_offshore_wealth",
    "switzerland_offshore_wealth",
    "other_havens_offshore_wealth",
    "american_havens_offshore_wealth",
    "asian_havens_offshore_wealth",
    "european_havens_offshore_wealth"
  )
  
  
  offshore_totals <- offshore_totals %>%
    mutate(
      year = as.numeric(year)
    ) %>%
    filter(
      year == i
    )
  
  
  # ============================================================================
  # I.2 ---- Merge annual offshore dataset from 6a
  # ============================================================================
  
  #offshore_i <- read_dta(file.path(work2,paste0("offshore",i,".dta")))
  offshore_i <- read_work_data(paste0("offshore", i))
  
  
  # Equivalent to:
  #
  # merge 1:m year using "$work/offshore`i'", nogenerate
  # replace year = `i'
  
  countries_i <- offshore_i %>%
    mutate(
      year = i
    ) %>%
    left_join(
      offshore_totals,
      by = "year"
    )
  
  
  # ============================================================================
  # I.3 ---- Calculate country offshore wealth
  # ============================================================================
  
  # Names generated in 6a:
  #
  # sh_fidu_smthg2001
  # sh_EU_smthg2001
  # sh_AS_smthg2001
  # sh_CR_smthg2001
  # ...
  #
  # The year changes in every iteration.
  
  
  # The final annual datasets from 6a use variable names
  # without year suffixes.
  
  sh_fidu <- "sh_fidu_smthg"
  sh_eu   <- "sh_EU_smthg"
  sh_as   <- "sh_AS_smthg"
  sh_cr   <- "sh_CR_smthg"
  
  gdp_var <- "gdp"
  
  
  
  # Check that required variables exist
  required_vars <- c(
    sh_fidu,
    sh_eu,
    sh_as,
    sh_cr,
    gdp_var
  )
  
  
  missing_vars <- setdiff(
    required_vars,
    names(countries_i)
  )
  
  
  if (length(missing_vars) > 0) {
    
    stop(
      paste0(
        "Missing variables in offshore",
        i,
        ".rds: ",
        paste(
          missing_vars,
          collapse = ", "
        )
      )
    )
  }
  
  
  # Stata:
  #
  # gen offshore_switzerland =
  #     switzerland_offshore_wealth * sh_fidu_smthg`i'
  #
  # etc.
  
  countries_i <- countries_i %>%
    mutate(
      offshore_switzerland =
        switzerland_offshore_wealth *
        .data[[sh_fidu]],
      
      offshore_EU_Havens =
        european_havens_offshore_wealth *
        .data[[sh_eu]],
      
      offshore_AS_Havens =
        asian_havens_offshore_wealth *
        .data[[sh_as]],
      
      offshore_CR_Havens =
        american_havens_offshore_wealth *
        .data[[sh_cr]],
      
      offshore_total = stata_rowtotal(
        offshore_switzerland,
        offshore_EU_Havens,
        offshore_AS_Havens,
        offshore_CR_Havens
      ),
      
      ratio_offshore_GDP =
        offshore_total /
        (.data[[gdp_var]] / 1000000000)
    )
  
  
  # ============================================================================
  # I.4 ---- Keep Switzerland-bank observations
  # ============================================================================
  
  # Stata:
  #
  # keep if bank == "CH"
  
  countries_i <- countries_i %>%
    filter(
      bank == "CH"
    ) %>%
    select(
      iso3saver,
      year,
      offshore_total,
      offshore_switzerland,
      offshore_EU_Havens,
      offshore_AS_Havens,
      offshore_CR_Havens,
      latin_am,
      europe,
      asia,
      africa
    )
  
  
  # ============================================================================
  # II ---- Total wealth attracted by each haven
  # ============================================================================
  
  # Stata:
  #
  # import excel ..., cellrange(A29:W51) sheet(T.A2b)
  
  haven_totals <- read_excel(
    file.path(raw, "FGZ-raw-data.xlsx"),
    sheet = "T.A2b",
    range = "A29:W51"
  ) %>%
    rename(
      year = `...1`,
      Totaloffshorewealth = `Total offshore wealth`,
      Switzerland = `Switzerland`,
      TaxhavensotherthanSwitzerland = `Tax havens other than Switzerland`,
      CaymanIslands = `Cayman Islands`,
      Panama = `Panama`,
      US = `US`,
      HongKong = `Hong Kong`,
      Singapore = `Singapore`,
      Macao = `Macao`,
      Malaysia = `Malaysia`,
      Bahrain = `Bahrain`,
      Bahamas = `Bahamas`,
      Bermuda = `Bermuda`,
      Guernsey = `Guernsey`,
      Jersey = `Jersey`,
      IsleofMan = `Isle of Man`,
      Luxembourg = `Luxembourg`,
      Cyprus = `Cyprus`,
      UK = `UK`,
      NetherlandsAntillesthenCuraç = `Netherlands Antilles (then Curaçao)`,
      Austria = `Austria`,
      Belgium = `Belgium`
    )
  
  
  haven_totals <- haven_totals %>%
    mutate(
      year = as.numeric(year)
    ) %>%
    filter(
      year == i
    )
  
  
  # Equivalent to:
  #
  # merge 1:m year using `countries', nogenerate
  
  countries_i <- countries_i %>%
    left_join(
      haven_totals,
      by = "year"
    )
  
  
  # ============================================================================
  # II.1 ---- Offshore wealth attracted by individual havens
  # ============================================================================
  
  # Stata:
  #
  # gen off6 = .
  # replace off6 = Switzerland if iso3saver == "CHE"
  # replace off6 = CaymanIslands if iso3saver == "CYM"
  # ...
  
  countries_i <- countries_i %>%
    mutate(
      off6 = case_when(
        
        iso3saver == "CHE" ~ Switzerland,
        
        iso3saver == "CYM" ~ CaymanIslands,
        
        iso3saver == "PAN" ~ Panama,
        
        iso3saver == "USA" ~ US,
        
        iso3saver == "HKG" ~ HongKong,
        
        iso3saver == "SGP" ~ Singapore,
        
        iso3saver == "MAC" ~ Macao,
        
        iso3saver == "MYS" ~ Malaysia,
        
        iso3saver == "BHR" ~ Bahrain,
        
        iso3saver == "BHS" ~ Bahamas,
        
        iso3saver == "BMU" ~ Bermuda,
        
        iso3saver == "GGY" ~ Guernsey,
        
        iso3saver == "JEY" ~ Jersey,
        
        iso3saver == "IMN" ~ IsleofMan,
        
        iso3saver == "LUX" ~ Luxembourg,
        
        iso3saver == "CYP" ~ Cyprus,
        
        iso3saver == "GBR" ~ UK,
        
        iso3saver == "AUT" ~ Austria,
        
        iso3saver == "BEL" ~ Belgium,
        
        iso3saver == "ANT" &
          year <= 2009 ~ NetherlandsAntillesthenCuraç,
        
        iso3saver == "CUW" &
          year > 2009 ~ NetherlandsAntillesthenCuraç,
        
        TRUE ~ NA_real_
      )
    )
  
  
  # ============================================================================
  # II.2 ---- Rename offshore wealth variables
  # ============================================================================
  
  # Stata:
  #
  # rename offshore_total off5
  # rename offshore_switzerland off4
  # rename offshore_EU_Havens off3
  # rename offshore_AS_Havens off2
  # rename offshore_CR_Havens off1
  
  countries_i <- countries_i %>%
    rename(
      off5 = offshore_total,
      off4 = offshore_switzerland,
      off3 = offshore_EU_Havens,
      off2 = offshore_AS_Havens,
      off1 = offshore_CR_Havens
    )
  
  
  # ============================================================================
  # II.3 ---- Reshape long
  # ============================================================================
  
  # Stata:
  #
  # reshape long off, i(iso3saver) j(haven_group)
  
  countries_i <- countries_i %>%
    pivot_longer(
      cols = c(
        off1,
        off2,
        off3,
        off4,
        off5,
        off6
      ),
      names_to = "haven_group",
      names_prefix = "off",
      values_to = "off"
    ) %>%
    mutate(
      haven_group = as.integer(
        haven_group
      )
    )
  
  
  # ============================================================================
  # II.4 ---- Haven group names
  # ============================================================================
  
  # Stata:
  #
  # 1 = americ
  # 2 = asian
  # 3 = europe
  # 4 = swiss
  # 5 = total
  # 6 = total_attracted
  
  countries_i <- countries_i %>%
    mutate(
      haven_group1 = case_when(
        haven_group == 5 ~ "total",
        haven_group == 6 ~ "total_attracted",
        haven_group == 4 ~ "swiss",
        haven_group == 3 ~ "europe",
        haven_group == 2 ~ "asian",
        haven_group == 1 ~ "americ",
        TRUE             ~ NA_character_
      ),
      
      unit = "USD Bn",
      
      label = case_when(
        haven_group1 == "americ" ~
          "offshore wealth in American tax havens",
        
        haven_group1 == "asian" ~
          "offshore wealth in Asian tax havens",
        
        haven_group1 == "europe" ~
          "offshore wealth in European tax havens",
        
        haven_group1 == "total" ~
          "total offshore wealth",
        
        haven_group1 == "swiss" ~
          "offshore wealth in Switzerland",
        
        haven_group1 == "total_attracted" ~
          "total offshore wealth attracted by this jurisdiction",
        
        TRUE ~ ""
      )
    ) %>%
    select(
      -haven_group
    ) %>%
    rename(
      value     = off,
      indicator = haven_group1,
      iso3      = iso3saver
    )
  
  
  # ============================================================================
  # II.5 ---- Keep variables
  # ============================================================================
  
  countries_i <- countries_i %>%
    select(
      iso3,
      value,
      indicator,
      year,
      unit,
      label,
      latin_am,
      europe,
      asia,
      africa
    )
  
  
  # ============================================================================
  # II.6 ---- Drop artificial shell-company observations
  # ============================================================================
  
  # Stata:
  #
  # drop if iso3 == "BEH" | iso3 == "CHH" | iso3 == "GBH" |
  #         iso3 == "IEH" | iso3 == "NLH" | iso3 == "USH"
  
  countries_i <- countries_i %>%
    filter(
      !iso3 %in% c(
        "BEH",
        "CHH",
        "GBH",
        "IEH",
        "NLH",
        "USH"
      )
    )
  
  
  # ============================================================================
  # Store annual dataset
  # ============================================================================
  
  countries_all[[as.character(i)]] <- countries_i
}


# ==============================================================================
# III ---- Append all years
# ==============================================================================

# Equivalent to the repeated Stata:
#
# append using "$work/countries"

countries <- bind_rows(
  countries_all
)


# ==============================================================================
# IV ---- Merge GDP data
# ==============================================================================

# Stata:
#
# import delimited "$raw/gdp_current.csv", clear
# merge 1:m year iso3 using "$work/countries", keep(2 3) nogenerate

gdp_current <- read_csv(
  file.path(
    raw,
    "gdp_current.csv"
  ),
  show_col_types = FALSE
)


# Inspect GDP variable name.
#
# In the original Stata file the final variable is called "gdp".
# If the CSV instead contains another name, e.g. gdp_current,
# rename it here.

if (!"gdp" %in% names(gdp_current)) {
  
  possible_gdp <- intersect(
    c(
      "gdp_current_dollars",
      "gdp_current",
      "GDP",
      "value"
    ),
    names(gdp_current)
  )
  
  if (length(possible_gdp) == 1) {
    
    gdp_current <- gdp_current %>%
      rename(
        gdp = all_of(
          possible_gdp
        )
      )
  }
}


# Stata keep(2 3):
#
# Keep all observations from countries and matched observations.
#
# Therefore countries is the left-hand dataset in the R translation.

countries <- countries %>%
  left_join(
    gdp_current,
    by = c(
      "year",
      "iso3"
    )
  )


# ==============================================================================
# V ---- Merge country frame
# ==============================================================================

# Original Stata:
#
# merge m:1 iso3 using "$raw/country_frame",
#     keepusing(country_name incomelevel regionname)
#     keep(1 3) nogenerate
#
# country_frame is a Stata file generated earlier in the replication package.

country_frame <- read_dta(
  file.path(
    raw,
    "country_frame.dta"
  )
)


# The original do-file refers to "incomelevel" in keepusing(),
# but later uses "incomelevelname".
#
# Standardise this here.

if (
  "incomelevel" %in% names(country_frame) &&
  !"incomelevelname" %in% names(country_frame)
) {
  
  country_frame <- country_frame %>%
    rename(
      incomelevelname = incomelevel
    )
}


country_frame <- country_frame %>%
  select(
    iso3,
    country_name,
    incomelevelname,
    regionname
  ) %>%
  distinct(
    iso3,
    .keep_all = TRUE
  )


# Stata keep(1 3) means:
# keep master-only and matched observations,
# but not country_frame-only observations.

countries <- countries %>%
  left_join(
    country_frame,
    by = "iso3"
  )


# ==============================================================================
# VI ---- Netherlands Antilles
# ==============================================================================

# Stata:
#
# replace country_name = "Netherlands Antilles" if iso3 == "ANT"
# replace incomelevelname = "High income" if iso3 == "ANT"
# replace regionname = "Latin America & Carribean" if iso3 == "ANT"

countries <- countries %>%
  mutate(
    country_name = if_else(
      iso3 == "ANT",
      "Netherlands Antilles",
      country_name
    ),
    
    incomelevelname = if_else(
      iso3 == "ANT",
      "High income",
      incomelevelname
    ),
    
    regionname = if_else(
      iso3 == "ANT",
      "Latin America & Carribean",
      regionname
    )
  )


# ==============================================================================
# VII ---- Final dataset
# ==============================================================================

# Drop continent dummy variables
countries <- countries %>%
  select(
    -any_of(
      c(
        "europe",
        "asia",
        "latin_am",
        "africa"
      )
    )
  )


# Stata:
#
# order year iso3 country_name indicator label unit value gdp
#       regionname incomelevelname
#
# sort year iso3 indicator

countries <- countries %>%
  select(
    year,
    iso3,
    country_name,
    indicator,
    label,
    unit,
    value,
    gdp,
    regionname,
    incomelevelname
  ) %>%
  arrange(
    year,
    iso3,
    indicator
  )


# ==============================================================================
# VIII ---- Variable labels
# ==============================================================================

attr(
  countries$value,
  "label"
) <- "Offshore wealth, in bn USD"

attr(
  countries$gdp,
  "label"
) <- "GDP, current prices"

attr(
  countries$year,
  "label"
) <- "Year"

attr(
  countries$country_name,
  "label"
) <- "Country"

attr(
  countries$iso3,
  "label"
) <- "Country ISO alpha-3 code"

attr(
  countries$unit,
  "label"
) <- "Unit and currency"

attr(
  countries$indicator,
  "label"
) <- "Abbr. of location of offshore wealth"

attr(
  countries$label,
  "label"
) <- "Location of offshore wealth"


# ==============================================================================
# IX ---- Export Excel
# ==============================================================================

# Stata:
#
# export excel using "$raw/FGZ-raw-data.xlsx",
#     sheet(ctrybyctry01-22)
#     firstrow(variables)
#     sheetreplace

output_excel <- file.path(work, "FGZ-raw-data-R.xlsx")

# Beim ersten Durchlauf die Originaldatei als Grundlage kopieren
if (!file.exists(output_excel)) {
  file.copy(
    file.path(raw, "FGZ-raw-data.xlsx"),
    output_excel
  )
}

if (file.exists(output_excel)) {
  
  wb <- loadWorkbook(
    output_excel
  )
  
} else {
  
  wb <- createWorkbook()
}


if ("ctrybyctry01-22" %in% names(wb)) {
  
  removeWorksheet(
    wb,
    "ctrybyctry01-22"
  )
}


addWorksheet(
  wb,
  "ctrybyctry01-22"
)


writeData(
  wb,
  sheet = "ctrybyctry01-22",
  x = countries,
  colNames = TRUE
)


saveWorkbook(
  wb,
  output_excel,
  overwrite = TRUE
)


# ==============================================================================
# X ---- Save Stata dataset
# ==============================================================================
saveRDS(countries, file = file.path(work, "countries.rds"))

# ==============================================================================
# XI ---- Checks
# ==============================================================================

cat(
  "\nFinished building country-level offshore wealth dataset.\n"
)

cat(
  "\nNumber of observations:",
  nrow(countries),
  "\n"
)

cat(
  "Years:",
  min(countries$year, na.rm = TRUE),
  "-",
  max(countries$year, na.rm = TRUE),
  "\n"
)

cat(
  "Countries:",
  n_distinct(countries$iso3),
  "\n"
)

cat(
  "\nIndicators:\n"
)

print(
  table(
    countries$indicator,
    useNA = "ifany"
  )
)

cat(
  "\nDataset saved to:\n",
  file.path(
    work,
    "countries.rds"
  ),
  "\n"
)