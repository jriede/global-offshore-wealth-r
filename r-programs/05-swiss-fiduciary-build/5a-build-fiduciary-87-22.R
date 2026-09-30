# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# This program constructs foreign owned time series of Swiss fiduciary deposits
# spanning 1987 to 2022.
#
# databases used: - fiduciary_1976-2014.dta
#                 - Codes-ISO-IFS-Region.xlsx
#                 - country-codes-iso3-ifs.xlsx
#                 - exchange_rates.xlsx
#                 - Multiplicative_FactorSNB.xlsx
#                 - snbdatafiduciary.csv
#                 - snbdomesticfiduciary.csv
#
# outputs:        - fiduciary-87-22.dta
#
# ==============================================================================


library(tidyverse)
library(readxl)
library(haven)


# ==============================================================================
# I ---- Cleaning and country code merge of SNB fiduciary data
# ==============================================================================


# ------------------------------------------------------------------------------
# I.1 - Adjust Swiss fiduciary variable names
# ------------------------------------------------------------------------------

fiduciary1 <- readr::read_delim(
  file.path(raw, "snbdatafiduciary.csv"),
  delim = ";",
  locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
  show_col_types = FALSE
) %>%
  rename(
    lfidu = Value,
    iso3  = INLANDAUSLAND,
    year  = Date
  ) %>%
  select(
    -BANKENGRUPPE,
    -WAEHRUNG,
    -KONSOLIDIERUNGSSTUFE
  )

fiduciary1 %>%
  filter(iso3 == "USA", year %in% 2014:2016) %>%
  select(year, iso3, lfidu)

names(fiduciary1)
# ------------------------------------------------------------------------------
# I.2 - Merge fiduciary accounts to ISO codes
# ------------------------------------------------------------------------------

codes_region <- read_excel(
  file.path(raw, "Codes-ISO-IFS-Region.xlsx"),
  sheet = "Stata-Regions"
) %>%
  distinct()


fiduciary2 <- codes_region %>%
  right_join(
    fiduciary1,
    by = "iso3"
  ) %>%
  rename(
    cn    = chart_name,
    ccode = iso3
  )


# ------------------------------------------------------------------------------
# I.3 - Minor adjustments to the data
# ------------------------------------------------------------------------------

fiduciary2 <- fiduciary2 %>%
  filter(ccode != "A") %>%
  mutate(
    cn = case_when(
      ccode == "BIZ_FR" ~ "France",
      ccode == "BIZ_PU" ~ "United States Minor Outlying Islands",
      ccode == "BIZ_1Z" ~ "West Indies UK",
      TRUE ~ cn
    )
  ) %>%
  filter(ccode != "XVU") %>%
  mutate(
    ccode = case_when(
      cn == "France" ~ "FRA",
      cn == "United States Minor Outlying Islands" ~ "UMI",
      ccode == "BIZ_1Z" ~ "VGB",
      ccode == "TAA" ~ "IOT",
      TRUE ~ ccode
    ),
    cn = case_when(
      ccode == "IOT" ~ "British Overseas Territories",
      ccode == "JEY" ~ "Jersey",
      ccode == "COG" ~ "Congo",
      TRUE ~ cn
    )
  )


# ------------------------------------------------------------------------------
# I.4 - Merge to conversion rates
# ------------------------------------------------------------------------------

exchange_rates <- read_excel(
  file.path(raw, "exchange_rates.xlsx"),
  sheet = "usd_chf"
) %>%
  filter(year >= 1987) %>%
  rename(
    uschf_end = `Domestic Currency per U.S. Dollar, End of Period`
  )


fiduciary3 <- exchange_rates %>%
  left_join(
    fiduciary2,
    by = "year",
    relationship = "one-to-many"
  )


# ------------------------------------------------------------------------------
# I.5 - Merge USD fiduciary accounts to IFS country codes
# ------------------------------------------------------------------------------

country_codes <- read_excel(
  file.path(raw, "country-codes-iso3-ifs.xlsx"),
  sheet = "Meged-ISO-IFS"
) %>%
  select(ifscode, ISO3) %>%
  mutate(
    ifscode = if_else(
      ISO3 == "VGB",
      371,
      as.numeric(ifscode)
    )
  ) %>%
  rename(
    ccode = ISO3
  ) %>%
  filter(
    ccode != "GLP",
    !(ifscode == 353 & ccode == "CUW"),
    !is.na(ccode),
    ccode != "",
    !is.na(ifscode)
  )


fiduciary4 <- country_codes %>%
  right_join(
    fiduciary3,
    by = "ccode"
  ) %>%
  filter(
    !(ccode == "UMI" & year >= 2001 & year <= 2004),
    !(ccode == "SRB" & year >= 2001 & year <= 2006)
  ) %>%
  mutate(
    ifscode = case_when(
      ccode == "JEY" ~ 1017,
      ccode == "COG" ~ 634,
      ccode == "IOT" ~ 372,
      TRUE ~ ifscode
    )
  ) %>%
  select(
    -any_of(c("region", "region_name"))
  ) %>%
  arrange(ifscode, year)


# ==============================================================================
# II --- Append havens not present anymore in Swiss fiduciary data
# ==============================================================================


# ------------------------------------------------------------------------------
# II.1 - Minor adjustments to the data
# ------------------------------------------------------------------------------

fiduciary_old <- read_dta(
  file.path(raw, "fiduciary_1976-2014.dta")
) %>%
  filter(
    cn == "Netherlands Antilles" |
      cn == "St. Kitts and Nevis" |
      cn == "Monaco" |
      (cn == "France" & year <= 2004 & year >= 1987) |
      cn == "Yugoslavia" |
      cn == "USSR" |
      cn == "British Antilles" |
      cn == "Antigua and Barbuda" |
      cn == "German Democratic Republic" |
      cn == "Tchecoslovakia" |
      cn == "Western Sahara" |
      (iso3 == "UMI" & year >= 2001 & year <= 2004) |
      (iso3 == "SRB" & year >= 2001 & year <= 2006)
  ) %>%
  mutate(
    cn = if_else(
      iso3 == "UMI",
      "United States Minor Outlying Islands",
      cn
    )
  )


fiduciary5 <- bind_rows(
  fiduciary_old,
  fiduciary4
) %>%
  filter(year >= 1987) %>%
  select(
    -any_of("lfidu_usd")
  ) %>%
  mutate(
    ccode = if_else(
      is.na(ccode) | ccode == "",
      iso3,
      ccode
    )
  ) %>%
  select(
    -any_of("iso3")
  ) %>%
  filter(
    !(is.na(lfidu) & ccode == "FRA"),
    !(is.na(lfidu) & cn == "West Indies UK")
  ) %>%
  mutate(
    lfidu = case_when(
      
      ccode %in% c(
        "ANT", "KNA", "MCO", "YUG", "USSR",
        "ATG", "GDR", "Tcheco", "ESH"
      ) ~ lfidu * 1000,
      
      cn == "British Antilles" ~ lfidu * 1000,
      
      ccode == "FRA" &
        year <= 2004 ~ lfidu * 1000,
      
      ccode == "UMI" &
        year >= 2001 &
        year <= 2004 ~ lfidu * 1000,
      
      ccode == "SRB" &
        year >= 2001 &
        year <= 2006 ~ lfidu * 1000,
      
      TRUE ~ lfidu
    )
  ) %>%
  filter(
    !(cn == "British Antilles" & year >= 2005)
  )


# ------------------------------------------------------------------------------
# II.2 - Fill missing country names
# ------------------------------------------------------------------------------

fiduciary5 <- fiduciary5 %>%
  mutate(
    cn = if_else(
      ccode == "SHN",
      "St Helen",
      cn
    )
  ) %>%
  filter(
    !is.na(cn),
    cn != ""
  )


# ==============================================================================
# III ------- The case of Liechtenstein
# ==============================================================================

# Before 1984, Liechtenstein is considered as a foreign country.
# After 1984, deposits from Liechtenstein are considered Swiss deposits.
#
# For the post-1984 period, deposits from Liechtenstein are estimated as
# 45% of Swiss-owned fiduciary deposits.


liechtenstein <- readr::read_delim(
  file.path(raw, "snbdomesticfiduciary.csv"),
  delim = ";",
  skip = 3,
  locale = readr::locale(
    decimal_mark = ".",
    grouping_mark = ","
  ),
  show_col_types = FALSE
)

# Stata imports v7, cubeid and v8.
# Because the exact names of the unnamed CSV columns depend on readr,
# identify the corresponding columns by position.
liechtenstein <- liechtenstein %>%
  rename(
    year  = Date,
    lfidu = Value
  ) %>%
  select(
    year,
    lfidu,
    uschf_end
  ) %>%
  mutate(
    year      = as.numeric(year),
    lfidu     = as.numeric(lfidu),
    uschf_end = as.numeric(uschf_end),
    
    ccode = "LIE",
    
    # Liechtenstein = 45% of Swiss-owned fiduciary deposits
    lfidu = 0.45 * lfidu,
    
    cn      = "Liechtenstein",
    ifscode = 9006
  ) %>%
  filter(
    year >= 1987,
    year <= 2022
  )


fiduciary6 <- bind_rows(
  liechtenstein,
  fiduciary5
)


# ==============================================================================
# IV ------ Bank-office level fiduciary deposits
# ==============================================================================

fiduciary6 <- fiduciary6 %>%
  mutate(
    # Fiduciary liabilities at parent company level,
    # millions of USD at end-of-period exchange rate
    lfidudol = lfidu / uschf_end
  ) %>%
  arrange(year)


# Multiplicative factor converting parent-company-level deposits
# to bank-office-level deposits

multiplicative_factor <- read_excel(
  file.path(raw, "Multiplicative_FactorSNB.xlsx"),
  range = "A26:D62"
) %>%
  select(
    year,
    factor
  ) %>%
  mutate(
    year   = as.numeric(year),
    factor = as.numeric(factor)
  )


fiduciary <- multiplicative_factor %>%
  left_join(
    fiduciary6,
    by = "year",
    relationship = "one-to-many"
  ) %>%
  mutate(
    
    # Fiduciary liabilities at bank-office level,
    # millions of CHF
    lfidu2 = lfidu * factor,
    
    # Fiduciary liabilities at bank-office level,
    # millions of USD
    lfidu2dol = lfidu2 / uschf_end
    
  ) %>%
  select(
    -factor,
    -uschf_end
  )


# ==============================================================================
# V ----- Definition of geographical areas
# ==============================================================================


# ------------------------------------------------------------------------------
# Euro area
# ------------------------------------------------------------------------------

euro11_countries <- c(
  "Austria",
  "Belgium",
  "Finland",
  "France",
  "Germany",
  "Ireland",
  "Italy",
  "Luxembourg",
  "Netherlands",
  "Portugal",
  "Spain"
)


euro17_additional <- c(
  "Cyprus",
  "Estonia",
  "Greece",
  "Malta",
  "Slovak Republic",
  "Slovenia"
)


fiduciary <- fiduciary %>%
  mutate(
    
    euro11 = as.integer(
      cn %in% euro11_countries
    ),
    
    euro17 = as.integer(
      euro11 == 1 |
        cn %in% euro17_additional
    ),
    
    euro16 = euro17,
    
    euro16 = if_else(
      cn == "Estonia",
      0L,
      euro16
    )
    
  ) %>%
  select(
    -euro11,
    -euro17
  )


# ------------------------------------------------------------------------------
# Rich and developing countries
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  mutate(
    
    rich = as.integer(
      ifscode < 200 |
        euro16 == 1
    ),
    
    rich = if_else(
      cn %in% c(
        "San Marino",
        "South Africa",
        "Turkey",
        "Vatican"
      ),
      0L,
      rich
    ),
    
    developing = as.integer(
      rich == 0
    )
    
  )


# ------------------------------------------------------------------------------
# Offshore financial centres
# ------------------------------------------------------------------------------

ofc_countries <- c(
  "San Marino",
  "Luxembourg",
  "Malta",
  "Costa Rica",
  "Panama",
  "Uruguay",
  "Antigua and Barbuda",
  "Bahamas",
  "Barbados",
  "Dominica",
  "Grenada",
  "Belize",
  "Netherlands Antilles",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "British Antilles",
  "British Overseas Territories",
  "Cayman Islands",
  "Turks and Caicos Islands",
  "Bahrain",
  "Cyprus",
  "Lebanon",
  "Malaysia",
  "Palau",
  "Singapore",
  "Liberia",
  "Mauritius",
  "Seychelles",
  "Gibraltar",
  "Nauru",
  "Vanuatu",
  "Samoa",
  "Marshall Islands",
  "Andorra",
  "Guernsey",
  "Isle of Man",
  "Jersey",
  "West Indies UK",
  "Macao",
  "Curacao",
  "Bonaire, Sint Eustatius and Saba",
  "St. Kitts and Nevis",
  "Monaco",
  "Sint Maarten (Dutch part)",
  "Aruba",
  "Liechtenstein",
  "Bermuda"
)


fiduciary <- fiduciary %>%
  mutate(
    ofc = as.integer(
      cn %in% ofc_countries |
        ccode %in% c("HKG", "FRO")
    )
  )


# ------------------------------------------------------------------------------
# Continents
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  mutate(
    
    # North America
    north_am = as.integer(
      cn %in% c(
        "United States of America",
        "Canada"
      )
    ),
    
    # Latin America
    latin_am = as.integer(
      (ifscode >= 200 & ifscode < 300) |
        cn == "Falkland Islands"
    ),
    
    latin_am = if_else(
      ccode == "PSE" |
        cn == "Yugoslavia",
      0L,
      latin_am
    ),
    
    # Caribbean
    caribbean = as.integer(
      (ifscode >= 300 & ifscode < 400) |
        cn == "Cuba"
    ),
    
    caribbean = if_else(
      cn %in% c(
        "Curacao",
        "Netherlands Antilles",
        "St. Kitts and Nevis",
        "British Antilles",
        "Falkland Islands",
        "Aruba"
      ),
      0L,
      caribbean
    ),
    
    # Middle East
    middle_east = as.integer(
      (ifscode >= 400 & ifscode < 500) |
        ccode == "PSE"
    ),
    
    middle_east = if_else(
      cn == "Cyprus",
      0L,
      middle_east
    )
    
  )


# ------------------------------------------------------------------------------
# Asia
# ------------------------------------------------------------------------------

asia_countries <- c(
  "Australia",
  "New Zealand",
  "Japan",
  "China",
  "Korea, Dem. Rep.",
  "Mongolia",
  "Tuvalu",
  "French Polynesia",
  "Vanuatu",
  "Tonga",
  "Papua New Guinea",
  "Nauru",
  "New Caledonia",
  "Wallis et Futuna",
  "St Helena",
  "Kiribati",
  "Solomon Islands",
  "Fiji",
  "Wallis and Futuna Islands",
  "Ouzbekistan",
  "Kyrgyz Republic",
  "Turkmenistan",
  "Tajikistan",
  "Uzbekistan",
  "Korea (Democratic People's Republic of)",
  "USSR",
  
  # Countries at the frontier between Europe and Asia
  "Georgia",
  "Russian Federation",
  "Armenia",
  "Azerbaijan",
  "Kazakhstan",
  "Turkey",
  "Kyrgyzstan",
  "St Helen"
)


fiduciary <- fiduciary %>%
  mutate(
    
    asia = as.integer(
      (ifscode >= 500 & ifscode < 600) |
        cn %in% asia_countries |
        ccode == "UMI"
    ),
    
    asia = if_else(
      cn %in% c(
        "Macao",
        "Bonaire, Sint Eustatius and Saba",
        "Sint Maarten (Dutch part)"
      ),
      0L,
      asia
    )
    
  )


# ------------------------------------------------------------------------------
# Africa
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  mutate(
    africa = as.integer(
      (ifscode >= 600 & ifscode < 700) |
        (ifscode >= 700 & ifscode < 800) |
        cn %in% c(
          "South Africa",
          "Western Sahara"
        )
    )
  )


# ------------------------------------------------------------------------------
# Europe
# ------------------------------------------------------------------------------

additional_europe <- c(
  "Croatia",
  "Estonia",
  "Ukraine",
  "Moldova",
  "Serbia",
  "Montenegro",
  "Czech Republic",
  "Romania",
  "Belarus",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Lithuania",
  "Latvia",
  "Slovakia",
  "Moldova (Republic of)",
  "Albania",
  "Poland",
  "Hungary",
  "Macedonia (former Yugoslav)",
  "Yugoslavia",
  "Tchecoslovakia",
  "German Democratic Republic"
)


fiduciary <- fiduciary %>%
  mutate(
    
    europe = as.integer(
      ifscode < 200 &
        north_am != 1 &
        asia != 1 &
        africa != 1 &
        cn != "Turkey"
    ),
    
    europe = if_else(
      euro16 == 1,
      1L,
      europe
    ),
    
    europe = if_else(
      cn == "Luxembourg",
      0L,
      europe
    ),
    
    europe = if_else(
      cn %in% additional_europe,
      1L,
      europe
    )
    
  )


# ------------------------------------------------------------------------------
# Drop offshore financial centres from continents and groups
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  mutate(
    
    rich = if_else(
      rich == 1 & ofc == 1,
      0L,
      rich
    ),
    
    developing = if_else(
      developing == 1 & ofc == 1,
      0L,
      developing
    ),
    
    europe = if_else(
      europe == 1 & ofc == 1,
      0L,
      europe
    ),
    
    middle_east = if_else(
      middle_east == 1 & ofc == 1,
      0L,
      middle_east
    ),
    
    africa = if_else(
      africa == 1 & ofc == 1,
      0L,
      africa
    ),
    
    asia = if_else(
      asia == 1 & ofc == 1,
      0L,
      asia
    ),
    
    caribbean = if_else(
      caribbean == 1 & ofc == 1,
      0L,
      caribbean
    ),
    
    latin_am = if_else(
      latin_am == 1 & ofc == 1,
      0L,
      latin_am
    )
    
  )


# ------------------------------------------------------------------------------
# Define continent and country-group variables
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  mutate(
    
    continent =
      1 * africa +
      2 * europe +
      3 * middle_east +
      4 * asia +
      5 * caribbean +
      6 * latin_am +
      7 * north_am +
      8 * ofc,
    
    group =
      1 * rich +
      2 * developing +
      3 * ofc
    
  )


# Optional labelled equivalents of the Stata value labels
fiduciary <- fiduciary %>%
  mutate(
    
    continent_label = case_when(
      continent == 1 ~ "Africa",
      continent == 2 ~ "Europe",
      continent == 3 ~ "Middle East",
      continent == 4 ~ "Asia",
      continent == 5 ~ "Caribbean",
      continent == 6 ~ "Latin and South America",
      continent == 7 ~ "North America",
      continent == 8 ~ "OFC",
      TRUE ~ NA_character_
    ),
    
    group_label = case_when(
      group == 1 ~ "Rich",
      group == 2 ~ "Developing",
      group == 3 ~ "OFC",
      TRUE ~ NA_character_
    )
    
  ) %>%
  arrange(
    ifscode,
    year
  )


# ==============================================================================
# SAVE OUTPUT
# ==============================================================================
saveRDS(fiduciary, file = file.path(work, "fiduciary-87-22.rds"))
# write_dta(fiduciary,file.path(work, "fiduciary-87-22.dta"))

write.csv(
  fiduciary,
  file.path(work, "fiduciary-87-22-r.csv"),
  row.names = FALSE,
  na = ""
)
