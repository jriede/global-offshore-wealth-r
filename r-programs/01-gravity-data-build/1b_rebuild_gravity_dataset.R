# ------------------------------------------------------------------------------
# Project: Offshore financial wealth database - update 2023
# Title: 1b_rebuild_gravity_dataset.R
# Purpose: reproduce and extend the dataset "data_gravity.dta"
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(haven)
library(zoo)
library(purrr)

# Helper paths assumed to exist:
# raw  <- "path/to/raw"
# work <- "path/to/work"

# ------------------------------------------------------------------------------
# 1. bilateral portfolio assets from CPIS
# ------------------------------------------------------------------------------

cpis <- read_csv(
  file.path(raw, "CPIS_06-08-2023_11-40-56-69_timeSeries",
            "CPIS_06-08-2023 11-40-56-69_timeSeries.csv"),
  show_col_types = FALSE
)

cpis <- cpis %>%
  # keep total equity and total debt liabilities
  filter(`Indicator Code` %in% c("I_L_D_T_T_BP6_DV_USD", "I_L_E_T_T_BP6_DV_USD")) %>%
  
  # rename indicator code column 
  mutate(
    `Indicator Name` = case_when(
      `Indicator Code` == "I_L_D_T_T_BP6_DV_USD" ~ "debt",
      `Indicator Code` == "I_L_E_T_T_BP6_DV_USD" ~ "equity",
      TRUE ~ `Indicator Name`
    )
  ) %>%
  # filter out "Status" attribute
  filter(`Attribute` == "Value") %>%
  
  # select specific columns
  select(
    `Country Name`, `Country Code`, `Indicator Name`,
    `Counterpart Country Name`, `Counterpart Country Code`, `2001`, `2002`, `2003`, 
    `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, 
    `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`
    #v12:v32
  ) %>%
  
  # years in columns -> one year per row
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "v"
  ) %>%
 # mutate(
  #  year = as.integer(str_remove(year, "^v")) + 1989
  #) %>%
  
  filter(!`Country Code` %in% c(1, 31)) %>%
  mutate(v = as.numeric(v)) %>%
  pivot_wider(
    names_from = `Indicator Name`,
    values_from = v,
    names_prefix = "v"
  ) %>%
  rename(
    host = `Country Code`,
    hostname = `Country Name`,
    source = `Counterpart Country Code`,
    sourcename = `Counterpart Country Name`,
    debtasset = vdebt,
    eqasset = vequity
  ) %>%
  mutate(
    debtasset = debtasset / 1e6,
    eqasset = eqasset / 1e6
  ) %>%
  arrange(hostname, sourcename, year)

# Collapse host lines for Curacao and Sint Maarten into one
cpis_curacao_bil_host <- cpis %>%
  filter(host %in% c(354, 352)) %>%
  mutate(host = 355) %>%
  group_by(host, source, year) %>%
  summarise(
    hostname = first(na.omit(hostname)),
    sourcename = first(na.omit(sourcename)),
    debtasset = sum(debtasset, na.rm = TRUE),
    eqasset = sum(eqasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(hostname = "Curacao and Sint Maarten")

# ##### convert host(double) to host (char) in cpis_curacao_bil_host
cpis_curacao_bil_host$host <- as.character(cpis_curacao_bil_host$host)

cpis <- cpis %>%
  filter(!host %in% c(354, 352)) %>%
  bind_rows(cpis_curacao_bil_host)

# Save CPIS source indicator
cpis_source <- cpis %>%
  distinct(source) %>%
  mutate(cpis = 1)

write_dta(cpis_source, file.path(work, "cpis_source.dta"))

# Duplicate relationships that exist as host-source also as source-host
missing_relationships <- cpis %>%
  transmute(
    year = year,
    source = host,
    host = source,
    sourcename = hostname,
    hostname = sourcename
  )

missing_relationships_append <- missing_relationships %>%
  anti_join(
    cpis %>% select(year, source, host),
    by = c("year", "source", "host")
  )

cpis_merge <- cpis %>%
  bind_rows(missing_relationships_append)

write_dta(cpis_merge, file.path(work, "cpis_merge.dta"))

# ------------------------------------------------------------------------------
# 2. CEPII database gravity controls
# ------------------------------------------------------------------------------

gravity <- read_dta(file.path(raw, "Gravity_dta_V202211", "Gravity_V202211.dta"))
countries <- read_dta(file.path(raw, "Gravity_dta_V202211", "Countries_V202211.dta"))

gravity_vars_1 <- gravity %>%
  rename(country_id = country_id_d) %>%
  left_join(countries, by = "country_id") %>%
  rename(
    country_id_d = country_id,
    countryname_d = country
  ) %>%
  filter(last_year >= 2001) %>%
  select(
    year, country_id_o, country_id_d, iso3_o, iso3_d,
    country_exists_o, country_exists_d, dist, comlang_off, col45,
    pop_o, pop_d, starts_with("countryname")
  ) %>%
  rename(country_id = country_id_o) %>%
  left_join(countries, by = "country_id") %>%
  rename(
    country_id_o = country_id,
    countryname_o = country
  ) %>%
  filter(last_year >= 2001, year > 2000) %>%
  select(
    year, country_id_o, country_id_d, iso3_o, iso3_d,
    countryname_o, countryname_d, country_exists_o, country_exists_d,
    dist, comlang_off, col45, pop_o, pop_d
  ) %>%
  filter(!(iso3_o == "IDN" & country_exists_o == 0),
         !(iso3_d == "IDN" & country_exists_d == 0),
         !(iso3_o == "SDN" & country_exists_o == 0),
         !(iso3_d == "SDN" & country_exists_d == 0),
         !(iso3_o == "SCG" & country_exists_o == 0),
         !(iso3_d == "SCG" & country_exists_d == 0),
         !(iso3_o == "SRB" & country_exists_o == 0),
         !(iso3_d == "SRB" & country_exists_d == 0),
         !(iso3_o == "SCG" & year == 2006),
         !(iso3_d == "SCG" & year == 2006)) %>%
  mutate(
    iso3_o = if_else(iso3_o == "SCG" & year < 2007, "SRB", iso3_o),
    iso3_d = if_else(iso3_d == "SCG" & year < 2007, "SRB", iso3_d)
  )

# More gravity vars: longitude, latitude, landlocked
geo_cepi <- read_dta(file.path(raw, "cepii", "geo_cepii.dta")) %>%
  select(iso3, country, landlocked, lat, lon, city_en, cap) %>%
  rename(
    landlocked_source = landlocked,
    lat_source = lat,
    lon_source = lon
  ) %>%
  group_by(iso3) %>%
  mutate(
    nvals = row_number(),
    help = mean(nvals)
  ) %>%
  ungroup() %>%
  filter(!(help != 1 & cap != 1)) %>%
  select(-nvals, -help, -city_en, -cap) %>%
  mutate(
    iso3 = case_when(
      iso3 == "TMP" ~ "TLS",
      iso3 == "PAL" ~ "PSE",
      iso3 == "ROM" ~ "ROU",
      iso3 == "YUG" ~ "SRB",
      iso3 == "ZAR" ~ "COD",
      TRUE ~ iso3
    )
  )

gravity_vars_2 <- gravity_vars_1 %>%
  left_join(
    geo_cepi %>% rename(iso3_o = iso3),
    by = "iso3_o"
  ) %>%
  filter(!is.na(country_id_o) | !is.na(country_id_d)) %>%
  select(-country)

gravity_vars <- gravity_vars_2 %>%
  left_join(
    geo_cepi %>%
      select(iso3, lat_source, lon_source) %>%
      rename(
        iso3_d = iso3,
        lat_host = lat_source,
        lon_host = lon_source
      ),
    by = "iso3_d"
  ) %>%
  select(-starts_with("country_id"), -starts_with("country_exists"), -starts_with("countryname"))

# Merge matching table iso3 / ifscode
iso_ifscode <- read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta")) %>%
  filter(iso3 != "")

write_dta(iso_ifscode, file.path(work, "iso_ifscode.dta"))

gravity_vars <- gravity_vars %>%
  rename(iso3 = iso3_o) %>%
  left_join(iso_ifscode, by = "iso3") %>%
  filter(!is.na(ifscode)) %>%
  select(-country) %>%
  rename(
    iso3_source = iso3,
    source = ifscode,
    our_code_source = our_code
  ) %>%
  rename(iso3 = iso3_d) %>%
  left_join(iso_ifscode, by = "iso3") %>%
  filter(!is.na(ifscode)) %>%
  select(-country) %>%
  rename(
    iso3_host = iso3,
    host = ifscode,
    our_code_host = our_code
  )

# Collapse Curacao and Sint Maarten into one line
gravity_vars <- gravity_vars %>%
  arrange(source, host, year) %>%
  mutate(source = if_else(source %in% c(352, 354), 355, source)) %>%
  group_by(source, host, year) %>%
  summarise(
    iso3_source = first(na.omit(iso3_source)),
    iso3_host = first(na.omit(iso3_host)),
    pop_d = first(na.omit(pop_d)),
    comlang_off = first(na.omit(comlang_off)),
    col45 = first(na.omit(col45)),
    lon_host = first(na.omit(lon_host)),
    lat_host = first(na.omit(lat_host)),
    landlocked_source = first(na.omit(landlocked_source)),
    our_code_source = first(na.omit(our_code_source)),
    our_code_host = first(na.omit(our_code_host)),
    dist = mean(dist, na.rm = TRUE),
    lon_source = mean(lon_source, na.rm = TRUE),
    lat_source = mean(lat_source, na.rm = TRUE),
    pop_o = sum(pop_o, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pop_o = na_if(pop_o, 0)) %>%
  mutate(host = if_else(host %in% c(352, 354), 355, host)) %>%
  group_by(source, host, year) %>%
  summarise(
    iso3_source = first(na.omit(iso3_source)),
    iso3_host = first(na.omit(iso3_host)),
    lat_source = first(na.omit(lat_source)),
    lon_source = first(na.omit(lon_source)),
    pop_o = first(na.omit(pop_o)),
    comlang_off = first(na.omit(comlang_off)),
    col45 = first(na.omit(col45)),
    landlocked_source = first(na.omit(landlocked_source)),
    our_code_source = first(na.omit(our_code_source)),
    our_code_host = first(na.omit(our_code_host)),
    dist = mean(dist, na.rm = TRUE),
    lon_host = mean(lon_host, na.rm = TRUE),
    lat_host = mean(lat_host, na.rm = TRUE),
    pop_d = sum(pop_d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pop_d = na_if(pop_d, 0),
    our_code_source = if_else(source == 355, 355, our_code_source),
    our_code_host = if_else(host == 355, 355, our_code_host)
  )

for (v in c("dist", "comlang_off", "col45", "pop_o", "pop_d",
            "lat_source", "lat_host", "lon_source", "lon_host",
            "landlocked_source")) {
  gravity_vars[[v]] <- ifelse(
    (gravity_vars$host == 355 & gravity_vars$source == 355) |
      (gravity_vars$source == 355 & gravity_vars$year < 2010) |
      (gravity_vars$host == 355 & gravity_vars$year < 2010),
    NA,
    gravity_vars[[v]]
  )
}

write_dta(gravity_vars, file.path(work, "gravity_vars.dta"))

# ##### convert 
gravity_vars$year <- as.character(gravity_vars$year)
gravity_vars$source <- as.character(gravity_vars$source)
gravity_vars$host <- as.character(gravity_vars$host)
############

# Merge CPIS and gravity vars
data_gravity_update <- gravity_vars %>%
  full_join(cpis_merge, by = c("year", "source", "host"))

write_dta(data_gravity_update, file.path(work, "data_gravity_update.dta"))

# ------------------------------------------------------------------------------
# 3. merge GDP
# ------------------------------------------------------------------------------

###
gdp <- gdp %>%
  rename(
  gdp_current = `gdp_current_dollars`,
  ) 
####

gdp <- read_dta(file.path(work, "iso_ifscode.dta")) %>%
  mutate(iso3 = if_else(iso3 == "XXK", "XKX", iso3)) %>%
  inner_join(
    read_dta(file.path(raw, "dta", "assembled_gdp_series_090623.dta")),
    by = "iso3"
  ) %>%
  rename(
    gdp_current = `gdp_current_dollars`
  ) %>%
  select(ifscode, gdp_current, year, iso3) %>%
  rename(
    source = ifscode,
    iso3_source = iso3
  ) %>%
  filter(!year %in% c(2000, 2022)) %>%
  mutate(source = if_else(source %in% c(352, 354), 355, source)) %>%
  group_by(year, source) %>%
  summarise(
    gdp_current = sum(gdp_current, na.rm = TRUE),
    iso3_source = first(na.omit(iso3_source)),
    .groups = "drop"
  ) %>%
  mutate(
    gdp_current = gdp_current / 1e6,
    gdp_current = na_if(gdp_current, 0)
  )

##### already in, no need to add

# Add GDP for Netherlands Antilles
gdp_353 <- read_dta(file.path(work, "ewn_gdp.dta")) %>%
  filter(source == 353) %>%
  select(-country)

gdp <- gdp %>%
  bind_rows(gdp_353) %>%
  mutate(
    gdp_current = if_else(source == 353 & is.na(gdp_current), gdp_us, gdp_current),
    iso3_source = if_else(source == 353, "ANT", iso3_source)
  ) %>%
  select(-gdp_us)
########

gdp_host <- gdp %>%
  rename(
    host = source,
    iso3_host = iso3_source
  )

#####
# ##### convert 
gdp_host$host <- as.character(gdp_host$host)
gdp_host$year <- as.character(gdp_host$year)
gdp$source <- as.character(gdp$source)
gdp$year <- as.character(gdp$year)
############

######

data_gravity_update <- data_gravity_update %>%
  left_join(
    gdp %>% select(source, year, gdp_current),
    by = c("source", "year")
  ) %>%
  rename(gdp_source = gdp_current) %>%
  left_join(
    gdp_host %>% select(host, year, gdp_current),
    by = c("host", "year")
  ) %>%
  rename(gdp_host = gdp_current)

write_dta(data_gravity_update, file.path(work, "data_gravity_update.dta"))

# ------------------------------------------------------------------------------
# 4. merge population from World Bank WDI
# ------------------------------------------------------------------------------


pop_wdi <- read_csv(
  file.path(raw, "API_SP.POP.TOTL_DS2_en_csv_v2_4902028",
            "API_SP.POP.TOTL_DS2_en_csv_v2_4902028.csv"),
  show_col_types = FALSE
) %>%
  #select(v1, v2, v46:v66) %>%
  select(1:2, 46:66) %>%
  rename(
    country_wdi = `Country Name`,
    iso3 = `Country Code`
  ) %>%
  #slice(-1, -2) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "pop_wdi"
  ) %>%
  mutate(
  #  year = as.integer(str_remove(year, "^v")) + 1955,
    pop_wdi = as.numeric(pop_wdi) * 1000,
    iso3 = if_else(iso3 == "XKX", "XXK", iso3)
  ) %>%
  left_join(read_dta(file.path(work, "iso_ifscode.dta")), by = "iso3") %>%
  filter(!is.na(ifscode)) %>%
  select(-country, -iso3) %>%
  rename(source = ifscode)

pop_355 <- pop_wdi %>%
  filter(source %in% c(354, 352)) %>%
  group_by(year) %>%
  summarise(pop_wdi = sum(pop_wdi, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = 355)

pop_source <- pop_wdi %>%
  bind_rows(pop_355) %>%
  filter(!source %in% c(354, 352)) %>%
  rename(pop_wdi_source = pop_wdi) %>%
  select(-country_wdi)

### convert ...
pop_source$source <- as.character(pop_source$source)
pop_source$year <- as.character(pop_source$year)

data_gravity_update <- read_dta(file.path(work, "data_gravity_update.dta"))

data_gravity_update <- data_gravity_update %>%
  left_join(pop_source, by = c("year", "source"))

data_gravity_update <- data_gravity_update %>%
  mutate(
    pop_wdi_source = pop_wdi_source / 1000000,
    pop_o = if_else(is.na(pop_o), pop_wdi_source, pop_o)
  ) %>%
  rename(pop_source = pop_o) %>%
  select(-pop_d, -starts_with("pop_wdi"))


# Complete population data for jurisdictions with available GDP
#### omitted for now, FIXME
data_gravity_update <- data_gravity_update %>%
  mutate(
    pop_source = if_else(source == 312, 15, pop_source),                         # Anguilla
    pop_source = if_else(source == 815 & year == 2021, 18, pop_source),         # Cook Islands
    pop_source = if_else(source == 351, 5, pop_source)                          # Montserrat
  )

# Guernsey
pop_guernsey2 <- read_excel(
  file.path(raw, "Guernsey_Historic_population_and_employment_data_(for_website).xlsx")
) %>%
  select(1:2) %>%
  rename(A = year) %>%
  rename(B = `Female and Male`) %>%
  filter(B != "") %>%
  mutate(
    A = as.numeric(A),
    B = as.numeric(B)
  ) %>%
  filter(A > 2000, !is.na(A)) %>%
  mutate(
    B = B / 1e6,
    source = 113
  ) %>%
  rename(year = A, pop_guernsey = B)

pop_guernsey2$year <- as.character(pop_guernsey2$year)
pop_guernsey2$source <- as.character(pop_guernsey2$source)

####
data_gravity_update <- data_gravity_update %>%
  left_join(pop_guernsey2, by = c("year", "source")) %>%
  mutate(
    pop_source = if_else(source == 113, pop_guernsey, pop_source)
  )

# Wir bauen eine Funktion, die Fehler einfach "schluckt"
safe_approx_ultimate <- function(y, x) {
  tryCatch({
    # Versuch der Interpolation
    # Wir fügen noch einen Check ein, ob y überhaupt ein Vektor ist
    if (sum(!is.na(y)) < 2) {
      return(as.numeric(y))
    }
    zoo::na.approx(y, x = x, na.rm = FALSE, rule = 2)
  }, 
  error = function(e) {
    # Falls DOCH ein Fehler auftritt (wie in Gruppe 187), 
    # gib einfach die Originaldaten als Zahl zurück
    return(as.numeric(y))
  })
}

# Anwendung
data_gravity_update <- data_gravity_update %>%
  group_by(source) %>%
  mutate(pop_epo = safe_approx_ultimate(pop_source, year)) %>%
  ungroup()


  ######

######## nicht ausführen, s.o.
data_gravity_update <- data_gravity_update %>%
  left_join(pop_guernsey2, by = c("year", "source")) %>%
  mutate(
    pop_source = if_else(source == 113, pop_guernsey, pop_source)
  ) %>%
  group_by(source) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    pop_epo = zoo::na.approx(pop_source, x = year, na.rm = FALSE, rule = 2),
    pop_source = if_else(source == 113, pop_epo, pop_source)
  ) %>%
  ungroup() %>%
  select(-pop_guernsey)
#########

# Fill gaps for Cook Islands and Taiwan
data_gravity_update <- data_gravity_update %>%
  mutate(
    pop_source = if_else(source == 815 & is.na(pop_source), pop_epo, pop_source),
    pop_source = if_else(source == 528 & is.na(pop_source), pop_epo, pop_source)
  ) %>%
  select(-pop_epo)

# Jersey
pop_jersey <- read_csv(
  file.path(raw, "Jersey_total-population-annual-change-natural-growth-net-migration-per-year-with-midyear.csv"),
  show_col_types = FALSE
) 

pop_jersey <- pop_jersey %>%
  select(Year, `End of year population estimate`) %>%
  filter(Year > 2000) %>%
  rename(pop_jersey = `End of year population estimate`) %>%
  mutate(
    pop_jersey = pop_jersey / 1e6,
    source = 117
  )

pop_jersey <- pop_jersey %>%
  rename(year = Year) 
pop_jersey$year <- as.character(pop_jersey$year)
pop_jersey$source <- as.character(pop_jersey$source)

data_gravity_update <- data_gravity_update %>%
  #left_join(pop_jersey, by = c("Year", "source")) %>%
  left_join(pop_jersey, by = c("year", "source")) %>%
  mutate(pop_source = if_else(source == 117, pop_jersey, pop_source)) %>%
  select(-pop_jersey)

# Merge population to host country
pop_host <- data_gravity_update %>%
  select(year, source, pop_source) %>%
  rename(
    host = source,
    pop_host = pop_source
  ) %>%
  distinct(year, host, .keep_all = TRUE)

data_gravity_update <- data_gravity_update %>%
  left_join(pop_host, by = c("year", "host"))

# ------------------------------------------------------------------------------
# 5. compute gravity variables
# ------------------------------------------------------------------------------

data_gravity_update <- data_gravity_update %>%
  mutate(
    logdist = log(dist),
    gap_lon = abs(lon_host - lon_source),
    gdppc_source = gdp_source / pop_source * 1000,
    gdppc_host = gdp_host / pop_host * 1000,
    gap_gdp = abs(gdp_source - gdp_host),
    gap_gdppc = abs(gdppc_source - gdppc_host),
    logeqasset = log(eqasset),
    logdebtasset = log(debtasset),
    logpop_source = log(pop_source),
    loggdppc_source = log(gdppc_source),
    loggap_gdp = log(gap_gdp),
    loggap_gdppc = log(gap_gdppc)
  )

# ------------------------------------------------------------------------------
# 6. complete gravity dataset
# ------------------------------------------------------------------------------

# balance panel
data_gravity_update <- data_gravity_update %>%
  complete(source, host, year)

# Harmonise country codes with Zucman (2013)
recode_zucman <- function(x) {
  case_when(
    x == 91  ~ 9998,
    x == 93  ~ 9999,
    x == 113 ~ 1006,
    x == 118 ~ 1012,
    x == 117 ~ 1017,
    x == 147 ~ 9006,
    x == 171 ~ 1001,
    x == 183 ~ 1003,
    x == 187 ~ 139,
    x == 359 ~ 1103,
    x == 371 ~ 1200,
    x == 373 ~ 1201,
    x == 585 ~ 372,
    x == 793 ~ 687,
    x == 814 ~ 1007,
    x == 818 ~ 1104,
    x == 849 ~ 1101,
    x == 851 ~ 1100,
    x == 857 ~ 889,
    x == 863 ~ 1102,
    x == 865 ~ 1008,
    x == 876 ~ 1009,
    x == 920 ~ 1005,
    TRUE ~ x
  )
}

data_gravity_update <- data_gravity_update %>%
  mutate(
    source = recode_zucman(source),
    host = recode_zucman(host)
  )


missing_gravity_vars <- read_dta(file.path(raw, "Zucman", "data_gravity.dta")) %>%
  distinct(source, host, .keep_all = TRUE) %>%
  rename(
    gap_lon_2013 = gap_lon,
    logdist_2013 = logdist,
    col45_2013 = col45,
    comlang_off_2013 = comlang_off,
    lat_source_2013 = lat_source,
    landlocked_source_2013 = landlocked_source
  ) %>%
  select(source, host, industrial, ends_with("_2013"), sifc_source)

missing_gravity_vars$source <- as.character(missing_gravity_vars$source)
missing_gravity_vars$host <- as.character(missing_gravity_vars$host)

data_gravity_update <- data_gravity_update %>%
  left_join(missing_gravity_vars, by = c("source", "host"))

for (v in c("comlang_off", "col45", "gap_lon", "logdist")) {
  v2013 <- paste0(v, "_2013")
  data_gravity_update[[v]] <- ifelse(
    data_gravity_update$source %in% c(1003, 1006, 1012, 1017, 9006) |
      data_gravity_update$host %in% c(1003, 1006, 1012, 1017, 9006),
    data_gravity_update[[v2013]],
    data_gravity_update[[v]]
  )
}

for (v in c("landlocked_source", "lat_source")) {
  v2013 <- paste0(v, "_2013")
  data_gravity_update[[v]] <- ifelse(
    data_gravity_update$source %in% c(1003, 1006, 1012, 1017, 9006),
    data_gravity_update[[v2013]],
    data_gravity_update[[v]]
  )
}

# Curacao and Sint Maarten: recycle time-constant vars from Netherlands Antilles
data_gravity_update <- data_gravity_update %>%
  mutate(
    dist = if_else(source == 355 | host == 355, NA_real_, dist),
    logdist = if_else(source == 355 | host == 355, NA_real_, logdist)
  )

gravity_355_source <- data_gravity_update %>%
  filter(source == 353, year <= 2010) %>%
  select(source, host, year, comlang_off, col45, lon_source, lat_source,
         landlocked_source, industrial, dist, logdist, gap_lon) %>%
  distinct(source, host, .keep_all = TRUE) %>%
  mutate(source = 355) %>%
  rename_with(~ paste0(.x, "_355_source"),
              .cols = c(comlang_off, col45, lon_source, lat_source,
                        landlocked_source, industrial, dist, logdist, gap_lon))

gravity_355_host <- data_gravity_update %>%
  filter(host == 353, year <= 2010) %>%
  select(source, host, year, comlang_off, col45, industrial, dist, logdist, gap_lon) %>%
  distinct(source, host, .keep_all = TRUE) %>%
  mutate(host = 355) %>%
  rename_with(~ paste0(.x, "_355_host"),
              .cols = c(comlang_off, col45, industrial, dist, logdist, gap_lon))
#####
gravity_355_source$host <- as.character(gravity_355_source$host)
gravity_355_source$source <- as.character(gravity_355_source$source)

gravity_355_host$host <- as.character(gravity_355_host$host)
gravity_355_host$source <- as.character(gravity_355_host$source)
#####

data_gravity_update <- data_gravity_update %>%
  left_join(gravity_355_source %>% select(-year), by = c("source", "host")) %>%
  left_join(gravity_355_host %>% select(-year), by = c("source", "host"))

for (v in c("comlang_off", "col45", "lon_source", "lat_source",
            "landlocked_source", "industrial", "dist", "logdist", "gap_lon")) {
  v355 <- paste0(v, "_355_source")
  data_gravity_update[[v]] <- ifelse(
    is.na(data_gravity_update[[v]]) &
      !is.na(data_gravity_update[[v355]]) &
      data_gravity_update$year > 2009,
    data_gravity_update[[v355]],
    data_gravity_update[[v]]
  )
}

for (v in c("comlang_off", "col45", "industrial", "dist", "logdist", "gap_lon")) {
  v355 <- paste0(v, "_355_host")
  data_gravity_update[[v]] <- ifelse(
    is.na(data_gravity_update[[v]]) &
      !is.na(data_gravity_update[[v355]]) &
      data_gravity_update$year > 2009,
    data_gravity_update[[v355]],
    data_gravity_update[[v]]
  )
}

for (v in c("comlang_off", "col45", "dist", "logdist", "gdp_source", "gdp_host",
            "pop_source", "pop_host", "landlocked_source", "lat_source",
            "lon_source", "gap_lon", "industrial")) {
  data_gravity_update[[v]] <- ifelse(
    (data_gravity_update$source == 353 & data_gravity_update$year > 2009) |
      (data_gravity_update$host == 353 & data_gravity_update$year > 2009) |
      (data_gravity_update$source == 355 & data_gravity_update$year < 2010) |
      (data_gravity_update$host == 355 & data_gravity_update$year < 2010),
    NA,
    data_gravity_update[[v]]
  )
}

data_gravity_update <- data_gravity_update %>%
  select(-ends_with("_2013"), -ends_with("_355_source"), -ends_with("_355_host"))

# Kosovo: recycle time-constant variables from Serbia
gravity_967_source <- data_gravity_update %>%
  filter(source == 942, year >= 2010) %>%
  select(source, host, year, comlang_off, col45, lon_source, lat_source,
         landlocked_source, industrial, dist, logdist, gap_lon) %>%
  distinct(source, host, .keep_all = TRUE) %>%
  mutate(source = 967) %>%
  rename_with(~ paste0(.x, "_967_source"),
              .cols = c(comlang_off, col45, lon_source, lat_source,
                        landlocked_source, industrial, dist, logdist, gap_lon))

gravity_967_host <- data_gravity_update %>%
  filter(host == 942, year >= 2010) %>%
  select(source, host, year, comlang_off, col45, industrial, dist, logdist, gap_lon) %>%
  distinct(source, host, .keep_all = TRUE) %>%
  mutate(host = 967) %>%
  rename_with(~ paste0(.x, "_967_host"),
              .cols = c(comlang_off, col45, industrial, dist, logdist, gap_lon))

gravity_967_source$source <- as.character(gravity_967_source$source)
gravity_967_source$host <- as.character(gravity_967_source$host)
gravity_967_host$source <- as.character(gravity_967_host$source)
gravity_967_host$host <- as.character(gravity_967_host$host)

data_gravity_update <- data_gravity_update %>%
  left_join(gravity_967_source %>% select(-year), by = c("source", "host")) %>%
  left_join(gravity_967_host %>% select(-year), by = c("source", "host"))

for (v in c("comlang_off", "col45", "lon_source", "lat_source",
            "landlocked_source", "industrial", "dist", "logdist", "gap_lon")) {
  v967 <- paste0(v, "_967_source")
  data_gravity_update[[v]] <- ifelse(
    is.na(data_gravity_update[[v]]) &
      !is.na(data_gravity_update[[v967]]) &
      data_gravity_update$year > 2009,
    data_gravity_update[[v967]],
    data_gravity_update[[v]]
  )
}

for (v in c("comlang_off", "col45", "industrial", "dist", "logdist", "gap_lon")) {
  v967 <- paste0(v, "_967_host")
  data_gravity_update[[v]] <- ifelse(
    is.na(data_gravity_update[[v]]) &
      !is.na(data_gravity_update[[v967]]) &
      data_gravity_update$year > 2009,
    data_gravity_update[[v967]],
    data_gravity_update[[v]]
  )
}

for (v in c("comlang_off", "col45", "dist", "logdist", "gdp_source", "gdp_host",
            "pop_source", "pop_host", "landlocked_source", "lat_source",
            "lon_source", "gap_lon", "industrial")) {
  data_gravity_update[[v]] <- ifelse(
    (data_gravity_update$source == 967 & data_gravity_update$year < 2010) |
      (data_gravity_update$host == 967 & data_gravity_update$year < 2010),
    NA,
    data_gravity_update[[v]]
  )
}

# Fill gaps in blank country rows
fill_mean_by <- function(data, group_vars, vars) {
  for (v in vars) {
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(.help = mean(.data[[v]], na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        .help = if_else(is.nan(.help), NA_real_, .help),
        !!v := if_else(is.na(.data[[v]]), .help, .data[[v]])
      ) %>%
      select(-.help)
  }
  data
}

data_gravity_update <- fill_mean_by(
  data_gravity_update,
  c("source", "year"),
  c("landlocked_source", "gdp_source", "loggdppc_source",
    "logpop_source", "pop_source", "gdppc_source", "lat_source", "sifc_source")
)

data_gravity_update <- fill_mean_by(
  data_gravity_update,
  c("host", "year"),
  c("gdppc_host", "gdp_host", "pop_host", "lon_host", "lat_host")
)

data_gravity_update <- fill_mean_by(
  data_gravity_update,
  c("source", "host", "year"),
  c("comlang_off", "col45", "gap_lon", "logdist",
    "dist", "industrial", "loggap_gdp", "loggap_gdppc")
)

#####
foo <- read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta"))
foo$ifscode <- as.character(foo$ifscode)
#########

# Merge indicator for CPIS-reporting countries
cpis_source_harmonised <- read_dta(file.path(work, "cpis_source.dta")) %>%
  rename(ifscode = source) %>%
  # left_join(read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta")),
  left_join(foo,
            by = "ifscode") %>%
  filter(!is.na(our_code)) %>%
  select(our_code, cpis) %>%
  rename(source = our_code)

cpis_source_harmonised$source <- as.character(cpis_source_harmonised$source)

data_gravity_update <- data_gravity_update %>%
  left_join(cpis_source_harmonised, by = "source")

# Harmonise country names
matching <- read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta"))
matching$our_code <- as.character(matching$our_code)
data_gravity_update <- data_gravity_update %>%
  select(-our_code) %>%
  rename(our_code = source) %>%
  left_join(matching, by = "our_code") %>%
  filter(!is.na(country)) %>%
  rename(source = our_code) %>%
  mutate(sourcename = country) %>%
  select(-country, -matches("^_merge$"))

data_gravity_update <- data_gravity_update %>%
  rename(our_code = host) %>%
  left_join(matching, by = "our_code") %>%
  filter(!is.na(country)) %>%
  rename(host = our_code) %>%
  mutate(hostname = country) %>%
  select(-country, -iso3_host, -matches("^_merge$"))

# Keep final variables
data_gravity_update <- data_gravity_update %>%
  select(
    year, source, host, sourcename, eqasset, debtasset, hostname,
    comlang_off, col45, landlocked_source, lat_source, lat_host, lon_host,
    sifc_source, cpis, gdp_source, gdppc_host, gap_lon, industrial,
    logeqasset, logdebtasset, logdist, loggap_gdp, loggap_gdppc,
    loggdppc_source, logpop_source
  )

# Final save
write_dta(data_gravity_update, file.path(work, "data_gravity_update.dta"))
