# -----------------------------------------------------------------------------#
# Project: Offshore financial wealth database - update 2023
# Title:   2_do_full_matrices.R
# Purpose: Construct exhaustive bilateral portfolio asset matrices
# -----------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(stringr)
library(purrr)


# Helpers for reading/writing Stata files
read_dta2  <- function(path) haven::read_dta(path)
write_dta2 <- function(data, path) haven::write_dta(data, path)

# Helper: repeated lead fill within group
fill_with_next <- function(x, n = 1) {
  out <- x
  for (i in seq_len(n)) {
    nxt <- dplyr::lead(out)
    out[is.na(out) | out == 0] <- nxt[is.na(out) | out == 0]
  }
  out
}

# Helper: repeated lag fill within group
fill_with_prev <- function(x, n = 1) {
  out <- x
  for (i in seq_len(n)) {
    prv <- dplyr::lag(out)
    out[is.na(out) | out == 0] <- prv[is.na(out) | out == 0]
  }
  out
}

# ------------------------------------------------------------------------------
# Read main gravity dataset
# ------------------------------------------------------------------------------

df <- read_dta(file.path(work, "data_gravity_update.dta"))

# ------------------------------------------------------------------------------
# Gravity-like model
# ------------------------------------------------------------------------------

df <- df %>%
  filter(host != source)

# My definition of OFC for the regressions
df <- df %>%
  mutate(
    ofc_source = if_else(sifc_source == 1, 1, 0)
  )

# Add jurisdictions to OFC list
add_ofc <- c(312, 9006, 1200, 1003, 815, 1100, 178, 137, 532, 576, 423, 355)
df <- df %>%
  mutate(
    ofc_source = if_else(source %in% add_ofc, 1, ofc_source)
  )

# Remove jurisdictions from OFC list
remove_ofc <- c(372, 321, 328, 668, 135, 298, 351, 565)
df <- df %>%
  mutate(
    ofc_source = if_else(source %in% remove_ofc, 0, ofc_source)
  )

# Host OFC indicator derived from source OFC list
ofc_tbl <- df %>%
  filter(ofc_source == 1) %>%
  distinct(source, ofc_source) %>%
  transmute(host = source, ofc_host = ofc_source)

df <- df %>%
  left_join(ofc_tbl, by = "host") %>%
  mutate(
    ofc_host = if_else(ofc_host == 1, 1, 0),
    logeqasset = if_else(eqasset == 0, 0, logeqasset),
    logdebtasset = if_else(debtasset == 0, 0, logdebtasset)
  )

#df <- df %>% 
#  mutate(ofc_host = replace_na(ofc_host, 0))

df <- df %>%
  #left_join(ofc_tbl, by = "host") %>%
  mutate(
    ofc_host = replace_na(ofc_host, 0),
    logeqasset = if_else(eqasset == 0, 0, logeqasset),
    logdebtasset = if_else(debtasset == 0, 0, logdebtasset)
  )
# ------------------------------------------------------------------------------
# Benchmark regressions
# ------------------------------------------------------------------------------

########## tests: NA?

df2 <- df %>% filter(ofc_source == 0, ofc_host == 0)

##################

mod_eq_bench <- feols(
  logeqasset ~ logdist + gap_lon + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + lat_source + landlocked_source +
    logpop_source + loggdppc_source | year + host,
  data = df %>% filter(ofc_source == 0, ofc_host == 0)
)

mod_debt_bench <- feols(
  logdebtasset ~ logdist + gap_lon + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + lat_source + landlocked_source +
    logpop_source + loggdppc_source | year + host,
  data = df %>% filter(ofc_source == 0, ofc_host == 0)
)

# Augmented regressions:
# Stata: reg y ... year_* host_* _IofcXhos_1_*
# In fixest, i(host, ofc_source) creates host-specific interaction effects
mod_eq <- feols(
  logeqasset ~ logdist + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + ofc_source + lat_source +
    landlocked_source + logpop_source + loggdppc_source +
    i(host, ofc_source, ref = 0) | year + host,
  data = df
)

mod_debt <- feols(
  logdebtasset ~ logdist + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + ofc_source + lat_source +
    landlocked_source + logpop_source + loggdppc_source +
    i(host, ofc_source, ref = 0) | year + host,
  data = df
)

df <- df %>%
  mutate(
    logeqp   = predict(mod_eq, newdata = .),
    logdebtp = predict(mod_debt, newdata = .),
    eqp      = exp(logeqp) - 1,
    debtp    = exp(logdebtp) - 1,
    eqp      = if_else(eqp < 0, 0, eqp),
    debtp    = if_else(debtp < 0, 0, debtp)
  )

# ------------------------------------------------------------------------------
# Comparison of predicted shares and true shares
# ------------------------------------------------------------------------------

df <- df %>%
  group_by(source, year) %>%
  mutate(
    toteqalloc   = sum(eqasset[host != 983], na.rm = TRUE),
    totdebtalloc = sum(debtasset[host != 983], na.rm = TRUE),
    shareeqalloc = if_else(toteqalloc == 0 | is.na(toteqalloc), 0, eqasset / toteqalloc),
    sharedebtalloc = if_else(totdebtalloc == 0 | is.na(totdebtalloc), 0, debtasset / totdebtalloc),
    toteqp       = sum(eqp, na.rm = TRUE),
    totdebtp     = sum(debtp, na.rm = TRUE),
    shareeqp     = eqp / toteqp,
    sharedebtp   = debtp / totdebtp
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Allocation of confidential + unallocated claims
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    eqasset   = replace_na(eqasset, 0),
    debtasset = replace_na(debtasset, 0)
  )

# Raw sums
df <- df %>%
  group_by(source, year) %>%
  mutate(
    toteqasset   = sum(eqasset, na.rm = TRUE),
    totdebtasset = sum(debtasset, na.rm = TRUE)
  ) %>%
  ungroup()

# Merge CPIS aggregates
toteq_update <- read_dta2(file.path(work, "data_toteq_update.dta")) %>%
  select(-any_of("cname"))
totdebt_update <- read_dta2(file.path(work, "data_totdebt_update.dta")) %>%
  select(-any_of("cname"))

df <- df %>%
  left_join(toteq_update, by = c("source", "year")) %>%
  left_join(totdebt_update, by = c("source", "year"))

# Stata later uses sumeqasset and sumdebtasset
df <- df %>%
  group_by(year) %>%
  mutate(
    help_total_eq   = sum(sumeqasset, na.rm = TRUE),
    help_total_debt = sum(sumdebtasset, na.rm = TRUE)
  ) %>%
  ungroup()

# Add missing residual to host 983
df <- df %>%
  mutate(
    missingeq   = sumeqasset - toteqasset,
    missingdebt = sumdebtasset - totdebtasset,
    eqasset     = if_else(host == 983 & !is.na(missingeq), eqasset + missingeq, eqasset),
    debtasset   = if_else(host == 983 & !is.na(missingdebt), debtasset + missingdebt, debtasset)
  ) %>%
  select(-missingeq, -missingdebt)

# Confidential / ignored jurisdictions
df <- df %>%
  mutate(
    confidentialeq = 0,
    confidentialdebt = 0,
    ignore_jur = case_when(
      host == 353 & year > 2009 ~ 1,
      host == 355 & year < 2010 ~ 1,
      host == 357 & year < 2010 ~ 1,
      host == 537 & year < 2002 ~ 1,
      host == 733 & year < 2011 ~ 1,
      host == 943 & year < 2006 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    confidentialeq = if_else(
      (eqasset == 0 | host %in% c(1001, 9006, 1003)) &
        !is.na(toteqasset) & toteqasset != 0 & host != 135 &
        coalesce(ignore_jur, 0) != 1,
      1, confidentialeq
    ),
    confidentialdebt = if_else(
      (debtasset == 0 | host %in% c(1001, 9006, 1003)) &
        !is.na(totdebtasset) & totdebtasset != 0 & host != 135 &
        coalesce(ignore_jur, 0) != 1,
      1, confidentialdebt
    )
  )

# Predicted shares among confidential set
df <- df %>%
  group_by(source, year) %>%
  mutate(
    eqpconf       = eqp * confidentialeq,
    debtpconf     = debtp * confidentialdebt,
    toteqpconf    = sum(eqpconf, na.rm = TRUE),
    totdebtpconf  = sum(debtpconf, na.rm = TRUE),
    shareeqpconf  = replace_na(eqpconf / toteqpconf, 0),
    sharedebtpconf = replace_na(debtpconf / totdebtpconf, 0)
  ) %>%
  ungroup()

# Pull unallocated/confidential totals from host 983
unalloc <- df %>%
  filter(host == 983) %>%
  transmute(source, year, totunalloceq = eqasset, totunallocdebt = debtasset)

df <- df %>%
  left_join(unalloc, by = c("source", "year")) %>%
  mutate(
    unalloceq    = if_else(source != 9999, shareeqpconf * totunalloceq, 0),
    unallocdebt  = if_else(source != 9999, sharedebtpconf * totunallocdebt, 0),
    augmeqasset  = eqasset,
    augmdebtasset = debtasset,
    augmeqasset  = if_else(host != 983 & source != 9999, eqasset + unalloceq, augmeqasset),
    augmdebtasset = if_else(host != 983 & source != 9999, debtasset + unallocdebt, augmdebtasset),
    augmeqasset  = if_else(host == 983, 0, augmeqasset),
    augmdebtasset = if_else(host == 983, 0, augmdebtasset)
  ) %>%
  mutate(
    augmeqasset   = if_else(source == 9999, augmeqasset + shareeqalloc * totunalloceq, augmeqasset),
    augmdebtasset = if_else(source == 9999, augmdebtasset + sharedebtalloc * totunallocdebt, augmdebtasset)
  )

write_dta2(df, file.path(work, "temp.dta"))
write_dta2(df, file.path(work, "temp_30.dta"))

# ------------------------------------------------------------------------------
# Allocation of Cayman Islands non-bank sector
# ------------------------------------------------------------------------------

cayman_tic <- read_dta2(file.path(work, "Cayman_TIC_Dec.dta"))

df <- df %>%
  full_join(cayman_tic, by = c("year", "source", "host"))

# drop if _merge==2 in Stata means keep master + matched only
# with full_join we simulate by filtering out rows only from using file
# If needed, use left_join instead:
df <- df %>%
  left_join(cayman_tic, by = c("year", "source", "host"))

df <- df %>%
  mutate(
    augmeqasset   = if_else(source == 377 & host == 111 & augmeqasset < eq_KY_TIC, eq_KY_TIC, augmeqasset),
    augmdebtasset = if_else(source == 377 & host == 111 & year < 2015, debt_KY_TIC, augmdebtasset)
  )

# US share in Cayman assets
df <- df %>%
  group_by(year, source) %>%
  mutate(
    help_toteq_KY   = if_else(source == 377, sum(augmeqasset, na.rm = TRUE), NA_real_),
    help_totdebt_KY = if_else(source == 377, sum(augmdebtasset, na.rm = TRUE), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    share_US_KY       = if_else(source == 377 & host == 111 & year > 2014, augmeqasset / help_toteq_KY, NA_real_),
    share_debt_US_KY  = if_else(source == 377 & host == 111 & year > 2014, augmdebtasset / help_totdebt_KY, NA_real_)
  ) %>%
  arrange(source, host, year) %>%
  group_by(source, host) %>%
  mutate(
    share_US_KY      = fill_with_next(share_US_KY, 14),
    share_debt_US_KY = fill_with_next(share_debt_US_KY, 14)
  ) %>%
  ungroup()

caymantot <- df %>%
  filter(host == 111, source == 377, year >= 2001, year <= 2014) %>%
  transmute(
    source, year,
    caymantoteq   = augmeqasset / share_US_KY,
    caymantotdebt = augmdebtasset / share_debt_US_KY
  ) %>%
  filter(!is.na(caymantoteq), !is.na(caymantotdebt))

df <- df %>%
  left_join(caymantot, by = c("source", "year"))

df <- df %>%
  group_by(year, source) %>%
  mutate(
    share_nonus_eq_old    = sum(shareeqp[host != 111], na.rm = TRUE),
    share_nonus_eq_new    = if_else(first(source) == 377, 1 - mean(share_US_KY, na.rm = TRUE), 1 - mean(share_US_KY, na.rm = TRUE)),
    share_nonus_debt_old  = sum(sharedebtp[host != 111], na.rm = TRUE),
    share_nonus_debt_new  = 1 - mean(share_debt_US_KY, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    share_nonus_eq_new   = if_else(host == 111, NA_real_, share_nonus_eq_new),
    share_nonus_debt_new = if_else(host == 111, NA_real_, share_nonus_debt_new),
    rescale_eq           = share_nonus_eq_new / share_nonus_eq_old,
    shareeq_KY           = if_else(host != 111, shareeqp * rescale_eq, NA_real_),
    rescale_debt         = share_nonus_debt_new / share_nonus_debt_old,
    sharedebt_KY         = if_else(host != 111, sharedebtp * rescale_debt, NA_real_)
  ) %>%
  mutate(
    augmeqasset   = if_else(source == 377 & host != 111 & year < 2015, caymantoteq * shareeq_KY, augmeqasset),
    augmdebtasset = if_else(source == 377 & host != 111 & year < 2015, caymantotdebt * sharedebt_KY, augmdebtasset)
  ) %>%
  select(-caymantotdebt, -caymantoteq)

write_dta2(df, file.path(work, "temp.dta"))

# ------------------------------------------------------------------------------
# Allocation of other CPIS countries
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    help_share_eq   = toteqasset / help_total_eq,
    help_share_debt = totdebtasset / help_total_debt
  ) %>%
  arrange(source, host, year)

# Bahrain
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 419, fill_with_next(.data[[hs]], 2), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 419 & year %in% c(2002, 2003, 2016), .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 419 & year %in% c(2002, 2003, 2016), .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Barbados
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(
      !!hs := if_else(source == 316, fill_with_next(.data[[hs]], 2), .data[[hs]]),
      !!hs := if_else(source == 316, fill_with_prev(.data[[hs]], 1), .data[[hs]])
    ) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 316 & (year < 2003 | year > 2015), .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 316 & (year < 2003 | year > 2015), .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Gibraltar
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 823, fill_with_next(.data[[hs]], 3), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 823 & toteqasset == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 823 & augmeqasset == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# India
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 534, fill_with_next(.data[[hs]], 3), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 534 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 534 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Kuwait
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 443, fill_with_next(.data[[hs]], 2), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 443 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 443 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Latvia
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 941, fill_with_next(.data[[hs]], 5), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 941 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 941 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Mexico
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 273, fill_with_next(.data[[hs]], 2), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 273 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 273 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Cayman
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 377, fill_with_next(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 377 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]])
    )
}

# Panama
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 283, fill_with_prev(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 283 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 283 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Venezuela
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 299, fill_with_prev(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 299 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 299 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Bahamas
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 313, fill_with_prev(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 313 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 313 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Pakistan
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 564, fill_with_next(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 564 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 564 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# Isle of Man
for (x in c("eq", "debt")) {
  hs <- paste0("help_share_", x)
  ta <- paste0("tot", x, "asset")
  aa <- paste0("augm", x, "asset")
  
  df <- df %>%
    group_by(source) %>%
    mutate(!!hs := if_else(source == 1012, fill_with_prev(.data[[hs]], 1), .data[[hs]])) %>%
    ungroup() %>%
    mutate(
      !!ta := if_else(source == 1012 & .data[[ta]] == 0, .data[[hs]] * .data[[paste0("help_total_", x)]], .data[[ta]]),
      !!aa := if_else(source == 1012 & .data[[aa]] == 0, .data[[ta]] * .data[[paste0("share", x, "p")]], .data[[aa]])
    )
}

# ------------------------------------------------------------------------------
# Missing Netherlands SFIs
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    toteqasset   = if_else(source == 138 & year == 2001, toteqasset + 3286, toteqasset),
    toteqasset   = if_else(source == 138 & year == 2002, toteqasset + 2601, toteqasset),
    totdebtasset = if_else(source == 138 & year == 2001, totdebtasset + 20984, totdebtasset),
    totdebtasset = if_else(source == 138 & year == 2002, totdebtasset + 16611, totdebtasset),
    augmeqasset  = if_else(source == 138, toteqasset * shareeqalloc, augmeqasset),
    augmdebtasset = if_else(source == 138, totdebtasset * sharedebtalloc, augmdebtasset)
  )

write_dta2(df, file.path(work, "temp.dta"))

# ------------------------------------------------------------------------------
# Allocation of China
# ------------------------------------------------------------------------------

tic_china <- read_dta2(file.path(work, "TIC_China_Dec.dta"))
imf_china <- read_dta2(file.path(work, "data_IMF_China.dta"))

china <- tic_china %>%
  left_join(imf_china, by = "year") %>%
  arrange(desc(year)) %>%
  mutate(
    growth_equity = eq_China_TIC / lead(eq_China_TIC),
    Equity_IMF = if_else(year < 2007, NA_real_, Equity_IMF)
  )

for (i in 1:6) {
  china <- china %>%
    mutate(Equity_IMF = if_else(is.na(Equity_IMF), lead(Equity_IMF) * growth_equity, Equity_IMF))
}

china <- china %>%
  mutate(growth_Debt = debt_China_TIC / lead(debt_China_TIC))

for (i in 1:3) {
  china <- china %>%
    mutate(Debt_IMF = if_else(is.na(Debt_IMF), lead(Debt_IMF) * growth_Debt, Debt_IMF))
}

china <- china %>%
  mutate(
    share = case_when(
      year == 2009 ~ 0.87,
      year == 2010 ~ 0.89,
      year == 2011 ~ 0.91,
      year == 2012 ~ 0.93,
      year > 2012  ~ 0.95,
      TRUE         ~ 0.85
    ),
    reserves_China = share * Reserves_IMF,
    equity_ratio_TIC = eq_China_TIC / total_China_TIC,
    totaleq_China_public = equity_ratio * reserves_China,
    totaldebt_China_public = (1 - equity_ratio) * reserves_China,
    totaleq_China = equity_ratio_TIC * (reserves_China + Equity_IMF + Debt_IMF),
    totaldebt_China = (1 - equity_ratio_TIC) * (share * Reserves_IMF + Equity_IMF + Debt_IMF)
  ) %>%
  select(year, source, host, eq_China_TIC, debt_China_TIC, share, equity_ratio_TIC,
         starts_with("totaleq_China"), starts_with("totaldebt_China"), reserves_China)

df <- df %>%
  left_join(china, by = c("year", "source")) %>%
  mutate(
    toteqasset   = if_else(source == 924, totaleq_China, toteqasset),
    totdebtasset = if_else(source == 924, totaldebt_China, totdebtasset),
    useqassetchina       = if_else(source == 924, eq_China_TIC, NA_real_),
    usdebtassetchina     = if_else(source == 924, debt_China_TIC, NA_real_),
    nonuseqassetchina    = if_else(source == 924, toteqasset - useqassetchina, NA_real_),
    nonusdebtassetchina  = if_else(source == 924, totdebtasset - usdebtassetchina, NA_real_)
  )

sefernonus <- df %>%
  filter(source == 9999, host != 111, host != 924) %>%
  group_by(year) %>%
  mutate(
    toteqnonus   = sum(augmeqasset, na.rm = TRUE),
    totdebtnonus = sum(augmdebtasset, na.rm = TRUE),
    shareeqsefernonus   = augmeqasset / toteqnonus,
    sharedebtsefernonus = augmdebtasset / totdebtnonus
  ) %>%
  ungroup() %>%
  select(year, host, shareeqsefernonus, sharedebtsefernonus)

df <- df %>%
  left_join(sefernonus, by = c("host", "year")) %>%
  mutate(
    augmeqasset   = if_else(source == 924 & host == 111, useqassetchina, augmeqasset),
    augmdebtasset = if_else(source == 924 & host == 111, usdebtassetchina, augmdebtasset),
    shareeqalloc  = if_else(source == 924 & host == 111, augmeqasset / toteqasset, shareeqalloc),
    sharedebtalloc = if_else(source == 924 & host == 111, augmdebtasset / totdebtasset, sharedebtalloc),
    augmeqasset   = if_else(source == 924 & host != 111, shareeqsefernonus * nonuseqassetchina, augmeqasset),
    augmdebtasset = if_else(source == 924 & host != 111, sharedebtsefernonus * nonusdebtassetchina, augmdebtasset)
  ) %>%
  select(
    -nonusdebtassetchina, -nonuseqassetchina,
    -usdebtassetchina, -useqassetchina,
    -shareeqsefernonus, -sharedebtsefernonus,
    -matches("_TIC$"), -share
  )

# ------------------------------------------------------------------------------
# Allocation of Middle East oil exporters
# ------------------------------------------------------------------------------

# This section is translated directly in structure.
# Some variable names in the original Stata code are inconsistent.
# Adjust if needed to match your actual .dta files.

tic_me <- read_dta2(file.path(work, "TIC_update_middleast.dta"))
bertaut_me <- read_dta2(file.path(work, "Bertaut_Judson_middleeast_Dec.dta"))

middle_east <- bind_rows(tic_me, bertaut_me) %>%
  select(-any_of(c("flag", "country_code", "month"))) %>%
  mutate(
    source = case_when(
      country == " Middle Eastern Oil Exporters" ~ 4566,
      country == "Bahrain" & year > 2010 ~ 419,
      country == "Iran" & year > 2010 ~ 429,
      country == "Iraq" & year > 2010 ~ 433,
      country == "Kuwait" & year > 2010 ~ 443,
      country == "Oman" & year > 2010 ~ 449,
      country == "Qatar" & year > 2010 ~ 453,
      country == "United Arab Emirates" & year > 2010 ~ 466,
      country == "Saudi Arabia" & year > 2010 ~ 456,
      country == " Middle Eastern Oil Exporters" & year > 2010 ~ NA_real_,
      TRUE ~ source
    )
  ) %>%
  filter(!is.na(source)) %>%
  group_by(year) %>%
  summarise(
    Total  = sum(Total, na.rm = TRUE),
    Equity = sum(Equity, na.rm = TRUE),
    Debtl  = sum(Debtl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(read_dta2(file.path(work, "adjust_period.dta")), by = "year") %>%
  mutate(
    Equity = if_else(year > 2010, Equity * adj_eq, Equity)
  ) %>%
  left_join(read_dta2(file.path(work, "shortterm_ratio_FOI.dta")), by = "year") %>%
  rename(ratio_shortlong = short_long_ratio) %>%
  mutate(
    Debt_est  = Debtl + (ratio_shortlong * (Debtl + Equity)),
    Total_est = Equity + Debt_est,
    USshare   = NA_real_
  ) %>%
  arrange(year) %>%
  mutate(
    USshare = case_when(
      year == 2001 ~ 0.70,
      TRUE ~ NA_real_
    )
  )

for (i in 2:nrow(middle_east)) {
  if (is.na(middle_east$USshare[i]) && middle_east$year[i] < 2013) {
    middle_east$USshare[i] <- round(middle_east$USshare[i - 1] - 0.02, 2)
  } else if (is.na(middle_east$USshare[i])) {
    middle_east$USshare[i] <- middle_east$USshare[i - 1]
  }
}

middle_east <- middle_east %>%
  transmute(
    year,
    totasset_ME     = Total_est / USshare,
    toteqasset_ME   = Equity / USshare,
    totdebtasset_ME = totasset_ME - toteqasset_ME,
    USshare
  )

cpis_reported_ME_assets <- df %>%
  filter(source %in% c(419, 443, 456)) %>%
  group_by(year, source) %>%
  summarise(
    toteqasset = first(toteqasset),
    totdebtasset = first(totdebtasset),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  summarise(
    toteqasset_reported = sum(toteqasset, na.rm = TRUE),
    totdebtasset_reported = sum(totdebtasset, na.rm = TRUE),
    .groups = "drop"
  )

middle_east2 <- middle_east %>%
  left_join(cpis_reported_ME_assets, by = "year") %>%
  mutate(
    toteqasset_ME   = toteqasset_ME - toteqasset_reported,
    totdebtasset_ME = totdebtasset_ME - totdebtasset_reported,
    source = 453
  ) %>%
  select(year, source, toteqasset_ME, totdebtasset_ME, USshare)

df <- df %>%
  left_join(middle_east2, by = c("source", "year")) %>%
  mutate(
    toteqasset   = if_else(source == 453, toteqasset_ME, toteqasset),
    totdebtasset = if_else(source == 453, totdebtasset_ME, totdebtasset),
    usoileqasset     = if_else(source == 453, USshare * toteqasset, NA_real_),
    usoildebtasset   = if_else(source == 453, USshare * totdebtasset, NA_real_),
    nonusoileqasset  = if_else(source == 453, toteqasset - usoileqasset, NA_real_),
    nonusoildebtasset = if_else(source == 453, totdebtasset - usoildebtasset, NA_real_)
  )

shareusp <- df %>%
  filter(host == 111) %>%
  select(source, year, shareequsp = shareeqp, sharedebtusp = sharedebtp)

df <- df %>%
  left_join(shareusp, by = c("source", "year")) %>%
  mutate(
    shareeqpnonus   = shareeqp / (1 - shareequsp),
    sharedebtpnonus = sharedebtp / (1 - sharedebtusp),
    augmeqasset     = if_else(source == 453 & host != 111, shareeqpnonus * nonusoileqasset, augmeqasset),
    augmdebtasset   = if_else(source == 453 & host != 111, sharedebtpnonus * nonusoildebtasset, augmdebtasset),
    augmeqasset     = if_else(source == 453 & host == 111, USshare * toteqasset, augmeqasset),
    augmdebtasset   = if_else(source == 453 & host == 111, USshare * totdebtasset, augmdebtasset)
  ) %>%
  select(-USshare, -usoileqasset, -usoildebtasset, -nonusoileqasset, -nonusoildebtasset)

# ------------------------------------------------------------------------------
# Allocation of other non-CPIS
# ------------------------------------------------------------------------------

ewn_update <- read_dta2(file.path(work, "data_ewn_update.dta")) %>%
  rename(ifscode = source) %>%
  left_join(read_dta2(file.path(work, "iso_ifscode.dta")), by = "ifscode") %>%
  filter(!is.na(our_code) | ifscode == 355) %>%
  mutate(our_code = if_else(ifscode == 355, 355, our_code)) %>%
  rename(source = our_code)

write_dta2(ewn_update, file.path(work, "data_ewn_update_ifs.dta"))

df <- df %>%
  left_join(ewn_update, by = c("source", "year")) %>%
  select(-lequity, -lportif_debt) %>%
  rename(ewn22_source = ewn22) %>%
  mutate(
    aportif_debt = if_else(is.na(aportif_debt) & !is.na(adebt), 0.2 * adebt, aportif_debt),
    toteqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), aequity, toteqasset),
    totdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), aportif_debt, totdebtasset)
  )

derived <- df %>%
  group_by(host, year) %>%
  summarise(
    augmeqliab = sum(augmeqasset, na.rm = TRUE),
    augmdebtliab = sum(augmdebtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(source = host)

df <- df %>%
  left_join(derived, by = c("source", "year")) %>%
  mutate(
    toteqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 429, 433, 466, 9998) & is.na(aequity), augmeqliab, toteqasset),
    totdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433, 9998) & is.na(aportif_debt), augmdebtliab, totdebtasset),
    augmeqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), shareeqp * toteqasset, augmeqasset),
    augmdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), sharedebtp * totdebtasset, augmdebtasset)
  )

# Reserves of non-CPIS countries
missingreserve <- read_dta2(file.path(work, "data_foreignexchange_update.dta")) %>%
  filter(!source %in% c(163, 309, 758, 759, 967))

df <- df %>%
  left_join(missingreserve, by = c("source", "year")) %>%
  mutate(
    debtreserveIFS = 0.74 * reserveIFS,
    eqreserveIFS   = 0.01 * reserveIFS
  )

missingreserve2 <- df %>%
  distinct(source, year, cpis, eqreserveIFS, debtreserveIFS) %>%
  filter(!cpis == 1,
         !source %in% c(924, 449, 453, 456, 466, 433, 429)) %>%
  group_by(year) %>%
  summarise(
    missingeqres = sum(eqreserveIFS, na.rm = TRUE),
    missingdebtres = sum(debtreserveIFS, na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(missingreserve2, by = "year")

sefer <- df %>%
  filter(source == 9999) %>%
  group_by(year) %>%
  mutate(
    toteq = sum(augmeqasset, na.rm = TRUE),
    totdebt = sum(augmdebtasset, na.rm = TRUE),
    shareeqsefer = augmeqasset / toteq,
    sharedebtsefer = augmdebtasset / totdebt
  ) %>%
  ungroup() %>%
  select(year, host, shareeqsefer, sharedebtsefer)

df <- df %>%
  left_join(sefer, by = c("host", "year")) %>%
  mutate(
    othereqreserve   = shareeqsefer * missingeqres,
    otherdebtreserve = sharedebtsefer * missingdebtres
  )

otherreserve <- df %>%
  distinct(host, year, othereqreserve, otherdebtreserve) %>%
  filter(!is.na(host)) %>%
  transmute(
    source = 9994,
    host,
    year,
    augmeqasset = othereqreserve,
    augmdebtasset = otherdebtreserve
  )

df <- bind_rows(df, otherreserve)

# ------------------------------------------------------------------------------
# EWNII and derived liabilities
# ------------------------------------------------------------------------------

ewn22_host <- read_dta2(file.path(work, "data_ewn_update_ifs.dta")) %>%
  rename(host = source) %>%
  select(year, host, lequity, lportif_debt, ewn22)

df <- df %>%
  left_join(ewn22_host, by = c("host", "year")) %>%
  rename(
    ewn22_host = ewn22,
    lequity_host = lequity,
    lportif_debt_host = lportif_debt
  )

# Corrections for EWNII liabilities
df <- df %>%
  mutate(
    eqliab_host = lequity_host,
    debtliab_host = lportif_debt_host,
    lequity_SFI = case_when(
      host == 138 & year == 2001 ~ 6635,
      host == 138 & year == 2002 ~ 8177,
      TRUE ~ NA_real_
    ),
    ldebt_SFI = case_when(
      host == 138 & year == 2001 ~ 258296,
      host == 138 & year == 2002 ~ 318308,
      TRUE ~ NA_real_
    ),
    eqliab_host = if_else(host == 138 & year < 2003, lequity_host + lequity_SFI, eqliab_host),
    debtliab_host = if_else(host == 138 & year < 2003, lportif_debt_host + ldebt_SFI, debtliab_host)
  )

# Cayman liabilities correction
KY_eqliab <- df %>%
  filter(source == 377) %>%
  group_by(year) %>%
  summarise(
    augmeqasset = sum(augmeqasset, na.rm = TRUE),
    augmdebtasset = sum(augmdebtasset, na.rm = TRUE),
    eqasset = sum(eqasset, na.rm = TRUE),
    debtasset = sum(debtasset, na.rm = TRUE),
    source = first(source),
    .groups = "drop"
  ) %>%
  mutate(
    eqliab_KY = augmeqasset + augmdebtasset,
    totliab_banks = if_else(year < 2015, eqasset + debtasset, NA_real_),
    host = source
  ) %>%
  select(year, host, eqliab_KY, totliab_banks) %>%
  left_join(read_dta2(file.path(work, "KY_banks.dta")), by = c("year", "host")) %>%
  mutate(
    totliab_banks = if_else(year > 2014, KY_assets_bank, totliab_banks)
  ) %>%
  left_join(read_dta2(file.path(work, "KY_liab_nfc.dta")), by = "year") %>%
  mutate(
    eqliab_nfc = eqliab_nfc / 1e6,
    eqliab_KY = eqliab_KY - totliab_banks + eqliab_nfc
  ) %>%
  select(year, host, eqliab_KY)

df <- df %>%
  left_join(KY_eqliab, by = c("year", "host")) %>%
  mutate(
    eqliab_host = if_else(host == 377, eqliab_KY, eqliab_host)
  )

# Raw derived liabilities
df <- df %>%
  group_by(host, year) %>%
  mutate(
    rawderivedeq = sum(eqasset, na.rm = TRUE),
    rawderiveddebt = sum(debtasset, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    missingliabeq = if_else(rawderivedeq > eqliab_host, rawderivedeq - eqliab_host, 0),
    missingliabdebt = if_else(rawderiveddebt > debtliab_host, rawderiveddebt - debtliab_host, 0),
    eqliab_host = eqliab_host + missingliabeq,
    debtliab_host = debtliab_host + missingliabdebt
  )

# International organizations
bis_io <- read_dta2(file.path(work, "BIS_total_debt_IO.dta"))

df <- df %>%
  left_join(bis_io, by = c("year", "host")) %>%
  mutate(
    eqliab_host = if_else(host == 9998, rawderivedeq, eqliab_host),
    debtliab_host = if_else(host == 9998, total_debt_BIS, debtliab_host)
  ) %>%
  select(-total_debt_BIS)

# Augmented derived liabilities for non-EWN nations
df <- df %>%
  group_by(host, year) %>%
  mutate(
    derivedeqliab_host = sum(augmeqasset, na.rm = TRUE),
    deriveddebtliab_host = sum(augmdebtasset, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    eqliab_host = if_else(is.na(eqliab_host) & !host %in% c(983, 9998, 9999), derivedeqliab_host, eqliab_host),
    debtliab_host = if_else(is.na(debtliab_host) & !host %in% c(983, 9998, 9999), deriveddebtliab_host, debtliab_host)
  )

# Bring host liability data back to source countries
liab <- df %>%
  select(host, year, lequity_host, lportif_debt_host,
         derivedeqliab_host, deriveddebtliab_host,
         eqliab_host, debtliab_host) %>%
  rename(
    source = host,
    lequity_source = lequity_host,
    lportif_debt_source = lportif_debt_host,
    derivedeqliab_source = derivedeqliab_host,
    deriveddebtliab_source = deriveddebtliab_host,
    eqliab_source = eqliab_host,
    debtliab_source = debtliab_host
  ) %>%
  group_by(year, source) %>%
  summarise(
    across(everything(), ~ first(.x)),
    .groups = "drop"
  )

df <- df %>%
  left_join(liab, by = c("source", "year")) %>%
  mutate(
    gapeq_host      = eqliab_host - derivedeqliab_host,
    gapdebt_host    = debtliab_host - deriveddebtliab_host,
    gapeq_source    = eqliab_source - derivedeqliab_source,
    gapdebt_source  = debtliab_source - deriveddebtliab_source
  )

write_dta2(df, file.path(work, "data_full_matrices.dta"))

# ------------------------------------------------------------------------------
# Gap checks
# ------------------------------------------------------------------------------

gap_host <- df %>%
  group_by(host, year) %>%
  summarise(
    hostname = first(hostname),
    eqliab_host = first(eqliab_host),
    derivedeqliab_host = first(derivedeqliab_host),
    gapeq_host = first(gapeq_host),
    debtliab_host = first(debtliab_host),
    deriveddebtliab_host = first(deriveddebtliab_host),
    gapdebt_host = first(gapdebt_host),
    .groups = "drop"
  )

gap_source <- df %>%
  group_by(source, year) %>%
  summarise(
    sourcename = first(sourcename),
    eqliab_source = first(eqliab_source),
    derivedeqliab_source = first(derivedeqliab_source),
    gapeq_source = first(gapeq_source),
    debtliab_source = first(debtliab_source),
    deriveddebtliab_source = first(deriveddebtliab_source),
    gapdebt_source = first(gapdebt_source),
    .groups = "drop"
  )

write_dta2(gap_source, file.path(work, "gap_source_update.dta"))