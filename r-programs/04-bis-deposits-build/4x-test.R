library(dplyr)
library(haven)
library(tidyr)


compare_bis <- function(type, value_col) {
  
  r <- readRDS(file.path(work, paste0("bis-", type, "-all-01-22.rds")))
  s <- read_dta(file.path(work2, paste0("bis-", type, "-all-01-22.dta")))
  
  keys <- c("year", "bank", "saver")
  
  cat("\n---", type, "---\n")
  
  print(data.frame(
    source = c("R", "Stata"),
    rows = c(nrow(r), nrow(s)),
    missing = c(sum(is.na(r[[value_col]])), sum(is.na(s[[value_col]]))),
    duplicate_keys = c(
      sum(duplicated(r[keys])),
      sum(duplicated(s[keys]))
    )
  ))
  
  comparison <- full_join(
    r %>% rename(value_r = all_of(value_col), ofc_r = OFC),
    s %>% rename(value_s = all_of(value_col), ofc_s = OFC),
    by = keys,
    na_matches = "never"
  )
  
  comparison <- comparison %>%
    mutate(
      abs_diff = abs(value_r - value_s),
      missing_mismatch = xor(is.na(value_r), is.na(value_s)),
      ofc_mismatch = ofc_r != ofc_s
    )
  
  print(comparison %>%
          summarise(
            unmatched_keys = sum(is.na(ofc_r) | is.na(ofc_s)),
            missing_mismatches = sum(missing_mismatch),
            ofc_mismatches = sum(ofc_mismatch, na.rm = TRUE),
            numeric_mismatches = sum(abs_diff > 1e-8, na.rm = TRUE),
            max_abs_diff = max(abs_diff, na.rm = TRUE)
          ))
  
  comparison %>%
    filter(missing_mismatch | ofc_mismatch | abs_diff > 1e-8) %>%
    arrange(desc(abs_diff))
}

diff_deposits <- compare_bis("deposits", "dep")
diff_interbank <- compare_bis("interbank", "totdep")

diff_deposits %>%
  filter(missing_mismatch) %>%
  group_by(year, bank) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n)) %>%
  print(n = 50)


diff_deposits %>%
  filter(!missing_mismatch, abs_diff > 1e-8) %>%
  group_by(bank) %>%
  summarise(
    n = n(),
    max_abs_diff = max(abs_diff),
    mean_abs_diff = mean(abs_diff),
    .groups = "drop"
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)


diff_deposits %>%
  filter(missing_mismatch) %>%
  select(year, bank, saver, value_r, value_s) %>%
  print(n = 30)

diff_deposits %>%
  filter(!missing_mismatch, abs_diff > 1e-8) %>%
  select(year, bank, saver, value_r, value_s, abs_diff) %>%
  arrange(desc(abs_diff)) %>%
  print(n = 30)

compare_bis("interbank", "totdep")

# -------------------

r <- readRDS(
  file.path(work, "bis-interbank-all-01-22.rds")
)

s <- read_dta(
  file.path(work2, "bis-interbank-all-01-22.dta")
)

components <- full_join(
  r %>% rename(value_r = totdep),
  s %>% rename(value_s = totdep),
  by = c("year", "bank", "saver", "OFC")
) %>%
  filter(
    year %in% c(2011, 2016, 2019),
    saver %in% c("5J", "US"),
    bank %in% c("5A", "1R", bilateral_banks)
  ) %>%
  mutate(
    diff = value_r - value_s
  )

components %>%
  filter(year == 2011, saver == "5J") %>%
  arrange(desc(abs(diff))) %>%
  select(bank, value_r, value_s, diff) %>%
  print(n = Inf)

components %>%
  group_by(year, saver) %>%
  summarise(
    diff_5A = diff[bank == "5A"],
    
    diff_bilateral = sum(
      diff[bank %in% bilateral_banks],
      na.rm = TRUE
    ),
    
    diff_1R = diff[bank == "1R"],
    
    expected_diff_1R = diff_5A - diff_bilateral,
    
    unexplained = diff_1R - expected_diff_1R,
    
    .groups = "drop"
  ) %>%
  print(n = Inf)


components %>%
  group_by(year, saver) %>%
  summarise(
    diff_1R = diff[bank == "1R"],
    mx_r = value_r[bank == "MX"],
    mx_s = value_s[bank == "MX"],
    remaining_diff = diff_1R + mx_r,
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ---- rundungsfehler testen
library(dplyr)

float32 <- function(x) {
  readBin(
    writeBin(as.numeric(x), raw(), size = 4),
    what = "numeric",
    n = length(x),
    size = 4
  )
}

diff_interbank %>%
  filter(bank == "1R", saver == "5J") %>%
  mutate(
    value_r_float = float32(value_r),
    diff_float = abs(value_r_float - value_s)
  ) %>%
  select(
    year, value_r, value_s,
    abs_diff, value_r_float, diff_float
  ) %>%
  arrange(desc(abs_diff)) %>%
  print(n = Inf)
