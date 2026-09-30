# ------------------------
# addendum zu 7b
# test, ob die Länderwerte der Baseline aus der Sensitivitätsanalyse (7b) mit
# der ursprünglichen Replikation (aus 6b) übereinstimmen
# in 7b übernommen, läuft aber auch eigenständig
# -------------------------
library(dplyr)
library(readr)

# 1. Originale Replikation --------------------------------------------
original <- readRDS(file.path(work, "countries.rds")) %>%
  filter(
    indicator == "total",
    between(year, 2001, 2021)
  ) %>%
  transmute(
    year = as.integer(year),
    iso3 = as.character(iso3),
    value_original = as.numeric(value)
  )

# 2. Sensitivitätsanalyse: Baseline ------------------------------------

sensitivity <- read_csv(
  file.path(sensitivity_dir, "sensitivity_country_allocation.csv"),
  show_col_types = FALSE
)

sensitivity_baseline <- sensitivity %>%
  filter(
    scenario == "Baseline",
    between(year, 2001, 2021)
  ) %>%
  transmute(
    year = as.integer(year),
    iso3 = as.character(iso3),
    value_sensitivity = as.numeric(offshore_total)
  )

# 3. Shell-Country-Beobachtungen entfernen -----------------------------

shell_codes <- c("BEH", "CHH", "GBH", "IEH", "NLH", "USH")

original <- original %>%
  filter(!iso3 %in% shell_codes)

sensitivity_baseline <- sensitivity_baseline %>%
  filter(!iso3 %in% shell_codes)

# 4. Prüfen, ob die Schlüssel eindeutig sind ---------------------------

stopifnot(
  !anyDuplicated(original[c("year", "iso3")]),
  !anyDuplicated(sensitivity_baseline[c("year", "iso3")])
)

# 5. Vergleich ---------------------------------------------------------

comparison <- full_join(
  original,
  sensitivity_baseline,
  by = c("year", "iso3")
) %>%
  mutate(
    difference = value_sensitivity - value_original,
    abs_difference = abs(difference)
  )

# 6. Validierung -------------------------------------------------------

tolerance <- 1e-5

problems <- comparison %>%
  filter(
    is.na(value_original) |
      is.na(value_sensitivity) |
      abs_difference > tolerance
  ) %>%
  arrange(desc(abs_difference))

cat("Original observations:", nrow(original), "\n")
cat("Sensitivity observations:", nrow(sensitivity_baseline), "\n")
cat("Problematic observations:", nrow(problems), "\n")

if (all(is.na(comparison$abs_difference))) {
  cat("Maximum absolute difference: NA\n")
} else {
  cat(
    "Maximum absolute difference:",
    max(comparison$abs_difference, na.rm = TRUE),
    "\n"
  )
}

print(problems, n = 30)

# 7. Ergebnis ----------------------------------------------------------

if (nrow(problems) == 0) {
  message("PASS: Baseline matches the original replication.")
} else {
  warning("FAIL: Baseline differs from the original replication.")
}

stopifnot(nrow(problems) == 0)
