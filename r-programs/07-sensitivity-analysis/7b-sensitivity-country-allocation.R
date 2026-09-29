# 7b extension: propagate haven-location scenarios to owners' countries.
# Run after 7b-sensitivity-distribution.R and 6a-build-offshore-01-22.R.
# FGZ country ownership shares are held fixed; this is a counterfactual allocation test.
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(countrycode)

sensitivity_dir <- file.path(work, "07-sensitivity-analysis")
dir.create(sensitivity_dir, recursive = TRUE, showWarnings = FALSE)
#dir.create(fig, recursive = TRUE, showWarnings = FALSE)

source_file <- file.path(sensitivity_dir, "sensitivity_distribution.rds")
if (!file.exists(source_file)) stop("Run 7b-sensitivity-distribution.R first: ", source_file)
distribution <- readRDS(source_file)
stopifnot(all(c("year", "scenario", "haven", "wealth_bn_usd", "switzerland", "global_wealth") %in% names(distribution)))

# Exactly the 19 locations in FGZ T.A2b. Map to the T.A2 accounting groups.
# NB: Published T.A2 groups Bahamas, Bermuda and Netherlands Antilles/Curaçao
# under AS. This is an accounting grouping, not a geographic classification.
haven_map <- tibble::tribble(
  ~haven, ~group,
  "Cayman Islands", "CR", "Panama", "CR", "US", "CR",
  
  "Hong Kong", "AS", "Singapore", "AS", "Macao", "AS",
  "Malaysia", "AS", "Bahrain", "AS", "Bahamas", "AS",
  "Bermuda", "AS",
  "Netherlands Antilles (then Curaçao)", "AS",
  "Guernsey", "EU", "Jersey", "EU", "Isle of Man", "EU",
  "Luxembourg", "EU", "Cyprus", "EU", "UK", "EU",
  "Austria", "EU", "Belgium", "EU"
)

unknown <- setdiff(unique(distribution$haven), haven_map$haven)
missing_havens <- setdiff(haven_map$haven, unique(distribution$haven))
if (length(unknown) || length(missing_havens)) {
  stop("Haven-name mismatch. Unknown: ", paste(unknown, collapse = ", "),
       "; absent: ", paste(missing_havens, collapse = ", "))
}
if (anyDuplicated(haven_map$haven)) stop("Duplicate haven in haven_map")

scenario_groups <- distribution %>%
  left_join(haven_map, by = "haven", relationship = "many-to-one") %>%
  group_by(year, scenario, group) %>%
  summarise(group_wealth = sum(wealth_bn_usd), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = group_wealth) %>%
  left_join(distribution %>%
              distinct(year, scenario, switzerland, global_wealth),
            by = c("year", "scenario")) %>%
  rename(CH = switzerland) %>%
  mutate(total = CH + CR + AS + EU)
stopifnot(nrow(scenario_groups) == 21L * 3L,
          all(abs(scenario_groups$total - scenario_groups$global_wealth) < 1e-5))

# Baseline must reproduce the published T.A2 group totals, not just the grand total.
# The original 7b results repeat these source columns for each haven.
reference <- distribution %>%
  filter(scenario == "Baseline") %>%
  distinct(year, american, asian, european)
baseline_check <- scenario_groups %>%
  filter(scenario == "Baseline") %>%
  left_join(reference, by = "year") %>%
  transmute(year, difference_CR = CR - american,
            difference_AS = AS - asian, difference_EU = EU - european)
write_csv(baseline_check, file.path(sensitivity_dir, "sensitivity_country_group_baseline_check.csv"))
if (any(abs(as.matrix(baseline_check[-1])) > 1e-5, na.rm = TRUE)) {
  stop("T.A2b individual havens do not reproduce T.A2 group totals. Inspect sensitivity_country_group_baseline_check.csv before continuing.")
}

# Load the final smoothed annual outputs from 6a. Do not use interim unsmoothed files.
load_offshore <- function(y) {
  path <- file.path(work, paste0("offshore", y, ".rds"))
  if (!file.exists(path)) stop("Missing 6a output: ", path)
  x <- readRDS(path)
  needed <- c("bank", "iso3saver", "sh_fidu_smthg", "sh_EU_smthg",
              "sh_AS_smthg", "sh_CR_smthg")
  if (!all(needed %in% names(x))) stop("Missing smoothed weights in ", path,
                                      ": ", paste(setdiff(needed, names(x)), collapse = ", "))
  x %>% filter(bank == "CH") %>%
    transmute(year = y, iso3 = iso3saver,
              sh_CH = as.numeric(sh_fidu_smthg),
              sh_EU = as.numeric(sh_EU_smthg),
              sh_AS = as.numeric(sh_AS_smthg),
              sh_CR = as.numeric(sh_CR_smthg))
}
weights <- bind_rows(lapply(2001:2021, load_offshore))
if (anyDuplicated(weights[c("year", "iso3")])) stop("Duplicate year/ISO3 in 6a ownership weights")

# Preserve Stata egen rowtotal(...), missing semantics.
rowtotal_missing <- function(...) {
  m <- cbind(...)
  z <- rowSums(m, na.rm = TRUE)
  z[rowSums(!is.na(m)) == 0L] <- NA_real_
  z
}

country_results <- weights %>%
  left_join(scenario_groups, by = "year", relationship = "many-to-many") %>%
  mutate(offshore_switzerland = CH * sh_CH,
         offshore_EU_Havens = EU * sh_EU,
         offshore_AS_Havens = AS * sh_AS,
         offshore_CR_Havens = CR * sh_CR,
         offshore_total = rowtotal_missing(offshore_switzerland,
                                           offshore_EU_Havens,
                                           offshore_AS_Havens,
                                           offshore_CR_Havens)) %>%
  select(year, scenario, iso3, offshore_total, offshore_switzerland,
         offshore_EU_Havens, offshore_AS_Havens, offshore_CR_Havens)

baseline <- country_results %>%
  filter(scenario == "Baseline") %>%
  select(year, iso3, baseline_bn_usd = offshore_total)
comparison <- country_results %>%
  left_join(baseline, by = c("year", "iso3"), relationship = "many-to-one") %>%
  mutate(change_bn_usd = offshore_total - baseline_bn_usd,
         change_pct = if_else(!is.na(baseline_bn_usd) & baseline_bn_usd != 0,
                              100 * change_bn_usd / baseline_bn_usd, NA_real_))

# Compare to the unmodified 6b output where available. Shell-company observations
# are excluded by 6b; do not silently force equality if a pipeline differs.
original_path <- file.path(work, "countries.rds")
if (file.exists(original_path)) {
  original <- readRDS(original_path) %>%
    filter(indicator == "total", year %in% 2001:2021) %>%
    transmute(year, iso3, original_6b_bn_usd = as.numeric(value))
  baseline_reconciliation <- baseline %>%
    left_join(original, by = c("year", "iso3")) %>%
    mutate(difference_bn_usd = baseline_bn_usd - original_6b_bn_usd)
  write_csv(baseline_reconciliation,
            file.path(sensitivity_dir, "sensitivity_country_baseline_reconciliation.csv"))
}

write_csv(scenario_groups, file.path(sensitivity_dir, "sensitivity_country_haven_groups.csv"))
write_csv(comparison, file.path(sensitivity_dir, "sensitivity_country_allocation.csv"))
saveRDS(comparison, file.path(sensitivity_dir, "sensitivity_country_allocation.rds"))

# Example plot: scenario effects on selected owner countries in 2021.
plot_iso3 <- c("DEU", "CHN", "USA", "GBR", "FRA", "RUS")
plot_data <- comparison %>%
  filter(year == 2021, iso3 %in% plot_iso3, scenario != "Baseline")
p <- ggplot(plot_data, aes(x = iso3, y = change_pct, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(x = NULL, y = "Change in estimated offshore wealth relative to baseline (%)",
       fill = NULL,
       title = "Owner-country sensitivity to haven allocation, 2021",
       caption = "Fixed FGZ ownership weights; unchanged global offshore wealth.") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(fig, "sensitivity_country_allocation_2021.pdf"), p,
       width = 9, height = 5)
ggsave(file.path(fig, "sensitivity_country_allocation_2021.png"), p,
       width = 9, height = 5, dpi = 300)
message("Country-allocation sensitivity completed; check baseline reconciliation before interpreting results.")

# -------- check baseline vs 6b results -------
message("Checking if baseline estimation matches 6b results ...")
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

####
#--------------------------------------- end check baseline

# ----------- 2021

results <- read_csv(
  file.path(sensitivity_dir, "sensitivity_country_allocation.csv"),
  show_col_types = FALSE
)

# Szenarien gegenüber der Baseline vergleichen
effects_2021 <- results %>%
  filter(
    year == 2021,
    !iso3 %in% c("BEH", "CHH", "GBH", "IEH", "NLH", "USH")
  ) %>%
  select(year, iso3, scenario, offshore_total) %>%
  pivot_wider(
    names_from = scenario,
    values_from = offshore_total
  ) %>%
  mutate(
    change_25 = `25% equal weights` - Baseline,
    change_50 = `50% equal weights` - Baseline,
    pct_change_25 = if_else(
      Baseline != 0,
      100 * change_25 / Baseline,
      NA_real_
    ),
    pct_change_50 = if_else(
      Baseline != 0,
      100 * change_50 / Baseline,
      NA_real_
    )
  )

# Länder mit den größten absoluten Veränderungen
effects_2021 %>%
  arrange(desc(abs(change_50))) %>%
  print(n = 20)

# ------- end 2021

# ------ Konsistenzcheck
results %>%
  filter(
    !iso3 %in% c("BEH", "CHH", "GBH", "IEH", "NLH", "USH")
  ) %>%
  group_by(year, scenario) %>%
  summarise(
    total = sum(offshore_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scenario,
    values_from = total
  ) %>%
  mutate(
    difference_25 = `25% equal weights` - Baseline,
    difference_50 = `50% equal weights` - Baseline
  ) %>%
  select(year, difference_25, difference_50) %>%
  print(n = Inf)

# ----

# --- Grafik
# 1. Daten einlesen -----------------------------------------------------

results <- read_csv(
  file.path(sensitivity_dir, "sensitivity_country_allocation.csv"),
  show_col_types = FALSE
)

shell_codes <- c("BEH", "CHH", "GBH", "IEH", "NLH", "USH")


# 2. Veränderungen für 2021 berechnen -------------------------------

plot_data <- results %>%
  filter(
    year == 2021,
    scenario %in% c("Baseline", "50% equal weights"),
    !iso3 %in% shell_codes
  ) %>%
  select(year, iso3, scenario, offshore_total) %>%
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = offshore_total
  ) %>%
  mutate(
    difference_total = `50% equal weights` - Baseline,
    country = countrycode(
      iso3,
      origin = "iso3c",
      destination = "country.name",
      custom_match = c("ANT" = "Netherlands Antilles")
    ),
    country = coalesce(country, iso3)
  ) %>%
  filter(!is.na(difference_total))


# 3. Zehn größte positive und negative Veränderungen -------------------

top_positive <- plot_data %>%
  slice_max(difference_total, n = 10, with_ties = FALSE)

top_negative <- plot_data %>%
  slice_min(difference_total, n = 10, with_ties = FALSE)

plot_data <- bind_rows(
  top_positive,
  top_negative
) %>%
  mutate(
    direction = if_else(
      difference_total >= 0,
      "Increase",
      "Decrease"
    ),
    country = fct_reorder(country, difference_total)
  )

# 4. Abbildung ---------------------------------------------------------

p <- ggplot(
  plot_data,
  aes(
    x = difference_total,
    y = country,
    fill = direction
  )
) +
  geom_col(width = 0.75) +
  geom_vline(
    xintercept = 0,
    colour = "grey30",
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      "Increase" = "#2878B5",
      "Decrease" = "#C44E52"
    )
  ) +
  scale_x_continuous(
    labels = scales::label_number(accuracy = 1)
  ) +
  labs(
    title = "Sensitivity of Country-Level Offshore Wealth Estimates",
    subtitle = "50% equal-weight scenario relative to baseline, 2021",
    x = "Change in estimated offshore wealth (USD billion)",
    y = NULL,
    fill = NULL,
    caption = paste(
      "Note: Global offshore wealth and the Swiss component",
      "are held constant."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      colour = "grey40"
    ),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

# 5. Export ------------------------------------------------------------

ggsave(
  filename = file.path(
    fig,
    "sensitivity_country_changes_2021.pdf"
  ),
  plot = p,
  width = 9,
  height = 7
)

ggsave(
  filename = file.path(
    fig,
    "sensitivity_country_changes_2021.png"
  ),
  plot = p,
  width = 9,
  height = 7,
  dpi = 300
)


#----