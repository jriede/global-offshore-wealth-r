# =============================================================================
# Independent validation checks for Figure 2 (Location of Global Offshore Wealth)
# FGZ (2023) replication in R
# Run after 0a-setup.R
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(ggplot2)

# ---- Configuration ----------------------------------------------------------

excel_file <- file.path(raw, "FGZ-raw-data.xlsx")
if (is.na(excel_file)) stop("FGZ-raw-data.xlsx not found. Set raw to its directory.")

#countries_candidates <- c(file.path(work, "countries.rds"), "countries.rds", "/mnt/data/countries.rds")
countries_file <- file.path(work, "countries.rds")
if (is.na(countries_file)) stop("countries.rds not found. Set work to its directory.")

out_dir <- file.path(work, "figure2-validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Original Figure 2: location shares (A:G) ----------------------------
location <- read_excel(excel_file, sheet = "T.A2", range = "A4:G25", col_names = FALSE)
names(location) <- c("year", "total_offshore", "switzerland", "other_havens",
                     "american", "asian", "other_european")
location <- location %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
  filter(year %in% 2001:2021) %>%
  arrange(year)
if (nrow(location) != 21 || anyDuplicated(location$year)) {
  stop("T.A2 A:G does not contain exactly one observation for each year 2001–2021.")
}

# ---- 2. Original T.A2: absolute values (H:N) --------------------------------
amounts <- read_excel(excel_file, sheet = "T.A2", range = "H4:N26", col_names = FALSE)
names(amounts) <- names(location)
amounts <- amounts %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
  filter(year %in% 2001:2021) %>%
  arrange(year)
if (nrow(amounts) != 21 || anyDuplicated(amounts$year)) {
  stop("T.A2 H:N does not contain exactly one observation for each year 2001–2021.")
}

# In T.A2 A:G, shares are proportions; H:N are levels (billions of USD).
# Check the denominator and the four location series independently within T.A2.
location_check <- location %>%
  select(year, switzerland, american, asian, other_european, other_havens) %>%
  left_join(amounts, by = "year", suffix = c("_published_share", "_amount")) %>%
  mutate(
    switzerland_recomputed = switzerland_amount / total_offshore,
    american_recomputed = american_amount / total_offshore,
    asian_recomputed = asian_amount / total_offshore,
    other_european_recomputed = other_european_amount / total_offshore,
    other_havens_recomputed = other_havens_amount / total_offshore,
    sum_four_shares = switzerland_published_share + american_published_share +
      asian_published_share + other_european_published_share,
    residual_to_one = sum_four_shares - 1,
    residual_other_havens = american_published_share + asian_published_share +
      other_european_published_share - other_havens_published_share
  )

for (g in c("switzerland", "american", "asian", "other_european", "other_havens")) {
  location_check[[paste0(g, "_difference_pp")]] <- 100 * (
    location_check[[paste0(g, "_published_share")]] -
      location_check[[paste0(g, "_recomputed")]]
  )
}
write_csv(location_check, file.path(out_dir, "01-location-share-check.csv"), na = "")

# ---- 3. R-produced country totals vs published global total -----------------
countries <- readRDS(countries_file)
required <- c("year", "indicator", "value")
missing_cols <- setdiff(required, names(countries))
if (length(missing_cols)) stop("countries.rds missing columns: ", paste(missing_cols, collapse = ", "))

# One row per country/year/indicator is expected. The 'total' indicator is in USD bn.
# Exclude missing observations; report counts so that missingness remains visible.
country_totals <- countries %>%
  filter(indicator == "total", year %in% 2001:2021) %>%
  group_by(year) %>%
  summarise(r_total_bn = sum(value, na.rm = TRUE),
            n_rows = n(), n_missing = sum(is.na(value)), .groups = "drop")

# Published absolute total is in the H:N block of T.A2.
total_check <- amounts %>%
  transmute(year, published_total_bn = total_offshore) %>%
  left_join(country_totals, by = "year") %>%
  mutate(difference_bn = r_total_bn - published_total_bn,
         difference_pct = 100 * difference_bn / published_total_bn)
write_csv(total_check, file.path(out_dir, "08-country-global-total-check.csv"), na = "")

# ---- 4. Figure 2 plot from the published input, not from countries.rds ------
plot_data <- location %>%
  select(year, switzerland, american, asian, other_european) %>%
  pivot_longer(-year, names_to = "haven_group", values_to = "share") %>%
  mutate(haven_group = recode(haven_group,
    switzerland = "Switzerland", american = "American tax havens",
    asian = "Asian tax havens", other_european = "Other European tax havens"),
    share_pct = 100 * share)
write_csv(plot_data, file.path(out_dir, "03-figure2-published-plot-data.csv"), na = "")

p <- ggplot(plot_data, aes(year, share_pct, group = haven_group,
                           linetype = haven_group, shape = haven_group)) +
  geom_line(linewidth = 0.5) + geom_point(size = 2) +
  scale_x_continuous(breaks = 2001:2021) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "% of the wealth held in all tax havens",
       linetype = NULL, shape = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "right")
ggsave(file.path(out_dir, "08-figure2-from-published-input.pdf"), p,
       width = 7, height = 5)

# ---- 5. Diagnostics ---------------------------------------------------------
cat("\nFigure 2 validation files written to: ", normalizePath(out_dir), "\n", sep = "")
cat("Maximum absolute difference between T.A2 share and amount-derived share (pp):\n")
print(location_check %>%
  select(ends_with("_difference_pp")) %>%
  summarise(across(everything(), ~ max(abs(.x), na.rm = TRUE))))
cat("\nR country totals versus published global totals (USD bn):\n")
print(total_check, n = Inf)
cat("\nNOTE: Figure 2 location shares are read from published T.A2.\n",
    "countries.rds allocates wealth to owner countries, not to haven locations.\n",
    "Matching Figure 2 here is NOT an independent replication of haven-location estimates.\n", sep = "")
