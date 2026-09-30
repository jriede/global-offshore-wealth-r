# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# Title: 6d-replicate-offshore-location.R
#
# Purpose:
#   Reproduce Figure 2 ("Where is the World's Offshore Household Wealth Located?")
#   from the R-generated intermediate datasets.
#
# Replicated inputs:
#   - offshore2001.rds ... offshore2021.rds, produced by 6a-build-offshore-01-22.R
#   - global_offshore_wealth.rds, produced by 3d_build_global_offshore_wealth.R
#
# Benchmark only:
#   - FGZ-raw-data.xlsx, sheet T.A2
#
# IMPORTANT:
#   The original FGZ T.A2 values are NOT used to construct the replicated series.
#   They are read only after the replication has been constructed, for validation.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(readxl)
library(ggplot2)

years <- 2001:2021

# ------------------------------------------------------------------------------
# 1. Read annual R-generated offshore datasets from 6a
# ------------------------------------------------------------------------------

offshore_panel <- map_dfr(
  years,
  function(y) {
    x <- read_work_data(paste0("offshore", y))

    required <- c("year", "bank", "amt_bis")
    missing <- setdiff(required, names(x))

    if (length(missing) > 0) {
      stop(
        "offshore", y, " is missing required variable(s): ",
        paste(missing, collapse = ", ")
      )
    }

    x %>%
      mutate(year = y)
  }
)

# ------------------------------------------------------------------------------
# 2. Non-Swiss haven-location weights from the FGZ aggregate observations
#
# IMPORTANT:
# 6a already constructs household-adjusted aggregate BIS observations with
# bank == "AS", "CR", and "EU". These are the relevant location aggregates.
# Do NOT reconstruct Figure 2 by summing the individual haven rows and do NOT
# use amt_inter: amt_inter is interbank deposits, whereas the location allocation
# is based on household-adjusted non-bank deposits (amt_bis).
# ------------------------------------------------------------------------------

required_location_banks <- c("AS", "CR", "EU")

missing_location_banks <- setdiff(
  required_location_banks,
  unique(offshore_panel$bank)
)

if (length(missing_location_banks) > 0) {
  stop(
    "The annual offshore datasets are missing aggregate haven observations: ",
    paste(missing_location_banks, collapse = ", "),
    ". Re-run 6a-build-offshore-01-22.R."
  )
}

# 6a constructs these aggregate observations after applying the heterogeneous
# household-deposit shares to the underlying haven deposits:
#   AS = Asian havens
#   CR = Caribbean/American havens
#   EU = Other European havens
#
# Sum over saver countries because Figure 2 concerns where wealth is located,
# not who owns it.
non_swiss_location <- offshore_panel %>%
  filter(bank %in% required_location_banks) %>%
  group_by(year, bank) %>%
  summarise(
    amount = sum(amt_bis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    haven_group = recode(
      bank,
      "CR" = "American tax havens",
      "AS" = "Asian tax havens",
      "EU" = "Other European tax havens"
    )
  )

# Conditional distribution of offshore wealth outside Switzerland.
# These three weights must sum to one in each year.
non_swiss_shares <- non_swiss_location %>%
  group_by(year) %>%
  mutate(
    non_swiss_total = sum(amount, na.rm = TRUE),
    share_within_non_swiss = amount / non_swiss_total
  ) %>%
  ungroup()

location_weight_check <- non_swiss_shares %>%
  group_by(year) %>%
  summarise(
    total = sum(share_within_non_swiss, na.rm = TRUE),
    .groups = "drop"
  )

stopifnot(
  all(abs(location_weight_check$total - 1) < 1e-10)
)

message(
  "Using household-adjusted aggregate non-bank BIS deposits (amt_bis) for ",
  "AS/CR/EU location weights."
)

# ------------------------------------------------------------------------------
# 4. Switzerland: construct its absolute offshore-wealth amount
#
# FGZ define Swiss offshore wealth as securities in Swiss custody plus
# fiduciary deposits. The saved 6a datasets contain the fiduciary distribution,
# not necessarily the aggregate Swiss securities series.
#
# If a R-generated Swiss total is already saved in work, use it. Otherwise stop:
# do NOT silently import the published T.A2 Swiss series into the replication.
# ------------------------------------------------------------------------------

swiss_candidates <- c(
  "switzerland_offshore_wealth",
  "swiss_offshore_wealth",
  "offshore_switzerland"
)

read_first_existing <- function(candidates) {
  for (nm in candidates) {
    path <- file.path(work, paste0(nm, ".rds"))
    if (file.exists(path)) {
      message("Using Swiss aggregate from: ", basename(path))
      return(readRDS(path))
    }
  }
  NULL
}

swiss_raw <- read_first_existing(swiss_candidates)

# Try to infer a usable year/value pair from a saved Swiss aggregate.
standardise_swiss <- function(x) {
  if (is.null(x)) return(NULL)

  x <- as.data.frame(x)

  year_candidates <- intersect(
    c("year", "Year"),
    names(x)
  )

  value_candidates <- intersect(
    c(
      "switzerland_offshore_wealth",
      "swiss_offshore_wealth",
      "offshore_switzerland",
      "total",
      "value",
      "amount"
    ),
    names(x)
  )

  if (length(year_candidates) == 0 || length(value_candidates) == 0) {
    return(NULL)
  }

  x %>%
    transmute(
      year = as.integer(.data[[year_candidates[1]]]),
      switzerland_amount = as.numeric(.data[[value_candidates[1]]])
    ) %>%
    filter(year %in% years) %>%
    group_by(year) %>%
    summarise(
      switzerland_amount = first(switzerland_amount),
      .groups = "drop"
    )
}

switzerland <- standardise_swiss(swiss_raw)

# ------------------------------------------------------------------------------
# 5. Load independently replicated global offshore wealth from 3d
# ------------------------------------------------------------------------------

global_path <- file.path(work, "global_offshore_wealth.rds")

if (!file.exists(global_path)) {
  stop(
    "global_offshore_wealth.rds not found. ",
    "Run 3d_build_global_offshore_wealth.R first."
  )
}

global_ow <- readRDS(global_path) %>%
  transmute(
    year = as.integer(year),
    global_offshore_wealth = as.numeric(offshore_wealth)
  ) %>%
  filter(year %in% years)

# ------------------------------------------------------------------------------
# 6. Construct replicated location shares
#
# If a fully R-generated Swiss aggregate exists:
#
#   Swiss share = Swiss OW / global OW
#   non-Swiss share = 1 - Swiss share
#
# The non-Swiss share is split across American, Asian and European havens using
# the R-reconstructed BIS location weights.
#
# If the Swiss aggregate is not available as a saved R object, we still save
# and plot the non-Swiss conditional shares, but stop before claiming a full
# Figure-2 replication.
# ------------------------------------------------------------------------------

if (!is.null(switzerland)) {

  location_replication <- global_ow %>%
    left_join(switzerland, by = "year") %>%
    mutate(
      Switzerland = switzerland_amount / global_offshore_wealth,
      non_swiss_share = 1 - Switzerland
    ) %>%
    select(year, Switzerland, non_swiss_share) %>%
    left_join(
      non_swiss_shares %>%
        select(year, haven_group, share_within_non_swiss) %>%
        pivot_wider(
          names_from = haven_group,
          values_from = share_within_non_swiss
        ),
      by = "year"
    ) %>%
    mutate(
      `American tax havens` =
        non_swiss_share * `American tax havens`,
      `Asian tax havens` =
        non_swiss_share * `Asian tax havens`,
      `Other European tax havens` =
        non_swiss_share * `Other European tax havens`
    ) %>%
    select(
      year,
      Switzerland,
      `American tax havens`,
      `Asian tax havens`,
      `Other European tax havens`
    ) %>%
    mutate(
      total_share =
        Switzerland +
        `American tax havens` +
        `Asian tax havens` +
        `Other European tax havens`
    )

} else {

  # Save useful intermediate output before stopping.
  write_csv(
    non_swiss_shares,
    file.path(work, "offshore_location_non_swiss_shares.csv")
  )

  saveRDS(
    non_swiss_shares,
    file.path(work, "offshore_location_non_swiss_shares.rds")
  )

  stop(
    paste0(
      "No independently R-generated aggregate Swiss offshore-wealth series ",
      "was found in work/. The non-Swiss BIS shares have been reconstructed ",
      "and saved, but a full Figure 2 replication requires the Swiss aggregate ",
      "(Swiss securities + fiduciary deposits). Do not substitute FGZ T.A2 ",
      "here, because that would make the replication circular."
    )
  )
}

# ------------------------------------------------------------------------------
# 7. Checks and save replicated series
# ------------------------------------------------------------------------------

location_replication <- location_replication %>%
  arrange(year)

print(location_replication, n = Inf)

message(
  "Maximum deviation of the four location shares from 100%: ",
  round(
    max(abs(location_replication$total_share - 1), na.rm = TRUE) * 100,
    6
  ),
  " percentage points."
)

saveRDS(
  location_replication,
  file.path(work, "offshore_location_replication.rds")
)

write_csv(
  location_replication,
  file.path(work, "offshore_location_replication.csv")
)

# ------------------------------------------------------------------------------
# 8. Original FGZ series -- BENCHMARK ONLY
# ------------------------------------------------------------------------------

fgz_file <- file.path(raw, "FGZ-raw-data.xlsx")

fgz_location <- read_excel(
  fgz_file,
  sheet = "T.A2",
  range = "H4:N26"
)

names(fgz_location)[1:7] <- c(
  "year",
  "Totaloffshorewealth",
  "Switzerland",
  "TaxhavensotherthanSwitzerland",
  "American tax havens",
  "Asian tax havens",
  "Other European tax havens"
)

fgz_location <- fgz_location %>%
  transmute(
    year = as.integer(year),
    `Switzerland` =
      as.numeric(Switzerland) / as.numeric(Totaloffshorewealth),
    `American tax havens` =
      as.numeric(`American tax havens`) / as.numeric(Totaloffshorewealth),
    `Asian tax havens` =
      as.numeric(`Asian tax havens`) / as.numeric(Totaloffshorewealth),
    `Other European tax havens` =
      as.numeric(`Other European tax havens`) / as.numeric(Totaloffshorewealth)
  ) %>%
  filter(year %in% years)

# ------------------------------------------------------------------------------
# 9. Year-by-year replication comparison
# ------------------------------------------------------------------------------

rep_long <- location_replication %>%
  select(
    year,
    Switzerland,
    `American tax havens`,
    `Asian tax havens`,
    `Other European tax havens`
  ) %>%
  pivot_longer(
    -year,
    names_to = "haven_group",
    values_to = "replicated"
  )

fgz_long <- fgz_location %>%
  pivot_longer(
    -year,
    names_to = "haven_group",
    values_to = "original"
  )

comparison <- rep_long %>%
  left_join(
    fgz_long,
    by = c("year", "haven_group")
  ) %>%
  mutate(
    replicated_pct = 100 * replicated,
    original_pct = 100 * original,
    difference_pp = replicated_pct - original_pct,
    abs_difference_pp = abs(difference_pp)
  )

print(
  comparison %>%
    select(
      year,
      haven_group,
      replicated_pct,
      original_pct,
      difference_pp
    ),
  n = Inf
)

comparison %>%
  filter(year == 2001) %>%
  print(width = Inf)

comparison_summary <- comparison %>%
  group_by(haven_group) %>%
  summarise(
    MAE_pp = mean(abs_difference_pp, na.rm = TRUE),
    max_abs_diff_pp = max(abs_difference_pp, na.rm = TRUE),
    mean_diff_pp = mean(difference_pp, na.rm = TRUE),
    .groups = "drop"
  )

print(comparison_summary)

saveRDS(
  comparison,
  file.path(work, "offshore_location_comparison.rds")
)

write_csv(
  comparison,
  file.path(work, "offshore_location_comparison.csv")
)

# ------------------------------------------------------------------------------
# 10. Figure 2 -- replicated series
# ------------------------------------------------------------------------------

plot_rep <- rep_long %>%
  mutate(
    share = 100 * replicated,
    haven_group = factor(
      haven_group,
      levels = c(
        "American tax havens",
        "Asian tax havens",
        "Other European tax havens",
        "Switzerland"
      )
    )
  )

p_location_replication <- ggplot(
  plot_rep,
  aes(
    x = year,
    y = share,
    group = haven_group,
    linetype = haven_group,
    shape = haven_group
  )
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_linetype_manual(
    values = c(
      "American tax havens" = "solid",
      "Asian tax havens" = "dotted",
      "Other European tax havens" = "dashed",
      "Switzerland" = "longdash"
    )
  ) +
  scale_shape_manual(
    values = c(
      "American tax havens" = 16,
      "Asian tax havens" = 17,
      "Other European tax havens" = 15,
      "Switzerland" = 3
    )
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  scale_y_continuous(
    name = "% of the wealth held in all tax havens",
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

print(p_location_replication)

ggsave(
  file.path(fig, "6d-offshore-location-global-wealth-replication.pdf"),
  p_location_replication,
  width = 9,
  height = 6
)

ggsave(
  file.path(fig, "6d-offshore-location-global-wealth-replication.png"),
  p_location_replication,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 11. Comparison plot: R replication vs FGZ
# ------------------------------------------------------------------------------

comparison_plot_data <- comparison %>%
  select(
    year,
    haven_group,
    `R replication` = replicated_pct,
    `FGZ (2023)` = original_pct
  ) %>%
  pivot_longer(
    cols = c(`R replication`, `FGZ (2023)`),
    names_to = "series",
    values_to = "share"
  )

p_location_comparison <- ggplot(
  comparison_plot_data,
  aes(
    x = year,
    y = share,
    group = interaction(haven_group, series),
    linetype = series
  )
) +
  geom_line(linewidth = 0.65) +
  facet_wrap(
    ~ haven_group,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_linetype_manual(
    values = c(
      "R replication" = "solid",
      "FGZ (2023)" = "dashed"
    )
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 5)
  ) +
  scale_y_continuous(
    name = "% of global offshore wealth",
    labels = function(x) paste0(round(x, 1), "%")
  ) +
  labs(linetype = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

print(p_location_comparison)

ggsave(
  file.path(fig, "6d-offshore-location-replication-vs-fgz.pdf"),
  p_location_comparison,
  width = 9,
  height = 6
)

ggsave(
  file.path(fig, "6d-offshore-location-replication-vs-fgz.png"),
  p_location_comparison,
  width = 9,
  height = 6,
  dpi = 300
)



# ------------------------------------------------------------------------------
# 12. Export numerical Figure-2 data and LaTeX appendix tables
# ------------------------------------------------------------------------------

# Numerical data underlying the replicated Figure 2 (percentage points).
figure2_numeric <- plot_rep %>%
  transmute(
    year = as.integer(year),
    haven_group = as.character(haven_group),
    share_pct = as.numeric(share)
  ) %>%
  arrange(year, haven_group)

write_csv(
  figure2_numeric,
  file.path(tables, "6d-figure2-replication-data.csv")
)

saveRDS(
  figure2_numeric,
  file.path(tables, "6d-figure2-replication-data.rds")
)

# Wide version: convenient for inspection and for the appendix table.
figure2_numeric_wide <- figure2_numeric %>%
  pivot_wider(
    names_from = haven_group,
    values_from = share_pct
  ) %>%
  select(
    year,
    Switzerland,
    `American tax havens`,
    `Asian tax havens`,
    `Other European tax havens`
  ) %>%
  arrange(year)

write_csv(
  figure2_numeric_wide,
  file.path(tables, "6d-figure2-replication-data-wide.csv")
)

# Comparison data underlying the replication-vs-FGZ validation plot.
figure2_comparison_numeric <- comparison %>%
  transmute(
    year,
    haven_group,
    replication_pct = replicated_pct,
    fgz_pct = original_pct,
    difference_pp
  ) %>%
  arrange(year, haven_group)

write_csv(
  figure2_comparison_numeric,
  file.path(tables, "6d-figure2-replication-vs-fgz-data.csv")
)

# Helper for LaTeX-safe text.
latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([%&_#$])", "\\\\\\1", x)
  x
}

# Appendix Table 1: replicated Figure-2 series.
appendix_table_rep <- figure2_numeric_wide %>%
  mutate(across(-year, ~ round(.x, 2)))

rep_header <- c(
  "Year",
  "Switzerland",
  "American tax havens",
  "Asian tax havens",
  "Other European tax havens"
)

rep_lines <- apply(appendix_table_rep, 1, function(r) {
  paste0(
    as.integer(r[[1]]), " & ",
    sprintf("%.2f", as.numeric(r[[2]])), " & ",
    sprintf("%.2f", as.numeric(r[[3]])), " & ",
    sprintf("%.2f", as.numeric(r[[4]])), " & ",
    sprintf("%.2f", as.numeric(r[[5]])),
    " \\\\"
  )
})

rep_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Replicated location of global offshore household wealth, 2001--2021}",
  "\\label{tab:appendix_offshore_location_replication}",
  "\\small",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  paste0(paste(latex_escape(rep_header), collapse = " & "), " \\\\"),
  "\\midrule",
  rep_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.96\\textwidth}",
  "\\footnotesize\\textit{Notes:} Entries are percentages of global offshore household wealth. The series are the numerical data underlying the replicated Figure 2. Values may not sum exactly to 100 due to rounding.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  rep_tex,
  file.path(tables, "6d-figure2-replication-appendix-table.tex")
)

# Appendix Table 2: replication benchmark against FGZ (2023).
# A compact layout with one row per year and group, suitable for a longtable.
appendix_table_comparison <- figure2_comparison_numeric %>%
  mutate(
    haven_group = as.character(haven_group),
    replication_pct = round(replication_pct, 2),
    fgz_pct = round(fgz_pct, 2),
    difference_pp = round(difference_pp, 2)
  )

comp_lines <- apply(appendix_table_comparison, 1, function(r) {
  paste0(
    as.integer(r[[1]]), " & ",
    latex_escape(as.character(r[[2]])), " & ",
    sprintf("%.2f", as.numeric(r[[3]])), " & ",
    sprintf("%.2f", as.numeric(r[[4]])), " & ",
    sprintf("%.2f", as.numeric(r[[5]])),
    " \\\\"
  )
})

comp_tex <- c(
  "\\begin{longtable}{llrrr}",
  "\\caption{Replication of the location of global offshore household wealth: comparison with FGZ (2023)}\\label{tab:appendix_offshore_location_comparison}\\\\",
  "\\toprule",
  "Year & Location & R replication (\\%) & FGZ (2023) (\\%) & Difference (pp) \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\multicolumn{5}{c}{\\tablename\\ \\thetable{} -- continued from previous page} \\\\",
  "\\toprule",
  "Year & Location & R replication (\\%) & FGZ (2023) (\\%) & Difference (pp) \\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{5}{r}{Continued on next page} \\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\multicolumn{5}{p{0.96\\textwidth}}{\\footnotesize \\textit{Notes:} Difference is the R replication minus the published FGZ (2023) value, measured in percentage points.} \\\\",
  "\\endlastfoot",
  comp_lines,
  "\\end{longtable}"
)

writeLines(
  comp_tex,
  file.path(tables, "6d-figure2-replication-vs-fgz-appendix-table.tex")
)

message(
  "Exported Figure-2 numerical data and LaTeX appendix tables to: ",
  tables
)


# ==============================================================================
# End
# ==============================================================================
