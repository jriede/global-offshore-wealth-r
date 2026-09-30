# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# This program graphs country group shares of fiduciary deposits in
# Swiss banks spanning 1987 to 2022.
#
# databases used: - fiduciary-87-22.dta
# outputs:        - update-swiss-fiduciary-87-22.pdf
#                 - Table B1
#                 - Table B2
# ==============================================================================

library(tidyverse)
library(haven)
library(openxlsx)

plot_data_dir <- file.path(work, "plot-data", "r")

export_plot_data <- function(data, filename) {
  readr::write_csv(
    data,
    file.path(plot_data_dir, paste0(filename, ".csv")),
    na = ""
  )
}

# ==============================================================================
# I ---- Graph
# ==============================================================================

# Load fiduciary deposit data
#fiduciary <- read_dta(file.path(work2, "fiduciary-87-22.dta"))
fiduciary <- read_work_data("fiduciary-87-22")

# ------------------------------------------------------------------------------
# I.1 Create total fiduciary deposits by country group
# ------------------------------------------------------------------------------

fiduciary <- fiduciary %>%
  rename(
    haven = ofc
  ) %>%
  mutate(
    all = 1
  )


# Country groups used in the Stata loop
country_groups <- c(
  "haven",
  "europe",
  "middle_east",
  "latin_am",
  "asia",
  "africa",
  "north_am",
  "caribbean",
  "rich",
  "developing"
)


# Calculate annual total fiduciary deposits for each country group
group_totals <- fiduciary %>%
  select(
    year,
    lfidudol,
    all_of(country_groups)
  ) %>%
  pivot_longer(
    cols = all_of(country_groups),
    names_to = "countryg",
    values_to = "member"
  ) %>%
  filter(
    member == 1
  ) %>%
  group_by(
    year,
    countryg
  ) %>%
  summarise(
    total = sum(lfidudol, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = countryg,
    values_from = total,
    names_prefix = "tot_"
  )


# Total fiduciary deposits by year
total_all <- fiduciary %>%
  group_by(year) %>%
  summarise(
    tot_all = sum(lfidudol, na.rm = TRUE),
    .groups = "drop"
  )


# Add totals to original dataset
fiduciary <- fiduciary %>%
  left_join(
    group_totals,
    by = "year"
  ) %>%
  left_join(
    total_all,
    by = "year"
  )


# ------------------------------------------------------------------------------
# I.2 Compute shares of fiduciary deposits
# ------------------------------------------------------------------------------

share_groups <- c(
  "haven",
  "europe",
  "middle_east",
  "latin_am",
  "asia",
  "africa",
  "north_am",
  "caribbean"
)


# Shares
for (g in share_groups) {
  
  fiduciary[[paste0("sh_", g)]] <-
    fiduciary[[paste0("tot_", g)]] /
    fiduciary$tot_all
}


# Stata:
# gen sh_all = .
# replace sh_all = tot_all / tot_all if all == 1

fiduciary <- fiduciary %>%
  mutate(
    sh_all = tot_all / tot_all
  )


# ------------------------------------------------------------------------------
# I.3 Convert shares into percentages
# ------------------------------------------------------------------------------

for (g in share_groups) {
  
  fiduciary[[paste0("pct_", g)]] <-
    fiduciary[[paste0("sh_", g)]] * 100
}


fiduciary <- fiduciary %>%
  mutate(
    pct_all = sh_all * 100
  )


# ==============================================================================
# II ---- Figure
# ==============================================================================

# One observation per year is sufficient for plotting because the annual
# percentages are identical across all countries belonging to a group.

graph_data <- fiduciary %>%
  select(
    year,
    pct_haven,
    pct_europe,
    pct_middle_east,
    pct_latin_am,
    pct_asia,
    pct_africa,
    pct_north_am
  ) %>%
  distinct() %>%
  pivot_longer(
    cols = -year,
    names_to = "group",
    values_to = "pct"
  ) %>%
  mutate(
    group = recode(
      group,
      pct_haven       = "Tax Havens",
      pct_europe      = "Europe",
      pct_middle_east = "Middle East",
      pct_latin_am    = "Latin and South America",
      pct_asia        = "Asia",
      pct_africa      = "Africa",
      pct_north_am    = "North America"
    ),
    
    group = factor(
      group,
      levels = c(
        "Tax Havens",
        "Europe",
        "Middle East",
        "Latin and South America",
        "Asia",
        "Africa",
        "North America"
      )
    )
  )


# Create figure
fig_fiduciary <- ggplot(
  graph_data,
  aes(
    x = year,
    y = pct,
    group = group,
    linetype = group,
    shape = group
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(1984, 2024, 4),
    limits = c(1984, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 80, 10),
    limits = c(0, 80),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = "% of total foreign-owned Swiss bank deposits",
    linetype = NULL,
    shape = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.70),
    legend.background = element_blank(),
    legend.key = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    
    panel.grid.minor = element_blank()
  )


# Display figure
fig_fiduciary


# Export figure
ggsave(
  filename = file.path(
    fig,
    "update-swiss-fiduciary-87-22.pdf"
  ),
  plot = fig_fiduciary,
  width = 7,
  height = 5
)

ggsave(
  filename = file.path(
    fig,
    "update-swiss-fiduciary-87-22.png"
  ),
  plot = fig_fiduciary,
  width = 7,
  height = 5
)


# ==============================================================================
# III ---- Rich and developing countries
# ==============================================================================

# Stata:
#
# foreach g in rich developing {
#     forval i = 1987/2022 {
#         replace pct_`g' = (tot_`g'/tot_all)*100
#             if year == `i' & `g' == 1
#     }
# }

fiduciary <- fiduciary %>%
  mutate(
    pct_rich = if_else(
      rich == 1,
      (tot_rich / tot_all) * 100,
      NA_real_
    ),
    
    pct_developing = if_else(
      developing == 1,
      (tot_developing / tot_all) * 100,
      NA_real_
    )
  )


# ------------------------------------------------------------------------------
# III.1 Euro Area 16
# ------------------------------------------------------------------------------

# Important:
# The original Stata code calculates this sum over the ENTIRE dataset,
# not separately by year:
#
# sum(lfidudol) if rich == 1 & euro16 == 1
# gen tot_rich_euro16 = r(sum)

tot_rich_euro16_value <- fiduciary %>%
  filter(
    rich == 1,
    euro16 == 1
  ) %>%
  summarise(
    total = sum(lfidudol, na.rm = TRUE)
  ) %>%
  pull(total)


fiduciary <- fiduciary %>%
  mutate(
    tot_rich_euro16 = tot_rich_euro16_value,
    
    pct_rich_euro16 =
      tot_rich_euro16 / tot_rich,
    
    pct_excl_middle_east =
      pct_developing - pct_middle_east
  )


# ==============================================================================
# IV ---- Table B1
# ==============================================================================

# Stata preserve:
#
# gen tot_rich_eu16 = .
# forval i = 1987/2022 {
#     sum(lfidudol) if year == `i' & rich == 1 & euro16 == 1
#     replace tot_rich_eu16 = r(sum) if year == `i'
# }

rich_euro16_by_year <- fiduciary %>%
  filter(
    rich == 1,
    euro16 == 1
  ) %>%
  group_by(year) %>%
  summarise(
    tot_rich_eu16 = sum(
      lfidudol,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


table_b1 <- fiduciary %>%
  left_join(
    rich_euro16_by_year,
    by = "year"
  ) %>%
  mutate(
    tot_excl_middle_east =
      tot_developing - tot_middle_east
  ) %>%
  group_by(year) %>%
  summarise(
    across(
      c(
        tot_haven,
        tot_europe,
        tot_middle_east,
        tot_latin_am,
        tot_asia,
        tot_africa,
        tot_north_am,
        tot_caribbean,
        tot_all,
        tot_rich,
        tot_rich_eu16,
        tot_developing,
        tot_excl_middle_east
      ),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


# ==============================================================================
# V ---- Table B2
# ==============================================================================

# Equivalent to Stata restore followed by collapse

table_b2 <- fiduciary %>%
  group_by(year) %>%
  summarise(
    across(
      c(
        pct_haven,
        pct_europe,
        pct_middle_east,
        pct_latin_am,
        pct_asia,
        pct_africa,
        pct_north_am,
        pct_caribbean,
        pct_all,
        pct_rich,
        pct_rich_euro16,
        pct_developing,
        pct_excl_middle_east
      ),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pct_ex_havens =
      pct_all - pct_haven
  )


# ------------------------------------------------------------------------------
# V.1 Round percentages
# ------------------------------------------------------------------------------

pct_variables <- c(
  "pct_haven",
  "pct_europe",
  "pct_middle_east",
  "pct_latin_am",
  "pct_asia",
  "pct_africa",
  "pct_north_am",
  "pct_caribbean",
  "pct_all",
  "pct_ex_havens",
  "pct_rich",
  "pct_rich_euro16",
  "pct_developing",
  "pct_excl_middle_east"
)


table_b2 <- table_b2 %>%
  mutate(
    across(
      all_of(pct_variables),
      ~ round(.x)
    )
  )


# ==============================================================================
# VI ---- Variable labels
# ==============================================================================

# Variable labels corresponding to the Stata labels

attr(table_b2$pct_haven, "label") <-
  "Tax Havens"

attr(table_b2$pct_europe, "label") <-
  "Europe"

attr(table_b2$pct_middle_east, "label") <-
  "Middle East"

attr(table_b2$pct_latin_am, "label") <-
  "Latin and South America"

attr(table_b2$pct_asia, "label") <-
  "Asia"

attr(table_b2$pct_africa, "label") <-
  "Africa"

attr(table_b2$pct_north_am, "label") <-
  "North America"

attr(table_b2$pct_caribbean, "label") <-
  "Caribbean"

attr(table_b2$pct_ex_havens, "label") <-
  "Total ex-tax havens"

attr(table_b2$pct_all, "label") <-
  "Total"

attr(table_b2$pct_rich, "label") <-
  "Rich countries"

attr(table_b2$pct_rich_euro16, "label") <-
  "Of which: Euro area 16"

attr(table_b2$pct_developing, "label") <-
  "Developing countries"

attr(table_b2$pct_excl_middle_east, "label") <-
  "Excl. Middle East"


# ==============================================================================
# VII ---- Export Table B1 and Table B2
# ==============================================================================

# R/openxlsx works with .xlsx rather than the old .xls format used by Stata.
#
# If FGZ-raw-data.xlsx already exists, load the existing workbook so that
# other worksheets are preserved.

output_file <- file.path(
  raw,
  "FGZ-raw-data.xlsx"
)


if (file.exists(output_file)) {
  
  wb <- loadWorkbook(
    output_file
  )
  
} else {
  
  wb <- createWorkbook()
}


# ------------------------------------------------------------------------------
# VII.1 Export Table B1
# ------------------------------------------------------------------------------

if ("TableB1" %in% names(wb)) {
  removeWorksheet(
    wb,
    "TableB1"
  )
}

addWorksheet(
  wb,
  "TableB1"
)

writeData(
  wb,
  sheet = "TableB1",
  x = table_b1,
  colNames = TRUE
)


# ------------------------------------------------------------------------------
# VII.2 Export Table B2
# ------------------------------------------------------------------------------

if ("TableB2" %in% names(wb)) {
  removeWorksheet(
    wb,
    "TableB2"
  )
}

addWorksheet(
  wb,
  "TableB2"
)

writeData(
  wb,
  sheet = "TableB2",
  x = table_b2,
  colNames = TRUE
)


# Save workbook
saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


# ==============================================================================
# VIII ---- Checks
# ==============================================================================

print(table_b1)
print(table_b2)

cat(
  "\nFigure saved to:\n",
  file.path(
    fig,
    "update-swiss-fiduciary-87-22.pdf"
  ),
  "\n"
)

cat(
  "\nTables saved to:\n",
  output_file,
  "\n"
)

#saveRDS(fiduciary, file = file.path(work, "fiduciary-87-2l.rds"))

# ==============================================================================
# IX ---- Save plots, plot data and tables
# ==============================================================================

# ------------------------------------------------------------------------------
# IX.1 Plot
# ------------------------------------------------------------------------------

# Save the ggplot object
saveRDS(
  fig_fiduciary,
  file.path(fig, "update-swiss-fiduciary-87-22.rds")
)

# Save the plot as PDF
ggsave(
  filename = file.path(fig, "update-swiss-fiduciary-87-22.pdf"),
  plot = fig_fiduciary,
  width = 7,
  height = 5
)

ggsave(
  filename = file.path(fig, "update-swiss-fiduciary-87-22.png"),
  plot = fig_fiduciary,
  width = 7,
  height = 5
)

# ------------------------------------------------------------------------------
# IX.2 Plot data
# ------------------------------------------------------------------------------

# RDS preserves the data types, including factor levels
saveRDS(
  graph_data,
  file.path(work, "update-swiss-fiduciary-87-22-data.rds")
)

# CSV for comparison with Stata
export_plot_data(graph_data,"update-swiss-fiduciary-87-22-data-r")
export_plot_data(table_b1,"table_b1-r")
export_plot_data(table_b2,"table_b2-r")

write.csv(
  table_b1,
  file.path(work, "table-b1-r.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  table_b2,
  file.path(work, "table-b2-r.csv"),
  row.names = FALSE,
  na = ""
)
# ------------------------------------------------------------------------------
# IX.3 Tables B1 and B2
# ------------------------------------------------------------------------------

saveRDS(
  table_b1,
  file.path(work, "table-b1.rds")
)

saveRDS(
  table_b2,
  file.path(work, "table-b2.rds")
)

