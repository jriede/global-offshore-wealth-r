# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# 3.5.1 Sensitivity analysis: bank deposit share
# Run after 0a-setup.R; uses its raw, work, fig, and tables paths.

library(readxl)
library(ggplot2)
library(dplyr)
library(knitr)

out <- file.path(work, "07-sensitivity-analysis", "deposit_shares")
fig_out <- file.path(fig, "07-sensitivity-analysis", "deposit_shares")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_out, recursive = TRUE, showWarnings = FALSE)

stopifnot(exists("raw"), exists("work"), exists("fig"), exists("tables"))
input <- file.path(raw, "FGZ-raw-data.xlsx")
if (!file.exists(input)) stop("Input file not found: ", input)
for (p in c(work, fig, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# Exact equivalent of Stata: sheet(T.A1), cellrange(A8:E28).
dat <- read_excel(input, sheet = "T.A1", range = "A8:E28", col_names = FALSE)
names(dat) <- c("year", "world_gdp", "published_wealth", "securities", "deposits")
dat[] <- lapply(dat, as.numeric)
stopifnot(nrow(dat) == 21L,
          identical(as.integer(dat$year), 2001:2021),
          !anyNA(dat),
          all(dat$world_gdp > 0), all(dat$securities > 0), all(dat$deposits >= 0))

# Reconstruct original annual deposit share and component-consistent baseline.
dat$d0 <- with(dat, deposits / (securities + deposits))
dat$baseline <- with(dat, securities / (1 - d0))
dat$baseline_published_gap <- with(dat, baseline - published_wealth)

# Four percentage-point shifts, holding securities and world GDP constant.
shifts <- c(m10 = -0.10, m5 = -0.05, p5 = 0.05, p10 = 0.10)
for (scenario in names(shifts)) {
  d_name <- paste0("d_", scenario)
  w_name <- paste0("wealth_", scenario)
  g_name <- paste0("gdp_", scenario)
  c_name <- paste0("change_", scenario)
  dat[[d_name]] <- dat$d0 + shifts[[scenario]]
  if (any(dat[[d_name]] < 0 | dat[[d_name]] >= 1)) {
    stop("Deposit share outside [0, 1) in scenario: ", scenario)
  }
  dat[[w_name]] <- dat$securities / (1 - dat[[d_name]])
  dat[[g_name]] <- 100 * dat[[w_name]] / dat$world_gdp
  dat[[c_name]] <- 100 * (dat[[w_name]] / dat$baseline - 1)
}
dat$gdp_baseline <- 100 * dat$baseline / dat$world_gdp
dat$gdp_published <- 100 * dat$published_wealth / dat$world_gdp

# Explicitly report published/component discrepancies (including 2015).
discrepancies <- dat[abs(dat$baseline_published_gap) > 0.01,
                     c("year", "baseline", "published_wealth", "baseline_published_gap")]
print(discrepancies, row.names = FALSE)
write.csv(dat, file.path(out, "7a-sensitivity_deposits.csv"), row.names = FALSE)
saveRDS(dat, file.path(work, "7a-sensitivity_deposits.rds"))

# Long data for plotting; use base R to avoid an extra tidyr dependency.
series <- c("gdp_m10", "gdp_m5", "gdp_baseline", "gdp_p5", "gdp_p10")
labels <- c("-10 pp", "-5 pp", "Baseline", "+5 pp", "+10 pp")
plot_dat <- do.call(rbind, lapply(seq_along(series), function(i) {
  data.frame(year = dat$year, value = dat[[series[i]]], scenario = labels[i])
}))
plot_dat$scenario <- factor(plot_dat$scenario, levels = labels)

p <- ggplot(plot_dat, aes(x = year, y = value, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = data.frame(year = dat$year, value = dat$gdp_published),
             aes(x = year, y = value, color = "Published FGZ"),
             inherit.aes = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(2001, 2021, by = 3), limits = c(2001, 2021)) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, by = 2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(name = NULL,
    values = c("-10 pp" = "#2878D0", "-5 pp" = "#C52C60",
               "Baseline" = "#009E73", "+5 pp" = "#D5A000",
               "+10 pp" = "#493092", "Published FGZ" = "#D94A28"),
    breaks = c(labels, "Published FGZ")) +
  scale_linetype_manual(name = NULL,
    values = c("-10 pp" = "dashed", "-5 pp" = "dashed",
               "Baseline" = "solid", "+5 pp" = "dashed", "+10 pp" = "dashed"),
    breaks = labels) +
  labs(x = "Year", y = "Offshore wealth (% of world GDP)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(),
        plot.margin = margin(8, 8, 8, 8)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE))

print(p)
ggsave(file.path(fig_out, "7a-sensitivity_deposits.png"), p,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(fig_out, "7a-sensitivity_deposits.pdf"), p,
       width = 8, height = 5)

# ---- generate latex tables for appendix A2

# Ergebnisse für den Appendix aufbereiten
appendix_a2 <- dat %>%
  transmute(
    Year = as.integer(year),
    `-10 pp` = gdp_m10,
    `-5 pp` = gdp_m5,
    Baseline = gdp_baseline,
    `+5 pp` = gdp_p5,
    `+10 pp` = gdp_p10
  )

# LaTeX-Tabelle erzeugen
latex_table <- knitr::kable(
  appendix_a2,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = c("c", rep("r", 5)),
  caption = paste(
    "Sensitivity of estimated global offshore wealth",
    "to alternative bank deposit shares, 2001--2021.",
    "Values are expressed as a percentage of world GDP."
  ),
  label = "tab:sensitivity_deposits",
  escape = FALSE
)

writeLines(
  latex_table,
  file.path(tables, "appendix_a2_deposits.tex")
)

#---