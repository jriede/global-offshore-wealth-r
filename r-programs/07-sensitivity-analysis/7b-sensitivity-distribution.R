# 7b – Sensitivity of geographic allocation across non-Swiss tax havens
# Input: raw CSV exports of FGZ-raw-data.xlsx, sheets T.A2 and T.A2b.
# This is an allocation stress test, NOT a reconstruction of original BIS security holdings.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set these to the directories used in your project if they differ.
input_dir <- file.path(work, "07-sensitivity-analysis", "csv")
output_dir <- file.path(work, "07-sensitivity-analysis")
figure_dir <- fig

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

read_raw <- function(name) read_csv(file.path(input_dir, name),
                                    col_types = cols(.default = col_character()),
                                    show_col_types = FALSE)
a2 <- read_raw("T.A2.csv")
a2b <- read_raw("T.A2b.csv")

# CSVs exported with col_names=FALSE: first CSV row is ...1, ...2, etc.
# T.A2: Excel row 5 / CSV data row 4 contains group labels; year starts CSV data row 5.
# T.A2b: Excel row 4 / CSV data row 3 contains individual haven labels; year starts row 4.
num <- function(x) suppressWarnings(as.numeric(x))

groups <- a2 %>%
  transmute(year = num(.data[["...1"]]),
            global_wealth = num(.data[["...9"]]),
            switzerland = num(.data[["...10"]]),
            non_swiss = num(.data[["...11"]]),
            american = num(.data[["...12"]]),
            asian = num(.data[["...13"]]),
            european = num(.data[["...14"]])) %>%
  filter(between(year, 2001, 2021))

haven_labels <- as.character(unlist(a2b[3, 5:23], use.names = FALSE))
stopifnot(length(haven_labels) == 19L, all(nzchar(haven_labels)))


individual <- a2b %>%
  filter(
    between(num(.data[["...1"]]), 2001, 2021),
    abs(num(.data[["...2"]]) - 1) < 1e-8
  ) %>%
  transmute(
    year = num(.data[["...1"]]),
    across(5:23, num)
  )

names(individual)[-1] <- haven_labels


haven_long <- individual %>%
  pivot_longer(-year, names_to = "haven", values_to = "share_global") %>%
  left_join(groups, by = "year") %>%
  mutate(weight_baseline = share_global / (non_swiss / global_wealth))

# Verify source-data consistency before applying any scenario.
check <- haven_long %>%
  group_by(year) %>%
  summarise(sum_haven_shares = sum(share_global, na.rm = FALSE),
            non_swiss_share = first(non_swiss / global_wealth),
            sum_weights = sum(weight_baseline, na.rm = FALSE), .groups = "drop")
print(check)
if (anyNA(check) || any(abs(check$sum_haven_shares - check$non_swiss_share) > 1e-6)) {
  stop("T.A2b haven shares do not add up to the T.A2 non-Swiss share. Check source data.")
}

# Blend the FGZ allocation weights with equal weights across the 19 non-Swiss havens.
# The uniform allocation is a hypothetical counterfactual, not an empirical estimate.
scenarios <- tibble(scenario = c("Baseline", "25% equal weights", "50% equal weights"),
                    alpha = c(0, 0.25, 0.50))
results <- tidyr::crossing(haven_long, scenarios) %>%
  group_by(year, scenario) %>%
  mutate(n_havens = n(),
         weight = (1 - alpha) * weight_baseline + alpha / n_havens,
         share_global_scenario = weight * (non_swiss / global_wealth),
         wealth_bn_usd = weight * non_swiss) %>%
  ungroup()

# Validation: preserve the non-Swiss total and the Swiss component in every scenario.
validation <- results %>%
  group_by(year, scenario) %>%
  summarise(sum_weights = sum(weight),
            allocated_non_swiss = sum(wealth_bn_usd),
            non_swiss = first(non_swiss),
            swiss = first(switzerland),
            global_wealth = first(global_wealth), .groups = "drop")
stopifnot(all(abs(validation$sum_weights - 1) < 1e-8),
          all(abs(validation$allocated_non_swiss - validation$non_swiss) < 1e-6),
          all(abs(validation$swiss + validation$non_swiss - validation$global_wealth) < 1e-5))

# Country-level visualization: 2021 shares of global offshore wealth.
plot_data <- results %>% filter(year == 2021) %>%
  mutate(haven = reorder(haven, share_global_scenario))
p <- ggplot(plot_data, aes(x = haven, y = 100 * share_global_scenario, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  coord_flip() +
  labs(x = NULL, y = "Share of global offshore wealth (%)", fill = NULL,
       title = "Geographic allocation across non-Swiss tax havens, 2021",
       caption = "Counterfactual equal-weight scenarios; Swiss wealth held constant.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

write_csv(results, file.path(output_dir, "sensitivity_distribution.csv"))
write_csv(validation, file.path(output_dir, "sensitivity_distribution_validation.csv"))
saveRDS(results, file.path(output_dir, "sensitivity_distribution.rds"))
ggsave(file.path(figure_dir, "sensitivity_distribution_2021.png"), p, width = 9, height = 7, dpi = 300)
ggsave(file.path(figure_dir, "sensitivity_distribution_2021.pdf"), p, width = 9, height = 7)
message("7b completed: allocation totals validated and outputs saved.")
