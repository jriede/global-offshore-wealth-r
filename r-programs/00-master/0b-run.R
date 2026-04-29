# ==============================================================================
# REPL: Global Offshore Wealth, 2001-2021
# jlenke, 2026-04-14
#
# This master file runs all programs, create data in work folder and figures.
#
#===============================================================================
# Clear workspace
# rm(list = ls())

# Turn off scientific/logging-like clutter
options(stringsAsFactors = FALSE)
graphics.off()

# Stata's "set memory" is not needed in modern R
# set more off / cap log close have no direct equivalent

################################################################################
# 1. Import and merge data sources
################################################################################

# Assume these path variables are defined somewhere earlier:
# work <- "path/to/work"
# do_path <- "path/to/do" --- we're using do_path instead
# tables <- "path/to/tables"
# fig <- "path/to/fig"

# Reproduce and extend the dataset "data_gravity.dta"
dir.create(work, recursive = TRUE, showWarnings = FALSE)

source(file.path(do_path, "01-gravity-data-build", "1a_import_EWN.R"))
source(file.path(do_path, "01-gravity-data-build", "1b_rebuild_gravity_dataset.R"))

# Save dataset
# Assumes an object like data_gravity_update exists after sourcing
haven::write_dta(data_gravity_update, file.path(work, "data_gravity_update2.dta"))

# Import other data sources
source(file.path(do_path, "01-gravity-data-build", "1c_import_auxiliary_data.R"))

################################################################################
# 2. Construct matrices of bilateral portfolio assets
################################################################################

# Load dataset
data_gravity_update <- haven::read_dta(file.path(work, "data_gravity_update.dta"))

source(file.path(do, "02-bilateral-portfolio-assets-matrices", "2_do_full_matrices.R"))

################################################################################
# 3. Produce output tables
################################################################################

dir.create(tables, recursive = TRUE, showWarnings = FALSE)

source(file.path(do_path, "03-produce-output-tables", "3_do_table_A1.R"))
source(file.path(do_path, "03-produce-output-tables", "3_do_table_A2.R"))
source(file.path(do_path, "03-produce-output-tables", "3_do_table_A3.R"))

################################################################################
# 4. BIS bilateral deposits
################################################################################

# Import bilateral deposits
source(file.path(do_path, "04-bis-deposits-build", "4a-import-bis.R"))

# Construct bilateral deposits non-banks & all counterparty for 2001-2022
source(file.path(do_path, "04-bis-deposits-build", "4b-build-bis-01-22.R"))

# Graph bilateral deposits non-banks
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
source(file.path(do_path, "04-bis-deposits-build", "4c-graph-bis.R"))

################################################################################
# 5. Swiss fiduciary accounts
################################################################################

# Construct fiduciary accounts from SNB data
source(file.path(do_path, "05-swiss-fiduciary-build", "5a-build-fiduciary-87-22.R"))

# Graph fiduciary accounts
source(file.path(do_path, "05-swiss-fiduciary-build", "5b-graph-fiduciary.R"))

################################################################################
# 6. Merge BIS and Swiss data, estimate countries' offshore wealth amounts
################################################################################

# Build bilateral data on offshore wealth
source(file.path(do_path, "06-offshore-wealth-analysis", "6a-build-offshore-01-22.R"))

# Build country offshore wealth data
source(file.path(do_path, "06-offshore-wealth-analysis", "6b-build-countries.R"))

# Graph offshore wealth estimates
source(file.path(do_path, "06-offshore-wealth-analysis", "6c-graph-offshore.R"))
