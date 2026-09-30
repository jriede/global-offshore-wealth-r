library(readxl)
library(readr)
library(purrr)

# Pfade...
input_file <- file.path(raw, "FGZ-raw-data.xlsx")
output_dir <- file.path(
  work,
  "07-sensitivity-analysis",
  "csv"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Verfügbare Arbeitsblätter anzeigen
sheets <- excel_sheets(input_file)
print(sheets)

# Für die Sensitivitätsanalyse relevante Arbeitsblätter
required_sheets <- c(
  "T.A2",
  "T.A2b",
  "sharehouseholddep"
)

# Prüfen, ob alle Blätter vorhanden sind
missing_sheets <- setdiff(required_sheets, sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Folgende Arbeitsblätter fehlen: ",
    paste(missing_sheets, collapse = ", ")
  )
}

# Arbeitsblätter einzeln einlesen und exportieren
walk(required_sheets, function(sheet_name) {
  
  df <- read_excel(
    input_file,
    sheet = sheet_name,
    col_names = FALSE,
    col_types = "text"
  )
  
  output_file <- file.path(
    output_dir,
    paste0(sheet_name, ".csv")
  )
  
  write_csv(df, output_file, na = "")
  
  message(
    sheet_name, ": ",
    nrow(df), " rows, ",
    ncol(df), " columns -> ",
    output_file
  )
})
