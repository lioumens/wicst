# agcal

# tables

library(readxl)
library(purrr)
library(DBI)
library(glue)
library(dplyr)

agcal <- "~/OneDrive - UW-Madison/Database development/agcal_db.xlsx"

plantings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/2 - Input Sheet - Plantings.xlsx"
limings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/3 - Input Sheet - Limings.xlsx"
fertilizers_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/4 - Input Sheet - Fertilizings.xlsx"

# plantings
raw_plantings <- read_xlsx(plantings_fp, sheet = "Sheet1", na = "-", 
                           col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "text"))

pre_plantings <- raw_plantings |> 
  mutate(comments = if_else(!is.na(`...8`),
                                   glue("Nolan Majerowski: \"{note}\"", 
                                        note = `...8`),
                                   NA)) |> 
  select(-`...8`)

# limings
raw_limings <- read_xlsx(limings_fp, sheet = "Sheet1", na = "-")
pre_limings <- raw_limings

raw_fertilizings <- read_xlsx(fertilizers_fp, sheet = "Sheet1", na = "-")
pre_fertilizings <- raw_fertilizings |> mutate(comments = if_else(!is.na(Notes),
                                        glue("Nolan Majerowski: \"{note}\"",
                                             note = Notes),
                                        NA)) |>
  select(-Notes)

# all_sheets <- excel_sheets(agcal)
# xl_agcal <- all_sheets |> 
#   set_names() |> 
#   map(\(x) read_xlsx(agcal, sheet = x, na = "-"))

xl_agcal <- list(
  limings = pre_limings,
  fertilizings = pre_fertilizings,
  plantings = pre_plantings
)

xl_agcal_raw <- list(
  limings = raw_limings,
  fertilizings = raw_fertilizings,
  plantings = raw_plantings
)


date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/agcal_", date_string, ".Rdata")

save(xl_agcal, xl_agcal_raw, file = filepath_date)
