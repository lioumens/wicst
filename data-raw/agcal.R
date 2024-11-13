# agcal

# tables

library(readxl)
library(purrr)
library(DBI)
library(glue)

agcal <- "~/OneDrive - UW-Madison/Database development/agcal_db.xlsx"

plantings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/2 - Input Sheet - Plantings.xlsx"
limings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/3 - Input Sheet - Limings.xlsx"
fertilizers_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/4 - Input Sheet - Fertilizings.xlsx"
# plantings
pre_plantings <- read_xlsx(plantings_fp, sheet = "Sheet1", na = "-", 
                           col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "text"))

tbl_plantings <- pre_plantings |> 
  mutate(comments = if_else(!is.na(`...8`),
                                   glue("Nolan Majerowski: \"{note}\"", 
                                        note = `...8`),
                                   NA)) |> 
  select(-`...8`)

# limings
pre_limings <- read_xlsx(limings_fp, sheet = "Sheet1", na = "-")
tbl_limings <- pre_liming

pre_fertilizings <- read_xlsx(fertilizers_fp, sheet = "Sheet1", na = "-")

tbl_fertilizings <- pre_liming |> mutate(comments = if_else(!is.na(Notes),
                                        glue("Nolan Majerowski: \"{note}\"",
                                             note = Notes),
                                        NA)) |>
  select(-Notes)

# 
# 
# all_sheets <- excel_sheets(agcal)
# xl_agcal <- all_sheets |> 
#   set_names() |> 
#   map(\(x) read_xlsx(agcal, sheet = x, na = "-"))

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/agcal_", date_string, ".Rdata")

save(tbl_plantings, tbl_limings, tbl_fertilizings, file = filepath_date)
