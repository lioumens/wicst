# agcal

# tables

library(readxl)
library(purrr)
library(DBI)

agcal <- "~/OneDrive - UW-Madison/Database development/agcal_db.xlsx"

all_sheets <- excel_sheets(agcal)
xl_agcal <- all_sheets |> 
  set_names() |> 
  map(\(x) read_xlsx(agcal, sheet = x, na = "-"))


date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/agcal_", date_string, ".Rdata")

save(xl_agcal, file = filepath_date)

xl_agcal |> names()
pre_2023
