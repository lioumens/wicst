# tables

library(readxl)
library(purrr)
library(DBI)
library(tidyr)
library(dplyr)
library(stringr)

core <- "~/OneDrive - UW-Madison/Database development/core_db.xlsx"

all_sheets <- excel_sheets(core)
xl_core <- all_sheets |> 
  set_names() |> 
  map(\(x) read_xlsx(core, sheet = x))

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/core_", date_string, ".Rdata")

save(xl_core, file = filepath_date)


