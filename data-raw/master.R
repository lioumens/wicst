# pull data from master file

library(here)
library(readxl)

xl_master <- read_excel(here("data-raw/2024-09-06 WICST_Master_Data - ML.xlsx"))

filepath_date <- "data/master_20240926.Rdata"

save(xl_master, file = filepath_date)