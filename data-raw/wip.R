library(readxl)
library(purrr)

wip <- "~/OneDrive - UW-Madison/Database development/wip_db.xlsx"

# sheets
all_sheets <- excel_sheets(wip)

xl_snap <- all_sheets |> 
  set_names() |>
  map(\(x) read_xlsx(wip, sheet = x, na = c("-", ".")))

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/wip_", date_string, ".Rdata")

save(xl_snap, file = filepath_date)

# load("snaps/wip_20241004.Rdata")
