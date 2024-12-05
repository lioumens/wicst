# cleaning arlington yields

library(readxl)
arl <- "~/OneDrive - UW-Madison/Database development/arl_yields_cleaned.xlsx"

# sheets
# all_sheets <- excel_sheets(arl)

# before 2012 different

# 2013-2019
xl_arl <- str_c(2013:2019) |> 
  set_names() |>
  map(\(x) read_xlsx(arl, sheet = x, na = c("-", ".")) |> 
        pivot_longer(cols = crop1:bushel4,
                     names_pattern = "(.*)([1234])",
                     names_to = c(".value","num")) |> 
        mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
        filter(!is.na(crop))) #TODO: this is excluding some observations

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/arl_", date_string, ".Rdata")

save(xl_arl, file = filepath_date)



#TODO: some crops are not labeled, maybe missing all sheets have a missing alfalfa harvest?
# read_xlsx(arl, sheet = "2011") |>
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |>
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture))

# moisture is flipped when coded as forage
# arl_2013 |> filter(!is.na(crop)) |> 
#   select(-mean, code)
# 
# tbl_2015_alf |> filter(plot %in% c(110, 208, 304, 413)) |> get_yield("alfalfa") |> 
#   arrange(harvest_date, plot) |> 
#   View()
# 
# tbl_2016_bio |> filter(plot %in% c(103, 213, 314, 410))
# tbl_2016_alf |> filter(plot %in% c(103, 213, 314, 410)) |> get_yield("alfalfa")


