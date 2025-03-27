# cleaning arlington yields

library(readxl)
library(tidyverse)
arl <- "~/OneDrive - UW-Madison/Database development/arl_yields_cleaned.xlsx"

# 2013-2019
library(stringr)

xl_arl <- str_c(1990:2019) |> 
  set_names() |>
  map(\(x) read_xlsx(arl, sheet = x, na = c("-", ".", "NA")) |> 
        pivot_longer(cols = crop1:bushel4,
                     names_pattern = "(.*)([1234])",
                     names_to = c(".value","num")) |> 
        mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
        filter(!is.na(crop)))


# s12 <- read_xlsx(arl, sheet = "2012", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |>
#   filter(!is.na(crop))
# 
# s11 <- read_xlsx(arl, sheet = "2011", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop))

# s11 |> filter(is.na(crop)) |> View()
# s10 <- read_xlsx(arl, sheet = "2010", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop), !(crop == "p" & yield == 0)) # slightly different to remove all zeros of extra pastures
# 
# 
# tmp10 <- read_xlsx(arl, sheet = "2010", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop))

# should go in 2010 yield
# s10_bio <- s10 |> filter(crop == "p" & is.na(moisture)) # hay exclosures have na moisture (just a proxy right now)
# s10_harvest <- s10 |> filter(crop != "p" | !is.na(moisture))
# 
# s09 <- read_xlsx(arl, sheet = "2009", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop)) # extra corn plot not sure what to do
# 
# s08 <- read_xlsx(arl, sheet = "2008", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop)) # extra switch grass columns

# s07 <- read_xlsx(arl, sheet = "2007", na = c(".", "-", "NA")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop), !(crop == "p" & yield == 0))

# # append in order
# xl_arl <- append(xl_arl,
#                  # list(
#                  #   `2007` = s07,
#                  #   `2008` = s08,
#                  #   `2009` = s09),
#                  after = 17)


date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/arl_", date_string, ".Rdata")

save(xl_arl, file = filepath_date)

# analyze 2012 and prior, need to extract the yield multipliers
# lbs1	moisture1	adjwt1	bushel1
# 
# read.delim("clipboard")
# read.table(pipe("pbpaste")) |> pull() |> str_match("([0-9.]+)\\*")
# read.table(pipe("pbpaste")) |> pull() |> str_extract("([0-9.]+)\\*", group = 1) |>
#   as.numeric() |> clipr::write_clip()

# 2012 --------------------------------------------------------------------

#TODO: some crops are not labeled, maybe missing all sheets have a missing alfalfa harvest?

# 2018 missing?
# read_xlsx(arl, sheet = "2018", na = c("-", ".")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(is.na(crop) & !is.na(lbs))
# 
# # 2014 missing alfalfa
# read_xlsx(arl, sheet = "2014", na = c("-", ".")) |> 
#   pivot_longer(cols = crop1:bushel4,
#                names_pattern = "(.*)([1234])",
#                names_to = c(".value","num")) |> 
#   mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
#   filter(!is.na(crop))
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


