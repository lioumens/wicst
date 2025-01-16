# cleaning arlington yields

library(readxl)
library(tidyverse)
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


s12 <- read_xlsx(arl, sheet = "2012", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |>
  filter(!is.na(crop))

s11 <- read_xlsx(arl, sheet = "2011", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

# s11 |> filter(is.na(crop)) |> View()
s10 <- read_xlsx(arl, sheet = "2010", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop), !(crop == "p" & yield == 0)) # slightly different to remove all zeros of extra pastures

# should go in 2010 yield
# s10_bio <- s10 |> filter(crop == "p" & is.na(moisture)) # hay exclosures have na moisture (just a proxy right now)
# s10_harvest <- s10 |> filter(crop != "p" | !is.na(moisture))

s09 <- read_xlsx(arl, sheet = "2009", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop)) # extra corn plot not sure what to do

s08 <- read_xlsx(arl, sheet = "2008", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop)) # extra switch grass columns

s07 <- read_xlsx(arl, sheet = "2007", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop), !(crop == "p" & yield == 0))

s06 <- read_xlsx(arl, sheet = "2006", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s05 <- read_xlsx(arl, sheet = "2005", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop)) # pasture 0s kept

s04 <- read_xlsx(arl, sheet = "2004", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s03 <- read_xlsx(arl, sheet = "2003", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s02 <- read_xlsx(arl, sheet = "2002", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s01 <- read_xlsx(arl, sheet = "2001", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s00 <- read_xlsx(arl, sheet = "2000", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s99 <- read_xlsx(arl, sheet = "1999", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s98 <- read_xlsx(arl, sheet = "1998", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s97 <- read_xlsx(arl, sheet = "1997", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s96 <- read_xlsx(arl, sheet = "1996", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s95 <- read_xlsx(arl, sheet = "1995", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s94 <- read_xlsx(arl, sheet = "1994", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s93 <- read_xlsx(arl, sheet = "1993", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s92 <- read_xlsx(arl, sheet = "1992", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

s91 <- read_xlsx(arl, sheet = "1991", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))


s90 <- read_xlsx(arl, sheet = "1990", na = c(".", "-", "NA")) |> 
  pivot_longer(cols = crop1:bushel4,
               names_pattern = "(.*)([1234])",
               names_to = c(".value","num")) |> 
  mutate(moisture = if_else(code > 5, 100 - moisture, moisture)) |> 
  filter(!is.na(crop))

xl_arl <- c(
  `1990` = list(s90),
  `1991` = list(s91),
  `1992` = list(s92),
  `1993` = list(s93),
  `1994` = list(s94),
  `1995` = list(s95),
  `1996` = list(s96),
  `1997` = list(s97),
  `1998` = list(s98),
  `1999` = list(s99),
  `2000` = list(s00),
  `2001` = list(s01),
  `2002` = list(s02),
  `2003` = list(s03),
  `2004` = list(s04),
  `2005` = list(s05),
  `2006` = list(s06),
  `2007` = list(s07),
  `2008` = list(s08),
  `2009` = list(s09),
  `2010` = list(s10),
  `2011` = list(s11),
  `2012` = list(s12),
  xl_arl
)

# xl_arl$`2012` <- NULL
# x <- list(a = 1, b = 2)
# c(x, `2012` = list(s12))
# append(x, list(c = 3), after = 0)


date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/arl_", date_string, ".Rdata")

save(xl_arl, file = filepath_date)

# analyze 2012 and prior
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


