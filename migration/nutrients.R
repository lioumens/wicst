# Nutrient analysis
library(tidyverse)
library(readxl)

# 2015 --------------------------------------------------------------------
rrl_fp <- "~/Library/CloudStorage/OneDrive-UW-Madison/Database development/rrl_cleaned.xlsx"
raw_2015_samples <- read_xlsx(rrl_fp, sheet = "2015_samples")
raw_2015_analysis <- read_xlsx(rrl_fp, sheet = "2015_analysis")


pre_2015_nutrients <- raw_2015_samples |>
  full_join(raw_2015_analysis, by = c("rrl_id", "plot", "harvest_date")) |> 
  mutate(material = coalesce(material.y, material.x)) |> 
  select(-material.x, -material.y)
# 4 extra wheat grains are excluded by the left_join (in raw analysis but not in sample list)

tbl_2015_nutrients <- pre_2015_nutrients |> 
  filter(!(rrl_id %in% c(325, 326, 327, 328)))


tbl_2015_nutrients |> filter(!is.na(total_moisture)) |> clipr::write_clip()

tbl_2015_nutrients |> filter(plot %in% (tbl_2015_alf |> pull(plot) |> unique())) |>
  count(plot)

xl_nutrients <- list(
  `2015` = tbl_2015_nutrients
)

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/nutrients_", date_string, ".Rdata")

save(xl_nutrients, file = filepath_date)
