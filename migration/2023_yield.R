# Library -----------------------------------------------------------------

library(readxl)
library(janitor)
library(lubridate)
library(glue)
library(purrr)

load("data/wip_20241004.Rdata")

# constants

c_ideal_percent_moisture = 15.5
c_bushel = 56
wg_ideal_percent_moisture = 13.5
wg_bushel = 60

# corn grain -------------------------------------------------------------

raw_2023_harvests_corn <- xl_snap$`2023_harvests_corn` |> clean_names()

# Main trial
pre_2023_harvests_corn <- raw_2023_harvests_corn |> 
  filter(plot_section == "Main") |> 
  select(harvest_date, plot, crop, harvest_lbs, percent_moisture, area_ft_2) |> 
  rename(harvest_area = area_ft_2)

dupe_2023_harvests_corn <- 
  pre_2023_harvests_corn |> 
  mutate(corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - c_ideal_percent_moisture)),
         corrected_bu = corrected_lbs / 56,
         acre_frac = harvest_area / 43560,
         corrected_lbs_per_acre = corrected_lbs / acre_frac,
         corrected_bu_per_acre = corrected_lbs_per_acre / c_bushel)

supp_2023_harvests_corn <- 
  raw_2023_harvests_corn |> 
  filter(plot_section == "Main") |> 
  select(harvest_date, plot, crop, length_ft, width_ft, notes) |> 
  rename(
    plot_length = "length_ft",
    plot_width = "width_ft"
  ) |> 
  mutate(notes = glue("Gregg Sanford: \"{notes[1]}\""))

# supp_2023_harvests_corn # join on harvest_date & plot


# wheat grain -------------------------------------------------------------

raw_2023_wg <- xl_snap$`2023_harvests_wheat_grain` |> clean_names()

# raw_2023_wg |> names()

pre_2023_wg <- raw_2023_wg |> 
  filter(subplot == "Main") |> 
  select(harvest_date, plot, crop, harvest_weight_lbs, percent_moisture, harvested_area_ft2) |> 
  rename(
    harvest_lbs = harvest_weight_lbs,
    harvest_area = harvested_area_ft2,
  )

dupe_2023_wg <- 
  pre_2023_wg |> 
  mutate(corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - wg_ideal_percent_moisture)),
         corrected_bu = corrected_lbs / 56,
         acre_frac = harvest_area / 43560,
         corrected_lbs_per_acre = corrected_lbs / acre_frac,
         corrected_bu_per_acre = corrected_lbs_per_acre / wg_bushel)

supp_2023_wg <- raw_2023_wg |> 
  filter(subplot == "Main") |> 
  select(harvest_date, plot, crop, plot_length_ft, plot_width_ft) |> 
  rename(
    plot_length = "plot_width_ft",
    plot_width = "plot_length_ft"
  )


# Wheat Straw -------------------------------------------------------------

raw_2023_ws <- xl_snap$`2023_harvests_wheat_straw` |> clean_names()

raw_2023_ws |> names()

pre_2023_ws <- raw_2023_ws |> 
  filter(subplot == "Main") |> 
  select(harvest_date, plot, crop, plot_wt_lbs, percent_mst, area_ft_2) |> 
  rename(
    harvest_lbs = plot_wt_lbs,
    harvest_area = area_ft_2,
    percent_moisture = percent_mst
  )

dupe_2023_ws <- 
  pre_2023_ws |> 
  mutate(lbs_dm = harvest_lbs * (1 - percent_moisture),
         acre_frac = harvest_area / 43560,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000)

dupe_2023_ws |> pull(tons_dm_per_acre)

supp_2023_ws <- raw_2023_ws |> 
  filter(subplot == "Main") |> 
  select(harvest_date, plot, length_ft, width_ft, 
         bag_weight_g, # bag
         bag_wet_weight_g, # wet sample
         dry_wt_g_no_bag # dry sample
         ) |> 
  rename(plot_width = width_ft,
         plot_length = length_ft,
         bag_weight = bag_weight_g)


# Assemble table ----------------------------------------------------------

tbl_harvests <- bind_rows(pre_2023_harvests_corn, pre_2023_wg, pre_2023_ws)
  



