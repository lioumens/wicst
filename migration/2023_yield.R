# Library -----------------------------------------------------------------
library(lubridate)
library(glue)
library(purrr)
library(tibble)
library(rlang)

load("data/wip_20241024.Rdata")

source("migration/yield_prep.R")

# corn grain -------------------------------------------------------------

raw_2023_c <- xl_snap$`2023_harvests_corn` |> clean_names()

# Main trial
pre_2023_c <- raw_2023_c |>
  mutate(
    plot = plot,
    subplot = str_c(plot, plot_section),
    section = plot_section,
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "corn"),
    harvest_area = area_ft_2,
    harvest_lbs = harvest_lbs,
    percent_moisture = percent_moisture,
    rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    comments = stitch_notes(notes, ml_notes = NA)
  )

tbl_2023_c <- pre_2023_c |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

tbl_2023_ei_c <- pre_2023_c |> 
  filter(section != "Main") |> 
  select(any_of(harvesting_cols))

dupe_2023_c <- pre_2023_c |> get_yield("corn")
dupe_2023_ei_c <- tbl_2023_ei_c |> get_yield("corn")

supp_2023_c <- pre_2023_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

supp_2023_ei_c <- pre_2023_c |> 
  filter(section != "Main") |> 
  select(any_of(supp_harvesting_cols))

# wheat grain -------------------------------------------------------------

raw_2023_wg <- xl_snap$`2023_harvests_wheat_grain` |> clean_names()

# raw_2023_wg |> names()

# Main trial
pre_2023_wg <- raw_2023_wg |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    section = subplot,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "wheat grain"),
    harvest_area = harvested_area_ft2,
    percent_moisture = percent_moisture,
    harvest_lbs = harvest_weight_lbs,
    # rrl_id = rrl_id,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    .before = 1
    # comments = stitch_notes(notes, ml_notes = NA)
  )

tbl_2023_wg <- pre_2023_wg |> 
  filter(section == "Main") |>
  select(any_of(harvesting_cols))

tbl_2023_ei_wg <- pre_2023_wg |>
  filter(section != "Main") |>
  select(any_of(ei_harvesting_cols))

# dupe_2023_wg <- tbl_2023_wg |> get_yield()
# dupe_2023_ei_wg <- tbl_2023_ei_wg |> get_yield()

supp_2023_wg <- pre_2023_wg |> 
  filter(section == "Main") |>
  select(any_of(supp_harvesting_cols))

supp_2023_ei_wg <- pre_2023_wg |> 
  filter(section != "Main") |>
  select(any_of(supp_harvesting_cols))

# Wheat Straw -------------------------------------------------------------

raw_2023_ws <- xl_snap$`2023_harvests_wheat_straw` |> clean_names()

# Main trial
pre_2023_ws <- raw_2023_ws |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    section = subplot,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "wheat straw"),
    harvest_area = area_ft_2,
    percent_moisture = percent_mst * 100,
    harvest_lbs = plot_wt_lbs,
    # rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    # comments = stitch_notes(notes, ml_notes = NA),
    .before = 1,
  )

tbl_2023_ws <- pre_2023_ws |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

tbl_2023_ei_ws <-  pre_2023_ws |> 
  filter(section != "Main") |> 
  select(any_of(harvesting_cols))

# dupe_2023_ws <- tbl_2023_ws |> get_yield("wheat_straw")
# dupe_2023_ei_ws <- tbl_2023_ei_ws |> 
#   mutate(crop = case_match(crop,
#                     "Wheat"~"wheat straw",
#                     "Barley"~"barley")) |> 
#   get_yield()

supp_2023_ws <- pre_2023_ws |> 
  filter(section == "Main") |>
  select(any_of(supp_harvesting_cols))

# supp_2023_ws

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


# soybean -----------------------------------------------------------------

raw_2023_sb <- xl_snap$`2023_harvests_soybean` |> clean_names()

# raw_2023_sb |> names()

# has notes
pre_2023_sb <- raw_2023_sb |> filter(plot_section == "Main") |> 
  select(harvest_date, plot, crop, harvest_lbs, percent_moisture, area_ft_2) |> 
  rename(
    # harvest_lbs = plot_wt_lbs,
    harvest_area = area_ft_2,
    # percent_moisture = percent_mst
  )

dupe_2023_sb <- pre_2023_sb |> 
  mutate(corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - sb_ideal_percent_moisture)),
         corrected_bu = corrected_lbs / sb_bushel,
         acre_frac = harvest_area / 43560,
         corrected_lbs_per_acre = corrected_lbs / acre_frac,
         corrected_bu_per_acre = corrected_lbs_per_acre / sb_bushel)

supp_2023_sb <- raw_2023_sb |> 
  filter(plot_section == "Main") |> 
  select(harvest_date, plot,
         length_ft, width_ft, 
         notes) |> 
  rename(plot_width = width_ft,
         plot_length = length_ft)


# alfalfa -----------------------------------------------------------------

raw_2023_alf <- xl_snap$`2023_harvests_alfalfa` |> clean_names()

# raw_2023_alf |> names()

pre_2023_alf <- raw_2023_alf |> 
  mutate(harvest_lbs = plot_wt_tons * 2000,
         harvest_area = plot_area_ft_2,
         percent_moisture = moisture_rrl) |> 
  select(harvest_date, plot, crop, harvest_lbs, percent_moisture, harvest_area)


dupe_2023_alf <- pre_2023_alf |> 
  mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
         acre_frac = harvest_area / 43560,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000) 

# dupe_2023_alf |> View()

supp_2023_alf <- raw_2023_alf |> 
  select(harvest_date, plot,
         plot_length_ft, plot_width_ft, 
         rrl_id,
         notes) |> 
  rename(plot_width = plot_width_ft,
         plot_length = plot_length_ft)


# Pasture -----------------------------------------------------------------

raw_2023_past <- xl_snap$`2023_harvests_pasture` |> clean_names()

# raw_2023_past |> names()

pre_2023_past <- raw_2023_past |> 
  mutate(harvest_lbs = plot_wt_lbs,
         harvest_area = plot_area_ft,
         percent_moisture = moisture) |> 
  select(harvest_date, plot, crop, harvest_lbs, percent_moisture, harvest_area)


dupe_2023_past <- pre_2023_past |> 
  mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
         acre_frac = harvest_area / 43560,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000) 

# dupe_2023_past |> View()

supp_2023_past <- raw_2023_past |>
  mutate(plot_width = plot_width_ft,
         plot_length = plot_length_ft,
         bucket_lbs = (plot_bucket_wt_kg - plot_wt_kg) * kg_to_lbs,
         notes = ml_notes) |> 
  select(harvest_date, plot,
         plot_length, plot_width,
         bucket_lbs,
         rrl_id, notes)
  
# Assemble table ----------------------------------------------------------


# harvests
tbl_2023_harvests <- bind_rows(
  pre_2023_c, 
  pre_2023_wg, 
  pre_2023_ws, 
  pre_2023_sb,
  pre_2023_alf,
  pre_2023_past) |>
  rownames_to_column(var = "harvest_id")

# ANPP evaluation



  



