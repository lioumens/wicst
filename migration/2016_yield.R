
source("migration/yield_prep.R")

# Prairie -----------------------------------------------------------------

raw_2016_prairie <- xl_snap$`2016_biofuels` |> clean_names()

pre_2016_prairie <- raw_2016_prairie |> mutate(
  crop = plot,
  plot = treatment,
  harvest_date = date,
  section = case_match(sub_plot,
                       "main"~"Macro",
                       "micro"~"Micro"),
  fuel_plot = str_c(plot, section),
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 product = "prairie",
                                 cut = 1),
  harvest_lbs = harvest_wt_lbs,
  harvest_date = date,
  wet_weight_w_bag = wet_wt_w_bag,
  dry_weight_w_bag = dry_wt_w_bag,
  wet_weight_no_bag = wet,
  dry_weight_no_bag = dry,
  bag_weight = bag_wt_g,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = area_ft_2,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
)

tbl_2016_prairie <- pre_2016_prairie |> select(any_of(fuel_harvesting_cols))
supp_2016_prairie <- pre_2016_prairie |> select(any_of(supp_fuel_harvesting_cols))


# wheat -------------------------------------------------------------------

raw_2016_wg <- xl_snap$`2016_harvests_wheat` |> clean_names()

pre_2016_wg <- raw_2016_wg |> mutate(
  plot = plot,
  harvest_date = as.POSIXct("2016-07-20", tz = "UTC"), # from agcal
  crop = "wheat", 
  section = "Main",
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 product = "wheat"),
  harvest_lbs = harvest_weight_lbs,
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_lbs_bu,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = harvested_area_ft2,
  comments = stitch_notes(notes, NA)
)

tbl_2016_wg <- pre_2016_wg |> select(any_of(harvesting_cols))
supp_2016_wg <- pre_2016_wg |> select(any_of(supp_harvesting_cols))


# Wheat Straw -------------------------------------------------------------

raw_2016_ws <- xl_snap$`2016_harvests_straw` |> clean_names()

pre_2016_ws <- raw_2016_ws |> mutate(
  plot = plot,
  harvest_date = date,
  crop = "wheat straw", 
  section = "Main",
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 product = "wheat straw",
                                 cut = 2),
  harvest_lbs = total_bale_weight_lbs,
  harvest_length = plot_length,
  harvest_width = plot_width,
  harvest_area = area,
  wet_weight_no_bag = wet_grab_wt,
  dry_weight_no_bag = dry_grab_wt,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
  wagon_weight = wagon_weight_lbs,
  num_bales = number_of_bales
)

tbl_2016_ws <- pre_2016_ws |> select(any_of(harvesting_cols))
supp_2016_ws <- pre_2016_ws |> select(any_of(supp_harvesting_cols))


# Alfalfa -----------------------------------------------------------------

raw_2016_alf <- xl_snap$`2016_harvests_alfalfa` |> clean_names()

pre_2016_alf <- raw_2016_alf |> mutate(
  plot = plot_number,
  harvest_date = date,
  section = "Main",
  # QA: in master, 2016 cut is labelled as oatlage for these 4 plots,
  crop = case_when(plot == 114 & cut == 1 ~ "oatlage",
                   plot == 211 & cut == 1 ~ "oatlage",
                   plot == 312 & cut == 1 ~ "oatlage",
                   plot == 403 & cut == 1 ~ "oatlage",
                   .default = "alfalfa"),
  cut = cut,
  harvest_width = 60,
  harvest_length = 510,
  harvest_area = ml_area,
  harvest_lbs = wet_wt_lbs,
  percent_moisture = 100 - percent_dm,
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 cut = cut,
                                 product = crop),
  comments = stitch_notes(NA, ml_notes)
)

tbl_2016_alf <- pre_2016_alf |> select(any_of(harvesting_cols))
supp_2016_alf <- pre_2016_alf |> select(any_of(supp_harvesting_cols))

# Pasture -----------------------------------------------------------------

raw_2016_past <- xl_snap$`2016_harvests_pasture` |> clean_names()

pre_2016_past <- raw_2016_past |> mutate(
  plot = plot,
  harvest_date = date,
  section = "Main",
  crop = "pasture",
  harvest_width = plot_width_ft,
  harvest_length = plot_length_ft,
  harvest_area = plot_area,
  harvest_lbs = coalesce(ml_plot_wt_kg * kg_to_lbs, ml_plot_wt_lbs),
  cut = row_number(),
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 cut = cut,
                                 product = "pasture"),
  comments = stitch_notes(notes, NA),
  biomass_width = harvest_width,
  biomass_length = harvest_length,
  biomass_area = harvest_area,
  biomass = crop,
  biomass_grams = harvest_lbs / kg_to_lbs * 1000,
  biomass_date = harvest_date,
  method = "exclosure",
  component = "shoots",
  coordinate = NA,
  biomassing_id = get_biomassing_id(year = 2016,
                                    plot = plot,
                                    section = section,
                                    cut = cut,
                                    biomass = biomass,
                                    coordinate = "X"),
  wet_weight_w_bag = grab_wet_wt_with_bag_g,
  dry_weight_w_bag = grab_dry_wt_with_bag_g,
  bag_weight = bag_wt_g,
  wet_weight_no_bag = grab_wet_g,
  dry_weight_no_bag = grab_dry_g,
  percent_moisture = coalesce((wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
                              moisture * 100),
  comments = stitch_notes(notes, NA),
  .by = c(plot)
  )

# harvests
tbl_2016_past <- pre_2016_past |>
  filter(harvest_area >= 10000 | is.na(harvest_area)) |> # include 302 harvest, but unknown amount and area
  select(any_of(harvesting_cols))
supp_2016_past <- pre_2016_past |>
  filter(harvest_area >= 10000 | is.na(harvest_area)) |> # include 302 harvest, but unknown amount and area
  select(any_of(supp_harvesting_cols))

tbl_2016_bio_past <- pre_2016_past |>
  filter(harvest_area < 10000) |>
  select(any_of(biomassing_cols))
supp_2016_bio_past <- pre_2016_past |>
  filter(harvest_area < 10000) |>
  select(any_of(supp_biomassing_cols))


# Corn --------------------------------------------------------------------

raw_2016_c <- xl_snap$`2016_harvests_corn` |> clean_names()

pre_2016_c <- raw_2016_c |> mutate(
  plot = plot,
  harvest_date = harvest_date,
  crop = crop,
  section = "Main",
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 product = "corn"),
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_bu_lb,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area
)

tbl_2016_c <- pre_2016_c |> select(any_of(harvesting_cols))
supp_2016_c <- pre_2016_c |> select(any_of(supp_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2016_sb <- xl_snap$`2016_harvests_soybean` |> clean_names()

pre_2016_sb <- raw_2016_sb |> mutate(
  plot = plot,
  harvest_date = harvest_date,
  crop = crop,
  section = "Main",
  harvesting_id = get_harvest_id(year = 2016,
                                 plot = plot,
                                 section = section,
                                 product = "soybean"),
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_bu_lb,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area
)

tbl_2016_sb <- pre_2016_sb |> select(any_of(harvesting_cols))
supp_2016_sb <- pre_2016_sb |> select(any_of(supp_harvesting_cols))


# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2016_harvests <- bind_rows(
  tbl_2016_wg,
  tbl_2016_ws,
  tbl_2016_alf,
  tbl_2016_past,
  tbl_2016_c,
  tbl_2016_sb
)

supp_2016_harvests <- bind_rows(
  supp_2016_wg,
  supp_2016_ws,
  supp_2016_alf,
  supp_2016_past,
  supp_2016_c,
  supp_2016_sb
)

# biomassings
tbl_2016_bio <- bind_rows(
  tbl_2016_bio_past
)
supp_2016_bio <- bind_rows(
  supp_2016_bio_past
)

# canopeo
tbl_2016_can <- bind_rows()
supp_2016_can <- bind_rows()

# losses
tbl_2016_loss <- bind_rows()
supp_2016_loss <- bind_rows()
tbl_2016_sysloss <- bind_rows()
supp_2016_sysloss <- bind_rows()

## Biofuel -----------------------------------------------------------------

# harvests
tbl_2016_prairie <- bind_rows(
  tbl_2016_prairie
)
supp_2016_prairie <- bind_rows(
  supp_2016_prairie
)

# QA ----------------------------------------------------------------------

# tbl_2016_harvests |> filter(crop == "pasture")
# supp_2016_harvests |> filter(harvesting_id == "H2016_A302MMX_PT_2") |> View()

