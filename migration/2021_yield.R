
source("migration/yield_prep.R")

# corn --------------------------------------------------------------------

raw_2021_c <- xl_snap$`2021_harvests_corn` |> clean_names()

pre_2021_c <- raw_2021_c |>
  filter(plot_section %in% c("Main", "main"),
         !is.na(harvest_lbs)) |>
  mutate(
  harvesting_id = get_harvest_id(year = 2021,
                                    plot = plot,
                                    section = str_to_title(plot_section),
                                    product = "corn"),
  plot = plot,
  harvest_area = area,
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  loss_area = ml_harvest_loss_m2 * m2_to_ft2,
  section_description = case_when(
    !is.na(ml_harvest_loss_m2) ~ "weed loss",
    .default = NA
  ),
  harvest_length = length_ft,
  harvest_width = width_ft,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2021_c_loss <- pre_2021_c |> filter(!is.na(loss_area)) |> 
  select(any_of(loss_harvesting_cols))

tbl_2021_c <- pre_2021_c |> select(any_of(harvesting_cols))

supp_2021_c <- pre_2021_c |> select(any_of(supp_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2021_sb <- xl_snap$`2021_harvests_soybean` |> clean_names()

# raw_2021_sb |> names()

pre_2021_sb <- raw_2021_sb |>
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = plot_section,
                                   product = "soybean"),
    harvest_date = harvest_date,
    plot = plot,
    crop = crop,
    harvest_area = area_ft_2,
    harvest_length = length_ft,
    harvest_width = width_ft,
    rrl_id = rrl_id,
    percent_moisture = percent_moisture,
    loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2021_sb <- pre_2021_sb |> select(any_of(harvesting_cols))

tbl_2021_sb_loss <- pre_2021_sb |>
  filter(!is.na(loss_area)) |> 
  select(any_of(loss_harvesting_cols)) |> 
  add_column(section_description = "grain loss by weeds")

supp_2021_sb <- pre_2021_sb |> 
  select(any_of(supp_harvesting_cols))


# wheat_grain -------------------------------------------------------------

raw_2021_wg <- xl_snap$`2021_harvests_wheat_grain` |> clean_names()

raw_2021_wg |> names()

pre_2021_wg <- raw_2021_wg |>
  filter(section == "main") |> 
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = crop),
    harvest_date = ml_date,
    plot = plot,
    crop = crop,
    harvest_area = area_ft2,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_weight_lbs,
    rrl_id = rrl_id_number,
    percent_moisture = moisture_percent,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = ml_notes
  )

tbl_2021_wg <- pre_2021_wg |> 
  select(any_of(harvesting_cols))

supp_2021_wg <- pre_2021_wg |> 
  select(any_of(supp_harvesting_cols))


# alfalfa -----------------------------------------------------------------


raw_2021_alf <- xl_snap$`2021_harvests_alfalfa` |> clean_names()


pre_2021_alf <- raw_2021_alf |>
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "alfalfa"),
    harvest_date = harvest_date,
    plot = plot,
    crop = crop,
    harvest_area = plot_area,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_wt_lbs,
    rrl_id = rrl_id,
    percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2021_alf <- pre_2021_alf |> 
  select(any_of(harvesting_cols))

supp_2021_alf <- pre_2021_alf |> 
  select(any_of(supp_harvesting_cols))

# pasture -----------------------------------------------------------------

raw_2021_alf <- xl_snap$`2021_harvests_alfalfa` |>
  clean_names()


pre_2021_alf <- raw_2021_alf |>
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "alfalfa"),
    harvest_date = harvest_date,
    plot = plot,
    crop = crop,
    harvest_area = plot_area,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_wt_lbs,
    rrl_id = rrl_id,
    percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2021_alf <- pre_2021_alf |> 
  select(any_of(harvesting_cols))

supp_2021_alf <- pre_2021_alf |> 
  select(any_of(supp_harvesting_cols))


# pasture -----------------------------------------------------------------

# anpp

# 1 row for harvesting
raw_2021_past <- xl_snap$`2021_harvests_pasture` |>
  clean_names()
raw_2021_past |> names()

pre_2021_past <- raw_2021_past |>
  filter(notes == "square bale") |>  # one row removed from plot
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "pasture",
                                   cut = cut),
    harvest_date = harvest_date,
    plot = plot,
    crop = crop,
    harvest_area = plot_area_ft,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_wt_lbs,
    rrl_id = rrl_id,
    percent_moisture = moisture * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )


# raw_2021_past |> names()
pre_2021_anpp_past <- raw_2021_past |>
  filter(notes != "square bale") |>  # one row removed, not considered biomassing
  mutate(
    biomassing_id = get_biomassing_id(year = 2021,
                                      plot = plot,
                                      section = "main",
                                      coordinate = "2", # from yellow binder
                                      biomass = "pasture",
                                      cut = cut),
    biomass_date = harvest_date,
    plot = plot,
    biomass = crop,
    biomass_area = plot_area_ft,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_grams = plot_wt_lbs / kg_to_lbs * 1000,
    rrl_id = rrl_id,
    percent_moisture = moisture * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )


tbl_2021_past <- pre_2021_past |> 
  select(any_of(harvesting_cols))

supp_2021_past <- pre_2021_past |> 
  select(any_of(supp_harvesting_cols))

tbl_2021_anpp_past <- pre_2021_anpp_past |>
  select(any_of(biomassing_cols))

supp_2021_anpp_past <- pre_2021_anpp_past |>
  select(any_of(supp_biomassing_cols))


# corn silage -------------------------------------------------------------

raw_2021_cs <- xl_snap$`2021_harvests_corn_silage_ei` |> clean_names()

# raw_2021_cs |> names()
pre_2021_cs <- raw_2021_cs |>
  filter(plot_2 == "Main") |>
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "corn silage"),
    harvest_date = harvest_date,
    plot = plot,
    crop = "corn silage",
    harvest_area = plot_area_ft,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_lbs = harvest_lbs,
    rrl_id = rrl_id,
    percent_moisture = ml_moisture * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2021_cs <-  pre_2021_cs |> 
  select(any_of(harvesting_cols))

# dupe
dupe_2021_cs <- pre_2021_cs |>
  mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
         acre_frac = harvest_area / acre_to_ft2,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000,
         my_acre_frac = harvest_area / 510 * 415 / acre_to_ft2,
         my_lbs_dm_per_acre = lbs_dm / my_acre_frac,
         my_tons_dm_per_acre = my_lbs_dm_per_acre / 2000)
dupe_2021_cs |> View()

supp_2021_cs <- pre_2021_cs |> 
  select(any_of(supp_harvesting_cols))

# wheat straw -------------------------------------------------------------

raw_2021_ws <- xl_snap$`2021_harvests_wheat_straw` |> clean_names()

raw_2021_ws |> names()
pre_2021_ws <- raw_2021_ws |>
  filter(sub_plot == "main") |>
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "wheat straw"),
    harvest_date = date,
    plot = plot,
    crop = "wheat straw",
    harvest_area = area_sf,
    harvest_length = length_f,
    harvest_width = width_f,
    harvest_lbs = lbs,
    rrl_id = rrl_id,
    wet_bag_weight = ml_wet_bag,
    dry_bag_weight = ml_dry_bag,
    percent_moisture = (ml_wet_grab_no_bag_g - (ml_dry_grab_bag_g - ml_wet_bag)) / ml_wet_grab_no_bag_g * 100, # recalculate with wet bag weight, not the dried bag
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(NA, ml_notes)
  )

tbl_2021_ws <-  pre_2021_ws |> 
  select(any_of(harvesting_cols))

supp_2021_ws <- pre_2021_ws |> 
  select(any_of(supp_harvesting_cols))


# anpp alfalfa ------------------------------------------------------------













