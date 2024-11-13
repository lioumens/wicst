

source("migration/yield_prep.R")


# rrl ids -----------------------------------------------------------------

pre_2020_rrl <- xl_snap$`2020_rrl` |> clean_names() |>
  filter(section == "main") |> 
  select(plot, rrl_id_number, section)

# corn --------------------------------------------------------------------

raw_2020_c <- xl_snap$`2020_harvests_corn` |> clean_names()
# raw_2020_c |> names()

pre_2020_c <- raw_2020_c |> 
  left_join(pre_2020_rrl, by = join_by("plot" == "plot",
                                       "plot_section" == "section")) |>
  mutate(
  plot = plot,
  section = plot_section,
  subplot = str_c(plot, section),
  crop = crop,
  harvesting_id = get_harvest_id(year = 2020,
                                 plot = plot,
                                 section = section,
                                 product = "corn",
                                 cut = 1),
  rrl_id = rrl_id_number,
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area
)

tbl_2020_c <- pre_2020_c |> 
  filter(section == "main") |> 
  select(any_of(harvesting_cols))

supp_2020_c <- pre_2020_c |> 
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2020_ei_c <- pre_2020_c |> 
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))

supp_2020_ei_c <- pre_2020_c |> 
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))

# soybean -----------------------------------------------------------------

raw_2020_sb <- xl_snap$`2020_harvests_soybean` |> clean_names()

pre_2020_sb <- raw_2020_sb |> 
  left_join(pre_2020_rrl, by = join_by("plot" == "plot",
                                       "plot_section" == "section")) |>
  mutate(
  plot = plot,
  section = plot_section,
  subplot = str_c(plot, section),
  crop = crop,
  harvesting_id = get_harvest_id(year = 2020,
                                 plot = plot,
                                 section = section,
                                 product = "soybean",
                                 cut = 1),
  harvest_date = harvest_date,
  rrl_id = rrl_id_number,
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area_ft,
  comments = stitch_notes(NA, ml_notes)
)


tbl_2020_sb <- pre_2020_sb |> 
  filter(section == "main") |> 
  select(any_of(harvesting_cols))

supp_2020_sb <- pre_2020_sb |> 
  filter(section == "main") |>
  select(any_of(supp_harvesting_cols))

tbl_2020_ei_sb <- pre_2020_sb |> 
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))

supp_2020_ei_sb <- pre_2020_sb |> 
  filter(section != "main") |>
  select(any_of(supp_ei_harvesting_cols))

# wheat grain -------------------------------------------------------------

raw_2020_wg <- xl_snap$`2020_harvests_wheat_grain` |> clean_names()

pre_2020_wg <- raw_2020_wg |> 
  left_join(pre_2020_rrl, by = join_by("plot" == "plot",
                                       "subplot" == "section")) |>
  mutate(
    plot = plot,
    section = subplot,
    subplot = str_c(plot, section),
    crop = crop,
    harvest_date = harvest_date,
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = plot,
                                   section = section,
                                   product = "wheat grain",
                                   cut = 1),
    rrl_id = rrl_id_number,
    harvest_lbs = harvest_weight_lbs,
    percent_moisture = percent_moisture,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = harvested_area_ft2
    # comments = stitch_notes(NA, ml_notes)
  )

# wg
tbl_2020_wg <- pre_2020_wg |>
  filter(section == "main") |>
  select(any_of(harvesting_cols))
supp_2020_wg <- pre_2020_wg |>
  filter(section == "main") |>
  select(any_of(supp_harvesting_cols))
# ei
tbl_2020_ei_wg <- pre_2020_wg |> 
  filter(section != "main") |>
  select(any_of(ei_harvesting_cols))
supp_2020_ei_wg <- pre_2020_wg |>
  filter(section != "main") |>
  select(any_of(supp_ei_harvesting_cols))


# wheat straw -------------------------------------------------------------

raw_2020_ws <- xl_snap$`2020_harvests_wheat_straw` |> clean_names()

pre_2020_ws <- raw_2020_ws |> 
  mutate(
    plot = plot,
    section = "Main",
    subplot = str_c(plot, section),
    crop = "wheat straw",
    harvest_date = date,
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = plot,
                                   section = section,
                                   product = "wheat straw",
                                   cut = 1),
    harvest_lbs = lbs,
    harvest_length = length_f,
    harvest_width = width_f,
    harvest_area = area_sf,
    wet_weight_w_bag = wet_grab_g_8,
    wet_weight_no_bag = wet_grab_g_10,
    dry_weight_w_bag = dry_grab_g_9,
    dry_weight_no_bag = dry_grab_g_11,
    wet_bag_weight = wet_weight_w_bag - wet_weight_no_bag,
    dry_bag_weight = dry_weight_w_bag - dry_weight_no_bag,
    percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100
    # comments = stitch_notes(NA, ml_notes)
  )

# ws
tbl_2020_ws <- pre_2020_ws |>
  select(any_of(harvesting_cols))

supp_2020_ws <- pre_2020_ws |>
  select(any_of(supp_harvesting_cols))

# alfalfa -----------------------------------------------------------------

raw_2020_alf <- xl_snap$`2020_harvests_alfalfa` |> clean_names()

pre_2020_alf <- raw_2020_alf |> 
  mutate(
    plot = plot,
    section = "Main",
    subplot = str_c(plot, section),
    crop = crop,
    harvest_date = harvest_date,
    cut = cut,
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = plot,
                                   section = section,
                                   product = "alfalfa",
                                   cut = cut),
    harvest_lbs = plot_wt_lbs,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area,
    percent_moisture = ml_percent_moisture,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2020_alf <- pre_2020_alf |>
  select(any_of(harvesting_cols))

supp_2020_alf <- pre_2020_alf |> 
  select(any_of(supp_harvesting_cols))

# pasture -----------------------------------------------------------------

raw_2020_past <- xl_snap$`2020_harvests_pasture` |> clean_names()

pre_2020_past <- raw_2020_past |> 
  mutate(
    plot = plot,
    section = "Main",
    subplot = str_c(plot, section),
    crop = "pasture",
    harvest_date = harvest_date,
    cut = cut,
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = plot,
                                   section = section,
                                   product = "pasture",
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2020,
                                              plot = plot,
                                              section = section,
                                              cut = cut,
                                              product = "pasture"),
    loss_width = 11,
    loss_length = 18,
    loss_area = loss_width * loss_length,
    loss_reason = "shed",
    harvest_lbs = plot_wt_lbs,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = harvest_length * harvest_width,
    percent_moisture = moisture_rrl * 100,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2020_past <- pre_2020_past |>
  select(any_of(harvesting_cols))

supp_2020_past <- pre_2020_past |> 
  select(any_of(supp_harvesting_cols))

# shed as direct loss
tbl_2020_loss_past <- pre_2020_past |>
  select(any_of(loss_cols))

supp_2020_loss_past <- pre_2020_past |>
  select(any_of(supp_loss_cols))



# corn silage -------------------------------------------------------------

raw_2020_cs <- xl_snap$`2020_harvests_corn_silage` |> clean_names()

pre_2020_cs <- raw_2020_cs |>
  mutate(
    plot = ml_plot,
    section = case_match(ml_section,
                         "West"~"West 15'",
                         "East"~"East 15'"),
    crop = "corn silage",
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = plot,
                                   section = section,
                                   product = crop),
    harvest_date = harvest_date,
    harvest_length = ml_plot_length, # modified 415 -> 510
    harvest_width = plot_width_ft,
    harvest_area = harvest_length * harvest_width,
    harvest_lbs = plot_wt_lbs,
    percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2020_cs <-  pre_2020_cs |> 
  select(any_of(harvesting_cols))

supp_2020_cs <-  pre_2020_cs |> 
  select(any_of(supp_harvesting_cols))

dupe_2020_cs <- tbl_2020_cs |> get_yield("corn silage")
# # dupe for 415/510 difference
# dupe_2020_cs <- pre_2020_cs |>
#   mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
#          acre_frac = harvest_area / acre_to_ft2,
#          lbs_dm_per_acre = lbs_dm / acre_frac,
#          tons_dm_per_acre = lbs_dm_per_acre / 2000,
#          my_acre_frac = harvest_area / 415 * 510 / acre_to_ft2,
#          my_lbs_dm_per_acre = lbs_dm / my_acre_frac,
#          my_tons_dm_per_acre = my_lbs_dm_per_acre / 2000)
# dupe_2020_cs |> View()


# corn silage ei ----------------------------------------------------------

raw_2020_ei_cs <- xl_snap$`2020_harvests_corn_silage_ei` |>
  


# Arrange Tables ----------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2020_harvests <- bind_rows(
  tbl_2020_c,
  tbl_2020_sb,
  tbl_2020_wg,
  tbl_2020_ws,
  tbl_2020_alf,
  tbl_2020_past
)
supp_2020_harvests <- bind_rows(
  supp_2020_c,
  supp_2020_sb,
  supp_2020_wg,
  supp_2020_ws,
  supp_2020_alf,
  supp_2020_past
)

# biomassings
tbl_2020_bio <- bind_rows()
supp_2020_bio <- bind_rows()

# canopeo
tbl_2020_can <- bind_rows()
supp_2020_can <- bind_rows()

# losses
tbl_2020_loss <- bind_rows(
  tbl_2020_loss_past
)
supp_2020_loss <- bind_rows(
  supp_2020_loss_past
)
tbl_2020_sysloss <- bind_rows()
supp_2020_sysloss <- bind_rows()

## EI ----------------------------------------------------------------------

# harvests
tbl_2020_ei_harvests <- bind_rows(
  tbl_2020_ei_c,
  tbl_2020_ei_sb,
  tbl_2020_ei_wg
)
supp_2020_ei_harvests <- bind_rows(
  supp_2020_ei_c,
  supp_2020_ei_sb,
  supp_2020_ei_wg
)

# biomassings
tbl_2020_ei_bio <- bind_rows()
supp_2020_ei_bio <- bind_rows()

# canopeo
tbl_2020_ei_can <- bind_rows()
supp_2020_ei_can <- bind_rows()

# losses
tbl_2020_ei_loss <- bind_rows()
supp_2020_ei_loss <- bind_rows()
tbl_2020_ei_sysloss <- bind_rows()
supp_2020_ei_sysloss <- bind_rows()

## Silage ------------------------------------------------------------------

# harvests
tbl_2020_silage <- bind_rows(tbl_2020_cs)
supp_2020_silage <- bind_rows(supp_2020_cs)

## Biofuel -----------------------------------------------------------------

# harvests
tbl_2020_prairie <- bind_rows()
supp_2020_prairie <- bind_rows()




