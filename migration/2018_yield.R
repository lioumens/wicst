# 2018 yield

if(!exists("xl_snap")) source("migration/yield_prep.R")

# Corn --------------------------------------------------------------------

raw_2018_c <- xl_snap$`2018_harvests_corn` |> clean_names()

pre_2018_c <- raw_2018_c |> mutate(
  plot = plot,
  harvest_date = harvest_date,
  section = "Main",
  crop = crop,
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = "corn"),
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2018_c <- pre_2018_c |> select(any_of(harvesting_cols))
supp_2018_c <- pre_2018_c |> select(any_of(supp_harvesting_cols))


# Soybeans ----------------------------------------------------------------

raw_2018_sb <- xl_snap$`2018_harvests_soybean` |> clean_names()

pre_2018_sb <- raw_2018_sb |> mutate(
  plot = plot,
  harvest_date = harvest_date,
  section = "Main",
  harvest_lbs = harvest_lbs,
  percent_moisture = percent_moisture,
  harvesting_id = get_harvest_id(year = 2018,
                                    plot = plot,
                                    section = section,
                                    product = "soybean"),
  harvest_width = width_ft,
  harvest_length = length_ft,
  harvest_area = area,
)

tbl_2018_sb <- pre_2018_sb |> 
  filter(experiment == "WICST") |> 
  select(any_of(harvesting_cols))

supp_2018_sb <- pre_2018_sb |> 
  filter(experiment == "WICST") |> 
  select(any_of(supp_harvesting_cols))


tbl_2018_115_sb <- pre_2018_sb |> 
  filter(experiment == "115") |> 
  select(any_of(fuel_115_harvesting_cols))

supp_2018_115_sb <- pre_2018_sb |> 
  filter(experiment == "WICST") |> 
  select(any_of(supp_fuel_115_harvesting_cols))

# Wheat Grain -------------------------------------------------------------

raw_2018_wg <- xl_snap$`2018_harvests_wheat` |> clean_names()

pre_2018_wg <- raw_2018_wg |>
  # single row from alfalfa sheet that is wheatlage to remove, and process separately
  mutate(
  plot = plot,
  harvest_date = ml_date,
  section = "Main",
  harvest_lbs = harvest_weight_lbs,
  percent_moisture = percent_moisture,
  crop = if_else(notes == "WHEATLAGE", "wheatlage", NA, "wheat grain"),
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = crop),
  harvestingloss_id = get_harvestingloss_id(year = 2018,
                                            plot = plot,
                                            section = section,
                                            product = crop),
  harvest_width = plot_width_ft,
  harvest_length = plot_length_ft,
  harvest_area = whole_plot,
  loss_area = gaps_to_exclude,
  loss_reason = if_else(notes == "WHEATLAGE", "weeds", NA, NA),
  bushel_lbs = as.numeric(na_if(test_wt_lbs_bu, "N/A")),
  num_bales = ml_num_bales,
  # comments = stitch_notes(notes, NA)
)

# take out the wheatlage one
tbl_2018_wg <- pre_2018_wg |> filter(is.na(notes)) |> select(any_of(harvesting_cols))
supp_2018_wg <- pre_2018_wg |> filter(is.na(notes)) |> select(any_of(supp_harvesting_cols))

tbl_2018_loss_wg <- pre_2018_wg |> filter(is.na(notes)) |> select(any_of(loss_cols))
# pre_2018_wg |> filter(is.na(notes)) |> select(any_of(supp_loss_cols)) # empty

# Wheat straw -------------------------------------------------------------

raw_2018_ws <- xl_snap$`2018_harvests_wheat_straw` |> clean_names()

pre_2018_ws <- raw_2018_ws |> mutate(
  plot = plot,
  harvest_date = date,
  section = "Main",
  harvest_lbs = total_bale_weight_lbs + total_weight_adjust, # same as final plot weight
  percent_moisture = moisture * 100,
  crop = "wheat straw",
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = "wheat straw",
                                 cut = 2),
  harvestingloss_id = get_harvestingloss_id(year = 2018,
                                            plot = plot,
                                            section = section,
                                            product = "wheat straw",
                                            cut = 2),
  harvest_width = 510,
  harvest_length = 60,
  harvest_area = start_area, # harvest area
  loss_area = gaps, # no reason given
  wet_weight_no_bag = wet_grab_wt,
  dry_weight_no_bag = dry_grab_wt,
  # bushel_lbs = na_if(test_wt_lbs_bu, "N/A"),
  num_bales = number_of_bales_in_wagon + bale_adjustment_from_plot_left_in_baler,
  wagon_weight = wagon_weight_lbs,
  comments = glue("Michael Liou: Final harvest weight calculated from trailer ({trailer_bales}) + plot ({plot_bales}) bales. We only had weights from trailer, so plot bales assumed average of trailer bale weight ({bale_avg}).",
                  trailer_bales = number_of_bales_in_wagon,
                  plot_bales = bale_adjustment_from_plot_left_in_baler,
                  bale_avg = round(average_bale_weight_lbs, 2))
)

tbl_2018_ws <- pre_2018_ws |> 
  select(any_of(harvesting_cols))
supp_2018_ws <- pre_2018_ws |> 
  select(any_of(supp_harvesting_cols))

tbl_2018_loss_ws <- pre_2018_ws |> 
  select(any_of(loss_cols))
supp_2018_loss_ws <- pre_2018_ws |> 
  select(any_of(supp_loss_cols))

# Alfalfa -----------------------------------------------------------------

raw_2018_alf <- xl_snap$`2018_harvests_alfalfa` |> clean_names()

pre_2018_alf <- raw_2018_alf |> mutate(
  plot = plot,
  harvest_date = cut_date,
  cut = cut,
  harvest_lbs = plot_wt_tons * 2000,
  section = "Main",
  crop = case_when(harvest_type == "Wheatlage"~"wheatlage",
                   plot %in% c(103, 213, 314, 410) & cut == 1 ~ "oatlage",
                   .default = "alfalfa"),
  percent_moisture = moisture_percent * 100,
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = crop,
                                 cut = cut),
  harvestingloss_id = get_harvestingloss_id(year = 2018,
                                             plot = plot,
                                             section = section,
                                             product = crop),
  comments = stitch_notes(harvest_type, ml_notes),
  harvest_length = ml_plot_length,
  harvest_width = ml_plot_width,
  harvest_area = ml_plot_area_ft2,
  loss_area = ml_loss_area_ft2,
  loss_reason = ml_loss_reason,
  num_bales = ml_num_bales
)

# remove wheatlage from alfalfa and process separate
tbl_2018_alf <- pre_2018_alf |> filter(crop != "wheatlage") |> select(any_of(harvesting_cols))

supp_2018_alf <- pre_2018_alf |> filter(crop != "wheatlage") |> select(any_of(supp_harvesting_cols))

tbl_2018_loss_alf <- pre_2018_alf |> filter(crop != "wheatlage") |>
  drop_na(loss_area) |>
  select(any_of(loss_cols))
  
supp_2018_loss_alf <- pre_2018_alf |> filter(crop != "wheatlage") |>
  select(any_of(supp_loss_cols)) |> 
  drop_na(loss_reason)


# Wheatlage ---------------------------------------------------------------

# should be cut 1 in plot 307 because no previous cuts, just mislabelled in master

tbl_2018_wl <- pre_2018_wg |> filter(crop == "wheatlage") |> select(any_of(harvesting_cols))
supp_2018_wl <- pre_2018_wg |> filter(crop == "wheatlage") |> 
  select(any_of(supp_harvesting_cols)) |> 
  left_join(pre_2018_alf |>
              filter(crop == "wheatlage") |>
              select(harvesting_id, comments), by = "harvesting_id")

tbl_2018_loss_wl <- pre_2018_wg |> filter(crop == "wheatlage") |> select(any_of(loss_cols))
supp_2018_loss_wl <- pre_2018_wg |> filter(crop == "wheatlage") |> select(any_of(supp_loss_cols))



# Pasture -----------------------------------------------------------------

raw_2018_past <- xl_snap$`2018_harvests_pasture` |> clean_names()

pre_2018_past <- raw_2018_past |> mutate(
  biomass_date = date,
  plot = plot,
  section = "Main",
  cut = cut,
  coordinate = NA,
  biomass = "pasture",
  biomassing_id = get_biomassing_id(year = 2018,
                                    plot = plot,
                                    section = section,
                                    cut = cut,
                                    biomass = biomass,
                                    coordinate = "X"),
  biomass_grams = plot_wt_kg * 1000,
  method = "exclosure",
  component = "shoots",
  biomass_length = plot_length_ft, 
  biomass_width = plot_width_ft,
  biomass_area = plot_area,
  wet_weight_no_bag = grab_wet_wt_g,
  dry_weight_no_bag = grab_dry_wt_g,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
  comments = stitch_notes(notes, NA)
)

tbl_2018_past <- pre_2018_past |> select(any_of(biomassing_cols))
supp_2018_past <- pre_2018_past |> select(any_of(supp_biomassing_cols))

# Prairie -----------------------------------------------------------------

raw_2018_prairie <- xl_snap$`2018_biofuels` |> clean_names()

pre_2018_prairie <- raw_2018_prairie |> mutate(
  crop = plot,
  plot = treatment,
  harvest_date = date,
  section = case_match(sub_plot,
                       "main"~"Macro",
                       "micro"~"Micro"),
  fuel_plot = str_c(plot, section),
  harvest_lbs = harvest_wt_lbs, # same as final plot weight
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = "prairie"),
  harvest_width = plot_width_ft,
  harvest_length = plot_length_ft,
  harvest_area = area_ft_2, # harvest area
  # loss_area = gaps, # no reason given
  wet_weight_no_bag = wet,
  dry_weight_no_bag = dry,
  percent_moisture = (wet - dry) / wet * 100,
  # bushel_lbs = na_if(test_wt_lbs_bu, "N/A"),
  # num_bales = number_of_bales_in_wagon + bale_adjustment_from_plot_left_in_baler,
  # wagon_weight = wagon_weight_lbs,
)

tbl_2018_prairie <- pre_2018_prairie |> select(any_of(fuel_harvesting_cols))
supp_2018_prairie <- pre_2018_prairie |> select(any_of(supp_fuel_harvesting_cols))


# oats/alf undercut -----------------------------------------------------------

raw_2018_under <- xl_snap$`2018_undercutting` |> clean_names()

pre_2018_under <- raw_2018_under |> mutate(
  plot = plot,
  biomass_date = sample_date,
  section = "Main",
  biomass = sample_type,
  biomass_grams = weight_g,
  percent_moisture = 0,
  coordinate = station,
  method = sample_event,
  component = case_match(ml_top_bottom,
                         "top"~"shoots",
                         "bottom"~"roots"),
  biomassing_id = get_biomassing_id(year = 2018,
                                    plot = plot,
                                    section = section,
                                    biomass = biomass,
                                    coordinate = coordinate,
                                    method = method,
                                    component = component),
  biomass_area = sample_area_ft2,
  cut = 1, #TODO: cut is ill defined throughout
  comments = ml_notes,
  # harvest_area = start_area,
  # loss_area = gaps,
  # wet_weight_no_bag = wet_grab_wt,
  # dry_weight_no_bag = dry_grab_wt,
  # bushel_lbs = na_if(test_wt_lbs_bu, "N/A"),
  # wagon_weight = wagon_weight_lbs,
)

tbl_2018_under <- pre_2018_under |> select(any_of(biomassing_cols))
# only comments
supp_2018_under <- pre_2018_under |> select(any_of(supp_biomassing_cols)) |> 
  drop_na(comments)

# biomassing something duplicated
# tbl_2018_under |> filter(biomassing_id == "B2018_A308MMC_WD_1_UCRT")
# pre_2018_under |> filter(plot == 308) |> count(station, top_bottom, sample_type) |> complete(station, top_bottom, sample_type)


# corn silage -------------------------------------------------------------

raw_2018_cs <- xl_snap$`2018_harvests_corn_silage` |> clean_names()

pre_2018_cs <- raw_2018_cs |> mutate(
  plot = plot,
  harvest_date = as.POSIXct("2018-09-14", tz = "UTC"),
  section = case_match(micro,
                       "W"~"West 15'",
                       "E"~"East 15'"),
  crop = "corn silage",
  sideplot = str_c(plot, case_match(section,
                                    "West 15'"~"WS",
                                    "East 15'"~"ES")),
  harvesting_id = get_harvest_id(year = 2018,
                                 plot = plot,
                                 section = section,
                                 product = "corn silage",
                                 cut = 1),
  systematicharvestingloss_id = get_harvestingloss_id(year = 2018,
                        plot = plot,
                        section = section,
                        product = "corn silage"),
  harvest_lbs = weight_lbs,
  harvest_area = plot_area,
  harvest_width = ml_plot_width,
  harvest_length = ml_plot_length,
  percent_moisture = moisture,
  loss_fraction = readr::parse_number(notes) / 100,
  loss_category = if_else(!is.na(notes), "lodging", NA),
  sysloss_comments = stitch_notes(notes, NA),
)

tbl_2018_cs <- pre_2018_cs |> select(any_of(cs_harvesting_cols))
supp_2018_cs <- pre_2018_cs |> select(any_of(supp_cs_harvesting_cols))
tbl_2018_sysloss_cs <- pre_2018_cs |> select(any_of(sysloss_cols)) |> drop_na(loss_fraction)
supp_2018_sysloss_cs <- pre_2018_cs |> select(any_of(supp_sysloss_cols)) |> drop_na(sysloss_comments)


# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2018_harvests <- bind_rows(
  tbl_2018_c,
  tbl_2018_sb,
  tbl_2018_wg,
  tbl_2018_ws,
  tbl_2018_alf,
  tbl_2018_wl,
)
supp_2018_harvests <- bind_rows(
  supp_2018_c,
  supp_2018_sb,
  supp_2018_wg,
  supp_2018_ws,
  supp_2018_alf,
  supp_2018_wl,
)

# biomassings
tbl_2018_bio <- bind_rows(
  tbl_2018_under,
  tbl_2018_past
)
supp_2018_bio <- bind_rows(
  supp_2018_under, # empty
  supp_2018_past
)

# canopeo
tbl_2018_can <- bind_rows()
supp_2018_can <- bind_rows()

# losses
tbl_2018_loss <- bind_rows(
  tbl_2018_loss_wg,
  tbl_2018_loss_ws,
  tbl_2018_loss_alf,
  tbl_2018_loss_wl
)
supp_2018_loss <- bind_rows(
  # supp_2018_loss_wg, # empty
  # supp_2018_loss_ws, # empty
  supp_2018_loss_alf,
  supp_2018_loss_wl
)
tbl_2018_sysloss <- bind_rows()
supp_2018_sysloss <- bind_rows()

## EI ----------------------------------------------------------------------

# harvests
# tbl_2018_ei_harvests <- bind_rows()
# supp_2018_ei_harvests <- bind_rows()
# 
# # biomassings
# tbl_2018_ei_bio <- bind_rows()
# supp_2018_ei_bio <- bind_rows()
# 
# # canopeo
# tbl_2018_ei_can <- bind_rows()
# supp_2018_ei_can <- bind_rows()
# 
# # losses
# tbl_2018_ei_loss <- bind_rows()
# supp_2018_ei_loss <- bind_rows()
# tbl_2018_ei_sysloss <- bind_rows()
# supp_2018_ei_sysloss <- bind_rows()

## Silage ------------------------------------------------------------------

# harvests
tbl_2018_silage <- bind_rows(
  tbl_2018_cs
)
supp_2018_silage <- bind_rows(
  supp_2018_cs
)

tbl_2018_silage_loss <- bind_rows()
supp_2018_silage_loss <- bind_rows()

tbl_2018_silage_sysloss <- bind_rows(
  tbl_2018_sysloss_cs
)
supp_2018_silage_sysloss <- bind_rows(
  supp_2018_sysloss_cs
)

## Biofuel -----------------------------------------------------------------

# harvests
tbl_2018_prairie <- bind_rows(
  tbl_2018_prairie
)
supp_2018_prairie <- bind_rows(
  supp_2018_prairie
)


## 115 ---------------------------------------------------------------------

tbl_2018_115 <- bind_rows(
  tbl_2018_115_sb
)

supp_2018_115 <- bind_rows(
  supp_2018_115_sb
)


