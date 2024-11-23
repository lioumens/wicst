

# corn silage -------------------------------------------------------------

raw_2017_cs <- xl_snap$`2017_harvests_corn_silage` |> clean_names()

pre_2017_cs <- raw_2017_cs |> mutate(
  plot = plot,
  harvest_lbs = weight_lbs,
  section = "East 15'",
  sideplot = str_c(plot, case_match(section,
                                    "West 15'"~"WS",
                                    "East 15'"~"ES")),
  harvest_date = as.POSIXct("2017-09-18", tz = "UTC"), # from agcal calendar
  # * In 2017 we began harvesting silage from CS4 and CS5. The 2017 silage harvests were taken only from the E 15' strips of the corn phase of CS4 and CS5. In an effort to continue in a statistically robust manner, starting in 2018 corn silage sub plots were randomly assigned.
  # This will be the location of the corn silage plots in perpetuity. 
  percent_moisture = moisture_percent,
  crop = "corn silage",
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = crop),
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area_ft2,
  # from rotation grid
  comments = "General: \"In 2017 we began harvesting silage from CS4 and CS5. The 2017 silage harvests were taken only from the E 15' strips of the corn phase of CS4 and CS5. In an effort to continue in a statistically robust manner, starting in 2018 corn silage sub plots were randomly assigned. This will be the location of the corn silage plots in perpetuity.\""
)

tbl_2017_cs <- pre_2017_cs |> select(any_of(cs_harvesting_cols))
supp_2017_cs <- pre_2017_cs |> select(any_of(supp_cs_harvesting_cols))


# Corn --------------------------------------------------------------------

raw_2017_c <- xl_snap$`2017_harvests_corn` |> clean_names()

pre_2017_c <- raw_2017_c |> mutate(
  plot = plot,
  section = "Main",
  crop = crop,
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "corn"),
  harvest_lbs = harvest_lbs,
  harvest_date = harvest_date,
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_bu_lb,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area
)

tbl_2017_c <- pre_2017_c |> select(any_of(harvesting_cols))
supp_2017_c <- pre_2017_c |> select(any_of(supp_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2017_sb <- xl_snap$`2017_harvests_soybean` |> clean_names()

pre_2017_sb <- raw_2017_sb |> mutate(
  plot = plot,
  section = "Main",
  crop = crop,
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "soybean"),
  harvest_lbs = harvest_lbs,
  harvest_date = harvest_date,
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_bu_lb,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = area
)

tbl_2017_sb <- pre_2017_sb |> select(any_of(harvesting_cols))
supp_2017_sb <- pre_2017_sb |> select(any_of(supp_harvesting_cols))


# Wheat -------------------------------------------------------------------

raw_2017_wg <- xl_snap$`2017_harvests_wheat` |> clean_names()

pre_2017_wg <- raw_2017_wg |> mutate(
  plot = plot,
  section = "Main",
  crop = "wheat",
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "wheat"),
  harvest_lbs = harvest_weight_lbs,
  harvest_date = as.POSIXct("2017-07-31", tz = "UTC"),
  percent_moisture = percent_moisture,
  bushel_lbs = test_wt_lbs_bu,
  harvest_area = harvested_area_ft2,
  comments = stitch_notes(notes, NA)
)

tbl_2017_wg <- pre_2017_wg |> select(any_of(harvesting_cols))
supp_2017_wg <- pre_2017_wg |> select(any_of(supp_harvesting_cols))


# Wheat straw -------------------------------------------------------------

raw_2017_ws <- xl_snap$`2017_harvests_wheat_straw` |> clean_names()

pre_2017_ws <- raw_2017_ws |> mutate(
  plot = plot,
  section = "Main",
  crop = "wheat straw",
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "wheat straw"),
  avg_bale_wt = total_bale_weight_lbs / number_of_bales,
  harvest_adj = avg_bale_wt * bale_adjustment_from_plot_left_in_baler,
  num_bales = number_of_bales + bale_adjustment_from_plot_left_in_baler,
  harvest_lbs = total_bale_weight_lbs + harvest_adj,
  wet_weight_no_bag = wet_grab_wt,
  dry_weight_no_bag = dry_grab_wt,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
  harvest_date = date,
  wagon_weight = wagon_weight_lbs,
  harvest_area = area,
  comments = glue("Michael Liou: Final harvest weight calculated from trailer ({trailer_bales}) + plot ({plot_bales}) bales. We only had weights from trailer, so plot bales assumed average of trailer bale weight ({bale_avg}). Original calculation was using unweighted average of average bale weights across all T4 plots.",
                  trailer_bales = number_of_bales,
                  plot_bales = bale_adjustment_from_plot_left_in_baler,
                  bale_avg = round(avg_bale_wt, 2))
)

tbl_2017_ws <- pre_2017_ws |> select(any_of(harvesting_cols))
supp_2017_ws <- pre_2017_ws |> select(any_of(supp_harvesting_cols))


# Alfalfa -----------------------------------------------------------------

raw_2017_alf <- xl_snap$`2017_harvests_alfalfa` |> clean_names()

pre_2017_alf <- raw_2017_alf |> mutate(
  plot = plot,
  section = "Main",
  crop = "alfalfa",
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "alfalfa"),
  harvest_lbs = plot_wt_tons * 2000,
  harvest_width = 60,
  harvest_length = 510,
  harvest_area = harvest_width * harvest_length,
  percent_moisture = moisture_percent,
  harvest_date = cut_date,
  cut = cut,
  comments = stitch_notes(harvest_type, NA)
)

tbl_2017_alf <- pre_2017_alf |> select(any_of(harvesting_cols))
supp_2017_alf <- pre_2017_alf |> select(any_of(supp_harvesting_cols))


# Pasture -----------------------------------------------------------------

raw_2017_past <- xl_snap$`2017_harvests_pasture` |> clean_names()

pre_2017_past <- raw_2017_past |> mutate(
  plot = plot,
  section = "Main",
  cut = cut,
  crop = "pasture",
  coordinate = NA,
  biomass = "pasture",
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = crop),
  biomassing_id = get_biomassing_id(year = 2017,
                                    plot = plot,
                                    section = section,
                                    biomass = crop,
                                    coordinate = "X"),
  wet_weight_no_bag = grab_wet_wt_g,
  dry_weight_no_bag = grab_dry_wt_g,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
  harvest_date = date,
  biomass_date = date,
  harvest_lbs = coalesce(plot_wt_kg * kg_to_lbs, plot_wt_lbs),
  biomass_grams = plot_wt_kg / 1000,
  harvest_width = plot_width_ft,
  biomass_width = plot_width_ft,
  harvest_length = plot_length_ft,
  biomass_length = plot_length_ft,
  num_bales = number_of_bales,
  harvest_area = harvest_width * harvest_length,
  biomass_area = biomass_width * biomass_length,
  method = if_else(plot_area > 10000, NA, "exclosure"),
  component = if_else(plot_area > 10000, NA, "shoots"), # assumed shoots
  cut = cut,
  comments = stitch_notes(notes, NA)
)

tbl_2017_past <- pre_2017_past |>
  filter(plot_area > 10000) |> 
  select(any_of(harvesting_cols))
supp_2017_past <- pre_2017_past |>
  filter(plot_area > 10000) |> 
  select(any_of(supp_harvesting_cols))

# bio
tbl_2017_bio_past <- pre_2017_past |>
  filter(plot_area <= 10000) |> 
  select(any_of(biomassing_cols))
supp_2017_bio_past <- pre_2017_past |>
  filter(plot_area <= 10000) |> 
  select(any_of(supp_biomassing_cols))



# Prairie -----------------------------------------------------------------

raw_2017_prairie <- xl_snap$`2017_biofuels` |> clean_names()

pre_2017_prairie <- raw_2017_prairie |> mutate(
  crop = plot,
  plot = treatment,
  section = case_match(sub_plot,
                       "main"~"Macro",
                       "micro"~"Micro"),
  fuel_plot = str_c(plot, section),
  harvesting_id = get_harvest_id(year = 2017,
                                 plot = plot,
                                 section = section,
                                 product = "prairie",
                                 cut = 1),
  harvest_lbs = harvest_wt_lbs,
  harvest_date = date,
  wet_weight_no_bag = wet,
  dry_weight_no_bag = dry,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = area_ft_2,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
)

tbl_2017_prairie <- pre_2017_prairie |> select(any_of(fuel_harvesting_cols))
supp_2017_prairie <- pre_2017_prairie |> select(any_of(supp_fuel_harvesting_cols))

# undercutting ------------------------------------------------------------

raw_2017_under <- xl_snap$`2017_undercutting` |> clean_names()

pre_2017_under <- raw_2017_under |>
  mutate(
  plot = plot,
  coordinate = station,
  section = "Main",
  method = "undercutting",
  biomass = str_extract(sample_type, "(.*)\\s", 1),
  component = case_match(top_bottom,
                         "bottom"~"roots",
                         "top"~"shoots")) |> 
  mutate(cut = row_number(),
         .by = c(plot, coordinate, component, biomass)) |>
  mutate(
  biomassing_id = get_biomassing_id(year = 2017,
                                    plot = plot,
                                    section = section,
                                    cut = cut,
                                    biomass = biomass,
                                    method = method,
                                    coordinate = coordinate,
                                    component = component),
  biomass_grams = weight_g,
  biomass_area = sample_area_ft2,
  biomass_date = sample_date
)

tbl_2017_under <- pre_2017_under |> select(any_of(biomassing_cols))
# supp_2017_under <- pre_2017_under |> select(any_of(supp_biomassing_cols))

# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2017_harvests <- bind_rows(
  tbl_2017_c,
  tbl_2017_sb,
  tbl_2017_wg,
  tbl_2017_ws,
  tbl_2017_alf,
  tbl_2017_past
)
supp_2017_harvests <- bind_rows(
  supp_2017_c,
  supp_2017_sb,
  supp_2017_wg,
  supp_2017_ws,
  supp_2017_alf,
  supp_2017_past
)

# biomassings
tbl_2017_bio <- bind_rows(
  tbl_2017_bio_past,
  tbl_2017_under
)
supp_2017_bio <- bind_rows(
  supp_2017_bio_past,
  # supp_2017_under
)

# canopeo
tbl_2017_can <- bind_rows()
supp_2017_can <- bind_rows()

# losses
tbl_2017_loss <- bind_rows()
supp_2017_loss <- bind_rows()
tbl_2017_sysloss <- bind_rows()
supp_2017_sysloss <- bind_rows()

## EI ----------------------------------------------------------------------

# harvests
tbl_2017_ei_harvests <- bind_rows()
supp_2017_ei_harvests <- bind_rows()

# biomassings
tbl_2017_ei_bio <- bind_rows()
supp_2017_ei_bio <- bind_rows()

# canopeo
tbl_2017_ei_can <- bind_rows()
supp_2017_ei_can <- bind_rows()

# losses
tbl_2017_ei_loss <- bind_rows()
supp_2017_ei_loss <- bind_rows()
tbl_2017_ei_sysloss <- bind_rows()
supp_2017_ei_sysloss <- bind_rows()

## Silage ------------------------------------------------------------------

# harvests
tbl_2017_silage <- bind_rows(
  tbl_2017_cs
)
supp_2017_silage <- bind_rows(
  supp_2017_cs
)

## Biofuel -----------------------------------------------------------------

# harvests
tbl_2017_prairie <- bind_rows(
  tbl_2017_prairie
)
supp_2017_prairie <- bind_rows(
  supp_2017_prairie
)
