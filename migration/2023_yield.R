# Library -----------------------------------------------------------------
library(lubridate)
library(glue)
library(purrr)
library(tibble)
library(rlang)

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
  select(any_of(ei_harvesting_cols))

dupe_2023_c <- pre_2023_c |> get_yield("corn")
dupe_2023_ei_c <- tbl_2023_ei_c |> get_yield("corn")

supp_2023_c <- pre_2023_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

supp_2023_ei_c <- pre_2023_c |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))

# wheat grain -------------------------------------------------------------

raw_2023_wg <- xl_snap$`2023_harvests_wheat_grain` |> clean_names()


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
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    dry_weight_w_bag = bag_dry_weight_g,
    dry_weight_no_bag = dry_wt_g_no_bag,
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
  select(any_of(ei_harvesting_cols))

# dupe_2023_ws <- tbl_2023_ws |> get_yield("wheat_straw")
# dupe_2023_ei_ws <- tbl_2023_ei_ws |> 
#   mutate(crop = case_match(crop,
#                     "Wheat"~"wheat straw",
#                     "Barley"~"barley")) |> 
#   get_yield()

supp_2023_ws <- pre_2023_ws |> 
  filter(section == "Main") |>
  select(any_of(supp_harvesting_cols))

supp_2023_ei_ws <- pre_2023_ws |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2023_sb <- xl_snap$`2023_harvests_soybean` |> clean_names()

# raw_2023_sb |> names()
pre_2023_sb <- raw_2023_sb |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    section = plot_section,
    crop = crop,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "soybean"),
    percent_moisture = percent_moisture,
    harvest_lbs = harvest_lbs,
    # bag_weight = bag_weight_g,
    # wet_weight_w_bag = bag_wet_weight_g,
    # dry_weight_w_bag = bag_dry_weight_g,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = area_ft_2,
    comments = stitch_notes(notes, ml_notes),
    .before = 1,
  )

tbl_2023_sb <-  pre_2023_sb |> filter(section == "Main") |> 
  select(any_of(harvesting_cols))

tbl_2023_ei_sb <-  pre_2023_sb |> filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

dupe_2023_sb <- pre_2023_sb |> filter(section == "Main") |> 
  get_yield("soybean")

dupe_2023_ei_sb <- pre_2023_sb |> filter(section != "Main") |> 
  get_yield("soybean")

supp_2023_sb <- pre_2023_sb |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

supp_2023_ei_sb <- pre_2023_sb |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))


# alfalfa -----------------------------------------------------------------

raw_2023_alf <- xl_snap$`2023_harvests_alfalfa` |> clean_names()

# raw_2023_alf |> names()

pre_2023_alf <- raw_2023_alf |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    cut = cut,
    section = "Main",
    crop = crop,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "alfalfa",
                                   cut = cut),
    percent_moisture = moisture_rrl,
    harvest_lbs = plot_wt_tons * 2000,
    # bag_weight = bag_weight_g,
    # wet_weight_w_bag = bag_wet_weight_g,
    # dry_weight_w_bag = bag_dry_weight_g,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    rrl_id = rrl_id,
    # 2023_harvests_oatlage has num bales info for cut 1... just add 2 manually
    num_bales = if_else(cut == 1 & plot %in% c(110, 208, 304, 413), 2, NA),
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area_ft_2,
    comments = stitch_notes(notes, ml_notes = NA),
    .before = 1,
  )

tbl_2023_alf <- pre_2023_alf |> 
  select(any_of(harvesting_cols))

supp_2023_alf <- pre_2023_alf |> 
  select(any_of(supp_harvesting_cols))

dupe_2023_alf <- pre_2023_alf |> get_yield("alfalfa")
# dupe_2023_alf |> View()


# Pasture Exclosures -----------------------------------------------------------------

raw_2023_past <- xl_snap$`2023_harvests_pasture` |> clean_names()

# raw_2023_past |> names()

pre_2023_bio_past <- raw_2023_past |>
  mutate(
    biomass_date = harvest_date,
    plot = plot,
    cut = cut,
    section = "Main",
    # 207 switched to 3 in 2023, and 4 in 2022 everything else same
    coordinate = if_else(plot == 207,  
                         "paddock 3",
                         "paddock 4"),
    biomass = "pasture",
    crop = crop,
    subplot = str_c(plot, section),
    biomassing_id = get_biomassing_id(year = 2023,
                                      plot = plot,
                                      section = section,
                                      biomass = biomass,
                                      coordinate = coordinate,
                                      cut = cut),
    percent_moisture = moisture,
    biomass_grams = plot_wt_kg * 1000,
    method = "exclosure",
    component = "shoots",
    # bag_weight = bag_weight_g,
    # wet_weight_w_bag = bag_wet_weight_g,
    # dry_weight_w_bag = bag_dry_weight_g,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    bucket_lbs = 1.55 * kg_to_lbs,
    rrl_id = rrl_id,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_area = plot_area_ft,
    comments = stitch_notes(NA, ml_notes),
    .before = 1,
  )

tbl_2023_bio_past <- pre_2023_bio_past |> select(any_of(biomassing_cols))
dupe_2023_bio_past <- tbl_2023_bio_past |> get_biomass()
supp_2023_bio_past <- pre_2023_bio_past |> select(any_of(supp_biomassing_cols))


# Corn Silage -------------------------------------------------------------

raw_2023_cs <- xl_snap$`2023_harvests_corn_silage_core` |> clean_names()

pre_2023_cs <- raw_2023_cs |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    # cut = cut,
    section = si_trt,
    crop = "corn silage",
    # subplot = str_c(plot, section),
    sideplot = str_c(plot, case_match(section,
                                      "West 15'"~"WS",
                                      "East 15'"~"ES")),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = "1"),
    percent_moisture = moisture_rrl,
    harvest_lbs = harvest_lbs,
    # bag_weight = bag_weight_g,
    # wet_weight_w_bag = bag_wet_weight_g,
    # dry_weight_w_bag = bag_dry_weight_g,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = plot_area_ft_2,
    # comments = stitch_notes(notes, ml_notes = NA),
    .before = 1,
  )

tbl_2023_cs <- pre_2023_cs |> select(any_of(cs_harvesting_cols))
dupe_2023_cs <- tbl_2023_cs |> get_yield("corn silage")
supp_2023_cs <- pre_2023_cs |> select(any_of(supp_cs_harvesting_cols))


# EI Corn Silage ----------------------------------------------------------

raw_2023_ei_cs <- xl_snap$`2023_harvests_corn_silage_ei` |> clean_names()

pre_2023_ei_cs <- raw_2023_ei_cs |>
  separate_wider_delim(plot, delim = "-", names = c("plot", "trt")) |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    # cut = cut,
    section = section,
    crop = "corn silage",
    subplot = str_c(plot, section),
    # sideplot = str_c(plot, case_match(section,
    #                                   "West 15'"~"WS",
    #                                   "East 15'"~"ES")),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = "1"),
    percent_moisture = moisture_rrl,
    harvest_lbs = harvest_lbs,
    # bag_weight = bag_weight_g,
    # wet_weight_w_bag = bag_wet_weight_g,
    # dry_weight_w_bag = bag_dry_weight_g,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = plot_area_ft_2,
    # comments = stitch_notes(notes, ml_notes = NA),
    .before = 1,
  )

tbl_2023_ei_cs <- pre_2023_ei_cs |> select(any_of(ei_harvesting_cols))
dupe_2023_ei_cs <- tbl_2023_ei_cs |> get_yield("corn silage")
supp_2023_ei_cs <- pre_2023_ei_cs |> select(any_of(supp_ei_harvesting_cols))


# prairie 115 -------------------------------------------------------------

raw_2023_115_prairie <- xl_snap$`2023_harvests_prairie_115` |> clean_names()
# raw_2023_115_prairie |> names()

pre_2023_115_prairie <- raw_2023_115_prairie |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    # cut = cut,
    section = subplot,
    crop = crop,
    # subplot = str_c(plot, section),
    # sideplot = str_c(plot, case_match(section,
    #                                   "West 15'"~"WS",
    #                                   "East 15'"~"ES")),
    # harvesting_id = get_harvest_id(year = 2023,
    #                                plot = plot,
    #                                section = section,
    #                                product = crop,
    #                                cut = "1"),
    percent_moisture = percent_mst * 100,
    harvest_lbs = plot_wt_lbs,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    wet_weight_no_bag = wet_wt_g_no_bag,
    dry_weight_w_bag = bag_dry_weight_g,
    dry_weight_no_bag = dry_wt_g_no_bag,
    # rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = area_ft_2,
    # comments = stitch_notes(notes, ml_notes = NA),
    .before = 1,
  )

# no harvesting id's b/c not part of Wicst
tbl_2023_115_prairie <- pre_2023_115_prairie |> select(any_of(fuel_115_harvesting_cols))
dupe_2023_115_prairie <- tbl_2023_115_prairie |> get_yield()
supp_2023_115_prairie <- pre_2023_115_prairie |> select(any_of(supp_fuel_115_harvesting_cols))


# Biofuels ----------------------------------------------------------------

raw_2023_prairie <- xl_snap$`2023_harvests_prairie_wicst` |> clean_names()

pre_2023_prairie <- raw_2023_prairie |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    # cut = cut,
    section = subplot |> case_match("Main"~"Macro",
                                    "Micro"~"Micro"),
    crop = crop,
    fuel_plot = str_c(plot, section),
    # subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   product = "prairie",
                                   cut = "1"),
    percent_moisture = percent_mst * 100,
    harvest_lbs = plot_wt_lbs,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    wet_weight_no_bag = wet_wt_g_no_bag,
    dry_weight_w_bag = bag_dry_weight_g,
    dry_weight_no_bag = dry_wt_g_no_bag,
    # rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = area_ft_2,
    comments = stitch_notes(NA, ml_notes),
    .before = 1,
  )

tbl_2023_prairie <- pre_2023_prairie |> select(any_of(fuel_harvesting_cols))
dupe_2023_prairie <- tbl_2023_prairie |> get_yield()
supp_2023_prairie <- pre_2023_prairie |> select(any_of(supp_fuel_harvesting_cols))


# wheat biomassing --------------------------------------------------------

raw_2023_bio_wheat <- xl_snap$`2023_anpp_wheat` |> clean_names()

pre_2023_bio_wheat <- raw_2023_bio_wheat |> 
  drop_na(wt_with_bag) |>
  summarize(
    bag_wt_total = sum(bag_wts, na.rm = T),
    total_wt_g = sum(wt_with_bag, na.rm = T),
    ml_notes = str_flatten(ml_notes, collapse = "|"), # idk if generalizes, only 1 comment though
    .by = c(date_harvested, plot, coordinate, name)) |>
  mutate(
    biomass_date = date_harvested,
    plot = plot,
    section = coordinate |> case_match(c("North", "Center", "South") ~ "Main",
                                       .default = coordinate),
    coordinate = coordinate |> case_match(c("NW", "NE", "E", "W", "SW", "SE") ~ NA,
                                          .default = coordinate),
    subplot = str_c(plot, section),
    cut = 1,
    biomass = name,
    biomassing_id = get_biomassing_id(year = 2023,
                                   plot = plot,
                                   section = section,
                                   coordinate = coordinate,
                                   biomass = biomass,
                                   cut = "1"),
    
    bag_weight = bag_wt_total,
    # wet_weight_w_bag = bag_wet_weight_g,
    # wet_weight_no_bag = wet_wt_g_no_bag,
    dry_weight_w_bag = total_wt_g,
    biomass_grams = pmax(total_wt_g - bag_wt_total, 0),
    mass_comment = if_else((total_wt_g - bag_wt_total) < 0,
            "Michael Liou: \"Changed negative dm to 0, -.21 -> 0\"",
            NA),
    # dry_weight_no_bag = ,
    # rrl_id = rrl_id,
    # biomass_length = length_ft,
    percent_moisture = if_else(biomass_grams == 0, NA, 0), #TODO: check
    biomass_width = m_to_ft,
    biomass_length = m_to_ft,
    biomass_area = 1 * m2_to_ft2, # 1m^2 quadrats
    .before = 1,
  ) |> rowwise() |> 
  # str_flatten is not vectorized unfortunately
  mutate(comments = na_if(str_flatten(c(mass_comment, ml_notes),
                                      collapse = " | ",
                                      na.rm = T), ""))

tbl_2023_bio_wheat <- pre_2023_bio_wheat |> 
  filter(section == "Main") |>
  select(any_of(biomassing_cols))
tbl_2023_ei_bio_wheat <- pre_2023_bio_wheat |> 
  filter(section != "Main") |>
  select(any_of(ei_biomassing_cols))
supp_2023_bio_wheat <- pre_2023_bio_wheat |> 
  filter(section == "Main") |>
  select(any_of(supp_biomassing_cols))
supp_2023_ei_bio_wheat <- pre_2023_bio_wheat |> 
  filter(section != "Main") |>
  select(any_of(supp_ei_biomassing_cols))


# Wheat canopeo -----------------------------------------------------------

raw_2023_can_wheat <- xl_snap$`2023_anpp_wheat_canopeo` |> clean_names()

# biomass
# from CS3-wheat
# M/C/E1/E3 - oats/berseem, drilley after wheat
# E2/E4/E5 - red clover, drill barley

pre_2023_can_wheat <- raw_2023_can_wheat |> 
  separate_wider_delim(plot, names = c("plot", "section"), delim = " ") |> 
  mutate(
    coverage_date = date,
    subplot = str_c(plot, section),
    biomass = case_match(subplot,
                         c("104Main", "301Main", "402Main")~"cl11", # main
                         c("104NW", "301W", "402SE")~"cl11", # control
                         c("104NE", "301NW", "402NE")~"cl",
                         c("104SW", "301E", "402SW")~"fsRC",
                         c("104W", "301SE", "402W")~"cl",
                         c("104E", "301SW", "402E")~"csRC",
                         c("104SE", "301NE", "402NW")~"RC"),
    canopeo_id = get_canopeo_id(year = 2023,
                                plot = plot,
                                section = section,
                                coordinate = coordinate,
                                biomass = "XX",
                                cut = "X"),
    percent_cover = percent_cover
  )

tbl_2023_can_wheat <- pre_2023_can_wheat |> 
  filter(section == "Main") |> 
  select(any_of(canopeo_cols))

tbl_2023_can_ei_wheat <- pre_2023_can_wheat |> 
  filter(section != "Main") |> 
  select(any_of(ei_canopeo_cols))

# no supp information


# ANPP alfalfa ------------------------------------------------------------

raw_2023_bio_alf <- xl_snap$`2023_anpp_alfalfa` |> clean_names()

# should count how many cuts per plot right? for cut numbering
how_many_alf <- tbl_2023_alf |> count(plot, name = "num_cuts")


pre_2023_bio_alf <- raw_2023_bio_alf |> 
  # group alfalfa1 and alfalfa2 together, they are just 2 different bags of alf
  mutate(name_group = str_extract(name, "[a-z]*"),
         wt_with_bag = replace_na(wt_with_bag, 0)) |>
  # sum the multiple bags and weights
  summarize(
    bag_wt_total = sum(bag_wt),
    total_wt_g = sum(wt_with_bag),
    ml_notes = str_flatten(ml_notes, collapse = "|"), # idk if generalizes, only 1 comment though
    .by = c(date_harvested, plot, coordinate, name_group)) |> 
  # attach number of cuts info
  left_join(how_many_alf, by = "plot") |> 
  # convert to needed biomassing cols
  mutate(
    biomass_date = date_harvested,
    plot = plot,
    section = coordinate |> case_match(c("North", "Center", "South") ~ "Main",
                                       .default = coordinate),
    coordinate = coordinate,
    # subplot = str_c(plot, section),
    cut = num_cuts + 1,
    biomass = name_group,
    biomassing_id = get_biomassing_id(year = 2023,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = case_match(biomass,
                                                           c("weed")~"weeds",
                                                           .default = biomass),
                                      cut = cut),
    bag_weight = bag_wt_total,
    # wet_weight_w_bag = bag_wet_weight_g,
    # wet_weight_no_bag = wet_wt_g_no_bag,
    dry_weight_w_bag = total_wt_g,
    biomass_grams = pmax(total_wt_g - replace_na(bag_wt_total, 0), 0),
    mass_comment = if_else((total_wt_g - replace_na(bag_wt_total, 0)) < 0,
                           glue("Michael Liou: \"Changed negative dm to 0, {old_entry} -> 0\"", 
                                old_entry = round(total_wt_g - replace_na(bag_wt_total), 2)),
                           NA),
    percent_moisture = if_else(biomass_grams == 0, NA, 0), #TODO: check
    # dry_weight_no_bag = ,
    # rrl_id = rrl_id,
    # biomass_length = length_ft,
    biomass_width = m_to_ft,
    biomass_length = m_to_ft,
    biomass_area = m2_to_ft2, # 1m^2 quadrats
    .before = 1,
  ) |> 
  rowwise() |> 
  # str_flatten is not vectorized unfortunately
  mutate(comments = na_if(str_flatten(c(mass_comment, ml_notes),
                                      collapse = " | ",
                                      na.rm = T), "")) |> 
  ungroup()

tbl_2023_bio_alf <- pre_2023_bio_alf |> select(any_of(biomassing_cols))
supp_2023_bio_alf <- pre_2023_bio_alf |> select(any_of(supp_biomassing_cols))




# Alfalfa Canopeo ---------------------------------------------------------

raw_2023_can_alf <- xl_snap$`2023_anpp_alfalfa_canopeo` |> clean_names()

pre_2023_can_alf <- raw_2023_can_alf |> 
  mutate(
    plot = as.character(plot),
    coverage_date = date,
    section = "Main",
    subplot = str_c(plot, section),
    biomass = "alfalfa", # not tracked
    canopeo_id = get_canopeo_id(year = 2023,
                                plot = plot,
                                section = section,
                                coordinate = coordinate,
                                biomass = "XX", # not tracked yet
                                cut = "X"),
    percent_cover = percent_cover
  )

tbl_2023_can_alf <- pre_2023_can_alf |> 
  filter(section == "Main") |>
  select(any_of(canopeo_cols))

# Assemble tables ---------------------------------------------------------


## Core Tables -------------------------------------------------------------
# harvests
tbl_2023_harvests <- bind_rows(
  tbl_2023_c, 
  tbl_2023_wg, 
  tbl_2023_ws, 
  tbl_2023_sb,
  tbl_2023_alf
)

# harvest details
supp_2023_harvests <- bind_rows(
  supp_2023_c, 
  supp_2023_wg, 
  supp_2023_ws, 
  supp_2023_sb,
  supp_2023_alf
)

# Biomassings
tbl_2023_bio <- bind_rows(
  tbl_2023_bio_past,
  tbl_2023_bio_wheat,
  tbl_2023_bio_alf
)

supp_2023_bio <- bind_rows(
  supp_2023_bio_past,
  supp_2023_bio_wheat,
  supp_2023_bio_alf
)

# canopeo
tbl_2023_can <- bind_rows(
  tbl_2023_can_wheat,
  tbl_2023_can_alf
) |> mutate(
  plot = as.numeric(plot)
)

## ei tables ---------------------------------------------------------------

# EI tables
tbl_2023_ei_harvests <- bind_rows(
  tbl_2023_ei_c,
  tbl_2023_ei_wg,
  tbl_2023_ei_ws,
  tbl_2023_ei_sb,
  tbl_2023_ei_cs # silage, but part of EI experiment
)

supp_2023_ei_harvests <- bind_rows(
  supp_2023_ei_c,
  supp_2023_ei_wg,
  supp_2023_ei_ws,
  supp_2023_ei_sb,
  supp_2023_ei_cs
)

# biomassings
tbl_2023_ei_bio <- bind_rows(
  tbl_2023_ei_bio_wheat
)
supp_2023_ei_bio <- bind_rows(
  supp_2023_ei_bio_wheat
)
# biolosses, none

# canopeo
tbl_2023_ei_can <- bind_rows(
  tbl_2023_can_ei_wheat
)
# no supp info


## Silage ------------------------------------------------------------------

# corn silaging exp
tbl_2023_silage <- tbl_2023_cs
supp_2023_silage <- supp_2023_cs

## Prairie -----------------------------------------------------------------

# biofuels
tbl_2023_prairie <- tbl_2023_prairie
supp_2023_prairie <- supp_2023_prairie

## 115 ---------------------------------------------------------------------

tbl_2023_115_prairie <- tbl_2023_115_prairie
supp_2023_115_prairie <- supp_2023_115_prairie # no id yet






# no losses


  



