
source("migration/yield_prep.R")


# corn --------------------------------------------------------------------

raw_2021_c <- xl_snap$`2021_harvests_corn` |> clean_names()

pre_2021_c <- raw_2021_c |>
  filter(!is.na(harvest_lbs)) |> # silaged rows are not included
  mutate(
    plot = plot,
    section = case_match(plot_section, c("Main", "main") ~ "Main", .default = plot_section),
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = "corn"),
    harvestingloss_id  = get_harvestingloss_id(year = 2021, plot = plot, section = section,
                                               product = "corn", cut = 1, loss_num = 1),
    systematicharvestingloss_id = harvestingloss_id,
    harvest_area = area,
    harvest_lbs = harvest_lbs,
    percent_moisture = percent_moisture,
    # losses are treated as systematic in original sheet calculations
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2, # ft2
    assessment_total_area = case_when(plot %in% ei_plots & section == "Main"~12600,
                                      !(plot %in% ei_plots) & section == "Main"~30600,
                                      .default = 30 * 50),
    assessment_loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    loss_fraction = assessment_loss_area / assessment_total_area,
    loss_category = case_when(
      !is.na(ml_harvest_loss_m2) ~ "weeds",
      .default = NA
    ),
    harvest_length = length_ft,
    harvest_width = width_ft,
    comments = if_else(is.na(loss_fraction), stitch_notes(notes, ml_notes), NA),
    loss_comments = if_else(is.na(loss_fraction), NA, stitch_notes(notes, ml_notes))
  )

# harvest
tbl_2021_c <- pre_2021_c |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

supp_2021_c <- pre_2021_c |>
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

# no direct lossees
# sysloss
tbl_2021_sysloss_c <- pre_2021_c |> filter(!is.na(loss_fraction)) |> 
  select(any_of(sysloss_cols))

supp_2021_sysloss_c <- pre_2021_c |> filter(!is.na(loss_fraction)) |> 
  select(any_of(supp_sysloss_cols))

# ei harvest
tbl_2021_ei_c <- pre_2021_c |> 
  filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

supp_2021_ei_c <- pre_2021_c |>
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))

# no ei direct loss
# the only ei loss was corn silage, not in this sheet
# tbl_2021_ei_sysloss_c <- pre_2021_c |>
# filter(section != "Main", !is.na(loss_fraction)) |>
# select(any_of(ei_sysloss_cols))

# soybean -----------------------------------------------------------------

raw_2021_sb <- xl_snap$`2021_harvests_soybean` |> clean_names()

# raw_2021_sb |> names()

pre_2021_sb <- raw_2021_sb |>
  mutate(
    plot = plot,
    section = plot_section,
    subplot = str_c(plot,section),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = "soybean"),
    systematicharvestingloss_id = get_harvestingloss_id(year = 2021,
                                                        plot = plot,
                                                        section = section,
                                                        product = "soybean",
                                                        cut = 1,
                                                        loss_num = 1),
    harvest_date = harvest_date,
    crop = crop,
    harvest_area = area_ft_2,
    harvest_length = length_ft,
    harvest_width = width_ft,
    rrl_id = rrl_id,
    percent_moisture = percent_moisture,
    assessment_total_area = case_when(plot %in% ei_plots & section == "Main"~12600,
                                      !(plot %in% ei_plots) & section == "Main"~30600,
                                      .default = 30 * 50),
    assessment_loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    loss_fraction = assessment_loss_area / assessment_total_area,
    loss_category = if_else(!is.na(ml_harvest_loss_m2), "weeds", NA),
    sysloss_comments = stitch_notes(notes, ml_notes)
  )

#harvest
tbl_2021_sb <- pre_2021_sb |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

supp_2021_sb <- pre_2021_sb |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

# no direct loss
# syslosses
tbl_2021_sysloss_sb <- pre_2021_sb |> 
  drop_na(loss_fraction) |> 
  filter(section == "Main") |> 
  select(any_of(sysloss_cols))

supp_2021_sysloss_sb <- pre_2021_sb |> 
  drop_na(loss_fraction) |> 
  filter(section == "Main") |> 
  select(any_of(supp_sysloss_cols))

# ei plots
tbl_2021_ei_sb <- pre_2021_sb |> 
  filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

supp_2021_ei_sb <- pre_2021_sb |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))

# no losses
# tbl_2021_ei_sysloss_sb <- pre_2021_sb |> 
#   drop_na(loss_fraction) |> 
#   filter(section != "Main") |> 
#   select(any_of(ei_sysloss_cols))
# 
# supp_2021_ei_sysloss_sb <- pre_2021_sb |> 
#   drop_na(loss_fraction) |> 
#   filter(section != "Main") |> 
#   select(any_of(supp_ei_sysloss_cols))


# wheat_grain -------------------------------------------------------------

raw_2021_wg <- xl_snap$`2021_harvests_wheat_grain` |> clean_names()

# raw_2021_wg |> names()

pre_2021_wg <- raw_2021_wg |>
  mutate(
    plot = plot,
    section = case_match(section, "main" ~ "Main", .default= section),
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = 1),
    harvest_date = ml_date,
    plot = plot,
    crop = crop,
    harvest_area = area_ft2,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_weight_lbs,
    rrl_id = as.numeric(na_if(rrl_id_number, "--")),
    percent_moisture = moisture_percent,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = ml_notes
  )

tbl_2021_wg <- pre_2021_wg |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

supp_2021_wg <- pre_2021_wg |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2021_ei_wg <- pre_2021_wg |> 
  filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

supp_2021_ei_wg <- pre_2021_wg |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))

# alfalfa -----------------------------------------------------------------

raw_2021_alf <- xl_snap$`2021_harvests_alfalfa` |> clean_names()

pre_2021_alf <- raw_2021_alf |>
  mutate(
    plot = plot,
    section = "Main",
    cut = cut,
    # call oatlage on first cut, and "alfalfa otherwise"
    crop = case_when(crop == "o/A" & cut == 1 ~ "oatlage",
                     crop == "o/A" & cut == 2 ~ "alfalfa",
                     .default = crop),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvest_date = harvest_date,
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

# harvest

# 1 row for harvesting
raw_2021_past <- xl_snap$`2021_harvests_pasture` |>
  clean_names()
# raw_2021_past |> names()

pre_2021_past <- raw_2021_past |>
  filter(notes == "square bale") |>  # one row removed from plot
  mutate(
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = "main",
                                   product = "pasture",
                                   cut = cut),
    # not relevant here, but its a biomassing
    harvest_date = case_when(lubridate::year(harvest_date) == 2019 ~ as.POSIXct("2021-06-21", tz = "UTC"),
                             .default = harvest_date),
    plot = plot,
    crop = crop,
    harvest_area = plot_area_ft,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_wt_lbs,
    rrl_id = rrl_id,
    num_bales = ml_num_bales,
    percent_moisture = moisture * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, ml_notes)
  )

# raw_2021_past |> names()
pre_2021_bio_past <- raw_2021_past |>
  filter(notes != "square bale") |>  # one row removed, not considered biomassing
  mutate(
    plot = plot,
    cut = cut,
    biomassing_id = get_biomassing_id(year = 2021,
                                      plot = plot,
                                      section = "main",
                                      coordinate = "2", # from yellow binder
                                      biomass = "pasture",
                                      cut = cut),
    # mislabelled date, case in which this moisture was actually using the 2019 moisture, but the date didn't get changed when copied? the yield should actually be 2021
    biomass_date = case_when(lubridate::year(harvest_date) == 2019 ~ as.POSIXct("2021-06-21", tz = "UTC"),
                             .default = harvest_date),
    biomass = crop,
    biomass_area = plot_area_ft,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_grams = plot_wt_lbs / kg_to_lbs * 1000,
    rrl_id = rrl_id,
    wet_weight_no_bag = grab_wet_wt_g,
    dry_weight_no_bag = grab_dry_wt_g,
    percent_moisture = moisture * 100,
    method = "exclosure",
    component = "shoots",
    comments = stitch_notes(notes, ml_notes)
  )

# harvests
tbl_2021_past <- pre_2021_past |> select(any_of(harvesting_cols))
supp_2021_past <- pre_2021_past |> select(any_of(supp_harvesting_cols))

# biomassings
tbl_2021_bio_past <- pre_2021_bio_past |> 
  select(any_of(biomassing_cols))
supp_2021_bio_past <- pre_2021_bio_past |> 
  select(any_of(supp_biomassing_cols))


# corn silage -------------------------------------------------------------

raw_2021_cs <- xl_snap$`2021_harvests_corn_silage_ei` |> clean_names()

# change comment with original value
orig_2021_cs <- xl_snap$`2021_harvests_corn_silage_main` |> clean_names() |> 
  separate_wider_position(plot, widths = c(plot = 3, section = 1)) |> 
  mutate(plot = as.numeric(plot))

pre_2021_cs <- raw_2021_cs |>
  left_join(orig_2021_cs |>
              select(plot, plot_wt_lbs) |>
              mutate(plot_2 = "Main"),
            by = c("plot", "plot_2")) |> 
  mutate(
    plot = plot,
    section = case_match(si_trt,
                         "East"~"East 15'",
                         "West"~"West 15'",
                         .default = plot_2),
    sideplot = str_c(plot, case_match(section,
                                      "West 15'"~"WS",
                                      "East 15'"~"ES")),
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = "corn silage",
                                   cut = 1),
    systematicharvestingloss_id = get_harvestingloss_id(year = 2021,
                                                        plot = plot,
                                                        section = section,
                                                        coordinate = "X",
                                                        product = "corn silage",
                                                        cut = 1,
                                                        loss_num = 1),
    harvest_date = harvest_date,
    crop = "corn silage",
    harvest_length = length_ft,
    harvest_width = case_when(plot_2 == "Main"~5,   # main plots should be 5'
                              plot_2 != "Main"~15), # ei plots are 15'
    harvest_area = harvest_length * harvest_width,
    harvest_lbs = harvest_lbs,
    rrl_id = rrl_id,
    percent_moisture = ml_moisture * 100,
    # instead of joining just manually specify value
    assessment_total_area = if_else(!is.na(area_excluding_potential_loss), 1500, NA),
    assessment_loss_area = if_else(!is.na(area_excluding_potential_loss), 26.59 * m2_to_ft2, NA),
    loss_fraction = assessment_loss_area / assessment_total_area,
    loss_category = if_else(!is.na(area_excluding_potential_loss), "weeds", NA),
    # 109SW rounded incorrectly
    ml_notes = case_when(subplot == "109SW" ~ "Michael Liou: \"harvest acreage crudely rounded resulting in final yield change, changed during db migration .02 -> 0.01721763 (calculated)\"",
                         plot_2 == "Main" ~ glue('Michael Liou: "conflicting data, using handwritten harvest lbs {orig_values}->{new_values} from pdf in db, confirmed and corroborated with Gregg and Mark, plot width also assumed 5\'. Using original moisture values."',
                                                 orig_values = plot_wt_lbs,
                                                 new_values = harvest_lbs),
                         .default = ml_notes),
    comments = stitch_notes(NA, ml_notes), # the general note is incorrect, move to loss comments and modify to be correct
    sysloss_comments = if_else(!is.na(notes), "Lexi Schank: \"corn loss by weed\"", NA)
  )

# silage exp
tbl_2021_cs <- pre_2021_cs |> 
  filter(section %in% c("East 15'", "West 15'")) |> 
  select(any_of(cs_harvesting_cols))
supp_2021_cs <- pre_2021_cs |> 
  filter(section %in% c("East 15'", "West 15'")) |> 
  select(any_of(supp_cs_harvesting_cols))

# ei plots
tbl_2021_ei_cs <- pre_2021_cs |> 
  filter(!(section %in% c("East 15'", "West 15'"))) |> 
  select(any_of(ei_harvesting_cols))
supp_2021_ei_cs <- pre_2021_cs |> 
  filter(!(section %in% c("East 15'", "West 15'"))) |> 
  select(any_of(supp_ei_harvesting_cols))

# ei harvest loss, sytematic
tbl_2021_ei_sysloss_cs <- pre_2021_cs |> 
  drop_na(loss_fraction) |> 
  filter(!(section %in% c("East 15'", "West 15'"))) |> 
  select(any_of(ei_sysloss_cols))
supp_2021_ei_sysloss_cs <- pre_2021_cs |> 
  drop_na(loss_fraction) |> 
  filter(!(section %in% c("East 15'", "West 15'"))) |> 
  select(any_of(supp_ei_sysloss_cols))

dupe_2021_ei_cs <- tbl_2021_ei_cs |> left_join(tbl_2021_ei_sysloss_cs, by = "harvesting_id") |> 
  mutate(harvest_area = harvest_area * (1 - coalesce(loss_fraction, 0))) |> 
  get_yield("corn silage")

dupe_2021_cs <- tbl_2021_cs |> get_yield("corn silage")


# wheat straw -------------------------------------------------------------

raw_2021_ws <- xl_snap$`2021_harvests_wheat_straw` |> clean_names()

# raw_2021_ws |> names()
pre_2021_ws <- raw_2021_ws |>
  mutate(
    plot = plot,
    section = sub_plot,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = "wheat straw",
                                   cut = 2),
    harvest_date = date,
    crop = "wheat straw",
    harvest_area = area_sf,
    harvest_length = length_f,
    harvest_width = width_f,
    harvest_lbs = lbs,
    rrl_id = rrl_id,
    wet_bag_weight = ml_wet_bag,
    dry_bag_weight = ml_dry_bag,
    wet_weight_w_bag = ml_wet_grab_bag_g,
    wet_weight_no_bag = ml_wet_grab_no_bag_g,
    dry_weight_w_bag = ml_dry_grab_bag_g,
    dry_weight_no_bag = ml_dry_grab_no_bag_g,
    grab_bag_moisture = (ml_wet_grab_no_bag_g - (ml_dry_grab_bag_g - ml_wet_bag)) / ml_wet_grab_no_bag_g * 100, # recalculate with wet bag weight, not the dried bag
    percent_moisture = coalesce(ml_rrl_mst, grab_bag_moisture),
    num_bales = ml_num_bales,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(NA, ml_notes)
  )

tbl_2021_ws <-  pre_2021_ws |> 
  filter(section == "main") |> 
  select(any_of(harvesting_cols))

supp_2021_ws <- pre_2021_ws |> 
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2021_ei_ws <-  pre_2021_ws |> 
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))

supp_2021_ei_ws <- pre_2021_ws |> 
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))

# bio/can alfalfa ------------------------------------------------------------

raw_2021_bio_alf <- xl_snap$`2021_anpp_alfalfa` |> clean_names()

# attach previous cuts
cut_2021_bio_alf <-  raw_2021_bio_alf |>
  left_join(tbl_2021_alf |>
              count(plot, name = "prev_cuts"),
            by = "plot") |>
  mutate(plot = plot,
         section = "Main",
         coordinate = case_match(coordinate, "N"~"North", "S"~"South", .default = coordinate),
         cut = prev_cuts + 1)

pre_2021_can_alf <- cut_2021_bio_alf |> 
  mutate(
    coverage_date = date,
    biomass = "alfalfa",
    canopeo_id = get_canopeo_id(year = 2021, plot = plot, section = "Main", coordinate = coordinate,
                                biomass = "alfalfa",
                                cut = cut),
    percent_cover = canopeo_percent
  )

pre_2021_bio_alf <- cut_2021_bio_alf |> 
  pivot_longer(cols = c(matches("^dry_[awr]"), matches("^wet_[awr]")),
               names_pattern = "(wet|dry)_(.*)_g",
               names_to = c(".value", "biomass")) |>
  mutate(
    plot = plot,
    section = "Main",
    coordinate = case_match(coordinate, "N"~"North", "S"~"South", .default = coordinate),
    cut = prev_cuts + 1,
    biomassing_id = get_biomassing_id(year = 2021,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = biomass,
                                      cut = cut),
    biomass_date = date,
    biomass = biomass,
    biomass_length = m_to_ft, # assumed
    biomass_width = m_to_ft, # assumed
    biomass_area = biomass_length * biomass_width,
    biomass_grams = wet,
    wet_bag_weight = if_else(wet == 0, NA, fresh_bag_g), # no bag when no mass
    dry_bag_weight = if_else(wet == 0, NA, dry_bag_g),
    percent_moisture = if_else(wet == 0,
                               NA,
                               100 * ((wet - wet_bag_weight) - (dry - dry_bag_weight)) / (wet - wet_bag_weight)),
    method = "quadrat",
    component = "shoots",
    comments = if_else(!is.na(ml_notes) & biomass == "rye",
                       stitch_notes(NA, ml_notes),
                       NA) # all NA just 1 note for selected
  )

tbl_2021_bio_alf <- pre_2021_bio_alf |> select(any_of(biomassing_cols))
supp_2021_bio_alf <- pre_2021_bio_alf |> select(any_of(supp_biomassing_cols))
tbl_2021_can_alf <- pre_2021_can_alf |> select(any_of(canopeo_cols))
# no canopeo comments
# no ei plots


# bio/can rye -----------------------------------------------------------------

raw_2021_ei_bio_rye <- xl_snap$`2021_anpp_rye` |> clean_names()

# the cut # doesn't really make sense but set it to 2 anyway since only 1 previous harvest, not sure about previous biomassings
# raw_2021_bio_rye |> mutate(
#   subplot = str_c(plot, si)
# ) |> left_join(tbl_2021_ei_harvests |> count(subplot), by = "subplot") |>
#   select(subplot, n) |> arrange(desc(n))


pre_2021_ei_can_rye <- raw_2021_ei_bio_rye |>
  mutate(
    plot = plot,
    section = si,
    coordinate = coordinate,
    subplot = str_c(plot, section),
    coverage_date = date,
    percent_cover = canopeo_percent,
    biomass = "rye", 
    canopeo_id = get_canopeo_id(year = 2021, plot = plot, section = section, coordinate = coordinate,
                                biomass = biomass,
                                cut = 2)
  )

pre_2021_ei_bio_rye <- raw_2021_ei_bio_rye |>
  pivot_longer(residue_g:weeds_g, names_pattern = "(.*)_g",
                                 names_to = "biomass",
                                 values_to = "biomass_grams") |> 
  drop_na(biomass_grams) |> 
  mutate(
    plot = plot,
    section = si,
    subplot = str_c(plot, section),
    coordinate = coordinate,
    biomass = biomass,
    biomass_date = date,
    cut = 2,
    biomassing_id = get_biomassing_id(year = 2021, plot = plot, section = section, coordinate = coordinate,
                                      biomass = biomass, cut = cut),
    method = "quadrat",
    component = "shoots",
    biomass_length = m_to_ft,
    biomass_width = m_to_ft,
    biomass_area = m2_to_ft2,
    percent_moisture = if_else(biomass_grams != 0, 0, NA, NA),
    comments = if_else(!is.na(ml_notes) & biomass_grams == 0,
                       ml_notes,
                       NA)
  )

tbl_2021_ei_can_rye <- pre_2021_ei_can_rye |> select(any_of(ei_canopeo_cols))
tbl_2021_ei_bio_rye <- pre_2021_ei_bio_rye |> select(any_of(ei_biomassing_cols))
supp_2021_ei_bio_rye <- pre_2021_ei_bio_rye |> select(any_of(supp_ei_biomassing_cols))

# bio/can clover ----------------------------------------------------------
raw_2021_ei_bio_clover <- xl_snap$`2021_anpp_clover` |> clean_names()

common_2021_ei_bio_clover <- raw_2021_ei_bio_clover |> 
  mutate(
    plot = section,
    section = si,
    coordinate = coordinate,
    subplot = str_c(plot, section)
  )

# just 1 previous cut for all
# tbl_2021_ei_harvests |> filter(subplot %in% unique(common_2021_ei_bio_clover$subplot)) |> count(subplot)
pre_2021_ei_can_clover <- common_2021_ei_bio_clover |> 
  mutate(
    coverage_date = date,
    percent_cover = canopeo,
    biomass = "red clover",
    canopeo_id = get_canopeo_id(year = 2021, plot = plot, section = section, coordinate = coordinate,
                                biomass = biomass,
                                cut = 2)
  )

pre_2021_ei_bio_clover <- common_2021_ei_bio_clover |>
  pivot_longer(residue:weeds,
               names_to = "biomass",
               values_to = "biomass_grams") |> 
  drop_na(biomass_grams) |> 
  mutate(
    biomass = biomass,
    biomass_date = date,
    cut = 2, 
    biomassing_id = get_biomassing_id(year = 2021, plot = plot, section = section, coordinate = coordinate,
                                      biomass = biomass, cut = cut),
    method = "quadrat",
    component = "shoots",
    biomass_length = m_to_ft,
    biomass_width = m_to_ft,
    biomass_area = m2_to_ft2,
    percent_moisture = if_else(biomass_grams != 0, 0, NA, NA) # assume all dry
  )

tbl_2021_ei_can_clover <- pre_2021_ei_can_clover |> select(any_of(ei_canopeo_cols))
# pre_2021_ei_can_clover |> select(any_of(supp_ei_canopeo_cols)) # nothing
tbl_2021_ei_bio_clover <- pre_2021_ei_bio_clover |> select(any_of(ei_biomassing_cols))
supp_2021_ei_bio_clover <- pre_2021_ei_bio_clover |> select(any_of(supp_ei_biomassing_cols))

# prairie -----------------------------------------------------------------

# join measurements
raw_2021_prairie_weights <- xl_snap$`2021_harvests_prairie_wicst` |> clean_names() |>
  mutate(
    section = str_to_lower(subplot)
  )

# join two sheets: harvests and measurements
raw_2021_prairie_lengths <- xl_snap$`2021_prairie_wicst_plots` |> clean_names() |>
  mutate(main_width = 19, #TODO: 19?
         micro_width = 10, #TODO: 10?
         main_length = main_length,
         micro_length= micro_length) |> 
  select(plot,matches("^(main|micro)_(width|length)")) |>
  pivot_longer(cols = main_length:micro_width,
               names_sep = "_",
               names_to = c("section", ".value")) 

pre_2021_prairie <- raw_2021_prairie_lengths |>
  left_join(raw_2021_prairie_weights, by = c("plot", "section")) |> 
  # common column names
  mutate(
    plot = plot,
    section = case_match(section,
                         "main"~"Macro",
                         "micro"~"Micro"),
    harvesting_id = get_harvest_id(year = 2021,
                                   plot = plot,
                                   section = section,
                                   product = "prairie",
                                   cut = 1),
    crop = case_match(plot,
                      c(501, 504, 508)~"switchgrass",
                      c(502, 506, 509)~"high diversity prairie",
                      c(503, 505, 507)~"low diversity prairie"),
    harvest_date = as.POSIXct("2021-11-08", tz = "UTC"), #TODO: 11/8/2021 from agcal? verify
    fuel_plot = str_c(plot, section),
    harvest_length = length,
    harvest_width = width,
    harvest_area = harvest_length * harvest_width,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    dry_weight_w_bag = bag_dry_weight_g,
    percent_moisture = (wet_weight_w_bag - dry_weight_w_bag) / (wet_weight_w_bag - bag_weight) * 100,
    harvest_lbs = plot_weight_lbs
  )

tbl_2021_prairie <- pre_2021_prairie |> select(any_of(fuel_harvesting_cols))
supp_2021_prairie <- pre_2021_prairie |> select(any_of(supp_fuel_harvesting_cols))


# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2021_harvests <- bind_rows(
  tbl_2021_c,
  tbl_2021_sb,
  tbl_2021_wg,
  tbl_2021_alf,
  tbl_2021_past,
  tbl_2021_ws
)

supp_2021_harvests <- bind_rows(
  supp_2021_c,
  supp_2021_sb,
  supp_2021_wg,
  supp_2021_alf,
  supp_2021_past,
  supp_2021_ws
)

# biomassings
tbl_2021_bio <- bind_rows(
  tbl_2021_bio_alf,
  tbl_2021_bio_past
)
supp_2021_bio <- bind_rows(
  supp_2021_bio_alf,
  supp_2021_bio_past
)

# canopeo
tbl_2021_can <- bind_rows(
  tbl_2021_can_alf
)
supp_2021_can <- bind_rows(
  # supp_2021_can_alf, # empty
)

# losses
tbl_2021_loss <- bind_rows()
supp_2021_loss <- bind_rows()
tbl_2021_sysloss <- bind_rows(
  tbl_2021_sysloss_c,
  tbl_2021_sysloss_sb
)
supp_2021_sysloss <- bind_rows(
  supp_2021_sysloss_c,
  supp_2021_sysloss_sb
)

## EI ----------------------------------------------------------------------

# harvests
tbl_2021_ei_harvests <- bind_rows(
  tbl_2021_ei_c,
  tbl_2021_ei_sb,
  tbl_2021_ei_wg,
  tbl_2021_ei_ws,
  tbl_2021_ei_cs
)
supp_2021_ei_harvests <- bind_rows(
  supp_2021_ei_c,
  supp_2021_ei_sb,
  supp_2021_ei_wg,
  supp_2021_ei_ws,
  supp_2021_ei_cs
)

# biomassings
tbl_2021_ei_bio <- bind_rows(
  tbl_2021_ei_bio_rye,
  tbl_2021_ei_bio_clover
)
supp_2021_ei_bio <- bind_rows(
  supp_2021_ei_bio_rye,
  supp_2021_ei_bio_clover
)

# canopeo
tbl_2021_ei_can <- bind_rows(
  tbl_2021_ei_can_rye,
  tbl_2021_ei_can_clover
)
supp_2021_ei_can <- bind_rows(
  # both empty
)

# losses
tbl_2021_ei_loss <- bind_rows()
supp_2021_ei_loss <- bind_rows()
tbl_2021_ei_sysloss <- bind_rows(
  tbl_2021_ei_sysloss_cs
)
supp_2021_ei_sysloss <- bind_rows(
  supp_2021_ei_sysloss_cs
)

## Silage ------------------------------------------------------------------

# harvests
tbl_2021_silage <- bind_rows(
  tbl_2021_cs
)
supp_2021_silage <- bind_rows(
  supp_2021_cs
)

## Biofuel -----------------------------------------------------------------

# harvests
tbl_2021_prairie <- tbl_2021_prairie
supp_2021_prairie <- supp_2021_prairie









