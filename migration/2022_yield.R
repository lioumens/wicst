source("migration/yield_prep.R")


# corn --------------------------------------------------------------------

raw_2022_c <- xl_snap$`2022_harvests_corn_corrected` |>
  clean_names()

raw_2022_c |> names()

pre_2022_c <- raw_2022_c |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    subplot = str_c(plot, plot_section),
    section = plot_section,
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot,
                                   section = section,
                                   product = "corn"),
    harvest_area = area_ft_2,
    harvest_lbs = harvest_lbs,
    percent_moisture = percent_moisture,
    rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2022_c <- pre_2022_c |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

tbl_2022_ei_c <- pre_2022_c |> 
  filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

dupe_2022_c <- pre_2022_c |> get_yield("corn")
dupe_2022_ei_c <- tbl_2022_ei_c |> get_yield("corn")

supp_2022_c <- pre_2022_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

supp_2022_ei_c <- pre_2022_c |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols)) |> 
  select(where(\(x) !all(is.na(x)))) # drop columns of all NA

# Corn Loss ---------------------------------------------------------------

raw_2022_loss_c <- xl_snap$`2022_harvests_corn_loss` |> clean_names()

# unique(c(loss_cols, supp_loss_cols))
# raw_2022_loss_c |> names()

pre_2022_loss_c <- raw_2022_loss_c |> mutate(
  section_old = section,
  section = case_when(main_si == "Main" ~ "Main",
                      .default = section),
  subplot = str_c(plot, section)) |> 
  # number the losses
  mutate(loss_num = row_number(),
         .by = subplot) |>
  # attach to common columns
  mutate(
    harvestingloss_id = get_harvestingloss_id(year = 2022,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X", 
                                              product = "corn",
                                              cut = 1,
                                              site = "A",
                                              loss_num = loss_num),
    # just number them the same across both loss tables, will still be unique by loss num
    systematicharvestingloss_id = harvestingloss_id,
    assessment_loss_length = length_ft,
    assessment_loss_width = width_ft,
    assessment_loss_rows = number_rows_wide,
    assessment_loss_area = length_ft * width_ft,
    assessment_total_length = case_when(!(plot %in% ei_plots)~510,
                                        main_si == "SI" ~ 50,
                                        .default = 210),
    assessment_total_width = case_when(main_si == "SI" ~ 30,
                                       .default = 60),
    assessment_total_area = assessment_total_width * assessment_total_length,
    sysloss_location = if_else(section != "Main", NA, section_old),
    sysloss_comments = "Lexi Schank: \"weed, cultivator blight, other confounding\"",
    loss_category = "weeds"
  ) |> 
  # join harvest table for id and harvest area
  left_join(pre_2022_c |> select(harvesting_id, subplot, harvest_area), by = "subplot") |> 
  mutate(loss_area = get0("loss_width", ifnotfound = NA) * get0("loss_length", ifnotfound = NA), # direct loss if available
         loss_fraction = assessment_loss_area / assessment_total_area) # proportional loss

# Direct loss table 
# empty
# tbl_2022_loss_c <- pre_2022_loss_c |> 
#   filter(section == "Main") |> 
#   select(any_of(loss_cols)) |> drop_na(loss_area)
# 
# supp_2022_loss_c <- pre_2022_loss_c |>
#   filter(section == "Main") |> 
#   select(any_of(supp_loss_cols)) |>
#   filter(harvestingloss_id %in% tbl_2022_loss_c$harvestingloss_id)

# Systematic
tbl_2022_sysloss_c <- pre_2022_loss_c |>
  filter(section == "Main") |> 
  select(any_of(sysloss_cols)) |> 
  drop_na(loss_fraction)
supp_2022_sysloss_c <- pre_2022_loss_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_sysloss_cols)) |>
  filter(systematicharvestingloss_id %in% tbl_2022_sysloss_c$systematicharvestingloss_id)

# EI
# no direct loss
# tbl_2022_ei_loss_c <- pre_2022_loss_c |> 
#   filter(section != "Main") |> 
#   select(any_of(loss_cols)) |> drop_na(loss_area)
# supp_2022_ei_loss_c <- pre_2022_loss_c |>
#   filter(section != "Main") |> 
#   select(any_of(supp_ei_loss_cols)) |>
#   filter(harvestingloss_id %in% tbl_2022_loss_c$harvestingloss_id)

tbl_2022_ei_sysloss_c <- pre_2022_loss_c |>
  filter(section != "Main") |> 
  select(any_of(ei_sysloss_cols)) |> 
  drop_na(loss_fraction)

supp_2022_ei_sysloss_c <- pre_2022_loss_c |>
  filter(section != "Main") |> 
  select(any_of(supp_ei_sysloss_cols)) |>
  filter(systematicharvestingloss_id %in% tbl_2022_ei_sysloss_c$systematicharvestingloss_id)


dupe_2022_c <- tbl_2022_c |> 
  left_join(
    tbl_2022_sysloss_c |> summarize(total_fraction_loss = sum(loss_fraction), .by = harvesting_id),
    by = "harvesting_id") |>
  mutate(harvest_area = harvest_area - coalesce(harvest_area * total_fraction_loss, 0)) |>
  get_yield("corn")
# matches!

# wheat grain -------------------------------------------------------------

raw_2022_wg <- xl_snap$`2022_harvests_wheat_grain` |> clean_names()
# raw_2022_wg |> names()


pre_2022_wg <- raw_2022_wg |> 
  mutate(
    harvest_date = harvest_date,
    section = subplot,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2022, 
                                   plot = plot,
                                   section = section,
                                   cut = 1,
                                   product = str_to_lower(crop)),
    harvest_lbs = harvest_weight_lbs,
    harvest_area = harvested_area_ft2,
    percent_moisture = percent_moisture,
    harvest_width = plot_width_ft,
    harvest_length = plot_length_ft,
    comments = ml_notes
  )

tbl_2022_wg <- pre_2022_wg |> 
  filter(section == "Main") |>
  select(any_of(harvesting_cols))

supp_2022_wg <- pre_2022_wg |>
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2022_ei_wg <- pre_2022_wg |> 
  filter(section != "Main") |>
  select(any_of(ei_harvesting_cols))

supp_2022_ei_wg <- pre_2022_wg |> 
  filter(section != "Main") |>
  select(any_of(supp_ei_harvesting_cols))

dupe_2022_wg <- tbl_2022_wg |> get_yield("wheat")
dupe_2022_ei_wg <- tbl_2022_ei_wg |>
  mutate(crop = case_match(crop,
                           "Wheat"~"wheat straw",
                           "Barley"~"barley")) |> 
  get_yield()

# wheat/barley straw -------------------------------------------------------------------

temp_2022_ws <- xl_snap$`2022_harvests_straw_main` |> clean_names()
temp_2022_ei_ws <- xl_snap$`2022_harvests_straw_ei` |> clean_names()

raw_2022_ws <- bind_rows(temp_2022_ws, temp_2022_ei_ws)

# raw_2022_ws$biomass
# need to join ei tables to get plot section
# xl_core$subplots
pre_2022_ws <- raw_2022_ws |> 
  # attach section information to ei plots, first masssage into right format
  mutate(
    treatment = case_match(sub_plot,
                           "C"~"CT",
                           .default = sub_plot),
    tmp_plot = str_c("A", plot),
    .after = plot) |> 
  left_join(xl_core$subplots,
            by = join_by("tmp_plot" == "plot",
                         "treatment" == "treatment")) |> 
  # separate product info
  separate_wider_regex(biomass, c(crop = ".*", "\\s*,\\s*", crop_description = ".*"),
                       cols_remove = FALSE,
                       too_few = "align_start") |> 
  mutate(
    section = coalesce(str_sub(subplot, 4), "Main"),
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot,
                                   section = section,
                                   cut = 2, # after wheat grain
                                   product = "wheat straw"),
    crop = str_to_lower(crop),
    harvest_date = date,
    harvest_area = area_ft_2,
    harvest_lbs = plot_wt_lbs,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    wet_weight_no_bag = wet_wt_g_no_bag,
    dry_weight_w_bag = bag_dry_weight_g,
    dry_weight_no_bag = dry_wt_g_no_bag,
    percent_moisture = percent_mst * 100,
    harvest_length = length_ft,
    harvest_width = width_ft,
    wagon_weight = ml_wagon_weight,
    wagon_color = ml_wagon_color,
    wet_bag_weight = bag_weight_g,
    comments = stitch_notes(biomass, ml_notes)) # biomass is basically notes

tbl_2022_ws <- pre_2022_ws |>
  filter(treatment == "main") |>
  select(any_of(harvesting_cols))

tbl_2022_ei_ws <- pre_2022_ws |>
  filter(treatment != "main") |>
  select(any_of(ei_harvesting_cols))

supp_2022_ws <- pre_2022_ws |>
  filter(treatment == "main") |>
  select(any_of(supp_harvesting_cols))

supp_2022_ei_ws <- pre_2022_ws |>
  filter(treatment != "main") |>
  select(any_of(supp_ei_harvesting_cols))

dupe_2022_ws <- tbl_2022_ws |> get_yield() # missing barley straw yield
dupe_2022_ei_ws <- tbl_2022_ei_ws |> get_yield() # missing barley straw yield
# matches!

# soybean -----------------------------------------------------------------

raw_2022_sb <- xl_snap$`2022_harvests_soybean` |> clean_names()
raw_2022_sb |> names()
pre_2022_sb <- raw_2022_sb |> 
  mutate(
    plot = plot,
    harvest_date = harvest_date,
    section = plot_section,
    subplot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot,
                                   section = section, 
                                   cut = 1,
                                   product = "soybean"),
    harvest_lbs = harvest_lbs,
    percent_moisture = percent_moisture,
    harvest_length = length_ft,
    harvest_width = width_ft,
    rrl_id = rrl_id,
    comments = ml_notes,
    harvest_area = area_ft_2)

tbl_2022_sb <- pre_2022_sb |>
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

tbl_2022_ei_sb <- pre_2022_sb |>
  filter(section != "Main") |> 
  select(any_of(ei_harvesting_cols))

supp_2022_sb <- pre_2022_sb |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

supp_2022_ei_sb <- pre_2022_sb |>
  filter(section != "Main") |> 
  select(any_of(supp_ei_harvesting_cols))

dupe_2022_sb <- tbl_2022_sb |> get_yield("soybean")
dupe_2022_ei_sb <- tbl_2022_ei_sb |> get_yield("soybean")

# Alfalfa Hay ---------------------------------------------------------------------

# this table is duplicated in the alfalfa table, it's first two cuts
# raw_2022_hay <- xl_snap$`2022_harvests_hay` |> clean_names()
# raw_2022_hay |> separate_wider_delim(field, names = c("field", "plot"), "-") |>
#   mutate(harvest_lbs = total_weight_in_tons) |>
#   select(plot, total_weight_in_tons) |>
#   group_by(plot) |>
#   summarize(all_weights = paste(total_weight_in_tons, collapse = ","))

raw_2022_alfalfa <- xl_snap$`2022_harvests_alfalfa` |> clean_names()
# raw_2022_alfalfa |> names()

pre_2022_alfalfa <- raw_2022_alfalfa |> 
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    section = "Main",
    cut = cut,
    crop = crop,
  harvesting_id = get_harvest_id(year = 2022, 
                                 plot = plot, 
                                 section = section,
                                 product = "alfalfa",
                                 cut = cut),
  rrl_id = rrl_id,
  trailer_weight = ml_trailer_weight,
  harvest_area = plot_area_ft_2,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_lbs = plot_wt_lbs,
  percent_moisture = moisture_rrl * 100,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2022_alfalfa <- pre_2022_alfalfa |> 
  select(any_of(harvesting_cols))

supp_2022_alfalfa <- pre_2022_alfalfa |> 
  select(any_of(supp_harvesting_cols))

dupe_2022_alfalfa <- tbl_2022_alfalfa |> get_yield("alfalfa")


# corn silage -------------------------------------------------------------
raw_2022_cs <- xl_snap$`2022_harvests_corn_silage` |> clean_names()

# raw_2022_cs |> names()

pre_2022_cs <- raw_2022_cs |> 
  separate_wider_delim(plot, delim = "-", names = c("plot", "trt"), too_few = "align_start") |>
  mutate(
    harvest_date = harvest_date,
    plot = plot,
    section = case_when(section == "main" ~ si_trt,
                        .default = section),
    subplot = str_c(plot, section),
    sideplot = str_c(plot, case_match(section,
                                      "West 15'"~"WS",
                                      "East 15'"~"ES")),
    crop = "corn silage",
    plot = as.numeric(plot),
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot, 
                                   section = section,
                                   product = "corn silage",
                                   cut = 1),
    harvest_area = plot_area_ft_2,
    harvest_lbs = harvest_lbs,
    rrl_id = rrl_id,
    harvest_width = width_ft,
    harvest_length = length_ft,
    percent_moisture = as.numeric(str_replace(moisture_rrl, "%$", "")),
    comments = ml_notes
  )

tbl_2022_cs <- pre_2022_cs |> 
  filter(is.na(trt)) |> 
  select(any_of(cs_harvesting_cols))

tbl_2022_ei_cs <- pre_2022_cs |> 
  filter(!is.na(trt)) |> 
  select(any_of(ei_harvesting_cols)) # part of ei harvests

supp_2022_cs <- pre_2022_cs |> 
  filter(is.na(trt)) |> 
  select(any_of(supp_cs_harvesting_cols))

supp_2022_ei_cs <- pre_2022_cs |> 
  filter(!is.na(trt)) |> 
  select(any_of(supp_ei_harvesting_cols)) # part of ei harvests

dupe_2022_cs <- tbl_2022_cs |> get_yield("corn silage")
dupe_2022_ei_cs <-  tbl_2022_ei_cs |>  get_yield("corn silage")


# pasture -----------------------------------------------------------------

raw_2022_past <- xl_snap$`2022_harvests_pasture` |> clean_names()

# raw_2022_past |> names()

pre_2022_past <- raw_2022_past |> 
  mutate(
    plot = plot,
    biomass_date = harvest_date,
    biomass = "pasture",
    method = "exclosure",
    component = "shoots",
    cut = cut,
    # 207 switched to 3 in 2023, and 4 in 2022 everything else same
    coordinate = if_else(plot == 207,
                         "paddock 4", "paddock 3"),
    biomassing_id = get_biomassing_id(year = 2022,
                                   plot = plot,
                                   section = "Main",
                                   biomass = "pasture",
                                   coordinate = coordinate,
                                   cut = cut),
    rrl_id = rrl_id,
    biomass_area = plot_area_ft,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_grams = plot_wt_lbs / kg_to_lbs * 1000,
    percent_moisture = moisture * 100,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2022_bio_past <- pre_2022_past |> 
  select(any_of(biomassing_cols))

supp_2022_bio_past <- pre_2022_past |> 
  select(any_of(supp_biomassing_cols))

dupe_2022_bio_past <- tbl_2022_bio_past |> get_biomass()

# rye oat ei biomassing --------------------------------------------------------

raw_2022_bio_r_o <- xl_snap$`2022_anpp_rye_oat` |> clean_names()

pre_2022_anpp_r_o <- raw_2022_bio_r_o |>
  mutate(
    plot = str_sub(sub_plot, end = 3),
    subplot = str_remove(sub_plot, "-"),
    section = str_extract(sub_plot, "-(.*)$", 1), # get part after -
    biomass_date = date_collected,
    biomass_area = m2_to_ft2,
    biomass_width = m_to_ft,
    biomass_length = m_to_ft,
    coordinate = NA,
    cut = 1
  ) |>
  pivot_longer(cols = rye_dm_g:residue_dm_g, names_to = "biomass", values_to = "biomass_grams") |> 
  separate_wider_delim(biomass, names = c("biomass", NA, NA), delim = "_") |>
  filter(!is.na(biomass_grams)) |>
  mutate(
    method = "quadrat",
    component = "shoots",
    biomassing_id = get_biomassing_id(year = 2022, plot = plot,
                                           section = section, 
                                           coordinate = "X",
                                           biomass = biomass, cut = 1))

tbl_2022_bio_ei_r_o <- pre_2022_anpp_r_o |> 
  select(any_of(ei_biomassing_cols)) |> 
  select(where(~!all(is.na(.x))))

supp_2022_bio_ei_r_o <- pre_2022_anpp_r_o |> 
  select(any_of(supp_ei_biomassing_cols))


# rye_oat_canopeo ---------------------------------------------------------

raw_2022_can_ei_r_o <- xl_snap$`2022_anpp_rye_oat_canopeo` |> clean_names()

# join for bio mass?

plot_trts <- xl_core$plots |> left_join(xl_core$treatments |>
                             select(treatment_id, system_id),
                           by = "treatment_id")
subplot_trts <- xl_core$subplots |> left_join(plot_trts |> select(plot_id, system_id),
                                              by = join_by("plot" == "plot_id"))

# e1 is oats?
pre_2022_can_ei_r_o <- raw_2022_can_ei_r_o |> 
  separate_wider_delim(plot, names = c("plot", "section"), delim = " ") |> 
  mutate(
    coverage_date = date_collected,
    subplot = str_c(plot, section),
  ) |> left_join(subplot_trts |>
                   select(-plot), by = "subplot") |> 
  mutate( 
    biomass = case_when((system_id == 3 & treatment %in% c("E1", "E4")) | 
                          (system_id == 1 & treatment %in% c("E1")) ~ "oat",
                        .default = "rye")) |> 
  pivot_longer(north:south, names_to = "coordinate",
               values_to = "percent_cover") |> 
  mutate(canopeo_id = get_canopeo_id(year = 2022,
                                     plot = plot,
                                     section = section,
                                     coordinate = coordinate,
                                     biomass = biomass,
                                     cut = "X"))

# canopeo_id = get_canopeo_id(),
tbl_2022_can_ei_r_o <- pre_2022_can_ei_r_o |> 
  select(any_of(ei_canopeo_cols))

# supp_ei_canopeo_cols not defined yet
# supp_2022_can_ei_r_o <- pre_2022_can_ei_r_o|> 
#   select(any_of(supp_ei_canopeo_cols))

# alfalfa anpp ------------------------------------------------------------

raw_2022_bio_alfalfa <- xl_snap$`2022_anpp_alfalfa` |> clean_names()

# raw_2022_anpp_alfalfa$quadrat_size |> str_extract("\\d") |>
# raw_2022_anpp_alfalfa |> names()
anpp_spec <- raw_2022_bio_alfalfa |>
  build_longer_spec(cols = dry_bag_weight_g_large_bag:weeds_dm_g,
                    names_to = "biomass") |> 
  mutate(.value = rep(c("bag_weight", "with_bag", "biomass_grams"), each = 2),
         biomass = rep(c("alfalfa", "weeds"), 3))

pre_2022_bio_alfalfa <- raw_2022_bio_alfalfa |> 
  mutate(
    plot = plot,
    section = "Main",
    biomass_date = date,
    biomass_area = as.numeric(str_extract(quadrat_size, "\\d")) * m2_to_ft2,
    biomass_width = m_to_ft,
    biomass_length = m_to_ft,
  ) |> 
  # pivot 2 by 2 to get alfalfa cut, this biomassing was after all the cuts
  pivot_longer_spec(anpp_spec) |> 
  left_join(tbl_2022_alfalfa |> count(plot, name = "num_cuts"), by = "plot") |> 
  mutate(
    dry_weight_w_bag = with_bag,
    bag_weight = bag_weight,
    cut = num_cuts + 1,
    method = "quadrat",
    component = "shoots",
    biomassing_id = get_biomassing_id(year = 2022,
                                           plot = plot, section = "Main",
                                           biomass = biomass,
                                           cut = cut,
                                           coordinate = coordinate)) 

tbl_2022_bio_alfalfa <- pre_2022_bio_alfalfa |>
  select(any_of(biomassing_cols))

supp_2022_bio_alfalfa <- pre_2022_bio_alfalfa |> 
  select(any_of(supp_biomassing_cols))

dupe_2022_bio_alfalfa <- tbl_2022_bio_alfalfa |> get_biomass(biomass = "alfalfa")


# Alfalfa Canopeo ---------------------------------------------------------

raw_2022_can_alfalfa <- xl_snap$`2022_anpp_alfalfa_canopeo` |> clean_names()

pre_2022_can_alfalfa <- raw_2022_can_alfalfa |> 
  # to add the cut, same as the biomassing
  left_join(tbl_2022_alfalfa |> count(plot, name = "num_cuts"), by = "plot") |> 
  mutate(
    plot = plot,
    section = "Main",
    coverage_date = date,
    coordinate = coordinate,
    biomass = "alfalfa",
    percent_cover =  canopeo_percent,
    cut = num_cuts + 1,
    canopeo_id = get_canopeo_id(year = 2022,
                                plot = plot,
                                section = section,
                                coordinate = coordinate,
                                biomass = biomass,
                                cut = cut))

tbl_2022_can_alfalfa <- pre_2022_can_alfalfa |> 
  select(any_of(canopeo_cols))

# cs3 ---------------------------------------------------------------------

raw_2022_bio_o <- xl_snap$`2022_anpp_cs3` |> clean_names()

# raw_2022_anpp_o |> names()
spec_2022_bio_o <- raw_2022_bio_o |> 
  build_longer_spec(cols = oats_g_large_bag_dry_weight:residue_dm_g,
                    names_to = "biomass") |>
  mutate(.value = rep(c("with_bag", "biomass_grams"), each = 5), # which column
         biomass = rep(c("oats", "berseem_clover", "red_clover", "weeds", "residue"), 2))

pre_2022_bio_o <- raw_2022_bio_o |>
  # residue assumed to be large bag weight when missing, 45.2
  replace_na(replace = list(residue_large_bag_dry_weight = 45.2,
                            residue_dm_g = 0)) |> 
  pivot_longer_spec(spec_2022_bio_o) |>
  filter(!is.na(biomass_grams)) |> 
  mutate(
    section = case_when(
      section %in% c("Main", "MAIN") ~ "Main",
      .default = coordinate
    ),
    coordinate = case_when(
      !(section %in% c("Main", "MAIN")) ~ NA,
      .default = str_to_lower(coordinate)
    ),
    bag_weight = case_when(
      is.na(with_bag)~NA,
      biomass == "residue" & plot == "313" & section == "E2" & coordinate == "SE" ~ 27.2,
      biomass == "weeds" ~ 27.2,
      biomass %in% c("oats", "berseem_clover", "red_clover", "residue")~45.2),
    comments = case_when(
      biomass %in% c("oats", "berseem_clover", "red_clover", "weeds") &
        plot == "313" &
        section == "E2" &
        coordinate == "SE" ~ NA,
      .default = notes
    ),
    cut = 3, # wg -> ws -> anpp
    method = "quadrat",
    component = "shoots",
    biomassing_id = get_biomassing_id(year = 2022, 
                                      plot = plot, 
                                      section = section, 
                                      biomass = biomass, 
                                      coordinate = coordinate,
                                      cut = cut),
    biomass_date = date,
    biomass_area = m2_to_ft2,
    biomass_length = m_to_ft,
    biomass_width = m_to_ft)

tbl_2022_bio_o <- pre_2022_bio_o |> 
  filter(section %in% c("Main", "MAIN")) |> 
  select(any_of(biomassing_cols))

supp_2022_bio_o <- pre_2022_bio_o |> 
  filter(section %in% c("Main", "MAIN")) |> 
  select(any_of(supp_biomassing_cols))

tbl_2022_bio_ei_o <- pre_2022_bio_o |> 
  filter(!(section %in% c("Main", "MAIN"))) |> 
  select(any_of(ei_biomassing_cols)) |> 
  select(where(~!all(is.na(.x))))

supp_2022_bio_ei_o <- pre_2022_bio_o |> 
  filter(!(section %in% c("Main", "MAIN"))) |> 
  select(any_of(supp_ei_biomassing_cols))

dupe_2022_bio_o <- tbl_2022_bio_o |> get_biomass()
dupe_2022_bio_ei_o <- tbl_2022_bio_ei_o |> get_biomass()


# cs3 canopeo --------------------------------------------------------------

raw_2022_can_o <- xl_snap$`2022_anpp_cs3_canopeo` |> clean_names()

biomass_list_by_plot <- pre_2022_bio_o |> select(biomass_date, plot, section, biomass) |>
  filter(biomass != "residue") |>
  distinct() |>
  summarize(biomass_string = str_flatten_comma(biomass,last = " and "),
            .by = c(plot, section))

pre_2022_can_o <- raw_2022_can_o |> 
  mutate(
    plot = plot,
    section = coalesce(si_coordinate, str_to_title(section)),
    subplot = str_c(plot, section),
  ) |> 
  # attach dates from the anpp table
  left_join(pre_2022_bio_o |> select(biomass_date, plot, section) |> distinct(),
            by = c("plot", "section")) |>
  # other common canopeo cols
  mutate(
    coordinate = location_in_plot,
    coverage_date = biomass_date,
  ) |> 
  # get biomassing list
  left_join(biomass_list_by_plot, by = c("plot", "section")) |>
  mutate(
    biomass = biomass_string,
    percent_cover =  canopeo_percent,
    cut = 3, # wg -> ws -> anpp
    canopeo_id = get_canopeo_id(year = 2022,
                                plot = plot,
                                section = section,
                                coordinate = coordinate,
                                biomass = biomass,
                                cut = cut),
    comments = stitch_notes(notes, 
                            ml_notes = "Michael Liou: \"Canopeo cover assumed measured same date as ANPP\""))

tbl_2022_can_o <- pre_2022_can_o |> 
  filter(section == "Main") |>
  select(any_of(canopeo_cols))

tbl_2022_can_ei_o <- pre_2022_can_o |> 
  filter(section != "Main") |>
  select(any_of(ei_canopeo_cols))

supp_2022_can_o <- pre_2022_can_o |> 
  filter(section == "Main") |>
  select(any_of(supp_canopeo_cols))

supp_2022_can_ei_o <- pre_2022_can_o |> 
  filter(section != "Main") |>
  select(any_of(supp_ei_canopeo_cols))


# WICST Pairie ------------------------------------------------------------

raw_2022_prairie <- xl_snap$`2022_harvests_prairie_wicst` |> clean_names()

pre_2022_prairie <- raw_2022_prairie |>
  separate_wider_delim(treatment, names = c("crop", "treatment"), delim = " ") |>
  mutate(
    harvest_date = date_harvested,
    plot = plot,
    section = subplot |> case_match("Main"~"Macro",
                                    "Micro"~"Micro"),
    crop = crop,
    fuel_plot = str_c(plot, section),
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot,
                                   section = section,
                                   product = "prairie",
                                   cut = "1"),
    # percent_moisture = percent_mst * 100,
    harvest_lbs = plot_weight_lbs,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    dry_weight_w_bag = bag_dry_weight_g,
    percent_moisture = (wet_weight_w_bag - dry_weight_w_bag) / (wet_weight_w_bag - bag_weight) * 100,
    # wet_weight_no_bag = wet_wt_g_no_bag,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    # rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_area = length_ft * width_ft,
    comments = stitch_notes(NA, ml_notes),
    .before = 1,
  )

tbl_2022_prairie <- pre_2022_prairie |> select(any_of(fuel_harvesting_cols))
dupe_2022_prairie <- tbl_2022_prairie |> get_yield()
supp_2022_prairie <- pre_2022_prairie |> select(any_of(supp_fuel_harvesting_cols))


# 115 Prairie -------------------------------------------------------------

raw_2022_115_prairie <- xl_snap$`2022_harvests_prairie_115` |> clean_names()
# raw_2022_115_prairie |> names()
pre_2022_115_prairie <- raw_2022_115_prairie |>
  mutate(
    harvest_date = date_harvested,
    plot = plot,
    # section = subplot |> case_match("Main"~"Macro",
    #                                 "Micro"~"Micro"),
    section = subplot,
    crop = crop,
    fuel_plot = str_c(plot, section),
    # harvesting_id = get_harvest_id(year = 2022,
    #                                plot = plot,
    #                                section = section,
    #                                product = "prairie",
    #                                cut = "1"),
    # percent_moisture = percent_mst * 100,
    harvest_lbs = plot_weight_lbs,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = bag_wet_weight_g,
    dry_weight_w_bag = bag_dry_weight_g,
    percent_moisture = (wet_weight_w_bag - dry_weight_w_bag) / (wet_weight_w_bag - bag_weight) * 100,
    # wet_weight_no_bag = wet_wt_g_no_bag,
    # dry_weight_no_bag = dry_wt_g_no_bag,
    # rrl_id = rrl_id,
    harvest_length = length_ft,
    harvest_width = coalesce(width_ft, 10), # based on comment
    harvest_area = harvest_length * harvest_width,
    ml_notes = if_else(section %in% c("East", "West"),
                       paste("Michael Liou: \"Filled in plot width NA -> 10\"", ml_notes, collapse = "|"),
                       ml_notes),
    comments = stitch_notes(NA, ml_notes),
    .before = 1,
  )

tbl_2022_115_prairie <- pre_2022_115_prairie |> select(any_of(fuel_115_harvesting_cols))
dupe_2022_115_prairie <- tbl_2022_115_prairie |> get_yield()
supp_2022_115_prairie <- pre_2022_115_prairie |> select(any_of(supp_fuel_115_harvesting_cols))


# Assemble 2022 Tables ----------------------------------------------------

## Core Tables ------------------------------------------------------------
tbl_2022_harvests <- bind_rows(tbl_2022_c,
                               tbl_2022_ws,
                               tbl_2022_sb,
                               tbl_2022_wg,
                               tbl_2022_alfalfa)
# supp harvest
supp_2022_harvests <- bind_rows(supp_2022_c,
                                supp_2022_ws,
                                supp_2022_sb,
                                supp_2022_wg,
                                supp_2022_alfalfa) |> 
  select(where(\(x)!all(is.na(x))))

# loss tables
tbl_2022_loss <- bind_rows()
supp_2022_loss <- bind_rows()
tbl_2022_sysloss <- bind_rows(tbl_2022_sysloss_c)
supp_2022_sysloss <- bind_rows(supp_2022_sysloss_c)


# Biomassing Tables
tbl_2022_bio <- bind_rows(tbl_2022_bio_past,
                          tbl_2022_bio_alfalfa,
                          tbl_2022_bio_o)

supp_2022_bio <- bind_rows(supp_2022_bio_past,
                           supp_2022_bio_alfalfa,
                           supp_2022_bio_o)

# canopeo tables
tbl_2022_can <- bind_rows(
  tbl_2022_can_alfalfa,
  tbl_2022_can_o
)

supp_2022_can <- bind_rows(
  supp_2022_can_o
)


## EI Tables -------------------------------------------------------------

tbl_2022_ei_harvests <- bind_rows(tbl_2022_ei_c,
                                  tbl_2022_ei_ws,
                                  tbl_2022_ei_sb,
                                  tbl_2022_ei_cs, # part of ei?
                                  tbl_2022_ei_wg)



supp_2022_ei_harvests <- bind_rows(supp_2022_ei_c,
                                  supp_2022_ei_ws,
                                  supp_2022_ei_sb,
                                  supp_2022_ei_cs,
                                  supp_2022_ei_wg) |> 
  select(where(\(x)!all(is.na(x))))


# Loss Tables
tbl_2022_ei_loss <- bind_rows()
supp_2022_ei_loss <- bind_rows()
tbl_2022_ei_sysloss <- bind_rows(tbl_2022_ei_sysloss_c)
supp_2022_ei_sysloss <- bind_rows(supp_2022_ei_sysloss_c)

# Biomassing Tables
tbl_2022_ei_bio  <- bind_rows(tbl_2022_bio_ei_o, 
                              tbl_2022_bio_ei_r_o)

supp_2022_ei_bio <- bind_rows(supp_2022_bio_ei_o,
                              supp_2022_bio_ei_r_o)


# canopeo supp
tbl_2022_ei_can <- bind_rows(
  tbl_2022_can_ei_o,
  tbl_2022_can_ei_r_o
)

supp_2022_ei_can <- bind_rows(
  supp_2022_can_ei_o
)

## Corn Silage ------------------------------------------------------------

# corn silage
tbl_2022_silage <- tbl_2022_cs
supp_2022_silage <- supp_2022_cs


## Biofuels ----------------------------------------------------------------
# tbl_2022_prairie
# supp_2022_prairie

# Biofuels 115
# tbl_2022_115_prairie

