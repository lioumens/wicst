
# get_harvest_id(1989, 169, "c", 1)

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

unique(c(loss_cols, supp_loss_cols))
raw_2022_loss_c |> names()

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
    loss_location = if_else(section_old != "Main", NA, section_old),
    loss_reason = "weed, cultivator blight, other confounding"
  ) |> 
  # join harvest table for id and harvest area
  left_join(pre_2022_c |> select(harvesting_id, subplot, harvest_area), by = "subplot") |> 
  mutate(loss_area = coalesce(
    get0("loss_width", ifnotfound = NA) * get0("loss_length", ifnotfound = NA), # direct loss if available
    harvest_area * assessment_loss_area / assessment_total_area # proportional loss otherwise
  ))


tbl_2022_loss_c <- pre_2022_loss_c |> 
  filter(section == "Main") |> 
  select(any_of(loss_cols))

tbl_2022_loss_ei_c <- pre_2022_loss_c |> 
  filter(section == "Main") |> 
  select(any_of(ei_loss_cols))

supp_2022_loss_c <- pre_2022_loss_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_loss_cols))

supp_2022_loss_ei_c <- pre_2022_loss_c |> 
  filter(section != "Main") |> 
  select(any_of(supp_ei_loss_cols))


dupe_2022_c <- tbl_2022_c |> 
  left_join(
    tbl_2022_loss_c |> summarize(total_area_loss = sum(loss_area), .by = harvesting_id),
    by = "harvesting_id") |>
  mutate(harvest_area = harvest_area - coalesce(total_area_loss, 0)) |>
  get_yield("corn") 
# matches!

# wheat grain -------------------------------------------------------------

raw_2022_wg <- xl_snap$`2022_harvests_wheat_grain` |> clean_names()
# raw_2022_wg |> names()

pre_2022_wg <- raw_2022_wg |> 
  filter(subplot == "Main") |> 
  mutate(
    harvesting_id = get_harvest_id(2022, plot, subplot, "wheat grain"),
    harvest_lbs = harvest_weight_lbs,
    harvest_area = harvested_area_ft2,
    percent_moisture = percent_moisture,
    harvest_width = plot_width_ft,
    harvest_length = plot_length_ft,
    comments = ml_notes
  )

tbl_2022_wg <- pre_2022_wg |> 
  select(harvesting_id, harvest_date, plot, crop, harvest_lbs, harvest_area, percent_moisture)

supp_2022_wg <- pre_2022_wg |>
  select(any_of(supp_harvesting_cols))

# dupe
# dupe_2022_wg <- pre_2022_wg |> 
#   mutate(corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - wg_ideal_percent_moisture)),
#          corrected_bu = corrected_lbs / wg_bushel,
#          acre_frac = harvest_area / acre_to_ft2,
#          corrected_lbs_per_acre = corrected_lbs / acre_frac,
#          corrected_bu_per_acre = corrected_lbs_per_acre / wg_bushel)

# straw -------------------------------------------------------------------

raw_2022_ws <- xl_snap$`2022_harvests_straw_main` |> clean_names()

raw_2022_ws

# raw_2022_ws$biomass |> str_split(",") |> list_transpose() |> set_names(nm = c("product", "desc")) |> as.data.frame()

# raw_2022_ws|> map(
#   \(x) str_split(x, ",") |> list_transpose() |> set_names(nm = c("product", "desc"))
# )
# raw_2022_ws |> names()

pre_2022_ws <- raw_2022_ws |> 
  separate_wider_regex(biomass, c(crop = ".*", "\\s*,\\s*", crop_description = ".*")) |> 
  mutate(
    harvesting_id = get_harvest_id(year = 2022,
                                   plot = plot,
                                   section = "Main",
                                   product = "wheat straw"),
    crop = str_to_lower(crop),
         harvest_date = date,
         harvest_area = area_ft_2,
         harvest_lbs = plot_wt_lbs,
         harvest_length = length_ft,
         harvest_width = width_ft,
         wagon_weight = ml_wagon_weight,
         wet_bag_weight = bag_weight_g,
         comments = ml_notes,
         percent_moisture = percent_mst * 100) 

tbl_2022_ws <- pre_2022_ws |>
  select(harvesting_id, harvest_date, plot, crop, crop_description, harvest_area, percent_moisture, harvest_lbs)

# dupe_2022_ws <- pre_2022_ws |>
#     mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
#            acre_frac = harvest_area / acre_to_ft2,
#            lbs_dm_per_acre = lbs_dm / acre_frac,
#            tons_dm_per_acre = lbs_dm_per_acre / 2000) |> View()


# supp
supp_2022_ws <- pre_2022_ws |>
  select(any_of(supp_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2022_sb <- xl_snap$`2022_harvests_soybean` |> clean_names()

pre_2022_sb <- raw_2022_sb |> filter(plot_section == "Main") |> 
  mutate(
    harvesting_id = get_harvest_id(2022, plot, plot_section, 
                                   product = "soybean"),
    harvest_length = length_ft,
    harvest_width = width_ft,
    rrl_id = rrl_id,
    comments = ml_notes,
    harvest_area = area_ft_2)

tbl_2022_sb <- pre_2022_sb |>
  select(harvesting_id, harvest_date, crop, plot, harvest_lbs, percent_moisture, harvest_area)

supp_2022_sb <- pre_2022_sb |> 
  select(any_of(supp_harvesting_cols))


# dupe_2022_sb <- pre_2022_sb |> 
#   mutate(corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - sb_ideal_percent_moisture)),
#        corrected_bu = corrected_lbs / sb_bushel,
#        acre_frac = harvest_area / acre_to_ft2,
#        corrected_lbs_per_acre = corrected_lbs / acre_frac,
#        corrected_tons_per_acre = corrected_lbs_per_acre / 2000,
#        corrected_bu_per_acre = corrected_lbs_per_acre / sb_bushel)
# dupe_2022_sb |> View()




# pasture -----------------------------------------------------------------

raw_2022_past <- xl_snap$`2022_harvests_pasture` |> clean_names()

# raw_2022_past |> names()

pre_2022_past <- raw_2022_past |> mutate(
  harvesting_id = get_harvest_id(2022, plot, "Main", product = "pasture", cut = cut),
  harvest_area = plot_area_ft,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_lbs = plot_wt_lbs,
  percent_moisture = moisture * 100,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2022_past <- pre_2022_past |> 
  select(harvesting_id, harvest_date, plot, crop, harvest_area, percent_moisture, harvest_lbs)

supp_2022_past <- pre_2022_past |> 
  select(any_of(supp_harvesting_cols))

# needs anpp
pre_2022_anpp_past <- raw_2022_past |> mutate(
  biomassing_id = get_biomassing_id(year = 2022,
                                    plot = plot,
                                    section = "Main",
                                    coordinate = "Central", #TODO: are these central?
                                    biomass = "pasture",
                                    cut = cut),
  biomass_date = harvest_date,
  coordinate = "Central", #TODO: are these really center? filled in.
  cut = cut,
  biomass_area = plot_area_ft,
  biomass_length = plot_length_ft,
  biomass_width = plot_width_ft,
  biomass_grams = plot_wt_lbs * 1000 / kg_to_lbs,
  percent_moisture = moisture * 100,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2022_anpp_past <- pre_2022_anpp_past |> 
  select(biomassing_id, plot, biomass_date, coordinate, cut, biomass_area, percent_moisture, biomass_grams)

supp_2022_anpp_past <- pre_2022_anpp_past |> 
  select(biomassing_id, biomass_length, biomass_width)

# Alfalfa Hay ---------------------------------------------------------------------

# this table is duplicated in the alfalfa table
# raw_2022_hay <- xl_snap$`2022_harvests_hay` |> clean_names()
# raw_2022_hay |> separate_wider_delim(field, names = c("field", "plot"), "-") |> 
#   mutate(harvest_lbs = total_weight_in_tons) |> 
#   select(plot, total_weight_in_tons) |> 
#   group_by(plot) |> 
#   summarize(all_weights = paste(total_weight_in_tons, collapse = ","))

raw_2022_alfalfa <- xl_snap$`2022_harvests_alfalfa` |> clean_names()
# raw_2022_alfalfa |> names()

pre_2022_alfalfa <- raw_2022_alfalfa |> mutate(
  harvesting_id = get_harvest_id(2022, plot, section = "Main",
                                 product = "alfalfa",
                                 cut = cut),
  harvest_area = plot_area_ft_2,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_lbs = plot_wt_lbs,
  percent_moisture = moisture_rrl * 100,
  comments = stitch_notes(notes, ml_notes)
)

tbl_2022_alfalfa <- pre_2022_alfalfa |> 
  select(harvesting_id, plot, crop, harvest_date, harvest_area, percent_moisture, harvest_lbs)

supp_2022_alfalfa <- pre_2022_alfalfa |> 
  select(harvesting_id, rrl_id, harvest_length, harvest_width, comments)

# dupe
# dupe_2022_alfalfa <- pre_2022_alfalfa |>
#       mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
#              acre_frac = harvest_area / acre_to_ft2,
#              lbs_dm_per_acre = lbs_dm / acre_frac,
#              tons_dm_per_acre = lbs_dm_per_acre / 2000)


# corn silage -------------------------------------------------------------
raw_2022_cs <- xl_snap$`2022_harvests_corn_silage` |> clean_names()

# raw_2022_cs |> names()

pre_2022_cs <- raw_2022_cs |> 
  filter(section == "main") |> 
  mutate(
    crop = "corn silage",
    plot = as.numeric(plot),
    harvesting_id = get_harvest_id(2022, plot, section, "corn silage"),
    harvest_area = plot_area_ft_2,
    harvest_lbs = harvest_lbs,
    harvest_width = width_ft,
    harvest_length = length_ft,
    percent_moisture = as.numeric(str_replace(moisture_rrl, "%$", "")),
    comments = ml_notes
  )

tbl_2022_cs <- pre_2022_cs |> 
  select(harvesting_id, plot, crop, harvest_date, harvest_area, percent_moisture, harvest_lbs)

supp_2022_cs <- pre_2022_cs |> 
  select(harvesting_id, harvest_width, harvest_length, rrl_id, ml_notes)

dupe
dupe_2022_cs <- pre_2022_cs |>
  mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
         acre_frac = harvest_area / acre_to_ft2,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000)
dupe_2022_cs |> View()



# anpp rye -----------------------------------------------------------------

# biomassing, ei plots 

raw_2022_anpp_r_o <- xl_snap$`2022_anpp_rye_oat` |> clean_names()

pre_2022_anpp_r_o <- raw_2022_anpp_r_o |>
  mutate(
    subplot = str_remove(sub_plot, "-"),
    section = str_extract(sub_plot, "-(.*)$", 1), # get part after -
    biomass_date = date_collected,
    biomass_area = quadrat_size_m_2 * m2_to_ft2,
    coordinate = NA,
    cut = 1
  ) |>
  pivot_longer(cols = rye_dm_g:residue_dm_g, names_to = "biomass", values_to = "biomass_grams") |> 
  separate_wider_delim(biomass, names = c("biomass", NA, NA), delim = "_") |>
  filter(!is.na(biomass_grams)) |>
  mutate(biomassing_id = get_biomassing_id(2022, plot, coordinate = "X", biomass = biomass, section, cut = 1))

tbl_2022_anpp_r_o <- pre_2022_anpp_r_o |> 
  select(biomassing_id, biomass_date, subplot, coordinate,
         cut, biomass_area, biomass, biomass_grams)
  


# alfalfa anpp ------------------------------------------------------------

raw_2022_anpp_alfalfa <- xl_snap$`2022_anpp_alfalfa` |> clean_names()

# raw_2022_anpp_alfalfa$quadrat_size |> str_extract("\\d") |>
# raw_2022_anpp_alfalfa |> names()
anpp_spec <- raw_2022_anpp_alfalfa |>
  build_longer_spec(cols = dry_bag_weight_g_large_bag:weeds_dm_g,
                    names_to = "biomass") |> 
  mutate(.value = rep(c("bag_weight", "with_bag", "biomass_grams"), each = 2),
         biomass = rep(c("alfalfa", "weeds"), 3))

pre_2022_anpp_alfalfa <- raw_2022_anpp_alfalfa |> 
  mutate(
    biomass_date = date,
    biomass_area = as.numeric(str_extract(quadrat_size, "\\d")) * m2_to_ft2,
    section = "main",
    cut = 1,
  ) |> 
  pivot_longer_spec(anpp_spec) |> 
  mutate(biomassing_id = get_biomassing_id(year = 2022,
                                           plot = plot, section = "Main",
                                           biomass = biomass,
                                           cut = 1, #TODO: assumed..
                                           coordinate = coordinate)) 

tbl_2022_anpp_alfalfa <- pre_2022_anpp_alfalfa |>
  select(biomassing_id, biomass_date, plot, coordinate,
         cut, biomass_area, biomass, biomass_grams)

supp_2022_anpp_alfalfa <- pre_2022_anpp_alfalfa |> 
  select(biomassing_id,  bag_weight)


# cs3 ---------------------------------------------------------------------

raw_2022_anpp_o <- xl_snap$`2022_anpp_cs3` |> clean_names()

# raw_2022_anpp_o |> names()
spec_2022_anpp_o <- raw_2022_anpp_o |> 
  build_longer_spec(cols = oats_g_large_bag_dry_weight:residue_dm_g,
                    names_to = "biomass") |>
  mutate(.value = rep(c("with_bag", "biomass_grams"), each = 5), # which column
         biomass = rep(c("oats", "berseem_clover", "red_clover", "weeds", "residue"), 2))

pre_2022_anpp_o <- raw_2022_anpp_o |> 
  pivot_longer_spec(spec_2022_anpp_o) |>
  mutate(
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
    biomassing_id = get_biomassing_id(year = 2022, 
                                      plot = plot, 
                                      section = section, 
                                      biomass = biomass, 
                                      coordinate = coordinate),
    biomass_date = date,
    biomass_area = m2_to_ft2, #TODO: verify quadrat size, assuming 1m2
    coordinate = case_when(
      !(section %in% c("Main", "MAIN")) ~ NA,
      .default = str_to_lower(coordinate)
    ),
    cut = 1, #TODO: assumed here
  )

tbl_2022_anpp_o <- pre_2022_anpp_o |> 
  filter(section %in% c("Main", "MAIN"), !is.na(biomass_grams)) |> 
  select(biomassing_id,
         biomass_date,
         biomass_area,
         plot,
         coordinate,
         cut,
         biomass,
         biomass_grams)

supp_2022_anpp_o <- pre_2022_anpp_o |> 
  select(biomassing_id, bag_weight, comments)


# combine 2022 ------------------------------------------------------------

tbl_2022_harvestings <- bind_rows(tbl_2022_c,
                                  tbl_2022_ws,
                                  tbl_2022_sb,
                                  tbl_2022_wg,
                                  tbl_2022_alfalfa,
                                  tbl_2022_cs)

tbl_2022_details <- bind_rows()

tbl_2022_harvestings |> count(plot)


tbl_2022_loss <- bind_rows(tbl_2022_c_loss)


# supp_2022_c |> names()
# supp_2022_sb |> names()
# supp_2022_ws |> names()
# supp_2022_wg |> names()
# supp_2022_alfalfa |> names()
# supp_2022_past |> names()
# supp_2022_cs |> names()
# tbl_2022_c_loss |> names()
# 
# tbl_2022_anpp_alfalfa |> names()
# tbl_2022_anpp_past |> names()
# tbl_2022_anpp_o |> names()
# tbl_2022_anpp_r_o |> names()
tbl_2022_biomassings <- bind_rows(tbl_2022_anpp_past,
                                  tbl_2022_anpp_alfalfa,
                                  tbl_2022_anpp_o)


tbl_2022_biomassings_ei <- bind_rows(tbl_2022_anpp_r_o)


