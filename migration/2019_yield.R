source("migration/yield_prep.R")

# Corn --------------------------------------------------------------------

raw_2019_c <- xl_snap$`2019_harvests_corn` |> clean_names()

pre_2019_c <- raw_2019_c |> mutate(
  plot = plot,
  section = plot_section,
  subplot = str_c(plot, section),
  harvesting_id = get_harvest_id(year = 2019,
                                 plot = plot,
                                 section = section,
                                 product = "corn",
                                 cut = 1),
  harvestingloss_id = get_harvestingloss_id(year = 2019,
                                            plot = plot,
                                            section = section,
                                            product = "corn",
                                            cut = 1,
                                            loss_num = 1),
  harvest_lbs = as.numeric(na_if(harvest_lbs, "NA")),
  harvest_date = harvest_date,
  percent_moisture = as.numeric(na_if(percent_moisture, "NA")),
  crop = crop,
  harvest_length = ml_length,
  harvest_width = ml_width,
  harvest_area = harvest_length * harvest_width,
  loss_area = ml_direct_harvest_loss,
  loss_reason = ml_loss_reason,
  loss_width = ml_direct_loss_width,
  loss_length = ml_direct_loss_length,
  loss_comments = ml_loss_notes,
  comments = ml_notes
)

tbl_2019_c <- pre_2019_c |>
  filter(section == "main") |> 
  select(any_of(harvesting_cols))
supp_2019_c <- pre_2019_c |>
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2019_ei_c <- pre_2019_c |>
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))
supp_2019_ei_c <- pre_2019_c |>
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))

# direct loss, no sysloss
tbl_2019_loss_c <- pre_2019_c |>
  drop_na(loss_area) |> 
  filter(section == "main") |> 
  select(any_of(loss_cols))

supp_2019_loss_c <- pre_2019_c |>
  drop_na(loss_area) |>
  filter(section == "main") |> 
  select(any_of(supp_loss_cols))

# no ei direct losses
# tbl_2019_ei_loss_c <- pre_2019_c |>
#   drop_na(loss_area) |> 
#   filter(section != "main") |> 
#   select(any_of(ei_loss_cols))
# # none
# 
# supp_2019_ei_loss_c <- pre_2019_c |>
#   drop_na(loss_area) |>
#   filter(section != "main") |> 
#   select(any_of(supp_ei_loss_cols))

  



# Soybean -----------------------------------------------------------------

raw_2019_sb <- xl_snap$`2019_harvests_soybean` |> clean_names()

pre_2019_sb <- raw_2019_sb |> mutate(
  plot = plot,
  section = ml_plot_section,
  subplot = str_c(plot, section),
  harvesting_id = get_harvest_id(year = 2019,
                                 plot = plot,
                                 section = section,
                                 product = "soybean",
                                 cut = 1),
  # harvestingloss_id = get_harvestingloss_id(year = 2019,
  #                                           plot = plot,
  #                                           section = section,
  #                                           product = "soybean",
  #                                           cut = 1,
  #                                           loss_num = 1),
  harvest_lbs = harvest_lbs,
  harvest_date = harvest_date,
  percent_moisture = percent_moisture,
  crop = crop,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = harvest_length * harvest_width,
  # loss_area = ml_direct_harvest_loss,
  # loss_reason = ml_loss_reason,
  # loss_width = ml_direct_loss_width,
  # loss_length = ml_direct_loss_length,
  # loss_comments = ml_loss_notes,
  # comments = ml_notes
)

tbl_2019_sb <- pre_2019_sb |>
  filter(section == "main") |> 
  select(any_of(harvesting_cols))
supp_2019_sb <- pre_2019_sb |>
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2019_ei_sb <- pre_2019_sb |>
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))
supp_2019_ei_sb <- pre_2019_sb |>
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))


# wheat grain -------------------------------------------------------------

# in alfalfa all section

# pasture -----------------------------------------------------------------

raw_2019_past <- xl_snap$`2019_harvests_pasture` |> clean_names()

pre_2019_past <- raw_2019_past |> mutate(
    plot = plot,
    section = "main",
    subplot = str_c(plot, section),
    cut = cut,
    biomass = "pasture",
    product = "pasture",
    method = "exclosure", #TODO: not quadrat...
    component = "shoots", #TODO: shoots
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = product,
                                   cut = cut),
    # harvestingloss_id = get_harvestingloss_id(year = 2019,
    #                                           plot = plot,
    #                                           section = section,
    #                                           product = "corn",
    #                                           cut = 1,
    #                                           loss_num = 1),
    biomassing_id = get_biomassing_id(year = 2019,
                                      plot = plot,
                                      section = section,
                                      biomass = biomass,
                                      cut = cut,
                                      coordinate = "X"), # 2020 is when paddock scheme started
    trailer_weight = if_else(plot == 112 & cut == 1, 5620, NA), # found in 2019_harvests_forage_all
    harvest_lbs = plot_wt_lbs,
    harvest_date = harvest_date,
    biomass_date = harvest_date,
    percent_moisture = moisture * 100,
    crop = crop,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area,
    biomass_area = plot_area,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_grams = plot_wt_lbs / kg_to_lbs * 1000,
    wet_weight_no_bag = grab_wet_wt_g,
    dry_weight_no_bag = grab_dry_wt_g,
    percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
    # loss_area = ml_direct_harvest_loss,
    # loss_reason = ml_loss_reason,
    # loss_width = ml_direct_loss_width,
    # loss_length = ml_direct_loss_length,
    # loss_comments = ml_loss_notes,
    comments = stitch_notes(notes, ml_notes = NA)
)

tbl_2019_past <- pre_2019_past |> 
  filter(harvest_area > 1000) |> 
  select(any_of(harvesting_cols))

supp_2019_past <- pre_2019_past |> 
  filter(harvest_area > 1000) |>
  select(any_of(supp_harvesting_cols))

tbl_2019_bio_past <- pre_2019_past |> 
  filter(harvest_area <= 1000) |> 
  select(any_of(biomassing_cols))

supp_2019_bio_past <- pre_2019_past |> 
  filter(harvest_area <= 1000) |> 
  select(any_of(supp_biomassing_cols))


# oat grain ---------------------------------------------------------------

raw_2019_og <- xl_snap$`2019_harvests_oat_grain` |> clean_names()

pre_2019_og <- raw_2019_og |> 
  mutate(
    plot = plot,
    section = subplot,
    subplot = str_c(plot, section),
    cut = 1,
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = "oat grain",
                                   cut = 1),
    # harvestingloss_id = get_harvestingloss_id(year = 2019,
    #                                           plot = plot,
    #                                           section = section,
    #                                           product = "corn",
    #                                           cut = 1,
    #                                           loss_num = 1),
    harvest_lbs = harvest_weight_lbs,
    harvest_date = harvest_date,
    percent_moisture = percent_moisture,
    crop = "oat grain",
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = harvested_area_ft2,
    # wet_weight_no_bag = grab_wet_wt_g,
    # dry_weight_no_bag = grab_dry_wt_g,
    # loss_area = ml_direct_harvest_loss,
    # loss_reason = ml_loss_reason,
    # loss_width = ml_direct_loss_width,
    # loss_length = ml_direct_loss_length,
    # loss_comments = ml_loss_notes,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2019_og <- pre_2019_og |> 
  filter(section == "main") |> 
  select(any_of(harvesting_cols))

supp_2019_og <- pre_2019_og |> 
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2019_ei_og <- pre_2019_og |> 
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))

supp_2019_ei_og <- pre_2019_og |> 
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))

dupe_2019_og <- tbl_2019_og |> get_yield("oats")


# oat straw ---------------------------------------------------------------

# This section is left out because master has different oat straw values. Not sure what these values are:
#   - different harvest area and moisture, but same harvest_lbs
raw_2019_os <- xl_snap$`2019_harvests_oat_straw` |> clean_names()

pre_2019_os <- raw_2019_os |> 
  mutate(
    plot = plot,
    section = subplot,
    subplot = str_c(plot, section),
    cut = 2,
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = "oat straw",
                                   cut = cut),
    # harvestingloss_id = get_harvestingloss_id(year = 2019,
    #                                           plot = plot,
    #                                           section = section,
    #                                           product = "corn",
    #                                           cut = 1,
    #                                           loss_num = 1),
    harvest_lbs = plot_wt_lbs,
    harvest_date = harvest_date,
    percent_moisture = moisture * 100,
    crop = crop,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area,
    bag_weight = bag_weight_g,
    wet_weight_w_bag = grab_wet_wt_g,
    wet_weight_no_bag = wet_wt_no_bag,
    dry_weight_no_bag = grab_dry_wt_no_bag_g,
    percent_moisture2 = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
    num_bales = ml_num_bales,
    # loss_area = ml_direct_harvest_loss,
    # loss_reason = ml_loss_reason,
    # loss_width = ml_direct_loss_width,
    # loss_length = ml_direct_loss_length,
    # loss_comments = ml_loss_notes,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2019_os <- pre_2019_os |> 
  filter(section == "main") |> 
  select(any_of(harvesting_cols))

supp_2019_os <- pre_2019_os |> 
  filter(section == "main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2019_ei_os <- pre_2019_os |> 
  filter(section != "main") |> 
  select(any_of(ei_harvesting_cols))

supp_2019_ei_os <- pre_2019_os |> 
  filter(section != "main") |> 
  select(any_of(supp_ei_harvesting_cols))

dupe_2019_os <- tbl_2019_os |> get_yield()

# corn silage -------------------------------------------------------------

raw_2019_cs <- xl_snap$`2019_harvests_corn_silage` |> clean_names()

# raw_2019_cs |> names()
pre_2019_cs <- raw_2019_cs |>
  separate_wider_position(cols = plot, widths = c("plot" = 3, "side" = 1),
                          too_few = "align_start") |>
  mutate(
    plot = as.numeric(plot),
    crop = "corn silage",
    section = case_match(side,
                         "W"~"West 15'",
                         "E"~"East 15'",
                         .default = subplot),
    subplot = str_c(plot, subplot),
    sideplot = str_c(plot, case_match(side,
                                      "W"~"WS",
                                      "E"~"ES")),
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = "corn silage",
                                   cut = 1),
    harvestingloss_id = get_harvestingloss_id(year = 2019,
                                              plot = plot,
                                              section = section,
                                              product = "corn silage"),
    harvest_date = harvest_date,
    harvest_length = ml_length,
    harvest_width = width_ft,
    harvest_area = harvest_length * harvest_width,
    harvest_lbs = wet_wt_lbs,
    # bag_weight = bag_weight_g,
    wet_weight_w_bag = wet_bag_wt_g,
    wet_weight_no_bag = wet_wt_bag_g,
    dry_weight_w_bag = dry_bag_wt_g,
    dry_weight_no_bag = dry_bag_wt_g_2,
    wet_bag_weight = wet_weight_w_bag - wet_weight_no_bag,
    dry_bag_weight = dry_weight_w_bag - dry_weight_no_bag,
    percent_moisture = coalesce((wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
                                moisture),
    # num_bales = ml_num_bales,
    loss_area = ml_direct_loss_ft2,
    loss_reason = ml_loss_reason,
    loss_width = ml_loss_width_ft,
    loss_length = ml_loss_length_ft,
    # loss_comments = ml_loss_notes,
    # rrl_id = rrl_id,
    # percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = ml_notes
  )

tbl_2019_cs <- pre_2019_cs |>
  filter(section %in% c("West 15'", "East 15'")) |> 
  select(any_of(cs_harvesting_cols))

supp_2019_cs <- pre_2019_cs |>
  filter(section %in% c("West 15'", "East 15'")) |> 
  select(any_of(supp_cs_harvesting_cols))

# ei plots
tbl_2019_ei_cs <- pre_2019_cs |> 
  filter(!(section %in% c("West 15'", "East 15'"))) |> 
  select(any_of(ei_harvesting_cols))

supp_2019_ei_cs <- pre_2019_cs |> 
  filter(!(section %in% c("West 15'", "East 15'"))) |> 
  select(any_of(supp_ei_harvesting_cols))

# directloss, silage
tbl_2019_loss_cs <- pre_2019_cs |>
  drop_na(loss_area) |> 
  filter(section %in% c("West 15'", "East 15'")) |> 
  select(any_of(loss_cols))

supp_2019_loss_cs <- pre_2019_cs |>
  drop_na(loss_area) |> 
  filter(section %in% c("West 15'", "East 15'")) |> 
  select(any_of(supp_loss_cols))


# alfalfa ------------------------------------------------------------------

raw_2019_alf <- xl_snap$`2019_harvests_forage` |>
  clean_names() |> 
  filter(plot != "PTP")

raw_2019_alf_all <- xl_snap$`2019_harvests_forage_all` |> clean_names()

# pastures can all be ignored, is completed in above data
# alfalfa yields is in all, but we do need the one bag weight
pre_2019_alf <- raw_2019_alf |>
  filter(crop != "P") |>
  mutate(
    plot = plot,
    cut = cut,
    section = "main",
    # coordinate = if_else(crop == "P",
    #                      "1", # pasture done previous
    #                      "X"),
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    biomassing_id = get_biomassing_id(year = 2019,
                                      plot = plot,
                                      section = section,
                                      biomass = crop,
                                      coordinate = "X",
                                      cut = cut),
    harvest_lbs = as.numeric(na_if(plot_wt_lbs, "STATION VALUE")),
    percent_moisture = as.numeric(na_if(moisture,"RRL VALUE")) * 100,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area,
    wet_weight_no_bag = grab_wet_wt_g,
    dry_weight_no_bag = grab_dry_wt_g,
    biomass_length = plot_length_ft,
    biomass_width = plot_width_ft,
    biomass_area = plot_area,
    trailer_weight = ml_trailer_weight,
    num_bales = ml_num_bales,
    comments = stitch_notes(notes, ml_notes)
  )

pre_2019_alf_all <- raw_2019_alf_all |> 
  mutate(
    plot = plot,
    section = "main",
    subplot = str_c(plot, section),
    # 
    crop = case_when(crop == "o/a" & cut == 1 ~ "oatlage",
                     crop == "o/a" & cut > 1 ~ "alfalfa",
                     crop == "w/cl" ~ "oat straw",
                     .default = crop),
    cut = cut,
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = section,
                                   product = crop, #TODO: not standard
                                   cut = cut),
    harvest_date = date_cut,
    harvest_lbs = weight_lbs,
    harvest_width = ml_width,
    harvest_length = ml_length,
    harvest_area = ml_area_ft2,
    percent_moisture = moisture,
    num_bales = bales,
    comments = stitch_notes(type, ml_notes)
  ) |> 
  left_join(pre_2019_alf |> select(harvesting_id, wet_weight_no_bag, dry_weight_no_bag),
            by = "harvesting_id")

# these are the wheat ones that have slight conflict
tbl_2019_wheat <- pre_2019_alf_all |> 
  filter(crop == "oat straw") |>
  select(any_of(harvesting_cols))

supp_2019_wheat <- pre_2019_alf_all |> 
  filter(crop == "oat straw") |>
  select(any_of(supp_harvesting_cols))

tbl_2019_alf <- pre_2019_alf_all |> 
  filter(crop != "oat straw") |>
  select(any_of(harvesting_cols))

supp_2019_alf <- pre_2019_alf_all |> 
  filter(crop != "oat straw") |>
  select(any_of(supp_harvesting_cols))




# tbl_2019_alf |> mutate(plot = as.numeric(plot)) |> select(plot, harvest_date, percent_moisture) |> arrange(plot, harvest_date)

# 
# # raw_2019_cs |> names()
# pre_2019_cs <- raw_2019_cs |>
#   separate_wider_position(cols = plot, widths = c("plot" = 3, "side" = 1),
#                           too_few = "align_start") |>
#   mutate(
#     plot = as.numeric(plot),
#     crop = "corn silage",
#     section = case_match(side,
#                          "W"~"West 15'",
#                          "E"~"East 15'",
#                          .default = subplot),
#     subplot = str_c(plot, subplot),
#     sideplot = str_c(plot, case_match(side,
#                                       "W"~"WS",
#                                       "E"~"ES")),
#     harvesting_id = get_harvest_id(year = 2019,
#                                    plot = plot,
#                                    section = section,
#                                    product = "corn silage",
#                                    cut = 1),
#     harvestingloss_id = get_harvestingloss_id(year = 2019,
#                                               plot = plot,
#                                               section = section,
#                                               product = "corn silage"),
#     harvest_date = harvest_date,
#     harvest_length = ml_length,
#     harvest_width = width_ft,
#     harvest_area = harvest_length * harvest_width,
#     harvest_lbs = wet_wt_lbs,
#     # bag_weight = bag_weight_g,
#     wet_weight_w_bag = wet_bag_wt_g,
#     wet_weight_no_bag = wet_wt_bag_g,
#     dry_weight_w_bag = dry_bag_wt_g,
#     dry_weight_no_bag = dry_bag_wt_g_2,
#     wet_bag_weight = wet_weight_w_bag - wet_weight_no_bag,
#     dry_bag_weight = dry_weight_w_bag - dry_weight_no_bag,
#     percent_moisture = coalesce((wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
#                                 moisture),
#     # num_bales = ml_num_bales,
#     loss_area = ml_direct_loss_ft2,
#     loss_reason = ml_loss_reason,
#     loss_width = ml_loss_width_ft,
#     loss_length = ml_loss_length_ft,
#     # loss_comments = ml_loss_notes,
#     # rrl_id = rrl_id,
#     # percent_moisture = moisture_rrl * 100,
#     # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
#     comments = ml_notes
#   )
# 


# oats bio ----------------------------------------------------------------

raw_2019_bio_oat <- xl_snap$`2019_anpp_oats` |> clean_names() |> 
  filter(ml_include == "yes")

# find cut information

pre_2019_bio_oat <- raw_2019_bio_oat |> 
  mutate(
    plot = plot,
    coordinate = if_else(section %in% c("S", "C", "N"), section, NA),
    section = if_else(section %in% c("S", "C", "N"), "Main", section),
    subplot = str_c(plot, section),
    biomass_date = date,
    biomass_length = m_to_ft,
    biomass_width = m_to_ft,
    biomass_area = m2_to_ft2,
    method = "quadrat",
    component = "shoots",
    comments = stitch_notes(notes, ml_notes),
    cut = if_else(section == "Main", 3, 1), # not filling in cut for EI plots, main is og then os. guessing for EI plots
  ) |> 
  pivot_longer(
    cols = oat_g:weed_g,
    names_pattern = "(.*)_g",
    names_to = "biomass",
    values_to = "biomass_grams"
  ) |> 
  mutate(
    biomassing_id = get_biomassing_id(year = 2019,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = biomass,
                                      cut = cut),
    percent_moisture = if_else(biomass_grams > 0, 0, NA) #TODO: assummed to be dry matter weights 
  )
tbl_2019_bio_oat <- pre_2019_bio_oat |> 
  filter(section == "Main") |> 
  select(any_of(biomassing_cols))
supp_2019_bio_oat <- pre_2019_bio_oat |> 
  filter(section == "Main") |>
  select(any_of(supp_biomassing_cols))

tbl_2019_ei_bio_oat <- pre_2019_bio_oat |>
  filter(section != "Main") |>
  select(any_of(ei_biomassing_cols))
supp_2019_ei_bio_oat <- pre_2019_bio_oat |>
  filter(section != "Main") |>
  select(any_of(supp_ei_biomassing_cols))

# bio alfalfa -------------------------------------------------------------

raw_2019_bio_alf <- xl_snap$`2019_anpp_alfalfa` |> clean_names()

pre_2019_bio_alf <- raw_2019_bio_alf |> 
  left_join(tbl_2019_alf |> count(plot, name = "prev_cuts"), by = "plot") |> 
  mutate(
    plot = plot,
    coordinate = if_else(section %in% c("S", "C", "N"), section, NA),
    section = if_else(section %in% c("S", "C", "N"), "Main", section),
    subplot = str_c(plot, section),
    cut = prev_cuts + 1,
    biomass_date = date,
    biomass_length = m_to_ft,
    biomass_width = m_to_ft,
    biomass_area = ml_biomassing_area_ft2,
    method = "undercutting",
    component = "shoots",
    comments = stitch_notes(NA, ml_notes)
  ) |> 
  pivot_longer(alfalfa_g:weed_g,
               names_pattern = "(.*)_g",
               names_to = "biomass",
               values_to = "biomass_grams") |> 
  mutate(
    biomassing_id = get_biomassing_id(year = 2019,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = biomass,
                                      cut = cut, 
                                      method = method,
                                      component = component), 
    percent_moisture = if_else(biomass_grams > 0, 0, NA)
  )

tbl_2019_bio_alf <- pre_2019_bio_alf |> select(any_of(biomassing_cols))
supp_2019_bio_alf <- pre_2019_bio_alf |> select(any_of(supp_biomassing_cols))


# bio undercutting --------------------------------------------------------

raw_2019_bio_under <- xl_snap$`2019_undercutting` |> clean_names()

pre_2019_bio_under <- raw_2019_bio_under |> 
  mutate(
    plot = plot,
    coordinate = subplot,
    section = "Main",
    subplot = str_c(plot, section),
    cut = 1,
    biomass = sample_type,
    biomass_date = sample_date,
    biomass_area = sample_area_ft2,
    biomass_grams = weight_g,
    method = ml_method,
    component = ml_crop_portion,
    biomassing_id = get_biomassing_id(year = 2019,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = biomass,
                                      cut = cut, 
                                      method = method,
                                      component = component), 
    percent_moisture = 0
  )

tbl_2019_bio_under <- pre_2019_bio_under |> select(any_of(biomassing_cols))
supp_2019_bio_under <- pre_2019_bio_under |> select(any_of(supp_biomassing_cols))


# Prairie -----------------------------------------------------------------

raw_2019_prairie <- xl_snap$`2019_biofuels` |> clean_names()

pre_2019_prairie <- raw_2019_prairie |> mutate(
  harvest_date = date,
  crop = plot,
  plot = treatment,
  section = case_match(sub_plot,
                       "main"~"Macro",
                       "micro"~"Micro"),
  fuel_plot = str_c(plot, section),
  harvesting_id = get_harvest_id(year = 2019,
                                 plot = plot,
                                 section = section,
                                 product = "prairie",
                                 cut = 1
  ),
  harvest_lbs = harvest_wt_lbs,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = area_ft_2,
  bag_weight = bag_wt_g,
  wet_weight_w_bag = wet_wt_w_bag,
  dry_weight_w_bag = dry_wt_w_bag,
  wet_weight_no_bag = wet,
  dry_weight_no_bag = dry,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100
)

tbl_2019_prairie <- pre_2019_prairie |> select(any_of(fuel_harvesting_cols))
supp_2019_prairie <- pre_2019_prairie |> select(any_of(supp_fuel_harvesting_cols))


# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2019_harvests <- bind_rows(
  tbl_2019_c,
  tbl_2019_sb,
  tbl_2019_og,
  # tbl_2019_os, # left out b/c it's actually in tbl_2019_wheat, just mislabel
  tbl_2019_wheat,
  tbl_2019_alf,
  tbl_2019_past
)

supp_2019_harvests <- bind_rows(
  supp_2019_c,
  supp_2019_sb,
  supp_2019_og,
  # supp_2019_os, # left out b/c it's actually in supp_2019_wheat, just mislabel
  supp_2019_wheat,
  supp_2019_alf,
  supp_2019_past
)

# biomassings
tbl_2019_bio <- bind_rows(
  tbl_2019_bio_past,
  tbl_2019_bio_alf,
  tbl_2019_bio_oat,
  tbl_2019_bio_under
)
supp_2019_bio <- bind_rows(
  supp_2019_bio_past,
  supp_2019_bio_alf,
  supp_2019_bio_oat,
  supp_2019_bio_under
)

# canopeo
tbl_2019_can <- bind_rows()
supp_2019_can <- bind_rows()

# losses
tbl_2019_loss <- bind_rows(
  tbl_2019_loss_c
)
supp_2019_loss <- bind_rows(
  supp_2019_loss_c
)
tbl_2019_sysloss <- bind_rows()
supp_2019_sysloss <- bind_rows()

## EI ----------------------------------------------------------------------

# harvests
tbl_2019_ei_harvests <- bind_rows(
  tbl_2019_ei_c,
  tbl_2019_ei_og,
  tbl_2019_ei_os,
  tbl_2019_ei_sb,
  tbl_2019_ei_cs
)
supp_2019_ei_harvests <- bind_rows(
  supp_2019_ei_c,
  supp_2019_ei_og,
  supp_2019_ei_os,
  supp_2019_ei_sb,
  supp_2019_ei_cs
)

# biomassings
tbl_2019_ei_bio <- bind_rows(
  tbl_2019_ei_bio_oat
)
supp_2019_ei_bio <- bind_rows(
  supp_2019_ei_bio_oat
)

# canopeo
tbl_2019_ei_can <- bind_rows()
supp_2019_ei_can <- bind_rows()

# losses
tbl_2019_ei_loss <- bind_rows()
supp_2019_ei_loss <- bind_rows()
tbl_2019_ei_sysloss <- bind_rows()
supp_2019_ei_sysloss <- bind_rows()

## Silage ------------------------------------------------------------------

# harvests
tbl_2019_silage <- bind_rows(
  tbl_2019_cs
)
supp_2019_silage <- bind_rows(
  supp_2019_cs
)

tbl_2019_silage_loss <- bind_rows(
  tbl_2019_loss_cs
)
supp_2019_silage_loss <- bind_rows(
  supp_2019_loss_cs
)

tbl_2019_silage_sysloss <- bind_rows()
supp_2019_silage_sysloss <- bind_rows()


## Biofuel -----------------------------------------------------------------

# harvests
tbl_2019_prairie <- bind_rows(
  tbl_2019_prairie
)
supp_2019_prairie <- bind_rows(
  supp_2019_prairie
)
