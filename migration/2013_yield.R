source("migration/yield_prep.R")

# corn
# dates from yield folder, "original lengths" and losses from arl.

pre_2013_harvest <- xl_arl$`2013` |> 
  rowwise() |> 
  mutate(
  # replace all with 510 for consistency
  plot = plot,
  section = "Main",
  crop = case_match(crop, !!!arl_crop_dict, .default = crop),
  harvesting_id = get_harvest_id(
    year = 2013,
    plot = plot,
    section = section,
    product = crop,
    cut = num
  ),
  harvestingloss_id = get_harvestingloss_id(
    year = 2013,
    plot = plot,
    section = section,
    coordinate = "X",
    product = crop,
    cut = 1
  ),
  harvest_length = if_else(crop != "pasture", 510, length), # replace the harvest length
  harvest_width = width,
  harvest_area = harvest_length * harvest_width,
  harvest_lbs = lbs,
  percent_moisture = moisture,
  loss_width = loss_width,
  loss_length = loss_length,
  loss_area = loss_width * loss_length,
  loss_reason = loss_reason,
  # log the original lengths into the note
  original_length = length + coalesce(loss_length, 0),
  length_msg = if_else(crop != "pasture", 
                       glue("Michael Liou: \"Plot length changed from {round(original_length, 2)}->{harvest_length} for standardization during db migration.\""),
                            NA),
  comments = case_when(is.na(note) & !is.na(length_msg)~length_msg,
                       is.na(length_msg) & !is.na(note)~note,
                       !is.na(note) & !is.na(length_msg) ~ str_flatten(c(length_msg, note), collapse = " | "),
                       .default = NA),
  loss_comments = loss_note,
)

# pre_2013_harvest |> select(length_msg, note, new_note) |> View()

# pre_2013_harvest |> select(harvest_length, length, loss_length) |> 
#   mutate(original_length = length + coalesce(loss_length, 0), dd)
# pre_2013_harvest |> select(harvest_length, length, length_msg) |> 
#   View()
 
# missing dates
tbl_arl_2013 <- pre_2013_harvest |> 
  filter(crop != "pasture") |>  # pasture should be in biomassing
  select(any_of(harvesting_cols)) |> 
  add_column(harvest_date = as.POSIXct(NA, tz = "UTC")) 
  

supp_arl_2013 <- pre_2013_harvest |> 
  filter(crop != "pasture") |> 
  select(any_of(supp_harvesting_cols)) |> 
  add_column(bag_weight = NA_real_, # used in wg
             wet_weight_no_bag = NA_real_,
             dry_weight_no_bag = NA_real_,
             dry_weight_w_bag = NA_real_,
             wagon_weight = NA_real_,
             num_bales = NA_real_)

tbl_arl_2013_loss <- pre_2013_harvest |> select(any_of(loss_cols)) |> drop_na()
supp_arl_2013_loss <- pre_2013_harvest |> filter(!is.na(loss_area)) |> 
  select(any_of(supp_loss_cols))


# pre_2013_harvest |> select(any_of(harvesting_cols)) |> 
#   filter(crop == "corn") |> left_join(tbl_2013_c, by = "harvesting_id") |> View()
# 
# pre_2013_harvest |> select(any_of(loss_cols)) |> 
#   drop_na(loss_area )

# corn --------------------------------------------------------------------
# took loss from yield folder
# - comments from both
raw_2013_c <- xl_snap$`2013_harvest_corn` |> clean_names()

# Main trial
pre_2013_c <- raw_2013_c |>
  mutate(
    plot = ml_plot,
    # subplot = str_c(plot, plot_section),
    section = ml_section,
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = "corn"),
    harvestingloss_id = get_harvestingloss_id(year = 2013,
                                              plot = plot,
                                              section = "Main",
                                              product = "corn"),
    harvest_date = ml_harvest_date,
    harvest_area = ml_harvest_area,
    harvest_lbs = plot_weight_lbs,
    percent_moisture = ml_moisture * 100,
    # rrl_id = rrl_id,
    harvest_length = ml_plot_length,
    harvest_width = ml_plot_width,
    loss_width =  ml_loss_width,
    loss_length = ml_loss_length,
    loss_area = ml_direct_loss,
    loss_reason = ml_loss_reason,
    comments = stitch_notes(NA, ml_notes = ml_notes),
    plot_comment = glue('Michael Liou: "Plot length changed from {round(ml_original_plot_length, 2)}->{ml_plot_length} for standardization during db migration.',
                        ml_original_plot_length = ml_original_plot_length,
                        ml_plot_length = ml_plot_length)) |>
  # meeting w/ greg, we decided to just change everything to 510 to be
  # consistent with other years instead of the precise measurements here,
  # so make a note of the original plot length used in the original calculations
  rowwise() |>
  mutate(
    new_comment = case_when(
      is.na(comments)~plot_comment,
      .default = str_flatten(c(plot_comment, comments), collapse = " | ")))

tbl_2013_c <- pre_2013_c |> select(any_of(harvesting_cols))
mid_supp_2013_c <- pre_2013_c |>
  select(any_of(supp_harvesting_cols))

# combine the comments from two files
supp_2013_c <- mid_supp_2013_c |> left_join(supp_arl_2013 |> select(-harvest_length, -harvest_width), by = "harvesting_id") |> 
  mutate(comments = case_when(is.na(comments.x) & !is.na(comments.y)~comments.y,
                              is.na(comments.y) & !is.na(comments.x)~comments.x,
                              !is.na(comments.y) & !is.na(comments.x) ~ str_flatten(c(comments.x, comments.y), collapse = " | "),
                              .default = NA)
  ) |>
  select(-comments.x, -comments.y)

tbl_2013_loss_c <- pre_2013_c |>
  select(any_of(loss_cols)) |>
  drop_na(loss_area)

# update the arl with yield folder information
tbl_arl_2013 <- tbl_arl_2013 |> 
  rows_patch(tbl_2013_c, by = "harvesting_id")
supp_arl_2013 <- supp_arl_2013 |> 
  rows_update(supp_2013_c, by = "harvesting_id")
tbl_arl_2013_loss <- tbl_arl_2013_loss |> 
  rows_upsert(tbl_2013_loss_c, by = c("harvestingloss_id"))
supp_arl_2013_loss <- supp_arl_2013_loss |> 
  rows_upsert(pre_2013_c |> 
                select(any_of(supp_loss_cols), -loss_reason) |> 
                drop_na(loss_width), by = "harvestingloss_id")

# the corn from yield folder is better for loss
#   harvesting_id      loss_width loss_length
# <glue>                  <dbl>       <dbl>
#   1 H2013_A401MMX_CN_1         30         5.5
# 2 H2013_A106MMX_CN_1         15         5.5
# 3 H2013_A304MMX_CN_1         30        13.5
# 4 H2013_A307MMX_CN_1         15        45  
# harvestingloss_id    loss_width loss_length loss_reason       loss_comments      
# <glue>                    <dbl>       <dbl> <chr>             <chr>              
#   1 L2013_A401MMX_CN_1_1         30           6 planting skip     "Alexander Paul Bu…
# 2 L2013_A307MMX_CN_1_1         15          45 cultivator damage "James: \"subtract…
# 3 L2013_A304MMX_CN_1_1         30          14 

# wheat grain -------------------------------------------------------------

raw_2013_wg <- xl_snap$`2013_harvests_wheat` |>
  clean_names()

pre_2013_wg <- raw_2013_wg |>
  mutate(
    plot = plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    crop = "wheat grain",
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = crop),
    harvestingloss_id = get_harvestingloss_id(year = 2013,
                                              plot = plot,
                                              section = "Main",
                                              product = crop),
    harvest_date = ml_harvest_date,
    harvest_area = ml_harvest_area,
    harvest_lbs = plot_weight_lbs,
    percent_moisture = ml_percent_moisture,
    # rrl_id = rrl_id,
    harvest_length = ml_plot_length,
    harvest_width = ml_plot_width,
    loss_width =  ml_loss_width,
    loss_length = ml_loss_length,
    loss_area = ml_direct_loss,
    loss_reason = ml_loss_reason,
    bag_weight = ml_bag_weight_g,
    comments = stitch_notes(NA, ml_notes = ml_notes)
  )
  # meeting w/ greg, we decided to just change everything to 510 to be
  # consistent with other years instead of the precise measurements here,
  # so make a note of the original plot length used in the original calculation

# adds no useful information, just
# - loss_comments
# - harvest dates
tbl_2013_wg <- pre_2013_wg |> select(any_of(harvesting_cols))
supp_2013_wg <- pre_2013_wg |>
  select(any_of(supp_harvesting_cols))

# not used
tbl_2013_loss_wg <- pre_2013_wg |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2013_loss_wg <- pre_2013_wg |> select(any_of(supp_loss_cols)) |> drop_na(loss_width)

# update arl with new info from table (dates), and comments/bag weight
tbl_arl_2013 <- tbl_arl_2013 |> 
  rows_patch(tbl_2013_wg |> 
               select(harvesting_id, harvest_date), 
             by = "harvesting_id")

# tbl_arl_2013 |> filter(harvesting_id |> str_detect("WG"))
supp_arl_2013 <- supp_arl_2013 |> 
  rows_update(supp_2013_wg |> 
                select(harvesting_id, bag_weight, comments),
              by = "harvesting_id")

# soybean -----------------------------------------------------------------

raw_2013_sb <- xl_snap$`2013_harvests_soybean` |> clean_names()

pre_2013_sb <- raw_2013_sb |>
  mutate(
    plot = plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    crop = "soybean",
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = crop),
    harvestingloss_id = get_harvestingloss_id(year = 2013,
                                              plot = plot,
                                              section = section,
                                              product = crop),
    harvest_date = ml_harvest_date,
    harvest_area = ml_harvest_area,
    harvest_lbs = plot_weight_lbs,
    percent_moisture = ml_percent_moisture * 100,
    # rrl_id = rrl_id,
    harvest_length = ml_plot_length,
    harvest_width = ml_plot_width,
    loss_width =  ml_loss_width,
    loss_length = ml_loss_length,
    loss_area = ml_direct_loss,
    loss_reason = ml_loss_reason,
    # bag_weight = ml_bag_weight_g,
    comments = stitch_notes(NA, ml_notes = ml_notes))

tbl_2013_sb <- pre_2013_sb |> select(any_of(harvesting_cols))
supp_2013_sb <- pre_2013_sb |> select(any_of(supp_harvesting_cols))

# not used, all information already accounted for
tbl_2013_sb_loss <- pre_2013_sb |> select(any_of(loss_cols)) |> 
  drop_na(loss_area)
supp_2013_sb_loss <- pre_2013_sb |> select(any_of(supp_loss_cols)) |>
  drop_na(loss_width)

# update arlington
tbl_arl_2013 <- tbl_arl_2013 |> 
  rows_upsert(tbl_2013_sb |> 
                select(harvesting_id, harvest_date),
              by = "harvesting_id")
supp_arl_2013 <- supp_arl_2013 |> rows_update(supp_2013_sb, by = "harvesting_id")

# no need to update losses
# tbl_arl_2013_loss
# supp_arl_2013_loss
# tbl_arl_2013 |> filter(str_detect(harvesting_id, "SB")) |> 
#   arrange(plot)

# supp_arl_2013_loss |> filter(str_detect(harvestingloss_id, "SB"))
# tbl_arl_2013_loss |> filter(str_detect(harvestingloss_id, "SB"))

## do the losses
# pre_2013_sb |> select(any_of(loss_cols)) |> drop_na(loss_area)


# wheat straw -------------------------------------------------------------

raw_2013_ws <- xl_snap$`2013_harvests_straw` |> clean_names()

pre_2013_ws <- raw_2013_ws |> mutate(
  plot = ml_plot,
  section = "Main",
  crop = "wheat straw",
  harvesting_id = get_harvest_id(year = 2013,
                                 plot = plot,
                                 section = section,
                                 product = crop,
                                 cut = 2),
  harvest_date = ml_sample_date,
  percent_moisture = ml_moisture * 100,
  wet_weight_no_bag = ml_wet_wbag_weight_g, # original file has no bag **
  dry_weight_no_bag = ml_dry_wbag_weight_g,
  harvest_lbs = ml_harvest_weight_lbs,
  num_bales = ml_num_bales,
  # area assumed 510 x 30
  harvest_length = 510,
  harvest_width = 60,
  harvest_area = harvest_length*harvest_width,
  comments = stitch_notes(NA, ml_notes)
)

tbl_2013_ws <- pre_2013_ws |> select(any_of(harvesting_cols))
supp_2013_ws <- pre_2013_ws |> select(any_of(supp_harvesting_cols))

# tbl_2013_ws |> get_yield("wheat straw") |> View()

# check arl data, conflicts. the final calculations are not all that different
# from master though, just use raw yield file to update
# tbl_arl_2013 |> filter(str_detect(harvesting_id, "WS"))

# update arl data, both table and supp
tbl_arl_2013 <- tbl_arl_2013 |> rows_update(
  tbl_2013_ws |>
    select(harvesting_id, percent_moisture, harvest_lbs, harvest_date),
  by = "harvesting_id"
)
# discard these comments from yield folder
supp_arl_2013 <- supp_arl_2013 |> rows_update(
  supp_2013_ws |> 
    select(-comments, -harvest_length, -harvest_width),
  by = "harvesting_id"
)


# Pasture -----------------------------------------------------------------

raw_2013_past <- xl_snap$`2013_harvests_pasture` |> clean_names()

pre_2013_past <- raw_2013_past |> mutate(
  plot = ml_plot,
  section = "Main",
  harvest_date = date,
  crop = "pasture",
  harvesting_id = get_harvest_id(year = 2013,
                                 plot = plot,
                                 section = section,
                                 product = crop),
  harvest_lbs = ml_harvest_lbs,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = plot_area,
  wet_weight_no_bag = ml_grab_wet_no_bag,
  dry_weight_w_bag = ml_grab_dry_with_bag,
  bag_weight = ml_bag_weight,
  percent_moisture = ml_moisture * 100,
  wagon_weight = ml_wagon_weight,
  comments = stitch_notes(harvest, ml_notes)
)

tbl_2013_past <- pre_2013_past |> select(any_of(harvesting_cols))
supp_2013_past <- pre_2013_past |> select(any_of(supp_harvesting_cols))

# update arl tables
tbl_arl_2013 <- tbl_arl_2013 |> rows_insert(tbl_2013_past, by = "harvesting_id")
supp_arl_2013 <- supp_arl_2013 |> rows_insert(supp_2013_past, by = "harvesting_id")


# Bio Pasture -----------------------------------------------------------------

# conflicts were difficult to resolve

raw_2013_past <- xl_snap$`2013_anpp_pasture` |> clean_names()

# bio pasturing
# assume sample weights. unknown are IN bag = 50

pre_2013_bio_past <- raw_2013_past |> group_by(ml_plot) |> 
  arrange(ml_sample_date) |> 
  mutate(
    plot = ml_plot,
    coordinate = NA,
    cut = row_number(),
    method = "exclosure",
    component = "shoots",
    biomass = "pasture",
    biomassing_id = get_biomassing_id(year = 2013,
                                      plot = plot,
                                   section = "Main",
                                   coordinate = "X",
                                   biomass = biomass,
                                   cut = cut,
                                   method = method,
                                   component = component),
    biomass_date = ml_sample_date,
    biomass_width = ml_plot_width,
    biomass_length = ml_plot_length,
    biomass_area = ml_harvest_area,
    biomass_grams = ml_harvest_weight_kg * 1000,
    comments = stitch_notes(NA, ml_notes),
    wet_wt = coalesce(ml_wet_wbag_weight_g - ml_bag_weight, ml_wet_nobag_weight_g),
    dry_wt = coalesce(ml_dry_wbag_weight_g - ml_bag_weight, ml_dry_nobag_weight_g),
    percent_moisture = (wet_wt - dry_wt) / wet_wt * 100) |> 
  ungroup()

tbl_2013_bio_past <- pre_2013_bio_past |> select(any_of(biomassing_cols))
supp_2013_bio_past <- pre_2013_bio_past |> select(any_of(supp_biomassing_cols))

# use the yield folder values, even when they conflict, but for biomassings
# tbl_arl_2013 |> filter(str_detect(harvesting_id, "PT"))
# tbl_2013_bio_past |> select(biomassing_id, plot, percent_moisture, biomass_area, biomass_grams) |> 
#   mutate(biomass_lbs = biomass_grams / 1000 * kg_to_lbs) |> 
#   arrange(biomassing_id) |> 
#   select(-biomass_grams)

# tbl_2013_bio_past |> get_biomass() |> filter(cut == 2) |> View()
# 
# tbl_arl_2013 |> filter(crop == "pasture")
# 
# tbl_2013_bio_past


# alfalfa -----------------------------------------------------------------

raw_2013_alf <- xl_snap$`2013_harvests_alfalfa` |> clean_names()

pre_2013_alf <- raw_2013_alf |> 
  # these are oatlage -> alfalfa plots, 
  filter(!(ml_plot %in% c(114, 211, 312, 403))) |>
  mutate(
    plot = ml_plot,
    crop = "alfalfa",
    section = "Main",
    harvest_lbs = ml_plot_wt_lbs,
    harvest_date = ml_harvest_date,
    # cut = ml_cut,
    harvest_length = ml_harvest_length,
    harvest_width = ml_harvest_width,
    harvest_area = ml_harvest_area,
    # percent_moisture = NA,
    num_bales = ml_num_bales,
    comments = stitch_notes(NA, ml_notes)
  ) |> drop_na(harvest_lbs) |>
  mutate(
    cut = row_number(),
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    .by = plot
  )

tbl_2013_alf <- pre_2013_alf |> select(any_of(harvesting_cols))
supp_2013_alf <- pre_2013_alf |> select(any_of(supp_harvesting_cols))

tbl_arl_2013 <- rows_update(tbl_arl_2013, 
                            tbl_2013_alf |>
                              select(-plot, crop, harvest_area),
                            by = "harvesting_id")


# update tbl with the comments, then update arl again
supp_alf_rows <- supp_2013_alf |> left_join(supp_arl_2013 |> 
                             select(harvesting_id, comments),
                           by = "harvesting_id") |> 
  rowwise() |> 
  mutate(comments = 
           case_when(is.na(comments.x)~comments.y,
                     is.na(comments.y)~comments.x,
                     !is.na(comments.x) & !is.na(comments.y)~str_flatten(c(comments.x, comments.y), collapse = " | ")
                     )
  ) |> 
  select(-comments.x, -comments.y)

supp_arl_2013 <- supp_arl_2013 |> rows_update(
  supp_alf_rows |> select(-harvest_length, -harvest_width),
  by = "harvesting_id")

# arl has the moistures, couldn't find any in yield db
# exclude the wheat/oatlage plots, no additional info
# tbl_arl_2013 |> filter(crop == "oatlage") |>
#   left_join(tbl_2013_alf, by = "harvesting_id") |> 
#   mutate(dlbs = harvest_lbs.y - harvest_lbs.x) |>
#   arrange(desc(dlbs)) |> View()
# 
# arl_alfs <- tbl_arl_2013 |> filter(crop == "alfalfa") |> pull(harvesting_id)
# tbl_alfs <- tbl_2013_alf |> pull(harvesting_id)
# 
# setdiff(arl_alfs, tbl_alfs)
# setdiff(tbl_alfs, arl_alfs)
# 
# tbl_2013_alf |> filter(plot == 114)
# 
# tbl_arl_2013 |> filter(plot %in% c(114, 211, 312, 403))


# Oatlage dates -----------------------------------------------------------
oatlage_dates <- tibble(harvesting_id = 
                          c("H2013_A114MMX_OL_1", "H2013_A114MMX_AF_2",
                            "H2013_A211MMX_OL_1", "H2013_A211MMX_AF_2",
                            "H2013_A312MMX_OL_1", "H2013_A312MMX_AF_2",
                            "H2013_A403MMX_OL_1", "H2013_A403MMX_AF_2"),
                        harvest_date = rep(c(lubridate::ymd("2013-7-8", tz = "UTC"),
                                             lubridate::ymd("2013-8-27", tz = "UTC")), 4)
)

tbl_arl_2013 <- tbl_arl_2013 |> rows_update(oatlage_dates, by = "harvesting_id")

# only 3 pastures empty
# tbl_arl_2013 |> filter(if_any(everything(), is.na))

# onto bios
# skipped anpp & undercutting due to uncertainty

tbl_2013_harvests <- tbl_arl_2013
supp_2013_harvests <- supp_arl_2013


tbl_2013_bio <- tbl_2013_bio_past
supp_2013_bio <- supp_2013_bio_past




# ARCHIVE SECTION ---------------------------------------------------------

# raw_2013_a <- xl_snap$`2013_harvests_alfalfa` |> clean_names()
# 
# raw_2013_a |> names()
# 
# # I messed up the cut number, some labelled 2 should actually be shifted left to acccount for missing values
# pre_2013_a <- raw_2013_a |>
#   drop_na(ml_plot_wt_lbs) |>
#   mutate(
#     plot = ml_plot,
#     # subplot = str_c(plot, plot_section),
#     section = "main",
#     cut = row_number(), # add cut by group
#     harvesting_id = get_harvest_id(year = 2013,
#                                    plot = plot,
#                                    section = section,
#                                    product = "alfalfa", # may not all be alfalfa
#                                    cut = cut),
#     harvest_date = ml_harvest_date,
#     harvest_area = ml_harvest_area,
#     harvest_lbs = ml_plot_wt_lbs,
#     # percent_moisture = ml_percent_moisture,
#     # rrl_id = rrl_id,
#     harvest_length = ml_harvest_length,
#     harvest_width = ml_harvest_width,
#     num_bales = ml_num_bales,
#     # loss_width =  ml_loss_width,
#     # loss_length = ml_loss_length,
#     # loss_area = ml_direct_loss,
#     # loss_reason = ml_loss_reason,
#     # bag_weight = ml_bag_weight_g,
#     comments = "Michael Liou: \"Moisture values taken from master during db migration\"",
#     .by = ml_plot
#     )
# 
# # missing moistures are pulled from master
# master_moisture <- xl_master |> 
#   filter(year == 2013) |> 
#   select(plot, year,
#          matches("crop[1-4]"),
#          matches("bu_T_ac[1-4]"),
#          matches("mst_dm[1-4]")) |> 
#   pivot_longer(cols = crop1:mst_dm4,
#                names_pattern = "(.*)([1-4])",
#                names_to = c(".value", "cut")) |> 
#   mutate(across(.cols = c(crop, bu_T_ac, mst_dm),
#                 .fns = \(x) na_if(na_if(x, "."), "0")),
#          cut = as.numeric(cut)) |> 
#   filter(if_any(.cols = c(bu_T_ac, mst_dm), \(x) !is.na(x)))
# 
# mid_2013_a <- pre_2013_a |> 
#   distinct(plot, cut, .keep_all = TRUE) |>
#   left_join(master_moisture, by = c("plot", "cut")) |>
#   mutate(percent_moisture = as.numeric(mst_dm))
# 
# # there's one NA moisture value that _does_ have a 43.94 but is eventually ommited in master
# # Gregg: SFAL number 43.94, ommitted due to concern over data quality
# mid_2013_a[mid_2013_a$harvesting_id == "H2013_A103MMX_af_1", "percent_moisture"] <- 43.94
# mid_2013_a[mid_2013_a$harvesting_id == "H2013_A103MMX_af_1", "comments"] <- "Michael Liou: \"moisture value restored NA -> 43.94 from comment in master\" | Gregg Sanford: \"SFAL number 43.94, ommitted due to concern over data quality\""
# 
# tbl_2013_a <- mid_2013_a |> select(any_of(harvesting_cols))
# supp_2013_a <- mid_2013_a |> select(any_of(supp_harvesting_cols))
# 
# 
# # pasture -----------------------------------------------------------------
# 
# raw_2013_past <- xl_snap$`2013_harvests_pasture` |> clean_names()
# 
# # raw_2013_past |> names()
# 
# pre_2013_past <- raw_2013_past |> 
#   mutate(
#     plot = ml_plot,
#     # subplot = str_c(plot, plot_section),
#     section = "main",
#     # cut = row_number(), # add cut by group
#     harvesting_id = get_harvest_id(year = 2013,
#                                    plot = plot,
#                                    section = section,
#                                    product = "pasture", 
#                                    cut = 1), # cut assumed to just be 1 for whole plot
#     harvest_date = date,
#     harvest_lbs = plot_wt_lbs,
#     wet_weight_no_bag = ml_grab_wet_no_bag,
#     dry_weight_w_bag = ml_grab_dry_with_bag,
#     percent_moisture = ml_moisture * 100,
#     # rrl_id = rrl_id,
#     harvest_length = plot_length_ft,
#     harvest_width = plot_width_ft,
#     harvest_area = plot_area,
#     # num_bales = ml_num_bales,
#     # loss_width =  ml_loss_width,
#     # loss_length = ml_loss_length,
#     # loss_area = ml_direct_loss,
#     # loss_reason = ml_loss_reason,
#     wagon_weight = ml_wagon_weight,
#     bag_weight = ml_bag_weight,
#     comments = stitch_notes(notes, ml_notes)
#   )
# 
# tbl_2013_past <- pre_2013_past |> select(any_of(harvesting_cols))
# supp_2013_past <- pre_2013_past |> select(any_of(supp_harvesting_cols))
# 
# # wheat straw --------------------------------------------------------------
# 
# 
# tbl_2013_past |> get_yield("pasture") |> View()
# 
# 
# # anpp pasture ------------------------------------------------------------

