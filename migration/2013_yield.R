


# corn --------------------------------------------------------------------

source("migration/yield_prep.R")

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

supp_2013_c <- pre_2013_c |> 
  select(any_of(supp_harvesting_cols))

# wheat grain -------------------------------------------------------------

raw_2013_wg <- xl_snap$`2013_harvests_wheat` |> clean_names()

pre_2013_wg <- raw_2013_wg |>
  mutate(
    plot = plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = "wheat grain"),
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
    comments = stitch_notes(NA, ml_notes = ml_notes))
  # meeting w/ greg, we decided to just change everything to 510 to be 
  # consistent with other years instead of the precise measurements here, 
  # so make a note of the original plot length used in the original calculation

tbl_2013_wg <- pre_2013_wg |> select(any_of(harvesting_cols))

supp_2013_wg <- pre_2013_wg |> 
  select(any_of(supp_harvesting_cols))

# soybean -----------------------------------------------------------------

raw_2013_sb <- xl_snap$`2013_harvests_soybean` |> clean_names()

pre_2013_sb <- raw_2013_sb |>
  mutate(
    plot = plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = "soybean"),
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
    # bag_weight = ml_bag_weight_g,
    comments = stitch_notes(NA, ml_notes = ml_notes))

tbl_2013_sb <- pre_2013_sb |> select(any_of(harvesting_cols))

supp_2013_sb <- pre_2013_sb |> 
  select(any_of(supp_harvesting_cols))


# alfalfa -----------------------------------------------------------------

raw_2013_a <- xl_snap$`2013_harvests_alfalfa` |> clean_names()

raw_2013_a |> names()

# I messed up the cut number, some labelled 2 should actually be shifted left to acccount for missing values
pre_2013_a <- raw_2013_a |>
  drop_na(ml_plot_wt_lbs) |>
  mutate(
    plot = ml_plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    cut = row_number(), # add cut by group
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = "alfalfa", # may not all be alfalfa
                                   cut = cut),
    harvest_date = ml_harvest_date,
    harvest_area = ml_harvest_area,
    harvest_lbs = ml_plot_wt_lbs,
    # percent_moisture = ml_percent_moisture,
    # rrl_id = rrl_id,
    harvest_length = ml_harvest_length,
    harvest_width = ml_harvest_width,
    num_bales = ml_num_bales,
    # loss_width =  ml_loss_width,
    # loss_length = ml_loss_length,
    # loss_area = ml_direct_loss,
    # loss_reason = ml_loss_reason,
    # bag_weight = ml_bag_weight_g,
    comments = "Michael Liou: \"Moisture values taken from master during db migration\"",
    .by = ml_plot
    )

# missing moistures are pulled from master
master_moisture <- xl_master |> 
  filter(year == 2013) |> 
  select(plot, year,
         matches("crop[1-4]"),
         matches("bu_T_ac[1-4]"),
         matches("mst_dm[1-4]")) |> 
  pivot_longer(cols = crop1:mst_dm4,
               names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  mutate(across(.cols = c(crop, bu_T_ac, mst_dm),
                .fns = \(x) na_if(na_if(x, "."), "0")),
         cut = as.numeric(cut)) |> 
  filter(if_any(.cols = c(bu_T_ac, mst_dm), \(x) !is.na(x)))

mid_2013_a <- pre_2013_a |> 
  distinct(plot, cut, .keep_all = TRUE) |>
  left_join(master_moisture, by = c("plot", "cut")) |>
  mutate(percent_moisture = as.numeric(mst_dm))

# there's one NA moisture value that _does_ have a 43.94 but is eventually ommited in master
# Gregg: SFAL number 43.94, ommitted due to concern over data quality
mid_2013_a[mid_2013_a$harvesting_id == "H2013_A103MMX_af_1", "percent_moisture"] <- 43.94
mid_2013_a[mid_2013_a$harvesting_id == "H2013_A103MMX_af_1", "comments"] <- "Michael Liou: \"moisture value restored NA -> 43.94 from comment in master\" | Gregg Sanford: \"SFAL number 43.94, ommitted due to concern over data quality\""

tbl_2013_a <- mid_2013_a |> select(any_of(harvesting_cols))
supp_2013_a <- mid_2013_a |> select(any_of(supp_harvesting_cols))


# pasture -----------------------------------------------------------------

raw_2013_past <- xl_snap$`2013_harvests_pasture` |> clean_names()

# raw_2013_past |> names()

pre_2013_past <- raw_2013_past |> 
  mutate(
    plot = ml_plot,
    # subplot = str_c(plot, plot_section),
    section = "main",
    # cut = row_number(), # add cut by group
    harvesting_id = get_harvest_id(year = 2013,
                                   plot = plot,
                                   section = section,
                                   product = "pasture", 
                                   cut = 1), # cut assumed to just be 1 for whole plot
    harvest_date = date,
    harvest_lbs = plot_wt_lbs,
    wet_weight_no_bag = ml_grab_wet_no_bag,
    dry_weight_w_bag = ml_grab_dry_with_bag,
    percent_moisture = ml_moisture * 100,
    # rrl_id = rrl_id,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_area = plot_area,
    # num_bales = ml_num_bales,
    # loss_width =  ml_loss_width,
    # loss_length = ml_loss_length,
    # loss_area = ml_direct_loss,
    # loss_reason = ml_loss_reason,
    wagon_weight = ml_wagon_weight,
    bag_weight = ml_bag_weight,
    comments = stitch_notes(notes, ml_notes)
  )

tbl_2013_past <- pre_2013_past |> select(any_of(harvesting_cols))
supp_2013_past <- pre_2013_past |> select(any_of(supp_harvesting_cols))

# wheat straw --------------------------------------------------------------


tbl_2013_past |> get_yield("pasture") |> View()


# anpp pasture ------------------------------------------------------------

#HOLD: until we figure 
raw_2013_past <- xl_snap$`2013_anpp_pasture`

# assume sample weights. unknown are IN bag = 50
raw_2013_past |> 
  mutate(dry_wt = ml_dry_wbag_weight_g - ml_bag_weight,
         wet_wt = coalesce(ml_wet_nobag_weight_g, ml_wet_wbag_weight_g - ml_bag_weight),
         mst = (wet_wt -dry_wt) / wet_wt,
         frac_acre = ml_harvest_area / acre_to_ft2,
         yield =ml_harvest_weight_lbs * (1 - mst) /2000 / frac_acre) |> 
  select(wet_wt, dry_wt, mst,frac_acre, yield)

