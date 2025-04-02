
if (!exists("harvesting_cols")) source("migration/yield_prep.R")
library(readxl)

# moisture
raw_2015_forage_moisture <- xl_snap$`2015_harvests_forage_moisture` |>
  clean_names()

# Pasture -----------------------------------------------------------------

# complicated! Too many conflicting sources, deferred to QA... or, I could just use
# assuming master yields, delete moistures, bale dates 6/3 and 6/19
raw_2015_forage <- xl_snap$`2015_harvests_forage` |> clean_names()
raw_2015_past <- xl_snap$`2015_harvests_pasture` |> clean_names()


pre_2015_past <- raw_2015_past |> 
  left_join(raw_2015_forage |>
              filter(trt == "T14") |>
              select(plot, bales),
            by = "plot") |> 
  mutate(
    plot = plot,
    section = "Main",
    crop = "pasture",
    harvesting_id = get_harvest_id(year = 2015,
                                   plot = plot,
                                   section = section,
                                   product = "pasture"),
    harvest_date = date,
    harvest_lbs = plot_wt_lbs,
    harvest_width = plot_width_ft,
    harvest_length = plot_length_ft,
    harvest_area = plot_area,
    percent_moisture = ml_moisture,
    comments = stitch_notes(notes, ml_notes)
  )

# bio massing from different file
raw_2015_bio_past <- xl_snap$`2015_anpp_pasture` |> clean_names()

pre_2015_bio_past <- raw_2015_bio_past |> mutate(
  plot = plot,
  section = "Main",
  cut = row_number(),
  biomass_date = harvest_date,
  method = "exclosure",
  component  = "shoots",
  biomass = "pasture",
  coordinate = NA,
  biomassing_id = get_biomassing_id(year = 2015,
                                    plot = plot,
                                    section = section,
                                    method = method,
                                    component = component,
                                    coordinate = "X",
                                    cut = cut,
                                    biomass = biomass),
  biomass_width = plot_width_ft,
  biomass_length = plot_length_ft,
  biomass_area = area_harvested,
  biomass_grams = total_plot_harvest_wet_weight_kg_weight_bucket * 1000,
  bag_weight = bag_weight_g_prior_to_drying,
  wet_weight_w_bag = harvest_grab_wet_weight_g,
  dry_weight_w_bag = harvest_grab_dry_weight_g,
  wet_weight_no_bag = harvest_grab_wet_weight_g - bag_weight,
  dry_weight_no_bag = harvest_grab_dry_weight_g - bag_weight,
  percent_moisture = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
  comments = glue("General: \"Sampled by {sampled_by}\"", sampled_by = sampled_by),
  .by = c(plot)
)

tbl_2015_bio_past <- pre_2015_bio_past |> 
  filter(!is.na(biomass_area)) |> 
  select(any_of(biomassing_cols))

supp_2015_bio_past <- pre_2015_bio_past |> 
  filter(!is.na(biomass_area)) |> 
  select(any_of(supp_biomassing_cols))

tbl_2015_past <- pre_2015_past |> 
  select(any_of(harvesting_cols))

supp_2015_past <- pre_2015_past |> 
  select(any_of(supp_harvesting_cols))

# corn --------------------------------------------------------------------

raw_2015_c <- xl_snap$`2015_harvests_corn` |> clean_names()

# loss in microplots not accounted for

pre_2015_c <- raw_2015_c |> mutate(
  plot = plot,
  section = location, # West Micro , East Micro, Weed Micro
  harvest_lbs = total_weight_lb,
  crop = "corn",
  harvesting_id = get_harvest_id(year = 2015,
                                 plot = plot,
                                 section = section,
                                 product = crop),
  percent_moisture = percent_moisture_6,
  bushel_lbs = as.numeric(na_if(test_weight_lb_bu, "N/A")),
  harvest_date = ml_harvest_date,
  harvest_length = ml_plot_length,
  harvest_width = ml_plot_width,
  harvest_area = ml_harvest_area,
  comments = stitch_notes(notes, NA)
)

tbl_2015_c <- pre_2015_c |> 
  filter(section == "Main") |> 
  select(any_of(harvesting_cols))

supp_2015_c <- pre_2015_c |> 
  filter(section == "Main") |> 
  select(any_of(supp_harvesting_cols))

tbl_2015_micro_c <- pre_2015_c |> 
  filter(section != "Main") |> 
  select(section, any_of(harvesting_cols))

supp_2015_micro_c <-  pre_2015_c |> 
  filter(section != "Main") |> 
  select(section, any_of(supp_harvesting_cols))


# soybean -----------------------------------------------------------------

raw_2015_sb <- xl_snap$`2015_harvests_soybean` |> clean_names()

pre_2015_sb <- raw_2015_sb |> mutate(
  plot = plot,
  section = "Main",
  crop = "soybean",
  harvesting_id = get_harvest_id(year = 2015,
                                 plot = plot,
                                 section = section,
                                 product = "soybean"),
  bushel_lbs = test_wt_bu_lb,
  percent_moisture = percent_moisture,
  harvest_length = length_ft,
  harvest_width = width_ft,
  harvest_area = ml_harvest_area
)

tbl_2015_sb <- pre_2015_sb |> select(any_of(harvesting_cols))
supp_2015_sb <- pre_2015_sb |> select(any_of(supp_harvesting_cols))

# wheat -------------------------------------------------------------------

raw_2015_wg <- xl_snap$`2015_harvests_wheat` |> clean_names()
pre_2015_wg <- raw_2015_wg |> mutate(
  plot = plot,
  section = "Main",
  harvest_width = ml_plot_width,
  harvest_length = ml_plot_length,
  harvest_lbs = plot_wt_lbs, 
  harvesting_id = get_harvest_id(year = 2015,
                                 plot = plot,
                                 section = section,
                                 product = "wheat"),
  harvest_area = harvest_width * harvest_length,
  percent_moisture = moisture_percent,
  bushel_lbs = test_wt_lbs_bu,
  harvest_date = as.POSIXct("2015-07-31", tz = "UTC"),
  crop = "wheat",
  comments = glue("Michael Liou: \"Harvest width is average of north ({north} ft) and south ({south} ft) measurements.\"",
                  north = cutting_width_north_ft,
                  south = cutting_width_south_ft)
)

tbl_2015_wg <- pre_2015_wg |> select(any_of(harvesting_cols))
supp_2015_wg <- pre_2015_wg |> select(any_of(supp_harvesting_cols))


# wheat straw -------------------------------------------------------------
# use harvest weights from "2015_harvest_forage" since it looks like station numbers
# says cut 1, but the wheat harvests are from before this date
# num bales are the same
# comment pulled from master


raw_2015_ws <- xl_snap$`2015_harvests_wheat_straw` |> clean_names()
pre_2015_ws <- raw_2015_ws |> left_join(raw_2015_forage |>
                                          filter(type == "SM STRAW") |>
                                          select(plot, wt_tons),
                                        by = "plot") |>  
  mutate(
    plot = plot,
    section = "Main",
    harvest_width = width_ft,
    harvest_length = length_ft,
    harvest_area = harvest_width * harvest_length,
    harvest_lbs = wt_tons * 2000, # using 2015_forage instead, only by 10 lbs, but more trustworthy
    num_bales = bales,
    bag_weight = bag_g,
    wet_weight_w_bag = grab_wet_g,
    dry_weight_w_bag = grab_dry_g,
    percent_moisture = percent_moisture,
    ml_notes = if_else(plot != 202,
                       "Gregg Sanford \"Based on notes and recollection: Wheat yields are based on the entier WICST plot (510 ft x 60ft), less the area removed for the CERES biomass data.
\"", NA),
    comments = stitch_notes(notes, ml_notes),
    crop = "wheat straw",
    harvesting_id = get_harvest_id(year = 2015,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = 2),
    loss_area = as.numeric(na_if(ft2area_removed_ceres, "none")),
    loss_comments = if_else(!is.na(loss_area), "Michael Liou: \"Loss area evaluated by CERES\"", NA),
    harvestingloss_id = get_harvestingloss_id(year = 2015,
                                              plot = plot,
                                              section = section,
                                              product = crop,
                                              cut = 2, # after wheat grain
                                              loss_num = 1)
  )

tbl_2015_ws <- pre_2015_ws |> select(any_of(harvesting_cols))
supp_2015_ws <- pre_2015_ws |> select(any_of(supp_harvesting_cols))
tbl_2015_loss_ws <- pre_2015_ws |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2015_loss_ws <- pre_2015_ws |> select(any_of(supp_loss_cols)) |> drop_na(loss_comments)

dupe_2015_ws <- tbl_2015_ws |>
  left_join(tbl_2015_loss_ws, by = "harvesting_id") |>
  mutate(harvest_area = harvest_area - coalesce(loss_area, 0)) |> 
  get_yield("wheat straw")

# Oatlage -----------------------------------------------------------------
# all the weights are in harvests_forage
# moistures if any are in dharves

raw_2015_oat_grab <- xl_snap$`2015_harvests_oatlage` |> clean_names()
# raw_2015_forage |> names()

pre_2015_oat <- raw_2015_forage |>
  filter(type == "SM GRAIN BALEAGE") |>  # oat rows
  left_join(raw_2015_oat_grab, by = "plot") |>
  mutate(
    plot = plot,
    section = "Main", 
    cut = cut, 
    crop = "oatlage",
    harvest_date = ml_bale_date,
    harvesting_id = get_harvest_id(year = 2015,
                                   plot = plot,
                                   section = section,
                                   product = "o/a",
                                   cut = 1),
    harvest_lbs = wt_tons * 2000, 
    harvest_length = 60,
    harvest_width = 510,
    harvest_area = 60 * 510,
    num_bales = bales,
    dry_weight_no_bag = dry_weight_g,
    percent_moisture = NA, # master uses averaged moisture, .5
    comments = stitch_notes(
      notes = "SM GRAIN BALEAGE, Oat Baleage",
      ml_notes = "Michael Liou: \"Deleted average moisture .5 -> NA during DB migration\" | Alexander Butz: \"Used average moisture for baleage.\"")
  )

tbl_2015_oat <- pre_2015_oat |> select(any_of(harvesting_cols))
supp_2015_oat <- pre_2015_oat |> select(any_of(supp_harvesting_cols))

# Alfalfa -----------------------------------------------------------------
# all the weights are in harvests_forage
# some of the wet weights for moistures are in harvests_forage_moisture, filling in.
# dry and percent moistures all match up

raw_2015_alf <- xl_snap$`2015_harvests_alf` |> 
  clean_names()

# sample type is correct for the cut, and has complete data, c(410, 103, 213, 314) first cut is oatlage
pre_2015_alf_grab <- raw_2015_alf |> 
  arrange(ml_date) |>
  mutate(
    plot = plot,
    section = "Main",
    cut = as.numeric(str_extract(sample_type, "\\d+")),
    # other_cut = row_number(), # just for counting
    # harvest_date = ml_date,
    harvest_width = 60,
    harvest_length = 510,
    crop = "alfalfa",
    harvesting_id = get_harvest_id(year = 2015,
                                   plot = plot,
                                   section = section,
                                   product = crop),
    # .by = plot
  )
# pre_2015_alf |> select(plot, cut, other_cut) |> filter(cut !=other_cut) |> View()
# pre_2015_alf |> summarize(maxcut = max(cut),
#                           countcut = n(), .by = plot)
# pre_2015_alf |> filter(plot %in% c(410, 103, 213, 314))
# xl_master |> filter(year == 2015, plot %in% ) |>
#   mutate(harvest_lbs = bu_T_ac1)

# do these numbers match up with forage?
pre_2015_alf <- pre_2015_alf_grab |>
  left_join(raw_2015_forage |>
              select(plot, cut, type, wt_tons, bales, ml_bale_date, percent_moisture, ml_moisture) |>
              rename(percent_moisture2 = percent_moisture),
            by = c("plot", "cut"))
# pre_2015_alf |> select(plot, cut, matches("^wet_weight|dry_weight"), my_moisture, percent_moisture,
#                        percent_moisture2) |> View()
# yes they all match up, but forage does have some values unaccounted for.

# extra moistures are in different sheet
pre_2015_forage_moisture <- raw_2015_forage_moisture |> 
  mutate(cut = readr::parse_number(cutting_number)) |> 
  select(plot, dry_weight_g, wet_weight_g, percent_moisture, cut, notes) |> 
  rename(other_moisture = percent_moisture,
         other_dry = dry_weight_g,
         other_wet = wet_weight_g)

mid_2015_alf <- pre_2015_alf |> 
  # join extra moistures
  left_join(
    pre_2015_forage_moisture,
    by = join_by("plot" == "plot", "cut" == "cut")) |>
  mutate(
    bag_weight = ml_bag_wt_g,
    wet_weight_w_bag = if_else(ml_bag_wt_included == "Yes", wet_weight_g, NA),
    dry_weight_w_bag = if_else(ml_bag_wt_included == "Yes", dry_weight_g, NA),
    wet_weight_no_bag = if_else(ml_bag_wt_included == "No", coalesce(wet_weight_g, other_wet),
                                NA,
                                coalesce(wet_weight_g, other_wet)), # assume not in bag when not stated
    dry_weight_no_bag = if_else(ml_bag_wt_included == "No", coalesce(dry_weight_g,other_dry),
                                NA,
                                coalesce(dry_weight_g,other_dry)), # assume not in bag when not stated
    percent_moisture = percent_moisture,
    my_moisture = coalesce((wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100,
                           (wet_weight_w_bag - dry_weight_w_bag) / (wet_weight_w_bag - bag_weight) * 100),
    harvest_date = ml_bale_date,
    percent_moisture = coalesce(my_moisture, percent_moisture, percent_moisture2),
    crop = "alfalfa",
    harvesting_id = get_harvest_id(year = 2015,
                                   plot = plot,
                                   section = "Main",
                                   product = "alfalfa",
                                   cut = cut),
    harvest_lbs = wt_tons * 2000,
    harvest_area = 60 * 510, # assume full plot length
)
# all moistures are consistent, but still doesn't match master
# mid_2015_alf |> select(my_moisture, percent_moisture, percent_moisture2) |> View()

# join with rrl ids for moisture, needs nutrients table
rrl_2015_alf_fp <- "~/OneDrive - UW-Madison/Database development/manual_fixes/harvest_rrl.xlsx"
rrl_2015_alf <- read_xlsx(rrl_2015_alf_fp, sheet = "2015_alf")

late_2015_alf <- mid_2015_alf |> 
  left_join(rrl_2015_alf |> select(harvesting_id, rrl_id), by = "harvesting_id") |> 
  left_join(xl_nutrients$`2015` |> select(rrl_id, total_moisture), by = "rrl_id") |> 
  mutate(moisture_source = case_when(!is.na(total_moisture)~"rrl",
                                     .default = NA),
         percent_moisture = coalesce(total_moisture, percent_moisture))

tbl_2015_alf <- late_2015_alf |> select(any_of(harvesting_cols))
supp_2015_alf <- late_2015_alf |> select(any_of(supp_harvesting_cols))

# Rye bio -----------------------------------------------------------------

#TODO: 2015_harvests_rye, just not sure what some cols mean
raw_2015_bio_rye <- xl_snap$`2015_harvests_rye` |>
  clean_names()

# Assemble Tables ---------------------------------------------------------

## Core --------------------------------------------------------------------

# harvests
tbl_2015_harvests <- bind_rows(
  tbl_2015_c,
  tbl_2015_sb,
  tbl_2015_wg,
  tbl_2015_ws,
  tbl_2015_oat,
  tbl_2015_alf,
  tbl_2015_past
)
# do number of harvets match with master?
# tbl_2015_harvests |> count(plot) |> mutate(plot = str_c("A", plot)) |>
#   left_join(xl_core$plots, by = join_by("plot" == "plot_id")) |>
#   arrange(treatment_id, plot)
# yes if you include pasture biomassings

supp_2015_harvests <- bind_rows(
  supp_2015_c,
  supp_2015_sb,
  supp_2015_wg,
  supp_2015_ws,
  supp_2015_oat,
  supp_2015_alf,
  supp_2015_past
)

# biomassings
tbl_2015_bio <- bind_rows(
  tbl_2015_bio_past
)
supp_2015_bio <- bind_rows(
  supp_2015_bio_past
)

# # canopeo
# tbl_2015_can <- bind_rows()
# supp_2015_can <- bind_rows()

# losses
tbl_2015_loss <- bind_rows(
  tbl_2015_loss_ws
)
supp_2015_loss <- bind_rows(
  supp_2015_loss_ws
)
tbl_2015_sysloss <- bind_rows()
supp_2015_sysloss <- bind_rows()

## Micro Side Experiment ---------------------------------------------------

tbl_2015_micro <- bind_rows(
  tbl_2015_micro_c
)

supp_2015_micro <- bind_rows(
  supp_2015_micro_c
)

