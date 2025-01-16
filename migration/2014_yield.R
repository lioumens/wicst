if (!exists("xl_snap")) source("migration/yield_prep.R")
library(readxl)

# 2014 yields
# uses mostly arlington yields sheets
# Stuff from yield folder
# - pasture bag info (moistures match, missing first cut info)
# - two harvests of pasture (grab logged, but moisture deleted)
# - alfalfa bag info

# Pasture & Bio -----------------------------------------------------------------

raw_2014_past <- xl_snap$`2014_harvests_pasture` |>
  clean_names()

raw_2014_bio_past <- xl_snap$`2014_moistures_pasture` |> clean_names()

pre_2014_past <- raw_2014_past |> mutate(
  plot = plot,
  section = "Main",
  harvest_date = date,
  harvesting_id = get_harvest_id(year = 2014,
                                 plot = plot,
                                 section = section,
                                 product = "pasture",
                                 cut = 1),
  harvest_lbs = plot_wt_lbs,
  harvest_length = plot_length_ft,
  harvest_width = plot_width_ft,
  harvest_area = plot_area,
  crop = harvest,
  wet_weight_w_bag = ml_grab_wet,
  dry_weight_w_bag = ml_grab_dr,
  percent_moisture = as.numeric(ml_moisture), #TODO: maybe drop it completely, probably too low
  comments = stitch_notes(notes, ml_notes)
)

# I think 4 bios and 1 chop?
tbl_2014_past <- pre_2014_past |> select(any_of(harvesting_cols))
supp_2014_past <- pre_2014_past |> select(any_of(supp_harvesting_cols))

pre_2014_bio_past <- raw_2014_bio_past |> 
  filter(!str_detect(description,"entire")) |>
  left_join(tbl_2014_past |> count(plot), by = "plot") |> 
  mutate(
    plot = plot,
    section = "Main",
    # cut = str_extract(description,"\\d+"),
    # cut = row_number() + coalesce(n, 0),
    cut = str_extract(description, "\\d") |> as.numeric() |> replace_na(1),
    biomass_date = date,
    wet_weight_no_bag = wet_weight_g,
    dry_weight_no_bag = dry_weight_g,
    percent_moisture = percent_moisture,
    method = "exclosure",
    component = "shoots",
    coordinate = NA,
    biomass = "pasture",
    biomassing_id = get_biomassing_id(year = 2014,
                                   plot = plot,
                                   section = "Main",
                                   biomass = "pasture",
                                   cut = cut,
                                   method = method,
                                   component = component,
                                   coordinate = "X"),
    biomass_area = NA, # need to fill in
    biomass_length = NA, # fill in
    biomass_width = NA, # fill in
    # biomass_grams = dry_weight_no_bag,
    .by = plot
  )

# the arl info seems more complete, do not use these. just get bag weights from supp_2014
tbl_2014_bio_past <- pre_2014_bio_past |> select(any_of(biomassing_cols))
supp_2014_bio_past <- pre_2014_bio_past |> select(any_of(supp_biomassing_cols))


# alfalfa -----------------------------------------------------------------

raw_2014_alfalfa <- xl_snap$`2014_moistures_alfalfa` |> clean_names()

raw_2014_alfalfa |> arrange(date) |> 
  mutate(tmp = str_extract(description, "\\d"), 
         tmp2 = row_number(),
         .by = plot) |> 
  select(plot, date, description, tmp, tmp2) |> 
  print(n = 100)

pre_2014_alfalfa <- raw_2014_alfalfa |>
  arrange(date) |> 
  mutate(
    plot = plot,
    harvest_date = date,
    wet_weight_no_bag = wet_weight_g,
    dry_weight_no_bag = dry_weight_g,
    percent_moisture = percent_moisture,
    cut = row_number(),
    section = "Main",
    crop = case_when(str_detect(description, "Oats/Alfalfa Baleage")~"oatlage",
                     str_detect(description, "wheat baleage")~"wheat straw",
                     .default = "alfalfa"),
    harvesting_id = get_harvest_id(year = 2014,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    .by = plot
  )

# joined later with arl data
# tbl_2014_alfalfa <- pre_2014_alfalfa |> select(any_of(harvesting_cols))
supp_2014_alfalfa <- pre_2014_alfalfa |> select(any_of(supp_harvesting_cols))


# undercutting ------------------------------------------------------------


raw_2014_under <- xl_snap$`2014_undercutting` |> clean_names()

pre_2014_under <- raw_2014_under |> 
  separate_wider_delim(cols = plot, delim = "-", names = c("plot", "coordinate")) |>
  mutate(
    plot = as.numeric(plot),
    section = "Main",
    method = "undercutting",
    component = "roots",
    coordinate = coordinate,
    biomassing_id = get_biomassing_id(year = 2014,
                                      plot = plot,
                                      section = section,
                                      coordinate = coordinate,
                                      biomass = "wheat straw",
                                      method = method,
                                      cut = 1,
                                      component = component),
    dry_weight_no_bag = dry_weight_g
  )

tbl_2014_under <- pre_2014_under |> select(any_of(biomassing_cols))
supp_2014_under <- pre_2014_under |> select(any_of(supp_biomassing_cols))


# arl data -------------------------------------------------------------------------

arl_crop_dict <- list("c"~"corn",
                      "sb"~"soybean",
                      "w"~"wheat grain",
                      c("oa", "o")~"oats",
                      c("o/a")~"oatlage",
                      "fc"~"filler corn",
                      "a"~"alfalfa",
                      c("p", "past")~"pasture",
                      "s"~"wheat straw",
                      "dsa"~"direct seeded alfalfa",
                      "c silage"~"corn silage")

# Using Arl yields, all missing dates.
raw_arl_2014 <- xl_arl$`2014`

pre_arl_2014 <- raw_arl_2014 |> 
  # oats are mislabeled, should be alfalfa
  # filter(!(plot %in% wrong_crop_plots & num == 3)) |> 
  filter(!(is.na(lbs) & crop == "a")) |> # get rid of empty alfalfa entries
  mutate(crop = if_else(crop == "o", "a", crop)) |>
  # exclude pastures, as these are biomassings
  filter(!(crop %in% c("past", "p"))) |> 
  mutate(
    plot = plot,
    cut = num,
    crop = case_match(crop, !!!arl_crop_dict),
    harvest_length = length,
    harvest_width = width,
    harvest_area = length * width,
    harvest_lbs = lbs,
    percent_moisture = moisture,
    loss_area = loss,
    harvesting_id = get_harvest_id(year = 2014,
                                   plot = plot,
                                   section = "Main",
                                   product = crop,
                                   cut = cut)
  )

pre_arl_2014_bio <- raw_arl_2014 |>
  filter(crop %in% c("past", "p")) |> 
  mutate(
    plot = plot,
    cut = num,
    biomass = case_match(crop, !!!arl_crop_dict),
    biomass_length = length,
    biomass_width = width,
    biomass_area = length * width,
    biomass_grams = lbs / kg_to_lbs * 1000,
    percent_moisture = moisture,
    method = 'exclosure', 
    component = "shoots",
    coordinate = NA,
    biomassing_id = get_biomassing_id(year = 2014,
                                   plot = plot,
                                   section = "Main",
                                   biomass = "pasture",
                                   cut = cut,
                                   coordinate = "X"),
    # manually added in order
    biomass_date = rep(lubridate::ymd(c("2014-06-10",
                                        "2014-07-17",
                                        "2014-08-09",
                                        "2014-09-10"),
                                      tz = "UTC"), 4)
  )

# # 1 moisture value different, ignored. moisture just won't match with supp info
# # H2014_A312MMX_AF_3: 57.736 (yield folder) -> 58.249 (arl yields)
# alfalfa_2014_rows <- tbl_2014_alfalfa |> filter(crop == "alfalfa") |> 
#   select(-crop, -plot, -harvest_date) |>
#   left_join(pre_arl_2014 |> select(-percent_moisture), by = "harvesting_id") |> 
#   mutate(crop = replace_na(crop,"alfalfa")) |> 
#   select(any_of(harvesting_cols))

# tmp to fill out
clip_arl_2014 <- pre_arl_2014 |> select(any_of(harvesting_cols))
supp_arl_2014 <- pre_arl_2014 |> select(any_of(supp_harvesting_cols))
# need to add bag supp info for alfalfa

# clip_arl_2014 |> clipr::write_clip()
# add dates manually
with_dates <- "~/OneDrive - UW-Madison/Database development/agcal_harvest_dates.xlsx"
tbl_arl_2014 <- read_xlsx(with_dates,
          sheet = "2014")

tbl_arl_2014_bio <- pre_arl_2014_bio |> select(any_of(biomassing_cols))
supp_arl_2014_bio <- pre_arl_2014_bio |> select(any_of(supp_biomassing_cols))

# bag information in bio_past to add
supp_2014_bio_pasture_combined <- supp_2014_bio_past |> select(-biomass_length, -biomass_width) |>
    full_join(supp_arl_2014_bio, by = "biomassing_id")


supp_2014_combined <- supp_arl_2014 |> left_join(supp_2014_alfalfa, by = "harvesting_id")

# tmp_supp <- supp_2014_bio_past |> select(-biomass_length, -biomass_width) |>
#   full_join(supp_arl_2014_bio, by = "biomassing_id") |>
#   mutate(mst = (wet_weight_no_bag - dry_weight_no_bag) / wet_weight_no_bag * 100)
# bag info matches moistures
# tmp_supp |> select(biomassing_id, mst) |>
#   left_join(tbl_arl_2014_bio |> select(-method, -biomass, -component), by = "biomassing_id") |> 
#   relocate(percent_moisture, .after = mst)

# compare
# xtb <- tbl_2014_bio_past |> select(biomassing_id, biomass_date, percent_moisture, biomass_grams)
# tbl_arl_2014_bio |> left_join(xtb, by = "biomassing_id") |> 
#   View()


# combined ----------------------------------------------------------------

tbl_2014_harvests <- bind_rows(
  tbl_arl_2014,
  tbl_2014_past
)

supp_2014_harvests <- bind_rows(
  supp_2014_combined,
  supp_2014_past
)

tbl_2014_bio <- bind_rows(
  tbl_arl_2014_bio,
  tbl_2014_under
)

supp_2014_bio <- bind_rows(
  supp_2014_bio_pasture_combined,
  supp_2014_under
)

