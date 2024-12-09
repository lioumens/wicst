if (!exists("xl_snap")) source("migration/yield_prep.R")


# Pasture -----------------------------------------------------------------

raw_2014_past <- xl_snap$`2014_harvests_pasture` |> clean_names()

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
  percent_moisture = ml_moisture, #TODO: maybe drop it completely, probably too low
  comments = stitch_notes(notes, ml_notes)
)

tbl_2014_past <- pre_2014_past |> select(any_of(harvesting_cols))
supp_2014_past <- pre_2014_past |> select(any_of(supp_harvesting_cols))


# Bio Pasture -------------------------------------------------------------

raw_2014_bio_past <- xl_snap$`2014_moistures_pasture` |> clean_names()

pre_2014_bio_past <- raw_2014_bio_past |> 
  filter(!str_detect(description,"entire")) |>
  left_join(tbl_2014_past |> count(plot), by = "plot") |> 
  mutate(
    plot = plot,
    section = "Main",
    # cut = str_extract(description,"\\d+"),
    cut = row_number() + coalesce(n, 0),
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
    biomass_width = NA, #TODO fill in
    # biomass_grams = dry_weight_no_bag,
    .by = plot
  )

tbl_2014_bio_past <- pre_2014_bio_past |> select(any_of(biomassing_cols))
supp_2014_bio_past <- pre_2014_bio_past |> select(any_of(supp_biomassing_cols))

# -------------------------------------------------------------------------
# seems not worthwhile to try and combine this data

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

pre_arl_2014 <- raw_arl_2014 |> filter(!(crop %in% c("past", "p"))) |> 
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

pre_arl_2014_bio <- raw_arl_2014 |> filter(crop %in% c("past", "p")) |> 
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
                                   coordinate = "X")
  )

tbl_arl_2014 <- pre_arl_2014 |> select(any_of(harvesting_cols))
supp_arl_2014 <- pre_arl_2014 |> select(any_of(supp_harvesting_cols))

tbl_arl_2014_bio <- pre_arl_2014_bio |> select(any_of(biomassing_cols))
supp_arl_2014_bio <- pre_arl_2014_bio |> select(any_of(supp_biomassing_cols))




# compare
xtb <- tbl_2014_bio_past |> select(biomassing_id, biomass_date, percent_moisture, biomass_grams)
tbl_arl_2014_bio |> left_join(xtb, by = "biomassing_id") |> 
  View()





