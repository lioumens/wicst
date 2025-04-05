# from arl yields
source("migration/yield_prep.R")

raw_2004 <- xl_arl$`2004`

pre_2004 <- raw_2004 |> 
  filter(is.na(bio)) |>
  mutate(
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60",
      .default = NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    guess_loss = NA
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2004,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2004,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X",
                                              product = crop,
                                              cut = cut),
    harvest_length = coalesce(length, guess_length),
    harvest_width = coalesce(width, guess_width),
    harvest_area = harvest_length * harvest_width,
    harvest_date = date,
    harvest_lbs = if_else(crop == "pasture" & is.na(lbs) & yield == 0, 
                          0,
                          lbs),
    percent_moisture = moisture,
    loss_area = loss,
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    comments = stitch_notes(NA, note),
    tenday = pasture_tendayperiod,
    cycle = pasture_cycle
  )

# no longer needed, pasture data more complete
# pre_2004_bio <- raw_2004 |> filter(bio == "yes", yield != 0) |>
#   rowwise() |> 
#   mutate(plot = plot,
#          method = "exclosure",
#          biomass = "pasture",
#          component = "shoots", 
#          cut = num,
#          biomassing_id = get_biomassing_id(year = 2004,
#                                            plot = plot,
#                                            section = "Main",
#                                            biomass = "pasture",
#                                            method = method, 
#                                            component = component,
#                                            coordinate = "X"),
#          # biomass_length = coalesce(length, 20),
#          # biomass_width = coalesce(length, 7),
#          biomass_area = 50, # from comment
#          biomass_date = date,
#          biomass_grams = deduce_pasture_grams(yield, area = biomass_area), 
#          percent_moisture = NA_real_,
#          ml_notes = str_c(glue("Michael Liou: \"Biomass of 50 used from excel comment. Percent moisture missing. Biomass grams deduced as dry matter.\"",
#                                      yield = yield),
#                           note, sep = " | "),
#          comments = ml_notes
#          )


tbl_2004 <- pre_2004 |> select(any_of(harvesting_cols))
supp_2004 <- pre_2004 |> select(any_of(supp_harvesting_cols))

tbl_2004_loss <- pre_2004 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2004_loss <- pre_2004 |> select(any_of(supp_loss_cols)) |>
  filter(if_any(starts_with("loss"), \(x)!is.na(x)))

# retired in favor of other biomassing cols
# tbl_2004_bio <- pre_2004_bio |> select(any_of(biomassing_cols))
# supp_2004_bio <- pre_2004_bio |> select(any_of(supp_biomassing_cols))

# 12 exclosures
pre_2004_excl_pasture <- xl_pasture$exclosures |>
  filter(year == 2004) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2004,
                                      plot = plot,
                                      section = section,
                                      cut = cut,
                                      biomass = crop,
                                      method = method,
                                      component = component,
                                      coordinate = NA_character_),
    comments = stitch_notes(note, ml_note),
    stubble_inches = case_when(method == "exclosure"~3)
  )

pre_2004_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2004, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2004,
                                           plot = plot,
                                           section = "main",
                                           coordinate = subsample,
                                           biomass = "pasture",
                                           cut = cut),
         biomass_area = area,
         percent_moisture = moisture,
         biomass_date = date,
         method = type,
         biomass = "pasture",
         component = "shoots",
         biomass_width = case_when(biomass_area == 10.76391 ~ m_to_ft),
         biomass_length = case_when(biomass_area == 10.76391 ~ m_to_ft),
         stubble_inches = stubble_height,
         tenday = tendayperiod,
         cycle = cycle, 
         biomass_grams = grams) |> 
  mutate(
    ml_notes = if_else(is.na(ml_note), "", glue("Michael Liou: {ml_note}", ml_note = ml_note)),
    comments = stitch_notes(NA, ml_notes)) |> 
  ungroup()


# biomassing
tbl_2004_bio_pasture <- pre_2004_bio_pasture |> select(any_of(biomassing_cols))
supp_2004_bio_pasture <- pre_2004_bio_pasture |> select(any_of(supp_biomassing_cols))

# exclosure
tbl_2004_excl_pasture <- pre_2004_excl_pasture |> select(any_of(biomassing_cols))
supp_2004_excl_pasture <- pre_2004_excl_pasture |> select(any_of(supp_biomassing_cols))


# collect -----------------------------------------------------------------

tbl_2004_harvests <- tbl_2004
supp_2004_harvests <- supp_2004

tbl_2004_loss <- tbl_2004_loss
supp_2004_loss <- supp_2004_loss

tbl_2004_bio <- bind_rows(
  tbl_2004_bio_pasture, # biomassings
  tbl_2004_excl_pasture # exclosure
)

supp_2004_bio <- bind_rows(
  supp_2004_bio_pasture, # biomassings
  supp_2004_excl_pasture # exclosure
)

# xl_pasture$massings |> filter(year == 2004, type == "harvest")
# tbl_2004_harvests |> filter(crop == "pasture") |> get_yield() |>
#   select(harvest_tons_dm_per_acre)

