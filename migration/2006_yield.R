source("migration/yield_prep.R")

# nothing here...

raw_2006 <- xl_arl$`2006`

pre_2006 <- raw_2006 |> 
  filter(bio != "yes" | is.na(bio)) |>
  # common dims
  mutate(
    mydim = case_when(
      yield_mult == 5.69 ~ "510_15",
      yield_mult == 2.847 ~ "510_30",
      yield_mult == 1.42353 ~ "510_60",
      .default = NA),
    guess_loss = if_else(num == 4 & plot %in% c("110", "208", "304", "413"),
                         loss_from_mult(yield_mult),
                         NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop)
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |>
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2006,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2006,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X",
                                              product = crop,
                                              cut = cut),
    harvest_length = coalesce(length, guess_length),
    harvest_width = coalesce(width, guess_width),
    harvest_area = harvest_length * harvest_width,
    harvest_date = date,
    harvest_lbs = lbs,
    percent_moisture = moisture,
    loss_area = coalesce(loss, guess_loss),
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    loss_comments = if_else(!is.na(guess_loss),
                            glue("Michael Liou: \"Loss estimated based on yield multiplier of {yield_mult} and comment about area adjusted for loss.\"",
                                 yield_mult = yield_mult),
                            NA),
    comments = stitch_notes(NA, note)
  )

# retired in favor of pastures, doesn't quite match up but kg->lbs and notes?
# pre_2006_bio <- raw_2006 |> 
#   filter(bio == "yes") |> 
#   mutate(
#     year = 2006,
#     biomass_date = date,
#     biomass = "pasture",
#     method = "exclosure",
#     component = "shoots",
#     cut = num,
#     biomass_grams = coalesce(lbs / kg_to_lbs / 2000, deduce_pasture_grams(yield)),
#     biomass_length = coalesce(length, 20),
#     biomass_width = coalesce(width, 7),
#     biomass_area = biomass_length * biomass_width,
#     biomass_date = date,
#     percent_moisture = NA,
#     biomassing_id = get_biomassing_id(year = year,
#                                       plot = plot,
#                                       section = "Main",
#                                       biomass = biomass,
#                                       method = method,
#                                       component = component,
#                                       cut = cut,
#                                       coordinate = "X"),
#     comments = glue("Michael Liou: \"Plot length x width (and biomass area) missing, assumed to be 20'x7'. Percent moisture also missing. Biomass grams deduced as dry matter from yield of {yield}.\"",
#                     yield = yield))

# pre biomassing because we don't have any of the calculation columns
# bioyield_cols <- c("biomassing_id", "plot", "biomass_date", "year",
#                    "percent_moisture", "biomass", 
#                    "method", "component", "yield")
# 
# supp_bioyield_cols <- c("biomassing_id", "comments")

tbl_2006 <- pre_2006 |> select(any_of(harvesting_cols))
supp_2006 <- pre_2006 |> select(any_of(supp_harvesting_cols))

tbl_2006_loss <- pre_2006 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2006_loss <- pre_2006 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(starts_with("loss"), \(x) !is.na(x)))

# retired
# tbl_2006_bio <- pre_2006_bio |> select(any_of(biomassing_cols))
# supp_2006_bio <- pre_2006_bio |> select(any_of(supp_biomassing_cols))

# calling it bioyield because it has different cols than biomassings table
# tbl_2006_bioyield <- pre_2006_bio |> select(any_of(bioyield_cols))
# supp_2006_bioyield <- pre_2006_bio |> select(any_of(supp_bioyield_cols))


## harvest pastures
pre_2006_pasture <- xl_pasture$massings |> 
  filter(type == "harvest", year == 2006) |>
  group_by(plot, subsample) |> 
  mutate(
    cut = row_number(),
    section = "Main",
    crop = "pasture",
    harvesting_id = get_harvest_id(year = 2006,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvest_date = date,
    harvest_area = coalesce(area, 510 * 60),
    harvest_length = case_when(harvest_area == 30600 ~ 510),
    harvest_width  = case_when(harvest_area == 30600 ~ 60),
    percent_moisture = coalesce(moisture, 80),
    tenday = tendayperiod,
    cycle = cycle,
    harvest_lbs = deduce_pasture_lbs(yield, area = harvest_area, moisture = percent_moisture),
    ) |> 
  rowwise() |>
  mutate(
    comments = case_when(is.na(area) & is.na(moisture) ~ 
                           glue("Michael Liou: assumed harvest area of 30600 and moisture of .80, harvest lbs deduced from yield of {yield}", yield = yield))
  ) |> 
  ungroup()

tbl_2006_pasture <- pre_2006_pasture |> select(any_of(harvesting_cols))
supp_2006_pasture <- pre_2006_pasture |> select(any_of(supp_harvesting_cols))

## biomassings
pre_2006_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2006, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2006,
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

tbl_2006_bio_pasture <- pre_2006_bio_pasture |> select(any_of(biomassing_cols))
supp_2006_bio_pasture <- pre_2006_bio_pasture |> select(any_of(supp_biomassing_cols))

# exclosures
pre_2006_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2006) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2006,
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

tbl_2006_bio_pasture_excl <- pre_2006_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2006_bio_pasture_excl <- pre_2006_bio_pasture_excl |> select(any_of(supp_biomassing_cols))

# collect -----------------------------------------------------------------

tbl_2006_harvests <- bind_rows(
  tbl_2006,
  tbl_2006_pasture
)
supp_2006_harvests <- bind_rows(
  supp_2006,
  supp_2006_pasture
)

tbl_2006_loss <- tbl_2006_loss
supp_2006_loss <- supp_2006_loss

tbl_2006_bio <- bind_rows(
  tbl_2006_bio_pasture,
  tbl_2006_bio_pasture_excl
)

supp_2006_bio <- bind_rows(
  supp_2006_bio_pasture,
  supp_2006_bio_pasture_excl
)

# matches up with 1.12 factor after converting from kg /lbs error
# tbl_2006_bio_pasture_excl |> get_biomass() |> 
#   mutate(old_bm = biomass_tons_dm_per_acre * kg_to_lbs * 1.12) |> 
#   select(biomass_date, plot, cut, percent_moisture, biomass_tons_dm_per_acre, old_bm) 
# pre_2006_bio |> View()

