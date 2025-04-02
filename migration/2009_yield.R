source("migration/yield_prep.R")

raw_2009 <- xl_arl$`2009`

pre_2009 <- raw_2009 |> 
  filter(crop != "p" & !(crop == "c" & plot == 409)) |> # exclude extra corn plot
  mutate(
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235, 1.424) ~ "510_60",
      .default = NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    guess_loss = NA,
    # guess_lbs = if_else(crop == "pasture" & yield != 0,
    #                     yield * (20 * 7 / 43560) * 2000 / kg_to_lbs / 1000,
    #                     NA)
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric),
         guess_area = if_else(yield == 0, NA, 43560 / yield_mult)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2009,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2009,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X",
                                              product = crop,
                                              cut = cut),
    # systematicharvestingloss_id = harvestingloss_id,
    harvest_length = coalesce(length, guess_length),
    harvest_width = coalesce(width, guess_width),
    harvest_area = coalesce(harvest_length * harvest_width, guess_area),
    harvest_date = date,
    harvest_lbs = if_else(crop == "pasture" & is.na(lbs) & yield == 0, 
                          0,
                          lbs),
    percent_moisture = moisture,
    loss_area = loss,
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    # loss_fraction = sysloss,
    # loss_category = syslossreason,
    note = case_when(is.na(harvest_length) & !is.na(yield_mult) & yield != 0 ~ 
                       glue("Michael Liou: \"harvest area deduced from yield multiplier of {mult}.\"",
                            mult = yield_mult),
                     .default = note),
    comments = stitch_notes(NA, note)
  )

# harvests
tbl_2009 <- pre_2009 |> select(any_of(harvesting_cols))
supp_2009 <- pre_2009 |> select(any_of(supp_harvesting_cols))

# losses
tbl_2009_loss <- pre_2009 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2009_loss <- pre_2009 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# harvests not in arl, sourced from pastures file
pre_2009_pasture <- xl_pasture$massings |> 
  filter(type == "harvest", year == 2009) |>
  group_by(plot, subsample) |> 
  mutate(
    cut = row_number(),
    section = "Main",
    crop = "pasture",
    harvesting_id = get_harvest_id(year = 2009,
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
    # assumes no existing note...
    comments = case_when(is.na(area) & is.na(moisture) ~ 
                           glue("Michael Liou: assumed harvest area of 30600 and moisture of .80, harvest lbs deduced from yield of {yield}", yield = yield))
  ) |> 
  ungroup()

tbl_2009_pasture <- pre_2009_pasture |> select(any_of(harvesting_cols))
supp_2009_pasture <- pre_2009_pasture |> select(any_of(supp_harvesting_cols))

# biomassings
# bio/exclosure
pre_2009_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2009, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2009,
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

tbl_2009_bio_pasture <- pre_2009_bio_pasture |> select(any_of(biomassing_cols))
supp_2009_bio_pasture <- pre_2009_bio_pasture |> select(any_of(supp_biomassing_cols))

# exclosures
pre_2009_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2009) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2009,
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

tbl_2009_bio_pasture_excl <- pre_2009_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2009_bio_pasture_excl <- pre_2009_bio_pasture_excl |> select(any_of(supp_biomassing_cols))

# tbl_2009_bio_pasture_excl |> get_biomass() |>
#   mutate(old_bm = biomass_tons_dm_per_acre * 1.12) |>
#   select(plot, cut, biomass_tons_dm_per_acre, old_bm) |>
#   arrange(plot)
# raw_2009 |> filter(crop == "p", yield_mult |> is.na()) |> View()

# collect -----------------------------------------------------------------

# make sure collection right

tbl_2009_harvests <- bind_rows(
  tbl_2009,
  tbl_2009_pasture
)

supp_2009_harvests <- bind_rows(
  supp_2009,
  tbl_2009_pasture
)

tbl_2009_loss <- tbl_2009_loss
supp_2009_loss <- supp_2009_loss

tbl_2009_bio <- bind_rows(
  tbl_2009_bio_pasture,
  tbl_2009_bio_pasture_excl
)

supp_2009_bio <- bind_rows(
  supp_2009_bio_pasture,
  supp_2009_bio_pasture_excl
)
