source("migration/yield_prep.R")

raw_2007 <- xl_arl$`2007`

pre_2007 <- raw_2007 |> 
  filter(!(crop == "p" & is.na(lbs))) |> 
  # filter(crop != "p") |> # exclude pastures
  mutate(
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235, 1.424) ~ "510_60",
      .default = NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    guess_loss = if_else(!is.na(lossreason) & is.na(loss), 
                         loss_from_mult(yield_mult, width = width, length = length),
                         NA),
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
    harvesting_id = get_harvest_id(year = 2007,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2007,
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
    loss_area = coalesce(loss, guess_loss),
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    # loss_fraction = sysloss,
    # loss_category = syslossreason
  ) |> 
  rowwise() |> 
  mutate(
    note = case_when(is.na(harvest_length) & !is.na(yield_mult) & yield != 0 & is.na(note) ~
                       glue("Michael Liou: \"harvest area deduced from yield multiplier of {mult}.\"",
                            mult = yield_mult),
                     is.na(loss) & !is.na(guess_loss) & !is.na(note) ~ 
                       str_c(c(
                         glue("Michael Liou: \"harvest losses deduced from {harvest_length}x{harvest_width}' plot with yield multiplier of {mult}\"",
                              harvest_length = harvest_length,
                              harvest_width = harvest_width,
                              mult = yield_mult),
                         note),
                         collapse = " | "),
                     .default = note),
    comments = stitch_notes(NA, note)
  )

# harvests
tbl_2007 <- pre_2007 |> select(any_of(harvesting_cols))
supp_2007 <- pre_2007 |> select(any_of(supp_harvesting_cols))

tbl_2007_loss <- pre_2007 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2007_loss <- pre_2007 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# left here for pasture
# pasture harvests d
# from cs6 hay yields 
# raw_2007 |> filter(crop == "p", !is.na(yield_mult)) |> 
#   mutate()

# biomassings?
# exclosures?

pre_2007_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2007, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2007,
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

tbl_2007_bio_pasture <- pre_2007_bio_pasture |> select(any_of(biomassing_cols))
supp_2007_bio_pasture <- pre_2007_bio_pasture |> select(any_of(supp_biomassing_cols))

# exclosures
pre_2007_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2007) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2007,
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

tbl_2007_bio_pasture_excl <- pre_2007_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2007_bio_pasture_excl <- pre_2007_bio_pasture_excl |> select(any_of(supp_biomassing_cols))

# 1.12x factor for exclosures for some reason
# tbl_2007_bio_pasture_excl |> get_biomass() |> 
#   mutate(old_bm = biomass_tons_dm_per_acre * 1.12) |> 
#   select(plot, cut, biomass_tons_dm_per_acre, old_bm) |>
#   arrange(plot)
# raw_2007 |> filter(crop == "p", yield_mult |> is.na()) |> View()

# collect -----------------------------------------------------------------

tbl_2007_harvests <- tbl_2007
supp_2007_harvests <- supp_2007

tbl_2007_loss <- tbl_2007_loss
supp_2007_loss <- supp_2007_loss

tbl_2007_bio <- bind_rows(
  tbl_2007_bio_pasture,
  tbl_2007_bio_pasture_excl
)

supp_2007_bio <- bind_rows(
  supp_2007_bio_pasture,
  supp_2007_bio_pasture_excl
)



