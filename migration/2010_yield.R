source("migration/yield_prep.R")

raw_2010 <- xl_arl$`2010`

pre_2010 <- raw_2010 |> 
  # filter(crop != "p") |> # filter all pastures for now?
  filter(bio != "yes" | is.na(bio)) |>
  mutate(
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60",
      crop == "p" & is.na(bio) & plot == 112 ~ "510_60", # assume for 112
      .default = NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    guess_loss = NA,
    guess_lbs = if_else(crop == "pasture" & yield != 0 & plot == 112,
                        deduce_pasture_lbs(yield, moisture = moisture),
                        NA)
    # guess_lbs = if_else(crop == "pasture" & yield != 0,
    #                     yield * (20 * 7 / 43560) * 2000 / kg_to_lbs / 1000, # to grams
    #                     NA)
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric),
         guess_area = 43560 / yield_mult) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2010,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2010,
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
    harvest_lbs = case_when(crop == "pasture" & is.na(lbs) & yield == 0 ~0,
                            is.na(lbs) & !is.na(guess_lbs) ~ guess_lbs,
                            .default = lbs),
    percent_moisture = moisture,
    loss_area = loss,
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    # loss_fraction = sysloss,
    # loss_category = syslossreason,
    note = case_when(plot == 112 & cut == 1 & is.na(bio) ~ glue("Michael Liou: \"harvest lbs deducded from pasture yield of 1.5, estimating 510x60' as harvest area\""),
                     is.na(harvest_length) & !is.na(yield_mult)~ glue("Michael Liou: \"harvest area deduced from yield multiplier of {mult}\"", 
                                                                      mult =  yield_mult),
                     .default = note),
    comments = stitch_notes(NA, note)
  )

# harvests
tbl_2010 <- pre_2010 |> select(all_of(harvesting_cols))
supp_2010 <- pre_2010 |> select(any_of(supp_harvesting_cols)) |> 
  filter(if_any(-harvesting_id, \(x) !is.na(x)))

# losses
tbl_2010_loss <- pre_2010 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2010_loss <- pre_2010 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# no sysloss
# pasture harvests already included
# biomassings
pre_2010_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2010, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2010,
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

tbl_2010_bio_pasture <- pre_2010_bio_pasture |> select(any_of(biomassing_cols))
supp_2010_bio_pasture <- pre_2010_bio_pasture |> select(any_of(supp_biomassing_cols))

# pasture exclosures
pre_2010_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2010) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2010,
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

tbl_2010_bio_pasture_excl <- pre_2010_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2010_bio_pasture_excl <- pre_2010_bio_pasture_excl |> select(any_of(supp_biomassing_cols))

# collect -----------------------------------------------------------------

tbl_2010_harvests <- tbl_2010
supp_2010_harvests <- supp_2010

# losses
tbl_2010_loss <- tbl_2010_loss
supp_2010_loss <- supp_2010_loss

tbl_2010_bio <- bind_rows(
  tbl_2010_bio_pasture,
  tbl_2010_bio_pasture_excl
)

supp_2010_bio <- bind_rows(
  supp_2010_bio_pasture,
  supp_2010_bio_pasture_excl
)

# QA ----------------------------------------------------------------------

# supp_2010_harvests |> filter(harvesting_id == "H2010_A112MMX_PT_1") 
# tbl_2010_bio_pasture_excl |> get_biomass() |>
#   mutate(old_bm = biomass_tons_dm_per_acre * 1.12) |>
#   select(plot, cut, biomass_tons_dm_per_acre, old_bm) |>
#   arrange(plot)
# raw_2010 |> filter(crop == "p", yield_mult |> is.na()) |> View()

# 185 vs 183 in master
# tbl_2010_harvests |> filter(plot == 106) |> 
#   # mutate(harvest_area = 15077.93) |> 
#   mutate(percent_moisture = 18.7) |> 
#   get_yield() |> pull(corrected_bu_per_acre)

# # plot 112 has 1 harvest 3 exclosures
# tbl_2010_bio |> filter(plot == 112) |> 
#   View()
# 
# tbl_2010_harvests |> filter(plot == 112) |> 
#   View()
