# from arl yields
source("migration/yield_prep.R")

raw_1999 <- xl_arl$`1999`

pre_1999 <- raw_1999 |> 
  mutate(
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60",
      .default = NA),
    guess_loss = loss_from_mult(yield_mult, width),
    # guess_lbs = if_else(crop == "pasture" & yield != 0,
    #                     yield * (20 * 7 / 43560) * 1999 / kg_to_lbs / 1000,
    #                     NA)
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 1999,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 1999,
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
    loss_area = coalesce(loss, guess_loss),
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    loss_comments = if_else(!is.na(guess_loss) & is.na(loss),
                            glue("Michael Liou: \"Loss deduced from yield multiplier of {yield_mult}\"",
                                 yield_mult = yield_mult),
                            NA),
    comments = stitch_notes(NA, note)
  )



tbl_1999 <- pre_1999 |> select(any_of(harvesting_cols))
supp_1999 <- pre_1999 |> select(any_of(supp_harvesting_cols)) |>
  filter(if_any(c(harvest_length, harvest_width, comments), \(x) !is.na(x)))

# no losses
# no bios


pre_1999_bio_pasture <- xl_pasture$massings |> 
  filter(year == 1999, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 1999,
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

tbl_1999_bio_pasture <- pre_1999_bio_pasture |> select(any_of(biomassing_cols))
supp_1999_bio_pasture <- pre_1999_bio_pasture |> select(any_of(supp_biomassing_cols))

# collect -----------------------------------------------------------------

tbl_1999_harvests <- tbl_1999
supp_1999_harvests <- supp_1999

tbl_1999_loss <- bind_rows()
supp_1999_loss <- bind_rows()

tbl_1999_bio <- bind_rows(
  tbl_1999_bio_pasture
)

supp_1999_bio <- bind_rows(
  supp_1999_bio_pasture
)
