source("migration/yield_prep.R")

raw_2012 <- xl_arl$`2012`

pre_2012 <- raw_2012 |> 
  filter(crop != "p") |> # exclude pastures
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
    harvesting_id = get_harvest_id(year = 2012,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2012,
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
    # loss_width = loss_width,
    # loss_length = loss_length,
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
tbl_2012 <- pre_2012 |> select(any_of(harvesting_cols))
supp_2012 <- pre_2012 |> select(any_of(supp_harvesting_cols))

# losses
tbl_2012_loss <- pre_2012 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2012_loss <- pre_2012 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# manually adding 2012 harvest
pre_2012_pasture <- tibble_row(
  plot = 207,
  crop = "pasture",
  section = "Main",
  harvesting_id = get_harvest_id(year = 2012,
                                 plot = plot,
                                 section = section,
                                 product = crop,
                                 cut = 1),
  harvest_date = as.POSIXct("2012-05-24", tz = "UTC"),
  harvest_length = 510,
  harvest_width = 60,
  harvest_area = harvest_length * harvest_width, 
  percent_moisture = 80, # assumed 80
  yield = 1.63,
  harvest_lbs = deduce_pasture_lbs(yield, area = harvest_area, moisture = percent_moisture),
  comments = glue("Michael Liou: area assumed to be 510'x60' and moisture assumed to be .8, harvest lbs deduced from yield of {yield}", yield = yield)
)

tbl_2012_pasture <- pre_2012_pasture |> select(any_of(harvesting_cols))
supp_2012_pasture <- pre_2012_pasture |> select(any_of(supp_harvesting_cols))

# no quadrats
pre_2012_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2012) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2012,
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

tbl_2012_bio_pasture_excl <- pre_2012_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2012_bio_pasture_excl <- pre_2012_bio_pasture_excl |> select(any_of(supp_biomassing_cols))


# collect -----------------------------------------------------------------

tbl_2012_harvests <- bind_rows(
  tbl_2012,
  tbl_2012_pasture
)
supp_2012_harvests <- bind_rows(
  supp_2012,
  supp_2012_pasture
)

tbl_2012_bio <- bind_rows(
  tbl_2012_bio_pasture_excl
)

supp_2012_bio <- bind_rows(
  supp_2012_bio_pasture_excl
)


# QA ----------------------------------------------------------------------

# tbl_2012_bio_pasture_excl |> get_biomass() |>
#   mutate(old_bm = biomass_tons_dm_per_acre * 1.12) |>
#   select(plot, cut, biomass_tons_dm_per_acre, old_bm) |>
#   arrange(plot)
# raw_2012 |> filter(crop == "p", yield_mult |> is.na()) |> View()





