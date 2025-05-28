#TODO: excluding pasture information
source("migration/yield_prep.R")

raw_2008 <- xl_arl$`2008`

pre_2008 <- raw_2008 |> 
  filter(!(crop == "p" & is.na(yield_mult)),
         crop != "swg") |> # exclude pasture exclosures and switchgrass
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
    harvesting_id = get_harvest_id(year = 2008,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2008,
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
    # note = case_when(is.na(harvest_length) & !is.na(yield_mult) & yield != 0 ~ 
    #                    glue("Michael Liou: \"harvest area deduced from yield multiplier of {mult}.\"",
    #                         mult = yield_mult),
                     # .default = note),
    comments = stitch_notes(NA, note),
    tenday = pasture_tendayperiod,
    cycle = pasture_cycle
  )

# harvests w/ pastures
tbl_2008 <- pre_2008 |> select(any_of(harvesting_cols))
supp_2008 <- pre_2008 |> select(any_of(supp_harvesting_cols))

# losses
tbl_2008_loss <- pre_2008 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2008_loss <- pre_2008 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# bio/exclosure
pre_2008_bio_pasture <- xl_pasture$massings |> 
  filter(year == 2008, !is.na(yield), type == "quadrat") |>
  group_by(year, plot, subsample) |> 
  arrange(date) |> 
  mutate(cut = row_number(),
         biomassing_id = get_biomassing_id(year = 2008,
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

tbl_2008_bio_pasture <- pre_2008_bio_pasture |> select(any_of(biomassing_cols))
supp_2008_bio_pasture <- pre_2008_bio_pasture |> select(any_of(supp_biomassing_cols))

# exclosures
pre_2008_bio_pasture_excl <- xl_pasture$exclosures |>
  filter(year == 2008) |> 
  mutate(
    crop = "pasture",
    biomass = crop,
    section = "Main",
    component = "shoots",
    biomass_date = date,
    biomassing_id = get_biomassing_id(year = 2008,
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

tbl_2008_bio_pasture_excl <- pre_2008_bio_pasture_excl |> select(any_of(biomassing_cols))
supp_2008_bio_pasture_excl <- pre_2008_bio_pasture_excl |> select(any_of(supp_biomassing_cols))

# 1.12x factor for exclosures for some reason, except for 207 and 302 on cut 2.
# area is different for those, see comments. Everything else uses multiplier
# tbl_2008_bio_pasture_excl |> get_biomass() |>
#   mutate(old_bm = biomass_tons_dm_per_acre * 1.12) |>
#   select(plot, cut, biomass_tons_dm_per_acre, old_bm) |>
#   arrange(plot)
# raw_2008 |> filter(crop == "p", yield_mult |> is.na()) |> View()



# collect -----------------------------------------------------------------

tbl_2008_harvests <- tbl_2008
supp_2008_harvests <- supp_2008

tbl_2008_loss <- tbl_2008_loss
supp_2008_loss <- supp_2008_loss

tbl_2008_bio <- bind_rows(
  tbl_2008_bio_pasture,
  tbl_2008_bio_pasture_excl
)

supp_2008_bio <- bind_rows(
  supp_2008_bio_pasture,
  supp_2008_bio_pasture_excl
)


