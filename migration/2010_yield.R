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
                        yield * (510 * 60 / 43560) * 2000,
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
# biomassing ignored

# collect -----------------------------------------------------------------

tbl_2010_harvests <- tbl_2010
supp_2010_harvests <- supp_2010

# QA ----------------------------------------------------------------------

# 185 vs 183 in master
# tbl_2010_harvests |> filter(plot == 106) |> 
#   # mutate(harvest_area = 15077.93) |> 
#   mutate(percent_moisture = 18.7) |> 
#   get_yield() |> pull(corrected_bu_per_acre)
