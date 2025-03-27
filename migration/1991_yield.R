# from arl yields
source("migration/yield_prep.R")

raw_1991 <- xl_arl$`1991`

pre_1991 <- raw_1991 |> 
  mutate(
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult %in% c(2.847, 2.84706) ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60",
      .default = NA),
    guess_loss = loss_from_mult(yield_mult, width),
    # guess_lbs = if_else(crop == "pasture" & yield != 0,
    #                     yield * (20 * 7 / 43560) * 2000 / kg_to_lbs / 1000,
    #                     NA)
  ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 1991,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 1991,
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



tbl_1991 <- pre_1991 |> select(any_of(harvesting_cols))
supp_1991 <- pre_1991 |> select(any_of(supp_harvesting_cols)) |>
  filter(if_any(c(harvest_length, harvest_width, comments), \(x) !is.na(x)))

# no losses
# no bios

# collect -----------------------------------------------------------------

tbl_1991_harvests <- tbl_1991
supp_1991_harvests <- supp_1991

tbl_1991_loss <- bind_rows()
supp_1991_loss <- bind_rows()




