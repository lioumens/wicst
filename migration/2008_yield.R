#TODO: excluding pasture information
source("migration/yield_prep.R")

raw_2008 <- xl_arl$`2008`

pre_2008 <- raw_2008 |> 
  filter(crop != "p" & crop != "swg") |> # exclude pastures and switchgrass
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
    comments = stitch_notes(NA, note)
  )

# harvests
tbl_2008 <- pre_2008 |> select(any_of(harvesting_cols))
supp_2008 <- pre_2008 |> select(any_of(supp_harvesting_cols))

# losses
tbl_2008_loss <- pre_2008 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2008_loss <- pre_2008 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(-harvestingloss_id, \(x) !is.na(x)))

# collect -----------------------------------------------------------------

tbl_2008_harvests <- tbl_2008
supp_2008_harvests <- supp_2008

