#TODO: Pastures ignored currently
# from arl yields
source("migration/yield_prep.R")

raw_2011 <- xl_arl$`2011`

pre_2011 <- raw_2011 |> 
  filter(crop != "p") |> 
  mutate(
    mydim = case_when(
      yield != 0 & yield_mult == 5.69 ~ "510_15",
      yield != 0 & yield_mult == 2.847 ~ "510_30",
      yield != 0 & yield_mult == 1.898 ~ "510_45",
      yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60",
      .default = NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop),
    guess_loss = NA,
    # guess_lbs = if_else(crop == "pasture" & yield != 0,
    #                     yield * (20 * 7 / 43560) * 2000 / kg_to_lbs / 1000,
    #                     NA)
  ) |>
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric),
         guess_area = 43560 / yield_mult) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2011,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2011,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X",
                                              product = crop,
                                              cut = cut),
    systematicharvestingloss_id = harvestingloss_id,
    harvest_length = coalesce(length, guess_length),
    harvest_width = coalesce(width, guess_width),
    harvest_area = coalesce(harvest_length * harvest_width, guess_area),
    harvest_date = date,
    harvest_lbs = if_else(crop == "pasture" & is.na(lbs) & yield == 0, 
                          0,
                          lbs),
    percent_moisture = moisture,
    loss_area = loss,
    # loss_width = loss_width,
    # loss_length = loss_length,
    loss_reason = lossreason,
    loss_fraction = sysloss,
    loss_category = syslossreason,
    sysloss_comments = case_when(
      crop == "alfalfa" & cut == 4 & plot == 211 ~ "Michael Liou: \"excel formula was 'new yield' = 'yield' / 0.8. This is an increase of 25% despite comment of increasing by 20%. I coded this as 20% systematic loss.\""),
    comments = stitch_notes(NA, note)
  )

tbl_2011 <- pre_2011 |> select(any_of(harvesting_cols))
supp_2011 <- pre_2011 |> select(any_of(supp_harvesting_cols)) |> 
  filter(if_any(-harvesting_id, \(x) !is.na(x)))

# losses
tbl_2011_loss <- pre_2011 |> select(any_of(loss_cols)) |> 
  drop_na(loss_area)
supp_2011_loss <- pre_2011 |> select(any_of(supp_loss_cols)) |> 
  drop_na(loss_reason)

# syslosses
tbl_2011_sysloss <- pre_2011 |> select(any_of(sysloss_cols)) |> 
  drop_na(loss_fraction)
supp_2011_sysloss <- pre_2011 |> select(any_of(supp_sysloss_cols)) |> 
  drop_na(sysloss_comments)

# collect -----------------------------------------------------------------

tbl_2011_harvests <- tbl_2011
supp_2011_harvests <- supp_2011


