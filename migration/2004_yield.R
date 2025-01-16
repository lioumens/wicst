# from arl yields
source("migration/yield_prep.R")

raw_2004 <- xl_arl$`2004`

pre_2004 <- raw_2004 |> 
  filter(is.na(bio)) |>
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
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2004,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2004,
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
    loss_area = loss,
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    comments = stitch_notes(NA, note)
  )

# 
pre_2004_bio <- raw_2004 |> filter(bio == "yes", yield != 0) |>
  rowwise() |> 
  mutate(plot = plot,
         method = "exclosure",
         biomass = "pasture",
         component = "shoots", 
         cut = num,
         biomassing_id = get_biomassing_id(year = 2004,
                                           plot = plot,
                                           section = "Main",
                                           biomass = "pasture",
                                           method = method, 
                                           component = component,
                                           coordinate = "X"),
         # biomass_length = coalesce(length, 20),
         # biomass_width = coalesce(length, 7),
         biomass_area = 50, # from comment
         biomass_date = date,
         biomass_grams = deduce_pasture_grams(yield, area = biomass_area), 
         percent_moisture = NA_real_,
         ml_notes = str_c(glue("Michael Liou: \"Biomass of 50 used from excel comment. Percent moisture missing. Biomass grams deduced as dry matter.\"",
                                     yield = yield),
                          note, sep = " | "),
         comments = ml_notes
         )



tbl_2004 <- pre_2004 |> select(any_of(harvesting_cols))
supp_2004 <- pre_2004 |> select(any_of(supp_harvesting_cols))

tbl_2004_loss <- pre_2004 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2004_loss <- pre_2004 |> select(any_of(supp_loss_cols)) |>
  filter(if_any(starts_with("loss"), \(x)!is.na(x)))

tbl_2004_bio <- pre_2004_bio |> select(any_of(biomassing_cols))
supp_2004_bio <- pre_2004_bio |> select(any_of(supp_biomassing_cols))





