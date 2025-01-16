source("migration/yield_prep.R")

# nothing here...




raw_2006 <- xl_arl$`2006`

pre_2006 <- raw_2006 |> 
  filter(bio != "yes" | is.na(bio)) |>
  # common dims
  mutate(
    mydim = case_when(
      yield_mult == 5.69 ~ "510_15",
      yield_mult == 2.847 ~ "510_30",
      yield_mult == 1.42353 ~ "510_60",
      .default = NA),
    guess_loss = if_else(num == 4 & plot %in% c("110", "208", "304", "413"),
                         loss_from_mult(yield_mult),
                         NA),
    crop = case_match(crop, !!!arl_crop_dict, .default = crop)
    ) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |>
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2006,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2006,
                                              plot = plot,
                                              section = section,
                                              coordinate = "X",
                                              product = crop,
                                              cut = cut),
    harvest_length = coalesce(length, guess_length),
    harvest_width = coalesce(width, guess_width),
    harvest_area = harvest_length * harvest_width,
    harvest_date = date,
    harvest_lbs = lbs,
    percent_moisture = moisture,
    loss_area = coalesce(loss, guess_loss),
    loss_width = loss_width,
    loss_length = loss_length,
    loss_reason = lossreason,
    loss_comments = if_else(!is.na(guess_loss),
                           glue("Michael Liou: \"Loss estimated based on yield multiplier of {yield_mult} and comment about area adjusted for loss.\"",
                                yield_mult = yield_mult),
                           NA),
    comments = stitch_notes(NA, note)
  )

pre_2006_bio <- raw_2006 |> 
  filter(bio == "yes") |> 
  mutate(
    year = 2006,
    biomass_date = date,
    biomass = "pasture",
    method = "exclosure",
    component = "shoots",
    cut = num,
    biomass_grams = coalesce(lbs / kg_to_lbs / 2000, deduce_pasture_grams(yield)),
    biomass_length = coalesce(length, 20),
    biomass_width = coalesce(width, 7),
    biomass_area = biomass_length * biomass_width,
    biomass_date = date,
    percent_moisture = NA,
    biomassing_id = get_biomassing_id(year = year,
                                      plot = plot,
                                      section = "Main",
                                      biomass = biomass,
                                      method = method,
                                      component = component,
                                      cut = cut,
                                      coordinate = "X"),
    comments = glue("Michael Liou: \"Plot length x width (and biomass area) missing, assumed to be 20'x7'. Percent moisture also missing. Biomass grams deduced as dry matter from yield of {yield}.\"",
                    yield = yield))

# pre biomassing because we don't have any of the calculation columns
# bioyield_cols <- c("biomassing_id", "plot", "biomass_date", "year",
#                    "percent_moisture", "biomass", 
#                    "method", "component", "yield")
# 
# supp_bioyield_cols <- c("biomassing_id", "comments")

tbl_2006 <- pre_2006 |> select(any_of(harvesting_cols))
supp_2006 <- pre_2006 |> select(any_of(supp_harvesting_cols))

tbl_2006_loss <- pre_2006 |> select(any_of(loss_cols)) |> drop_na(loss_area)
supp_2006_loss <- pre_2006 |> select(any_of(supp_loss_cols)) |> 
  filter(if_any(starts_with("loss"), \(x) !is.na(x)))

tbl_2006_bio <- pre_2006_bio |> select(any_of(biomassing_cols))
supp_2006_bio <- pre_2006_bio |> select(any_of(supp_biomassing_cols))

# calling it bioyield because it has different cols than biomassings table
# tbl_2006_bioyield <- pre_2006_bio |> select(any_of(bioyield_cols))
# supp_2006_bioyield <- pre_2006_bio |> select(any_of(supp_bioyield_cols))
