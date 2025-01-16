source("migration/yield_prep.R")

raw_2005 <- xl_arl$`2005`

pre_2005 <- raw_2005 |> 
  filter(is.na(bio)) |>
  mutate(
  mydim = case_when(
    yield != 0 & yield_mult == 5.69 ~ "510_15",
    yield != 0 & yield_mult == 2.847 ~ "510_30",
    yield != 0 & yield_mult == 1.898 ~ "510_45",
    yield != 0 & yield_mult %in% c(1.42353, 1.4235) ~ "510_60"
    .default = NA),
  guess_loss = NA,
# guess_loss = case_when(num == 1 & plot %in% c("101","214","303","401") ~ loss_from_mult(yield_mult, width = 30),
#                        num == 1 & plot %in% c("404") ~ loss_from_mult(yield_mult, width = 60),
#                        .default = NA),
  crop = case_match(crop, !!!arl_crop_dict, .default = crop),
  guess_lbs = if_else(crop == "pasture" & yield != 0,
                      yield * (20 * 7 / 43560) * 2000 / kg_to_lbs / 1000,
                      NA)
) |> 
  separate_wider_delim(mydim, delim = "_", names = c("guess_length", "guess_width")) |>
  mutate(across(c("guess_length", "guess_width"), as.numeric)) |> 
  mutate(
    plot = plot,
    section = "Main",
    cut = num,
    harvesting_id = get_harvest_id(year = 2005,
                                   plot = plot,
                                   section = section,
                                   product = crop,
                                   cut = cut),
    harvestingloss_id = get_harvestingloss_id(year = 2005,
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

deduce_pasture_grams <- function(yield) {
  yield * 20 * 7 / 43560 * 2000 / kg_to_lbs * 1000
}

pre_2005_bio <- raw_2005 |> 
  filter(bio == "yes") |>
  mutate(
    year = 2006,
    biomass_grams = deduce_pasture_grams(yield),
    biomass_length = coalesce(length, 20),
    biomass_width = coalesce(width, 7),
    biomass_area = biomass_length * biomass_width,
    biomass_date = date,
    percent_moisture = NA,
    biomass = "pasture",
         method = "exclosure",
         component = "shoots",
         cut = num,
         biomassing_id = get_biomassing_id(year = year,
                                           plot = plot,
                                           section = "Main",
                                           biomass = biomass,
                                           method = method,
                                           component = component,
                                           cut = cut,
                                           coordinate = "X"),
    ml_notes = glue("Michael Liou: \"Plot length x width (and biomass area) missing, assumed to be 20'x7'. Percent moisture also missing. Biomass grams deduced as dry matter from yield of {yield}.\"",
                    yield = yield),
    comments = ml_notes)



tbl_2005 <- pre_2005 |> select(any_of(harvesting_cols))
supp_2005 <- pre_2005 |> select(any_of(supp_harvesting_cols)) |> 
  filter(if_any(-harvesting_id, \(x)!is.na(x)))

tbl_2005_loss <- pre_2005 |> select(any_of(loss_cols)) |> 
  drop_na(loss_area)
supp_2005_loss <- pre_2005 |> select(any_of(supp_loss_cols)) |>
  filter(if_any(starts_with("loss"), \(x) !is.na(x)))

tbl_2005_bio <- pre_2005_bio |> select(any_of(biomassing_cols))
supp_2005_bio <- pre_2005_bio |> select(any_of(supp_biomassing_cols))

# tbl_2005_bioyield <- pre_2005_bio |> select(any_of(bioyield_cols))

# ((g / 1000) * kg_to_lbs / 2000) / (20 * 7 / 43560) = y
# y * (20 * 7 / 43560) * 2000 / kg_to_lbs * 1000 
# tons / acre

