db_harvestings |> 
  left_join(db_harvestingdetails, by = "harvesting_id") |> 
  filter(product == "pasture",
         harvest_lbs > 0,
         between(year(harvest_date), 1993, 1994)) |> 
  get_yield() |>
  mutate(type = "harvest") |>
  select(harvesting_id, harvest_date, harvest_lbs, harvest_tons_dm_per_acre, plot_id, tenday, cycle, comments) |> 
  bind_rows(db_biomassings |> 
              filter(biomass == "pasture", biomass_grams > 0, between(year(biomass_date), 1993, 1994)) |> 
              get_biomass() |> 
              mutate(type = "massing")) |> 
  mutate(yield = coalesce(harvest_tons_dm_per_acre, biomass_tons_dm_per_acre),
         date = coalesce(harvest_date, biomass_date)) |> 
  select(harvesting_id, date, type, plot_id, yield) |>
  arrange(date, plot_id) |> pull(yield) |> 
  clipr::write_clip()

# nothing
db_biomassings |> filter(biomass == "pasture", year(biomass_date) == 2017) |> 
  get_biomass() |> 
  select(biomass_date, plot_id, biomass_tons_dm_per_acre) |> 
  group_by(plot_id) |> 
  summarize(avg_yield = sum(biomass_tons_dm_per_acre))
db_harvestings |> filter(product == "pasture", year(harvest_date) == 2017) |> get_yield() |> 
  select(plot_id, harvest_tons_dm_per_acre)

db_biomassings |> filter(biomass == "pasture", year(biomass_date) == 2015, method == "quadrat")  |> View()
db_harvestings |> filter(product == "pasture", year(harvest_date) == 2015) |> left_join(db_harvestingdetails, by= "harvesting_id") |> View()

# 2013 very weird
db_biomassings |> filter(biomass == "pasture", year(biomass_date) == 2013) |> 
  get_biomass() |> View()
db_harvestings |> filter(product == "pasture", year(harvest_date) == 2013)

# 2013 very weird
db_biomassings |> filter(biomass == "pasture", year(biomass_date) == 2021) |> 
  get_biomass() |> View()
db_harvestings |> filter(product == "pasture", year(harvest_date) == 2021) |> get_yield() |> View()

