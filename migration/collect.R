
# source files
if (!exists("tbl_2023_harvests")) source("migration/2023_yield.R")
if (!exists("tbl_2022_harvests")) source("migration/2022_yield.R")
if (!exists("tbl_2021_harvests")) source("migration/2021_yield.R")
if (!exists("tbl_2020_harvests")) source("migration/2020_yield.R")
if (!exists("tbl_2019_harvests")) source("migration/2019_yield.R")
if (!exists("tbl_2018_harvests")) source("migration/2018_yield.R")
if (!exists("tbl_2017_harvests")) source("migration/2017_yield.R")
if (!exists("tbl_2016_harvests")) source("migration/2016_yield.R")
if (!exists("tbl_2015_harvests")) source("migration/2015_yield.R")

load("data/agcal_20241210.Rdata")
load("data/core_20241206.Rdata")

# wicst ------------------------------------------------------------------


## core --------------------------------------------------------------------

# as is

## agcal -------------------------------------------------------------------

db_plantings <- xl_agcal$plantings
db_limings <- xl_agcal$limings 
db_fertilizings <- xl_agcal$fertilizings


## harvestings -------------------------------------------------------------
tbl_harvests <- bind_rows(
  tbl_2023_harvests,
  tbl_2022_harvests,
  tbl_2021_harvests,
  tbl_2020_harvests,
  tbl_2019_harvests,
  tbl_2018_harvests,
  tbl_2017_harvests,
  tbl_2016_harvests,
  tbl_2015_harvests,
) |> mutate(plot = glue("A{plot}"))

supp_harvests <- bind_rows(
  supp_2023_harvests,
  supp_2022_harvests,
  supp_2021_harvests,
  supp_2020_harvests,
  supp_2019_harvests,
  supp_2018_harvests,
  supp_2017_harvests,
  supp_2016_harvests,
  supp_2015_harvests
)


## harvest losses ----------------------------------------------------------

tbl_loss <- bind_rows(
  # tbl_2023_loss # no 2023 losses
  # tbl_2022_loss # also empty
  tbl_2021_loss, # empty
  tbl_2020_loss,
  tbl_2019_loss,
  tbl_2018_loss,
  tbl_2017_loss, # empty
  tbl_2016_loss, # empty
  tbl_2015_loss,
)


supp_loss <- bind_rows(
  supp_2022_loss, # empty
  supp_2021_loss, # empty
  supp_2020_loss,
  supp_2019_loss,
  supp_2018_loss,
  supp_2017_loss, # empty
  supp_2016_loss, # empty
  supp_2015_loss
)

tbl_sysloss <- bind_rows(
  # tbl_2023_sysloss # none, no def
  tbl_2022_sysloss,
  tbl_2021_sysloss,
  tbl_2020_sysloss, # empty
  tbl_2019_sysloss, # empty
  tbl_2018_sysloss, # empty,
  tbl_2017_sysloss, # empty
  tbl_2016_sysloss, # empty
  tbl_2015_sysloss, # empty
)

supp_sysloss <- bind_rows(
  # supp_2023_sysloss # none, no def
  supp_2022_sysloss,
  supp_2021_sysloss,
  supp_2020_sysloss, # empty
  supp_2019_sysloss, # empty
  supp_2018_sysloss, # empty,
  supp_2017_sysloss, # empty
  supp_2016_sysloss, # empty
  supp_2015_sysloss, # empty
)


## Biomassings -------------------------------------------------------------

tbl_bio <- bind_rows(
  tbl_2023_bio,
  tbl_2022_bio,
  tbl_2021_bio,
  tbl_2020_bio,
  tbl_2019_bio,
  tbl_2018_bio,
  tbl_2017_bio,
  tbl_2016_bio,
  tbl_2015_bio,
) |> mutate(plot = glue("A{plot}"))

supp_bio <- bind_rows(
  supp_2023_bio,
  supp_2022_bio,
  supp_2021_bio,
  supp_2020_bio,
  supp_2019_bio,
  supp_2018_bio, # empty
  supp_2017_bio,
  supp_2016_bio,
  supp_2015_bio,
)

## canopeo ----------------------------------------------------------------
tbl_can <- bind_rows(
  tbl_2023_can,
  tbl_2022_can,
  tbl_2021_can,
  tbl_2020_can,
  tbl_2019_can, # empty
  tbl_2018_can, # empty
  # empty 2017 - 2015
)

supp_can <- bind_rows(
  # supp_2023_can,
  supp_2022_can,
  supp_2021_can, # empty
  supp_2020_can, # empty
  supp_2019_can, # empty
  supp_2018_can, # empty
  # empty 2017 - 2015
)

db_can <- tbl_can |> 
  mutate(
    plot = str_c("A", plot),
    coordinate = case_match(coordinate,
                            c("North", "N")~"North",
                            c("Center", "Central", "C")~"Center",
                            c("South", "S")~"South"))
db_candetails <- supp_can

## yieldings ---------------------------------------------------------------

product_codes <- list(
  c("A1", "A2", "Ai", "Aii", "ORG A1", "alfalfa", "a", "dsA") ~ "alfalfa",
  c("Continuous Corn", "Corn", "Organic Corn", "Strip Till Corn", "Strip-till Corn", "Strip-till corn", "organic corn", "corn") ~ "corn",
  c("No-till Soybean", "No-till Soybeans","NT soy", "Org. soy", "Organic Soybean", "Organic Soybeans", "organic soybeans", "soybean", "soybeans") ~ "soybean",
  c("oat grain")~"oat grain",
  c("Oat Straw")~ "oat straw",
  c("Pasture", "pasture")~ "pasture",
  c("Wheat", "wheat grain", "w/cl", "wheat") ~ "wheat grain",
  c("oatlage", "o/A", "o/a") ~ "oatlage",
  c("wheat straw") ~ "wheat straw",
  c("wheatlage") ~ "wheatlage"
)

biomass_codes <- list(
  c("Pasture", "pasture")~ "pasture",
  c("weeds", "weed")~"weed",
  c("rye")~"rye",
  c("bc", "berseem clover", "berseem_clover")~"berseem clover",
  c("clover")~ "clover",
  c("alfalfa")~"alfalfa",
  c("oats", "oat") ~ "oat",
  c("quack")~"quack",
  c("wheat")~"wheat",
  c("residue")~"residue"
)

coordinate_codes <- list(
  c("Central", "C", "Center", "central") ~ "Center",
  c("N", "North", "north") ~ "North",
  c("South", "S", "south")~"South"
)

pre_yield <- bind_rows(tbl_harvests |> add_column(yielding_type = "harvesting"),
                       tbl_bio |> add_column(yielding_type = "biomassing")) |> 
  mutate(yielding_id = coalesce(harvesting_id, biomassing_id),
         plot_id = plot,
         product_description = crop,
         product = case_match(crop, !!!product_codes),
         biomass = case_match(biomass, !!!biomass_codes),
         coordinate = case_match(coordinate, !!!coordinate_codes, .default = coordinate))


## db ----------------------------------------------------------------------

# col order
db_harvesting_cols <- append(harvesting_cols, "yielding_type", after = 1) %>%
  case_match(.,
             "plot"~"plot_id",
             "crop"~"product",
             .default = .) %>% 
  append("product_description", after = 5)

db_biomassing_cols <- append(biomassing_cols, "yielding_type", after = 1) %>%
  case_match(., "plot"~"plot_id", .default = .)

# yields
db_yieldings <- pre_yield |> 
  select(yielding_id, yielding_type)
db_harvestings <- pre_yield |> 
  filter(yielding_type == "harvesting") |> 
  select(all_of(db_harvesting_cols))
db_harvestingdetails <- supp_harvests
db_biomassings <- pre_yield |> 
  filter(yielding_type == "biomassing") |> 
  select(all_of(db_biomassing_cols))

db_biomassingdetails <- supp_bio

# losses
pre_genlosses <- bind_rows(tbl_loss |> add_column(loss_type = "direct") |> 
                             rename(directloss_id = harvestingloss_id),
                           tbl_sysloss |> add_column(loss_type = "systematic") |> 
                             rename(systematicloss_id = systematicharvestingloss_id)) |> 
  mutate(loss_id = coalesce(directloss_id, systematicloss_id))

db_genlosses <- pre_genlosses |> select(loss_id, loss_type, harvesting_id)

db_directlosses <- pre_genlosses |> filter(loss_type == "direct") |>
  select(directloss_id, any_of(loss_cols))
db_directlossdetails <- supp_loss |> rename(directloss_id = harvestingloss_id)
db_systematiclosses <- pre_genlosses |> filter(loss_type == "systematic") |> 
  select(systematicloss_id, any_of(sysloss_cols))
db_systematiclossdetails <- supp_sysloss |>
  rename(systematicloss_id = systematicharvestingloss_id)

# ei ----------------------------------------------------------------------

## harvestings -------------------------------------------------------------
tbl_ei_harvests <- bind_rows(
  tbl_2023_ei_harvests,
  tbl_2022_ei_harvests,
  tbl_2021_ei_harvests,
  tbl_2020_ei_harvests,
  tbl_2019_ei_harvests,
)

supp_ei_harvests <- bind_rows(
  supp_2023_ei_harvests,
  supp_2022_ei_harvests,
  supp_2021_ei_harvests,
  supp_2020_ei_harvests,
  supp_2019_ei_harvests,
)

## harvest losses ---------------------------------------------------------
tbl_ei_loss <- bind_rows(
  # tbl_2023_ei_loss # none
  # tbl_2022_ei_loss # none
  # tbl_2021_ei_loss # empty
  # tbl_2020_ei_loss # empty
  # tbl_2019_ei_loss # empty
)

tbl_ei_sysloss <- bind_rows(
  # tbl_2023_ei_sysloss # none
  tbl_2022_ei_sysloss, 
  tbl_2021_ei_sysloss,
  tbl_2020_ei_sysloss, # empty
  tbl_2019_ei_sysloss, # empty
)

supp_ei_loss <- bind_rows(
  # supp_2023_ei_loss, # none
  # supp_2022_ei_loss, # none
  # supp_2021_ei_loss, # none
  # supp_2020_ei_loss, # none
  # supp_2019_ei_loss, # none
)
supp_ei_sysloss <- bind_rows(
  # supp_2023_ei_sysloss # none
  supp_2022_ei_sysloss,
  supp_2021_ei_sysloss,
  supp_2020_ei_sysloss, # empty
  supp_2019_ei_sysloss, # empty
)


## biomassings -------------------------------------------------------------
tbl_ei_bio <- bind_rows(
  tbl_2023_ei_bio,
  tbl_2022_ei_bio,
  tbl_2021_ei_bio,
  tbl_2020_ei_bio,
  tbl_2019_ei_bio
)

# tbl_ei_bio |> count(year(biomass_date), subplot) |> 
#   separate_wider_regex(subplot, patterns = c(plot = "[0-9]{3}", section = "\\D+?")) |>
#   count(`year(biomass_date)`, plot) |> 
#   pivot_wider(names_from = "plot", values_from = "n")

supp_ei_bio <- bind_rows(
  supp_2023_ei_bio,
  supp_2022_ei_bio,
  supp_2021_ei_bio,
  supp_2020_ei_bio,
  supp_2019_ei_bio
)

## canopeo ----------------------------------------------------------------
tbl_ei_can <- bind_rows(
  tbl_2023_ei_can,
  tbl_2022_ei_can,
  tbl_2021_ei_can,
  tbl_2020_ei_can,
  tbl_2019_ei_can, # empty
)

supp_ei_can <- bind_rows(
  # supp_2023_ei_can, # none
  supp_2022_ei_can,
  supp_2021_ei_can, # empty
  supp_2020_ei_can, # empty
  supp_2019_ei_can, # empty
)


db_ei_can <- tbl_ei_can |> 
  mutate(
    coordinate = case_match(coordinate,
                            c("North", "N", "north")~"North",
                            c("Center", "Central", "C", "center")~"Center",
                            c("South", "S", "south")~"South"))
db_ei_candetails <- supp_ei_can

## yieldings ---------------------------------------------------------------
ei_product_codes <- list(
  c("barley straw")~"barley straw",
  c("Barley", "barley")~"barley",
  c("Continuous Corn", "Organic Corn", "Strip Till Corn", "Strip-till Corn", "Strip-till corn")~"corn",
  c("oat grain")~"oat grain",
  c("Oat Straw")~"oat straw",
  c("NT soy", "Organic Soybeans", "No-till Soybean", "No-till Soybeans", "Org. soy", "Organic Soybean")~"soybean",
  c("Wheat", "wheat")~"wheat grain",
  c("wheat straw")~"wheat straw"
)

ei_biomass_codes <- list(
  c("berseem clover", "berseem_clover", "bc")~"berseem clover",
  c("oat", "oats")~"oat",
  c("red clover", "red_clover")~"red clover",
  c("residue") ~ "residue",
  c("rye")~"rye",
  c("weed", "weeds")~"weed",
  c("wheat straw")~"wheat straw"
)


pre_ei_yield <- bind_rows(tbl_ei_harvests |> add_column(yielding_type = "harvesting"),
                          tbl_ei_bio |> add_column(yielding_type = "biomassing")) |> 
  mutate(
    yielding_id = coalesce(harvesting_id, biomassing_id),
    product_description = crop,
    biomass = case_match(biomass, !!!ei_biomass_codes),
    product = case_match(crop, !!!ei_product_codes),
    coordinate = case_match(coordinate,
                            "Central"~"Center",
                            .default = coordinate))


## db ----------------------------------------------------------------------


db_ei_yieldings <- pre_ei_yield |> select(yielding_id, yielding_type)
db_ei_harvestings <- pre_ei_yield |>
  filter(yielding_type == "harvesting") |> 
  select(yielding_type, any_of(harvesting_cols), -crop, product, product_description)
db_ei_harvestingdetails <- supp_ei_harvests
db_ei_biomassings <- pre_ei_yield |> 
  filter(yielding_type == "biomassing") |> 
  select(yielding_type, any_of(biomassing_cols))
db_ei_biomassingdetails <- supp_ei_bio

# loss
pre_ei_genlosses <- bind_rows(tbl_ei_loss |> add_column(directloss_id = NA, loss_type = "direct"),  # empty
                              tbl_ei_sysloss |> 
                                add_column(loss_type = "systematic") |> 
                                rename(systematicloss_id = systematicharvestingloss_id)) |> 
  mutate(loss_id = coalesce(systematicloss_id, NA))

db_ei_genlosses <- pre_ei_genlosses |> select(loss_id, loss_type, harvesting_id)

# hackish, since empty just filter out all the tables for 0
db_ei_loss <- tbl_loss |> filter(harvesting_id == "sdlflsdkjfdsf") |> rename(directloss_id = harvestingloss_id)
db_ei_lossdetails <- supp_loss_cols |> enframe(name = "value", value = "name") |> pivot_wider() |> slice(-1) # empty

db_ei_sysloss <- pre_ei_genlosses |> filter(loss_type == "systematic") |> select(loss_type, systematicloss_id,  any_of(sysloss_cols))
db_ei_syslossdetails <- supp_ei_sysloss |> rename(systematicloss_id = systematicharvestingloss_id)


# silage ------------------------------------------------------------------

tbl_silage <- bind_rows(
  tbl_2023_silage,
  tbl_2022_silage,
  tbl_2021_silage,
  tbl_2020_silage,
  tbl_2019_silage,
  tbl_2018_silage,
  tbl_2017_silage
)

supp_silage <- bind_rows(
  supp_2023_silage,
  supp_2022_silage,
  supp_2021_silage,
  supp_2020_silage,
  supp_2019_silage,
  supp_2018_silage,
  supp_2017_silage
)

#direct losses
tbl_silage_loss <- bind_rows(
  tbl_2019_silage_loss,
  tbl_2018_silage_loss # empty
)

supp_silage_loss <- bind_rows(
  supp_2019_silage_loss,
  supp_2018_silage_loss, # empty
)

tbl_silage_sysloss <- bind_rows(
  # 2019-2023 empty, 2017 empty too
  tbl_2018_silage_sysloss
)

supp_silage_sysloss <- bind_rows(
  # 2019-2023 empty, 2017 empty too
  supp_2018_silage_sysloss
)

db_silage_harvestings <- tbl_silage |> 
  rename(product = crop) |> add_column(product_description = NA)
db_silage_harvestingdetails <- supp_silage

db_silage_directlosses <- tbl_silage_loss |> 
  rename(directloss_id = harvestingloss_id) |> 
  mutate(loss_type = "direct")
db_silage_directlossdetails <- supp_silage_loss |> 
  rename(directloss_id = harvestingloss_id)
db_silage_sysloss <- tbl_silage_sysloss |>
  rename(systematicloss_id = systematicharvestingloss_id) |> 
  mutate(loss_type = "systematic")
db_silage_syslossdetails <- supp_silage_sysloss |> 
  rename(systematicloss_id = systematicharvestingloss_id)

db_silage_genlosses <- bind_rows(
  db_silage_directlosses,
  db_silage_sysloss
) |> 
  mutate(loss_id = coalesce(directloss_id, systematicloss_id)) |> 
  select(loss_id, loss_type, harvesting_id)




# prairie -----------------------------------------------------------------

tbl_prairie <- bind_rows(
  tbl_2023_prairie,
  tbl_2022_prairie,
  tbl_2021_prairie,
  tbl_2020_prairie,
  tbl_2019_prairie,
  tbl_2018_prairie,
  tbl_2017_prairie,
  tbl_2016_prairie,
  # 2015 empty
)

supp_prairie <- bind_rows(
  supp_2023_prairie,
  supp_2022_prairie,
  supp_2021_prairie,
  supp_2020_prairie,
  supp_2019_prairie,
  supp_2018_prairie,
  supp_2017_prairie,
  supp_2016_prairie,
  # 2015
)

db_prairie_harvestings <- tbl_prairie |> mutate(
  product = case_match(crop,
                       c("HDP", "high diversity prairie")~"high diversity prairie",
                       c("LDP", "low diversity prairie")~"low diversity prairie",
                       c("switch", "switchgrass", "Switchgrass")~"switchgrass")) |> 
  select(any_of(fuel_harvesting_cols), -crop, product)
db_prairie_harvestingdetails <- supp_prairie

# 115 ---------------------------------------------------------------------

tbl_115 <- bind_rows(
  tbl_2023_115_prairie,
  tbl_2022_115_prairie,
  # tbl_2021_115_prairie, none
  # tbl_2020_115_prairie, none
  # tbl_2019_115_prairie, none
  tbl_2018_115 # sb data, not prairie
)

supp_155 <- bind_rows(
  supp_2023_115_prairie,
  supp_2022_115_prairie,
  supp_2018_115 # sb data, not prairie
)


# Micro -------------------------------------------------------------------
tbl_micro <- bind_rows(
  tbl_2015_micro
)

tbl_micro <- bind_rows(
  supp_2015_micro
)
