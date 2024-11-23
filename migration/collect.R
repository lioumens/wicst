
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

# wicst ------------------------------------------------------------------

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
)

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

tbl_silage_sysloss <- bind_rows(
  # 2019-2023 empty, 2017 empty too
  tbl_2018_silage_sysloss
)

supp_silage_sysloss <- bind_rows(
  # 2019-2023 empty, 2017 empty too
  supp_2018_silage_sysloss
)
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
