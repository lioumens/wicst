
# source files
source("migration/2023_yield.R")

# wicst ------------------------------------------------------------------

## harvestings -------------------------------------------------------------
tbl_harvests <- bind_rows(
  tbl_2023_harvests |> mutate(plot = glue("A{plot}")),
  tbl_2022_harvests |> mutate(plot = glue("A{plot}")),
  tbl_2021_harvests |> mutate(plot = glue("A{plot}")),
  tbl_2020_harvests |> mutate(plot = glue("A{plot}"))
)

supp_harvests <- bind_rows(
  supp_2023_harvests,
  supp_2022_harvests
)


## harvest losses ----------------------------------------------------------
tbl_loss <- bind_rows(
  # tbl_2023_loss # no 2023 losses
  # tbl_2022_loss # also empty
)

tbl_sysloss <- bind_rows(
  # tbl_2023_sysloss # none, no def
  tbl_2022_sysloss
)


## canopeo ----------------------------------------------------------------
tbl_can <- bind_rows(
  tbl_2023_can
)

# ei ----------------------------------------------------------------------

## biomassings -------------------------------------------------------------
tbl_bio <- bind_rows(
  tbl_2023_biomassing,
  tbl_2022_bio
) |> mutate(plot = glue("A{plot}"))

supp_bio <- bind_rows(
  supp_2023_biomassing,
  supp_2022_bio
)

## harvestings -------------------------------------------------------------
tbl_ei_harvests <- bind_rows(
  tbl_2023_ei_harvests,
  tbl_2022_ei_harvests
)

supp_ei_harvests <- bind_rows(
  supp_2023_ei_harvests,
  supp_2022_ei_harvests
)

## harvest losses ---------------------------------------------------------
tbl_ei_loss <- bind_rows(
  # tbl_2023_ei_loss # none
  # tbl_2022_ei_loss # none
)

tbl_ei_sysloss <- bind_rows(
  # tbl_2023_ei_sysloss # none
  tbl_2022_ei_sysloss
)
supp_ei_loss <- bind_rows(
  # supp_2023_ei_loss, # none
  # supp_2022_ei_loss, # none
)
supp_ei_sysloss <- bind_rows(
  # supp_2023_ei_sysloss # none
  supp_2022_ei_sysloss
)


## canopeo ----------------------------------------------------------------
tbl_ei_can <- bind_rows(
  tbl_2023_ei_can,
  tbl_2022_can_e
)

