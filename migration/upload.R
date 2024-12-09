# options

oo <- list(
  overwrite = TRUE
)



# libraries and setup -----------------------------------------------------

library(dbplyr)
library(RPostgres)
library(DBI)
library(here)

# data --------------------------------------------------------------------

load("data/core_20241108.Rdata")
load("data/agcal_20241119.Rdata")

# connect -----------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(),
                      driver = "ODBC Driver 18 for SQL Server", 
                      server = "carrington.doit.wisc.edu",
                      uid = "WEIR_dbo",
                      database = "WEIR",
                      pwd = rstudioapi::askForPassword("Database Password"),
                      TrustServerCertificate = "yes", 
                      port = 1433)
# con <- DBI::dbConnect(RPostgres::Postgres(),
#                       host = "127.0.0.1",
#                       dbname = "wicst",
#                       user = "postgres",
#                       password = rstudioapi::askForPassword("Database Password"))

# DBI::dbListTables(con)
# dplyr::tbl(con, "treatment_grid")


# Source Files --------------------------------------------------

if (!exists("tbl_harvests")) source(here("migration/collect.R"))

# wicst ------------------------------------------------------------------
## core --------------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "plots"), xl_core$plots, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "sites"), xl_core$sites, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "treatments"), xl_core$treatments, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "rotations"), xl_core$rotations, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "systems"), xl_core$systems, overwrite = oo$overwrite)

# dbWriteTable(con, Id(schema = "wicst", table = "Plots"), xl_core$plots, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "Sites"), xl_core$sites, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "Treatments"), xl_core$treatments, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "Rotations"), xl_core$rotations, overwrite = oo$overwrite)

## Agcal ------------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "plantings"), db_plantings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "fertilizings"), db_fertilizings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "limings"), db_limings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "manurings"), tbl_manurings)


## harvestings -------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "harvestings"), db_harvestings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "harvestingdetails"), db_harvestingdetails, overwrite = oo$overwrite)

# dbWriteTable(con, Id(schema = "wicst", table = "Harvestings"), db_harvestings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "HarvestingDetails"), db_harvestingdetails, overwrite = oo$overwrite)


## biomassings -------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "biomassings"), db_biomassings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "biomassingdetails"), db_biomassingdetails, overwrite = oo$overwrite)

# dbWriteTable(con, Id(schema = "wicst", table = "Biomassings"), db_biomassings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "BiomassingDetails"), db_biomassingdetails, overwrite = oo$overwrite)


## yieldings ---------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "yieldings"), db_yieldings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "Yieldings"), db_yieldings, overwrite = oo$overwrite)

## Losses ----------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "losses"), db_genlosses, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "directlosses"), db_directlosses, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "directlossdetails"), db_directlossdetails, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "systematiclosses"), db_systematiclosses, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "systematiclossdetails"), db_systematiclossdetails, overwrite = oo$overwrite)

## canopeo -----------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "canopeo"), db_can, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "canopeodetails"), db_candetails, overwrite = oo$overwrite)


# ei ----------------------------------------------------------------------
## core --------------------------------------------------------------------

dbWriteTable(con, Id(schema = "ei", table = "subplots"), xl_core$subplots, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "rotations"), xl_core$eirotations, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "treatments"), xl_core$eitreatments, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "assignments"), xl_core$eiassignments, overwrite = oo$overwrite)

## yieldings ---------------------------------------------------------------

dbWriteTable(con, Id(schema = "ei", table = "yieldings"), db_ei_yieldings, overwrite = oo$overwrite)

dbWriteTable(con, Id(schema = "ei", table = "harvestings"), db_ei_harvestings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "harvestingdetails"), db_ei_harvestingdetails, overwrite = oo$overwrite)

dbWriteTable(con, Id(schema = "ei", table = "biomassings"), db_ei_biomassings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "biomassingdetails"), db_ei_biomassingdetails, overwrite = oo$overwrite)

# dbWriteTable(con, Id(schema = "ei", table = "Harvestings"), db_ei_harvestings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "ei", table = "HarvestingDetails"), db_ei_harvestingdetails, overwrite = oo$overwrite)

## losses ------------------------------------------------------------------

dbWriteTable(con, Id(schema = "ei", table = "losses"), db_ei_genlosses, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "directlosses"), db_ei_loss, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "directlossdetails"), db_ei_lossdetails, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "systematiclosses"), db_ei_sysloss, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "systematiclossdetails"), db_ei_syslossdetails, overwrite = oo$overwrite)

## canopeo -----------------------------------------------------------------

dbWriteTable(con, Id(schema = "ei", table = "canopeo"), db_ei_can, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "canopeodetails"), db_ei_candetails, overwrite = oo$overwrite)


# silage ------------------------------------------------------------------

dbWriteTable(con, Id(schema = "silage", table = "sideplots"), xl_core$sideplots, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "silage", table = "harvestings"), db_silage_harvestings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "silage", table = "harvestingdetails"), db_silage_harvestingdetails, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "silage", table = "systematiclosses"), db_silage_sysloss, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "silage", table = "systematiclossdetails"), db_silage_syslossdetails, overwrite = oo$overwrite)


# biofuels ----------------------------------------------------------------

dbWriteTable(con, Id(schema = "prairie", table = "fuelplots"), xl_core$fuelplots, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "prairie", table = "harvestings"), db_prairie_harvestings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "prairie", table = "harvestingdetails"), db_prairie_harvestingdetails, overwrite = oo$overwrite)




