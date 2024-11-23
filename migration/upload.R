# options

oo <- list(
  overwrite = FALSE
)



# libraries and setup -----------------------------------------------------

library(dbplyr)
library(RPostgres)
library(DBI)
library(here)

# data --------------------------------------------------------------------

load("data/core_20241104.Rdata")
load("data/agcal_20241108.Rdata")

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
dplyr::tbl(con, "treatment_grid")


# Source Files --------------------------------------------------

if (!exists("tbl_harvests")) source(here("migration/collect.R"))

# wicst ------------------------------------------------------------------
## core --------------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "plots"), xl_core$plots, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "sites"), xl_core$sites, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "treatments"), xl_core$treatments, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "rotations"), xl_core$rotations, overwrite = oo$overwrite)

## Agcal ------------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "plantings"), tbl_plantings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "fertilizings"), tbl_fertilizings, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "limings"), tbl_limings, overwrite = oo$overwrite)
# dbWriteTable(con, Id(schema = "wicst", table = "manurings"), tbl_manurings)


## harvestings -------------------------------------------------------------
dbWriteTable(con, Id(schema = "wicst", table = "harvestings"), tbl_harvests, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "wicst", table = "harvestingdetails"), supp_harvests, overwrite = oo$overwrite)

dbWriteTable(con, Id(schema = "ei", table = "harvestings"), tbl_ei_harvests, overwrite = oo$overwrite)
dbWriteTable(con, Id(schema = "ei", table = "harvestingdetails"), supp_ei_harvests, overwrite = oo$overwrite)


## Direct Harvest Losses ----------------------------------------------------------

## Systematic Harvest Losses ---------------------------------------------------

## canopeo -----------------------------------------------------------------

dbWriteTable(con, Id(schema = "wicst", table = "canopeo"), tbl_can, overwrite = oo$overwrite)



# ei ----------------------------------------------------------------------
## core --------------------------------------------------------------------

dbWriteTable(con, Id(schema = "ei", table = "subplots"), xl_core$subplots, overwrite = oo$overwrite)

## canopeo -----------------------------------------------------------------
dbWriteTable(con, Id(schema = "ei", table = "canopeo"), tbl_ei_can, overwrite = oo$overwrite)

# biofuels ----------------------------------------------------------------



