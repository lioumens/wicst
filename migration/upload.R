# libraries and setup -----------------------------------------------------

library(dbplyr)
library(RPostgres)
library(DBI)

# data --------------------------------------------------------------------

load("data/core_20241005.Rdata")
load("data/agcal_20241005.Rdata")

# connect -----------------------------------------------------------------

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host = "127.0.0.1",
                      dbname = "wicst",
                      user = "postgres",
                      password = rstudioapi::askForPassword("Database password"))

DBI::dbListTables(con)
dplyr::tbl(con, "treatment_grid")

# Upload db ---------------------------------------------------------------

# DBI::dbListTables(con)
# DBI::dbListFields(con, "harvests")
# dbRemoveTable(con, "Harvests")

# dbCreateTable(con, "Harvests", tbl_harvests)
# dbRemoveTable(con, "cities")
dbRemoveTable(con, "harvestings")


# core tables
dbWriteTable(con, "plots", xl_core$plots, overwrite = TRUE)
dbWriteTable(con, "sites", xl_core$sites, overwrite = TRUE)
dbWriteTable(con, "treatments", xl_core$treatments, overwrite = TRUE)
dbWriteTable(con, "rotations", xl_core$rotations, overwrite = TRUE)


# agricultural calendars
dbWriteTable(con, "plantings", xl_agcal$`2023_plantings`)

tbl_fertilizings <- bind_rows(xl_agcal$`2023_fertilizings`,xl_agcal$`2022_fertilizings`)
dbWriteTable(con, "fertilizings", tbl_fertilizings)

tbl_manurings <- bind_rows(xl_agcal$`2022_manurings`, xl_agcal$`2021_manurings`)
dbWriteTable(con, "manurings", tbl_manurings)
dbWriteTable(con, "harvestings", tbl_harvests)








