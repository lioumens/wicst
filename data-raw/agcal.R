# agcal

# tables

library(readxl)
library(purrr)
library(DBI)
library(glue)
library(dplyr)
library(janitor)
library(lubridate)

agcal <- "~/OneDrive - UW-Madison/Database development/agcal_db.xlsx"

plantings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/2 - Input Sheet - Plantings.xlsx"
limings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/3 - Input Sheet - Limings.xlsx"
fertilizers_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/4 - Input Sheet - Fertilizings.xlsx"
manurings_fp <- "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/5 - Input Sheet - Manurings.xlsx"
tillings_fp <-  "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/6 - Input Sheet - Tillings.xlsx"
pesticidings_fp <-  "~/OneDrive - UW-Madison/Database development/WICST agcal data entry/7 - Input Sheet - Pesticidings.xlsx"

# plantings
raw_plantings <- read_xlsx(plantings_fp, 
                           sheet = "Sheet2", # updated to sheet2 after qa
                           col_types = c("guess", "guess", "guess", "guess",
                                         "guess", "guess", "guess", "guess",
                                         "guess", "guess", "guess", "text"))

pre_plantings <- raw_plantings |> 
  mutate(planting_date = ymd(planting_date, tz = "UTC")) # date format

# limings
raw_limings <- read_xlsx(limings_fp, sheet = "Sheet1", na = "-")
pre_limings <- raw_limings |> rename(plot_id = plot)


## fertilizings ------------------------------------------------------------

raw_fertilizings <- read_xlsx(fertilizers_fp, sheet = "Sheet1", na = "-")
pre_fertilizings <- raw_fertilizings |> clean_names() |> 
  filter(keep_or_remove != "remove" | is.na(keep_or_remove)) |> 
  mutate(comments = if_else(!is.na(notes),
                                        glue("Nolan Majerowski: \"{note}\"",
                                             note = notes),
                                        NA)) |>
  select(-notes) |>
  rename(plot_id = plot,
         fertilizing_date = date)


## manurings ---------------------------------------------------------------
raw_manurings <- read_xlsx(manurings_fp, sheet = "Sheet1", na = "-")
pre_manurings <- raw_manurings |> clean_names() |> 
  filter(keep_or_remove != "remove" | is.na(keep_or_remove)) |>
  mutate(comments = if_else(!is.na(notes),
                            glue("General: {notes}", notes = notes),
                            NA)) |> 
  select(-notes) |> 
  rename(plot_id = plot,
         manuring_date = date)

# tillings ----------------------------------------------------------------
raw_tillings <- read_xlsx(tillings_fp, sheet = "Sheet1", na = "-")
pre_tillings <- raw_tillings |> clean_names() |> 
  mutate(comments = if_else(!is.na(notes),
                            glue("General: {notes}", notes = notes),
                                 NA)) |> 
  select(-notes) |> 
  rename(plot_id = plot,
         tilling_date = date)

# tillings_translate <- read_xlsx("~/Downloads/tillings_translate.xlsx",
#                                 sheet = "Sheet1")
# 
# raw_tillings |> left_join(tillings_translate |>
#                             distinct(type, implement, pick(starts_with("gregg"))),
#                           by = c("type", "implement")) |>
#   mutate(type = gregg_type,
#          implement = gregg_implement) |> 
#   mutate(modifier = gregg_modifier, .after = "implement") |>
#   select(-starts_with("gregg")) |> 
#   clipr::write_clip()

# pesticidings ----------------------------------------------------------------
raw_pesticidings <- read_xlsx(pesticidings_fp, sheet = "Sheet1", na = "-",
                              col_types = c("guess", "guess", "guess", "guess",
                                            "guess", "guess", "text"))
pre_pesticidings <- raw_pesticidings |> clean_names() |> 
  mutate(comments = if_else(!is.na(notes),
                            glue("General: {notes}", notes = notes),
                                 NA)) |> 
  select(-notes) |> 
  rename(plot_id = plot,
         pesticiding_date = date)


xl_agcal <- list(
  limings = pre_limings,
  fertilizings = pre_fertilizings,
  plantings = pre_plantings,
  manurings = pre_manurings,
  tillings = pre_tillings,
  pesticidings = pre_pesticidings
)

xl_agcal_raw <- list(
  plantings = raw_plantings,
  limings = raw_limings,
  fertilizings = raw_fertilizings, 
  manurings = raw_manurings,
  tillings = raw_tillings,
  pesticidings = raw_pesticidings
)

date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/agcal_", date_string, ".Rdata")

save(xl_agcal, xl_agcal_raw, file = filepath_date)

# work section ------------------------------------------------------------
# 
# no longer relevant, just to update excel sheets
# load("data/agcal_20241210.Rdata")
# load("data/qa_20241210.Rdata")
# 
# library(tidyverse)
# xl_agcal$plantings |> count(type)
# xl_agcal$plantings |> count(method)
# 
# xl_agcal$plantings
# translate_names <- xl_qa$`michael tmp` |> rowwise() |> 
#   mutate(form = list(as.formula(glue("\"{`new name`}\"~\"{`old name`}\"", .literal = TRUE))))
# translate_names$form
# 
# translate_names <- xl_agcal$plantings |>
#   filter(!(xl_agcal$plantings$variety %in% xl_qa$`variety counts`$variety)) |>
#   pull(variety) |> 
#   unique() |> 
#   cbind(xl_qa$`michael tmp`$oldold[-c(32, 33)]) |> 
#   as.data.frame() |> 
#   set_names(c("old", "oldold")) |> 
#   pull(oldold, name = old)
#   
# fixed_names <- xl_agcal$plantings |> 
#   mutate(
#     fixed_name = case_when(variety %in% names(translate_names) ~ translate_names[variety],
#                            .default = variety)) |>
#   pull(fixed_name)
# 
# # should be empty
# xl_agcal$plantings |> filter(!(fixed_names %in% xl_qa$`variety counts`$variety)) # all accounted for in QA book
# 
# xl_qa$`variety counts` |> mutate(fixed_crop = str_to_lower(crop)) |> 
#   count(fixed_crop, variety, gregg_crop, gregg_source, gregg_variety, gregg_attribute_listed) |> 
#   filter(n > 1)
# 
# distinct_lookup <- xl_qa$`variety counts` |> mutate(crop = str_to_lower(crop)) |> 
#   distinct(crop, variety, gregg_crop, gregg_source, gregg_variety, gregg_attribute_listed)
# 
# final_planting <- xl_agcal$plantings |> mutate(
#     variety = case_when(variety %in% names(translate_names) ~ translate_names[variety],
#                            .default = variety)) |> 
#   rename("old_variety" = "variety") |> 
#   mutate(crop = str_to_lower(case_when(crop == "berseeem clover"~"berseem clover",
#                                        crop == "alfalfa" & old_variety == "Pioneer 54Q32" ~ "direct seeded alfalfa",
#                                        old_variety == "Freedom red clover"~"Clover",
#                                        .default = crop)),
#          old_variety = str_replace_all(old_variety, '""', '"')) |> 
#   left_join(distinct_lookup, by = join_by("crop" == "crop", "old_variety" == "variety")) |> 
#   select(planting_date, plot_id, gregg_crop, gregg_source, gregg_variety, gregg_attribute_listed,
#          old_variety, rate, rate_unit, type, method, comments) %>%
#   set_names(., ~str_remove(string = ., pattern = "^gregg_")) |> 
#   mutate(
#     # crop = case_match(crop,
#     #                        "oats"~"oat",
#     #                        "field peas"~"field pea",
#     #                        "soybeans"~"soybean",
#     #                        .default = crop),
#          planting_date = ymd(planting_date))
# final_planting |> clipr::write_clip()

# final_planting <- xl_agcal$plantings |> 
#   rename("old_variety" = "variety") |> 
#   left_join(xl_qa$`variety counts`, by = join_by("crop" == "crop", "old_variety" == "variety")) |> 
#   select(planting_date, plot_id, gregg_crop, gregg_source, gregg_variety, gregg_attribute_listed,
#          old_variety, rate, rate_unit, type, method, comments) %>%
#   set_names(., ~str_remove(string = ., pattern = "^gregg_")) |> 
#   mutate(crop = case_match(crop,
#                            "oats"~"oat",
#                            "field peas"~"field pea",
#                            "soybeans"~"soybean",
#                            .default = crop),
#          planting_date = ymd(planting_date))
