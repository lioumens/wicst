# agcal

# tables

library(readxl)
library(purrr)
library(DBI)
library(glue)
library(dplyr)
library(janitor)
library(lubridate)

load("data/manure_master_20250516.Rdata")
load("data/translate_20250517.Rdata")

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
  rowwise() |> 
  mutate(nolan_comment = if_else(!is.na(notes),
                                 glue("Nolan Majerowski: \"{note}\"",
                                      note = notes),
                                 NA),
         agcal_comment = if_else(!is.na(agcal_notes),
                                 
                                 glue("Agcal: \"{note}\"",
                                      note = agcal_notes),
                                 NA),
         comments = case_when(is.na(nolan_comment) & is.na(agcal_comment)~NA,
                              .default = str_flatten(c(nolan_comment, agcal_comment),
                                                     collapse = " | ",
                                                     na.rm = T)),
         fertilizer = case_match(fertilizer_type,
                                 !!!xl_translate$flist_fertilizings,
                                 .default = "unknown translation")
         ) |>
  select(-notes, -agcal_notes, -nolan_comment, -agcal_comment, -fertilizer_type,
         -keep_or_remove) |>
 mutate(
  fy_n    = case_when(fertilizer == "chicken pellet manure"~n, 
                      .default = NA),
  fy_p2o5 = case_when(fertilizer == "chicken pellet manure"~p2o5,
                      .default = NA),
  fy_k2o  = case_when(fertilizer == "chicken pellet manure"~k2o,
                      .default = NA),
  fy_s    = case_when(fertilizer == "chicken pellet manure"~s, 
                      .default = NA),
  fy_ca   = case_when(fertilizer == "chicken pellet manure"~ca,
                      .default = NA),
  fy_mg   = case_when(fertilizer == "chicken pellet manure"~mg,
                      .default = NA),
  year = year(date),
  .after = mg
) |> 
  left_join(xl_manure$cpm |> 
              mutate(m_type = case_match(m_type, "cpm"~"chicken pellet manure"),
                     cpm_s = 0,
                     cpm_ca = 0,
                     cpm_mg = 0),
            by = join_by(fertilizer == m_type, year == year),
            relationship = "many-to-one") |> 
  mutate(n = coalesce(tn, n),
         p2o5 = coalesce(tp2, p2o5),
         k2o = coalesce(tk2, k2o),
         s = coalesce(cpm_s, s),
         ca = coalesce(cpm_ca, ca),
         mg = coalesce(cpm_mg, mg)) |> 
  rename(plot_id = plot,
         fertilizing_date = date,
         timing = timing_notes) |>
  relocate(fertilizer, .after = plot_id)

# pre_fertilizings |> filter(fertilizer == "potassium sulfate", s == 0)
# pre_fertilizings |> count(fertilizer, n, p2o5, k2o, s, ca) |> View()


# pre_fertilizings$comments
# pre_fertilizings$comments[!is.na(pre_fertilizings$comments)]


## manurings ---------------------------------------------------------------
raw_manurings <- read_xlsx(manurings_fp, sheet = "Sheet1", na = "-")

pre_manurings <- raw_manurings |> clean_names() |> 
  filter(keep_or_remove != "remove" | is.na(keep_or_remove)) |>
  mutate(comments = if_else(!is.na(notes),
                            glue("General: {notes}", notes = notes),
                            NA)) |> 
  select(-notes, -keep_or_remove) |> 
  rename(plot_id = plot,
         manure_date = date)

# tillings ----------------------------------------------------------------
raw_tillings <- read_xlsx(tillings_fp, sheet = "Sheet1", na = "-")
pre_tillings <- raw_tillings |> clean_names() |> 
  mutate(comments = if_else(!is.na(notes),
                            glue("General: \"{notes}\"", notes = notes),
                                 NA)) |>
  # translate implement to gregg implements and modifier
  mutate(new_implement =
           case_match(implement,
                      !!!xl_translate$flist_tillings_implement),
         modifier = case_match(implement,
                                   !!!xl_translate$flist_tillings_modifier)) |> 
  # implement types
  left_join(dd_tillings_type |> 
              filter(gregg_implement != "disk"), # disk can be primary or secondary
            by = join_by(new_implement == gregg_implement)) |> 
  # rename
  mutate(implement = coalesce(new_implement, implement),
         type = coalesce(type, gregg_type)) |>
  select(-new_implement, -gregg_type, -notes) |>
  rename(plot_id = plot,
         tilling_date = date)
# 
# pre_tillings |> 
#   mutate(plot_id = factor(plot_id, levels = plots_by_treatment)) |>
#   arrange(plot_id, tilling_date) |>
#   select(-planting, -passes, -comments) |>
#   # summarize(n = n(), .by = c(tilling_date, plot_id)) |> 
#   # filter(n > 1L)
#   pivot_wider(names_from = plot_id, values_from = c(type, implement), names_vary = "slowest",
#               values_fn = ~str_flatten(.x, collapse = ", ")) |> 
#   clipr::write_clip()

# pre_tillings |>
#   left_join(xl_core$plots |> select(plot_id, treatment_id), by = "plot_id") |>
#   mutate(plot_id = factor(plot_id, levels = plots_by_treatment),
#          year = year(tilling_date)) |>
#   arrange(year, plot_id, type, tilling_date) |> 
#   relocate(year, .before = 1) |> 
#   relocate(treatment_id, .after = plot_id) |> 
#   filter(is.na(plot_id))

# pre_tillings |> filter(!(plot_id %in% plots_by_treatment)) |> View()


pre_tillings |> filter(!is.na(modifier) & year(tilling_date) < 1993)


# QA get rid of duplicates, larger table first
pre_tillings |> count(year(tilling_date), plot_id, type, implement) |> 
  arrange(desc(n)) |> 
  filter(type == "primary", n > 1) |> View()

pre_tillings |> filter(implement == "chisel", year(tilling_date) == 1997, plot_id == "A104")


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
                                            "guess", "text", "text", "text", "text"))
pre_pesticidings <- raw_pesticidings |> clean_names() |> 
  mutate(comments = if_else(!is.na(notes), 
                            glue("General: {notes}", notes = notes),
                                 NA)) |> 
  select(-notes) |> 
  rename(plot_id = plot,
         pesticiding_date = date)

pre_pesticidings


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
