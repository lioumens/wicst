# QUACK

source("migration/yield_prep.R")
source("migration/collect.R")
library(lubridate)

# helper tables -----------------------------------------------------------
plots_by_treatment <- xl_core$plots |> arrange(treatment_id,plot_id) |> 
  pull(plot_id)

# year_plot_crop
# xl_core$plots$plot_id
system_maxphase <- xl_core$rotations |> summarize(max_phase = max(phase),
                                                  .by = system_id)

crop_lookup <- expand_grid(year = 1989:2024, plot = xl_core$plots$plot_id) |> 
  left_join(xl_core$plots, by = join_by("plot" == "plot_id")) |> 
  left_join(xl_core$treatments, by = "treatment_id") |>
  left_join(system_maxphase, by = "system_id") |> 
  mutate(since_start = year - (start_year - 1),
         current_phase = since_start %% max_phase + 1) |> 
  left_join(xl_core$rotations, join_by("current_phase" == "phase", "system_id" == "system_id")) |> 
  mutate(actual_crop = if_else(since_start < 1, "fc", crop)) |> 
  select(year, plot, actual_crop)


# qas ---------------------------------------------------------------------


xl_agcal$plantings |> count(year(planting_date), plot) |> 
  mutate(plot = factor(plot, levels = c(plots_by_treatment, "A501", "A504", "A508"))) |> 
  arrange(plot) |> 
  pivot_wider(names_from = plot, values_from = n) |>
  arrange(`year(planting_date)`) |> 
  clipr::write_clip()

xl_agcal$plantings |> group_by(year(planting_date), plot) |> 
  summarize(plants = paste(variety, collapse = "; "), .groups = "drop") |>
  mutate(plot = factor(plot, levels = c(plots_by_treatment, "A501", "A504", "A508"))) |> 
  arrange(plot) |> 
  pivot_wider(names_from = plot, values_from = plants) |>
  arrange(`year(planting_date)`) |> 
  clipr::write_clip()

# with rotation info

xl_agcal$plantings 
xl_agcal$plantings |> count(year(planting_date), plot) |>
  mutate(plot = factor(plot, levels = c(plots_by_treatment, "A501", "A504", "A508"))) |> 
  
  arrange(plot) |> rename(year = 1) |> complete(year, plot) |> 
  filter(!str_starts(plot, "L")) |> 
  left_join(crop_lookup, by = c("year", "plot")) |> 
  mutate(value = glue("{actual_crop} {n}")) |> 
  pivot_wider(names_from = plot, id_cols = year) |> 
  arrange(year) |> 
  clipr::write_clip()

# variety names
xl_agcal$plantings |> count(crop, variety) |> arrange(crop, variety) |> clipr::write_clip()

xl_agcal$plantings |> count(rate, rate_unit) |> arrange(rate_unit) |> clipr::write_clip()

# months planted
xl_agcal$plantings |> count(month(planting_date))

xl_agcal$fertilizings |> count(fertilizer_type, N, P2O5, K2O, S, Ca, Mg) |> clipr::write_clip()


# Limings ----------------------------------------------------------------

xl_agcal$limings |> count(year(date), grading, rate) |> clipr::write_clip()
xl_agcal$limings |> count(grading, rate, rate_unit) |> clipr::write_clip()


# xtabs
yield_counts <- tbl_harvests |> count(plot, year(harvest_date)) |> rename(year = 2) |>
  complete(plot, year) |> 
  left_join(crop_lookup, by = c("plot", "year")) |> 
  mutate(
    plot = factor(plot, levels = plots_by_treatment),
    cell = glue("{actual_crop} ({n})"), .keep = "all") 

yield_counts |> pivot_wider(names_from = year, values_from = cell, id_cols = plot) |>
  arrange(plot) |> 
  left_join(xl_core$plot |> select(plot_id, treatment_id), by = join_by("plot" == "plot_id")) |> 
  clipr::write_clip()

# master xtabs
# clean master file

master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)
master_counts <- master_yield |> 
  filter(year %in% 2015:2023, !(crop %in% c("c_silage", "c silage"))) |> 
  count(plot, year) |>
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment)) |> 
  rename(master_n = n) 

master_counts |> 
  complete(plot, year) |>  # all complete
  left_join(yield_counts) |> 
  filter(master_n != n, actual_crop != "past") |> 
  arrange(year, plot)  |> clipr::write_clip()
# all accounted for except 1!


# verify pasture separately, need to pull biomassings together for that.  
tbl_2016_harvests |> filter(plot %in% c(105, 203, 308, 406))  

# need biomassings
pasture_yields <- bind_rows(tbl_harvests |> filter(crop %in% c("pasture", "Pasture")), tbl_bio |> filter(biomass %in% c("Pasture", "pasture"))) |> 
  mutate(
    plot = factor(plot, levels = plots_by_treatment),
    year = coalesce(year(harvest_date), year(biomass_date)),
    date = coalesce(harvest_date, biomass_date),
    area = coalesce(harvest_area, biomass_area)) 
pasture_yields |> 
  count(plot, year) |> 
  left_join(master_counts) |> 
  arrange(year, plot) |> 
  clipr::write_clip()

# 112, 2016
pasture_yields |> filter(year == 2016, plot == "A112") |> 
  get_biomass()
# that weird thing
pasture_yields |> filter(year == 2019, plot == "A112") |> 
  get_biomass("past") |> View()
pasture_yields |> filter(year == 2021,plot == "A112") |> 
  get_biomass("past") |> View()

supp_harvests |> filter(harvesting_id == "H2019_A112MMX_PT_1")





  

