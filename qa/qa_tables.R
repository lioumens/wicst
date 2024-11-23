# QUACK

source("migration/yield_prep.R")
library(lubridate)

plots_by_treatment <- xl_core$plots |> arrange(treatment_id,plot_id) |> 
  pull(plot_id)

# year_plot_crop
xl_core$plots$plot_id
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

