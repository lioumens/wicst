
library(readxl)
library(tidyverse)
pasture_fp <- "~/OneDrive - UW-Madison/Database development/Michael/pasture_cleaned.xlsx"

raw_past_massings <- read_excel(pasture_fp, sheet = "massings",na = ".", range = cell_cols("A:K"))
raw_past_exclosures <- read_excel(pasture_fp, sheet = "exclosures")
raw_past_management <- read_excel(pasture_fp, sheet = "management")
raw_past_summary <- read_excel(pasture_fp, sheet = "summary")
raw_past_animal <- read_excel(pasture_fp, sheet = "animal")

xl_pasture_raw <- list(
  massings = raw_past_massings,
  exclosures = raw_past_exclosures,
  management = raw_past_management,
  summary =  raw_past_summary,
  animal = raw_past_animal
)

# clean a little ----------------------------------------------------------

past_exclosures <- raw_past_exclosures |> 
  filter(type != "harvest") |> # excluding single harvest?
   mutate(biomass_grams = coalesce(lbs / 2.2046226 * 1000, kg * 1000),
          biomass_area = areaft2,
          biomass_length = length,
          biomass_width = width,
          method = type,
          percent_moisture = moisture * 100,
          bag_weight = bagg,
          wet_weight_no_bag = wetg,
          dry_weight_no_bag = dryg,
          yield = coalesce(yield, biomass_grams * (100 - percent_moisture) / 100 * 2.2046226 / 1000 / 2000 / (biomass_area / 43560))
          ) |> 
  select(-wetg, -dryg, -bagg, -type, -width, -length, -areaft2, -lbs, -kg)


# Assumptions: stubble height 1
# back calculate assumed 20% moisture and 1 sq m quadrat
# not actually exclosures,
past_massings <- raw_past_massings |> 
  mutate(cycle = case_when(year == 2008 & plot == 405 & tendayperiod == 9 ~ 3,
                           .default = cycle),
         stubble_height = case_when(type == "quadrat" & !is.na(yield)~1),
         type = case_match(type, "exclosure"~"harvest",
                           .default = type),
         ml_note = case_when(type == "quadrat" &
                               !is.na(yield) & 
                               is.na(moisture) &
                               is.na(area) ~ "assumed 20% moisture and 1 sq m quadrats, deduced grams from yield value"),
         area = case_when(type == "quadrat" & !is.na(yield) & is.na(area)~10.76391), # assume sq m quadrat
         moisture = case_when(type == "quadrat" & !is.na(yield) & is.na(moisture) ~ 20),
         grams = yield * 2000 / 2.2046226 * 1000 / ((100 - moisture) / 100) * (area / 43560)) # t/acre to grams

past_animal <- raw_past_animal
past_management <- raw_past_management
past_summary <- raw_past_summary

# collect
xl_pasture <- list(
  massings = past_massings,
  exclosures = past_exclosures,
  animal = past_animal,
  management = past_management,
  summary = past_summary
)

# QA Section -------------------------------------------------------------

# first level avg/max
# mass_pt <- raw_past_massings |>
#   group_by(year, plot, date, cycle) |> 
#   mutate(cycle = case_when(year == 2008 & plot == 405 & tendayperiod == 9 ~ 3,
#                            .default = cycle)) |>
#   summarize(avg = mean(yield, na.rm = T),
#             max = max(yield, na.rm = T),
#             avg_no = mean(yield[subsample != "hay"], na.rm = T),
#             max_no = max(yield[subsample != "hay"], na.rm = T)) |> 
#   mutate(across(avg:max_no, \(x) case_when(is.infinite(x) | is.nan(x)~NA,
#                                            .default = x)))
# 
# # second level avg/max
# mass_pt2 <- mass_pt |> 
#     group_by(year, plot, cycle) |> 
#   summarize(avg_avg = mean(avg, na.rm = T),
#             max_max = max(max, na.rm = T),
#             avg_avg_no = mean(avg_no, na.rm = T),
#             max_max_no = max(max_no, na.rm = T))  |>
#   mutate(across(avg_avg:max_max_no, \(x) case_when(is.infinite(x) | is.nan(x)~NA,
#                                            .default = x)))
    # pivot_wider(names_from = "cycle", values_from = c("avg_avg", "max_max"), names_vary = "slowest")

# mass_pt2 |> View()

# # third level sum
# qa_mass <- mass_pt2 |> group_by(year, plot) |> 
#   summarize(n = n(),
#             sum_avg_avg = sum(avg_avg, na.rm = T),
#             sum_max_max = sum(max_max, na.rm = T),
#             sum_avg_avg_no = sum(avg_avg_no, na.rm = T),
#             sum_max_max_no = sum(max_max_no, na.rm = T),
#             adj_sum_avg_avg = .75 * sum_avg_avg,
#             adj_sum_max_max = .75 * sum_max_max,
#             adj_sum_avg_avg_no = .75 * sum_avg_avg_no,
#             adj_sum_max_max_no = .75 * sum_max_max_no) |> 
#   arrange(year, plot)

# one missing cycle, why?
# raw_past_massings |> filter(is.na(cycle))
# raw_past_massings |> filter(year == 2008, plot == 405)
# changes from 1.25 to 1.22 for 2008
# 
# qa_exclosures <- past_exclosures |> group_by(year, plot) |> 
#   summarize(sum_yield = sum(yield, na.rm = T),
#             n_he = n(),
#             stub1_he = .75 * sum_yield,
#             stub2_he = .875 * sum_yield, 
#             stub3_he = sum_yield) |> 
#   select(-sum_yield)
# 
# 
# raw_past_summary |> 
#   pivot_wider(names_from = source, values_from = ton_ac) |>
#   full_join(qa_mass, by = c("year", "plot")) |>
#   full_join(qa_exclosures, by = c("year", "plot")) |> 
#   clipr::write_clip()

# master_past |> filter(between(year, 2004, 2012)) |>
#   full_join(past_exclosures, by = c("year", "plot", "cut")) |> 
#   clipr::write_clip()

# useful
date_string <- Sys.Date() %>% format(format = "%Y%m%d")
filepath_date <- paste0("data/pasture_", date_string, ".Rdata")

save(xl_pasture_raw, xl_pasture, file = filepath_date)


