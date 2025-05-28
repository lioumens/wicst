# QUACK

library(lubridate)
library(clipr)
library(readxl)
source("migration/yield_prep.R")
source("migration/collect.R")


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

# duplicate harvesting ids
tbl_harvests |> count(harvesting_id) |> arrange(desc(n))
# unique!
supp_harvests |> count(harvesting_id) |> arrange(desc(n))
# unique!
tbl_bio |> count(biomassing_id) |> arrange(desc(n))
supp_bio |>  count(biomassing_id) |> arrange(desc(n))
# unique!

supp_loss |>  count(harvestingloss_id) |> arrange(desc(n))
tbl_sysloss |> count(systematicharvestingloss_id) # unique!
supp_sysloss |> count(systematicharvestingloss_id) # unique!
tbl_loss |> count(harvestingloss_id) |> arrange(desc(n))


tbl_can |> count(canopeo_id) |> arrange(desc(n))
supp_can |> count(canopeo_id) |> arrange(desc(n))

tbl_ei_harvests |> count(harvesting_id) |> arrange(desc(n))
supp_ei_harvests |> count(harvesting_id) |> arrange(desc(n))
tbl_ei_bio |> count(biomassing_id) |> arrange(desc(n))

tbl_ei_sysloss |> count(systematicharvestingloss_id) # unique!
tbl_prairie |> count(harvesting_id) |> arrange(desc(n))

# Yield Comparison --------------------------------------------------------

master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

# not robust the way it's implemented now, but still valid
db_adj_harvestings <- db_harvestings |> left_join(
  db_directlosses |>
    summarize(direct_loss = sum(loss_area), .by = harvesting_id),
  by = "harvesting_id"
) |> left_join(
  db_systematiclosses |>
    summarize(sys_loss = sum(loss_fraction), .by = c(harvesting_id, loss_category)),
  by = "harvesting_id"
) |> mutate(harvest_area = (harvest_area - coalesce(direct_loss, 0)) * (1 - coalesce(sys_loss, 0))) |> 
  get_yield(product)

# adj silage for loss
db_adj_silage_harvestings <- db_silage_harvestings |> left_join(
  db_silage_directlosses |> 
    summarize(total_loss = sum(loss_area),
              .by = harvesting_id),
  by = "harvesting_id"
) |>
  left_join(
    # works for now since no duplicate loss types
    db_silage_sysloss |> summarize(total_frac = sum(loss_fraction), .by = c(harvesting_id, loss_type)), by = "harvesting_id"
  ) |> 
  mutate(harvest_area = (harvest_area - coalesce(total_loss, 0)) * (1 - coalesce(total_frac, 0))) |> 
  get_yield("corn silage")

harvest_for_qa <- bind_rows(db_adj_harvestings,
                            db_biomassings |> get_biomass(),
                            db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date)),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

# harvest_for_qa |> filter(year == 2017, product == "corn silage")
# db_harvestings |> filter( product == "oatlage")
# harvest_for_qa |> filter(product == "corn silage")
# db_biomassings |> filter(year(biomass_date) == 2017, plot_id == "A311") |> get_biomass() |> View()

yield_compare <- master_yield_for_qa |> full_join(harvest_for_qa,
                                                  by = join_by("year" == "year", "plot" == "plot_id",
                                                               "cut" == "cut", "product" == "product")) |> 
  select(id, site, year, block, system, phase, cycle, plot, cut, crop, product, mst_dm, percent_moisture,
         bu_t_ac, everything()) |> 
  mutate(plot = factor(plot, levels = plots_by_treatment)) |> 
  arrange(year, plot, product, cut) |>
  mutate(
    # for excel coloring
    bu_t_ac = as.numeric(bu_t_ac),
    delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac), NA),
                     abs(harvest_tons_dm_per_acre - bu_t_ac),
                     abs(biomass_tons_dm_per_acre - bu_t_ac), 
                     Inf),
    delta = case_when(delta < .001 ~ NA,
                      .default = delta),
    relative_delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac) / bu_t_ac, NA),
                              abs(harvest_tons_dm_per_acre - bu_t_ac) / (bu_t_ac),
                              abs(biomass_tons_dm_per_acre - bu_t_ac) / (bu_t_ac), 
                              Inf),
    relative_delta = case_when(relative_delta < .01 ~ NA,
                               .default = relative_delta)
  ) 

# temp for  comparisons
yield_compare |> 
  filter(relative_delta > .8, relative_delta != Inf) |> 
  View()
yield_compare |> 
  pull(relative_delta) |>
  na_if(Inf) |> 
  quantile(probs = seq(.1, .9, .1), na.rm = TRUE)


# db_harvestings |> count(product)

# harvest_for_qa |> filter(year == 2017, plot_id == "A311")


harvest_for_qa_exclosure <- bind_rows(db_adj_harvestings,
                                      db_biomassings |> filter(method == "exclosure") |> get_biomass(),
                                      db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date)),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, 
                                                             suppressWarnings(as.numeric(str_sub(id, start = 18, end = 18))),
                                                             row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)


yield_compare_no_biomass <- master_yield_for_qa |> 
  full_join(harvest_for_qa_exclosure,
            by = join_by("year" == "year", "plot" == "plot_id",
                         "cut" == "cut", "product" == "product")) |> 
  select(id, site, year, block, system, phase, cycle, plot, cut, crop, product, mst_dm, percent_moisture,
         bu_t_ac, everything()) |> 
  mutate(plot = factor(plot, levels = plots_by_treatment)) |> 
  arrange(year, plot, product, cut) |>
  mutate(
    # for excel coloring
    bu_t_ac = as.numeric(bu_t_ac),
    delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac), NA),
                     abs(harvest_tons_dm_per_acre - bu_t_ac),
                     abs(biomass_tons_dm_per_acre - bu_t_ac), 
                     Inf),
    delta = case_when(delta < .001 ~ NA,
                      .default = delta),
    relative_delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac) / bu_t_ac, NA),
                              abs(harvest_tons_dm_per_acre - bu_t_ac) / (bu_t_ac),
                              abs(biomass_tons_dm_per_acre - bu_t_ac) / (bu_t_ac), 
                              Inf),
    relative_delta = case_when(relative_delta < .01 ~ NA,
                               .default = relative_delta)
  ) 

yield_compare_no_biomass |> clipr::write_clip()

yield_compare_no_biomass |> filter(year == 2019, crop == "c silage")
db_silage_harvestings |> 
  filter(str_sub(sideplot, 1, 3) == "203") |> 
  left_join(db_silage_harvestingdetails,
            by = "harvesting_id") |>
  View()

db_silage_harvestings |> 
  filter(year(harvest_date) == 2019 & str_sub(sideplot, 1, 3) == "203")



# yield_compare_3 --------------------------------------------------

master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)

# master_counts <- master_yield |> 
#   filter(year %in% 2015:2023, !(crop %in% c("c_silage", "c silage"))) |> 
#   count(plot, year) |>
#   mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment)) |> 
#   rename(master_n = n) 

master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

# not robust the way it's implemented now, but still valid
db_adj_harvestings <- db_harvestings |> left_join(
  db_directlosses |>
    summarize(direct_loss = sum(loss_area), .by = harvesting_id),
  by = "harvesting_id"
) |> left_join(
  db_systematiclosses |>
    summarize(sys_loss = sum(loss_fraction), .by = c(harvesting_id, loss_category)),
  by = "harvesting_id"
) |> mutate(harvest_area = (harvest_area - coalesce(direct_loss, 0)) * (1 - coalesce(sys_loss, 0))) |> 
  get_yield(product)

# adj silage for loss
db_adj_silage_harvestings <- db_silage_harvestings |> left_join(
  db_silage_directlosses |> 
    summarize(total_loss = sum(loss_area),
              .by = harvesting_id),
  by = "harvesting_id"
) |>
  left_join(
    # works for now since no duplicate loss types
    db_silage_sysloss |> summarize(total_frac = sum(loss_fraction), .by = c(harvesting_id, loss_type)), by = "harvesting_id"
  ) |> 
  mutate(harvest_area = (harvest_area - coalesce(total_loss, 0)) * (1 - coalesce(total_frac, 0))) |> 
  get_yield("corn silage")

harvest_for_qa <- bind_rows(db_adj_harvestings,
                            db_biomassings |> get_biomass(),
                            db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date)),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

# extra
harvest_for_qa_exclosure <- bind_rows(db_adj_harvestings,
                                      db_biomassings |> filter(method == "exclosure") |> get_biomass(),
                                      db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date), as.numeric(str_sub(harvesting_id, 2, 5))),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, 
                                                             suppressWarnings(as.numeric(str_sub(id, start = 18, end = 18))),
                                                             row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

yield_compare_3 <- master_yield_for_qa |> filter(product != "pasture", site == "ARL") |> 
  full_join(harvest_for_qa_exclosure |> filter(product != "pasture"),
            by = join_by("year" == "year", "plot" == "plot_id",
                         "cut" == "cut", "product" == "product")) |> 
  select(id, site, year, block, system, phase, cycle, plot, cut, crop, product, mst_dm, percent_moisture,
         bu_t_ac, everything()) |> 
  mutate(plot = factor(plot, levels = plots_by_treatment)) |> 
  arrange(year, plot, product, cut) |>
  mutate(
    # for excel coloring
    bu_t_ac = as.numeric(bu_t_ac),
    delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac), NA),
                     abs(harvest_tons_dm_per_acre - bu_t_ac),
                     abs(biomass_tons_dm_per_acre - bu_t_ac), 
                     Inf),
    delta = case_when(delta < .001 ~ NA,
                      .default = delta),
    relative_delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac) / bu_t_ac, NA),
                              abs(harvest_tons_dm_per_acre - bu_t_ac) / (bu_t_ac),
                              abs(biomass_tons_dm_per_acre - bu_t_ac) / (bu_t_ac), 
                              Inf),
    relative_delta = case_when(relative_delta < .01 ~ NA,
                               .default = relative_delta)
  )

yield_compare_3 |> write_clip()

# experimenting and interactive QA
# yield_compare_3 |> filter(id == "H1990_A110MMX_OT_1")
# db_adj_harvestings |> filter(harvesting_id == "H1990_A110MMX_OT_1")

db_harvestings |> filter(year(harvest_date) == 1991, plot_id == "A109") 
db_harvestings |> filter(harvesting_id == "H1991_A109MMX_CN_1")
db_adj_harvestings |> filter(harvesting_id == "H1991_A109MMX_CN_1")
yield_compare_3 |> filter(id == "B2022_A112MM3_PT_1_EXST")

harvest_for_qa_exclosure |> filter(id == "B2022_A112MM3_PT_1_EXST")

tbl_2008_harvests |> filter(crop == "soybean") |> get_yield() |> View()
yield_compare_3 |> filter(id == "H2008_A101MMX_SB_1")

db_adj_harvestings |> filter(year(harvest_date) == 2009, plot_id == "A111")

db_harvestings |> filter(harvesting_id == "H2008_A213MMX_CN_1") |>
  get_yield() |> 
  View()
db_adj_harvestings |> filter(harvesting_id == "H2008_A213MMX_CN_1") |> View()

db_adj_harvestings |> filter(harvesting_id == "H1996_A409MMX_AF_1") 

tbl_1996_harvests |> View()

# 409 missing in 1996?
tbl_1996_harvests |> filter(plot == 409) |> slice(1) |> 
  mutate(harvest_area = 30600,
         percent_moisture = 32.3) |> 
  get_yield() |> View()

# H2002_A206MMX_CN_1 differs by 25%
tbl_2002_harvests |> filter(harvesting_id == "H2002_A206MMX_CN_1") |> 
  get_yield("corn") |> View()
#
tbl_2002_harvests |> filter(harvesting_id == "H2002_A113MMX_CN_1") |> 
  get_yield("corn") |> View()
tbl_2002_loss |> filter(harvesting_id == "H2002_A204MMX_CN_1")
tbl_2008_harvests |> filter(harvesting_id == "H2008_A101MMX_SB_1") |> get_yield("soybean") |> View()
tbl_2008_loss |> filter(harvesting_id == "H2008_A101MMX_SB_1")
db_directlosses |> filter(harvesting_id == "H2008_A101MMX_SB_1")
db_adj_harvestings |> filter(harvesting_id == "H2008_A101MMX_SB_1") 
yield_compare_3 |> filter(id == "H2008_A101MMX_SB_1")|> View()

tbl_2008_harvests |> filter(harvesting_id == "H2008_A104MMX_WG_1") |> get_yield("old_wheat") |> 
  View()
# weirdness with first cut
db_harvestings |> filter(harvesting_id == "H2009_A203MMX_AF_1") |>
  mutate(harvest_area = 30600) |>  get_yield() |> View()
db_directlosses |> filter(harvesting_id == "H2009_A210MMX_AF_1")

# weirdness
db_harvestings |> filter(harvesting_id == "H2009_A213MMX_AF_2")
db_genlosses |> filter(harvesting_id == "H2009_A213MMX_AF_2")

db_harvestings |> filter(harvesting_id == "H2010_A202MMX_CN_1") |> 
  mutate(harvest_area = 510 * 30 - 450) |> get_yield() |> View()
db_directlosses |> filter(harvesting_id == "H2010_A202MMX_CN_1")

db_adj_harvestings |> filter(harvesting_id == "H2008_A109MMX_CN_1")

db_directlosses |> filter(harvesting_id == "H2013_A206MMX_SB_1")

db_harvestings |> filter(harvesting_id == "H2013_A402MMX_SB_1") 


## Manurings ---------------------------------------------------------------

# cross tab, year and plot
xt_man_year_plot <- xl_agcal$manurings |>
  mutate(year = year(manuring_date),
         plot = factor(plot_id, levels = plots_by_treatment)) |> 
  count(year, plot) |> 
  complete(year, plot) |> 
  arrange(plot) |>
  filter(!str_starts(plot, "L") | is.na(plot)) |> # no lakeland
  left_join(crop_lookup, by = c("year", "plot")) |> 
  mutate(value = glue("{actual_crop} {n}")) |> 
  pivot_wider(names_from = plot, id_cols = year) |> 
  arrange(year)

xt_man_year_plot |> clipr::write_clip()

xl_agcal$manurings |>
  count(source, type, method, rate, rate_unit) |>
  clipr::write_clip()
xl_agcal$manurings |> count(year(manuring_date)) |>
  clipr::write_clip()

xl_agcal$manurings |> filter(year(manuring_date) == 1990)

## Tillings ----------------------------------------------------------------

xt_till_year_plot <- xl_agcal$tillings |>
  mutate(year = year(tilling_date),
         plot = factor(plot_id, levels = plots_by_treatment)) |> 
  count(year, plot) |> 
  complete(year, plot) |> 
  arrange(plot) |>
  filter(!str_starts(plot, "L") | is.na(plot)) |> # no lakeland
  left_join(crop_lookup, by = c("year", "plot")) |> 
  mutate(value = glue("{actual_crop} {n}")) |> 
  pivot_wider(names_from = plot, id_cols = year) |> 
  arrange(year)

xt_till_year_plot |> 
  clipr::write_clip()

xl_agcal$tillings |> 
  count(type, implement, planting) |>
  clipr::write_clip()


## Fertilizers -------------------------------------------------------------

xt_fert_year_plot <- xl_agcal$fertilizings |> 
  mutate(year = year(fertilizing_date),
         plot = factor(plot_id, levels = plots_by_treatment)) |> 
  count(year, plot) |> 
  complete(year, plot) |> 
  arrange(plot) |>
  filter(!str_starts(plot, "L") | is.na(plot)) |> # no lakeland
  left_join(crop_lookup, by = c("year", "plot")) |> 
  mutate(value = glue("{actual_crop} {n}")) |> 
  pivot_wider(names_from = plot, id_cols = year) |> 
  arrange(year)

xt_fert_year_plot |> clipr::write_clip()

xl_agcal$fertilizings |> count(fertilizer_type, n, p2o5, k2o, s, ca, mg) |> clipr::write_clip()
xl_agcal$fertilizings |> count(fertilizer_type, rate, rate_unit) |> clipr::write_clip()

## Pesticidings -------------------------------------------------------------

xt_pest_year_plot <- xl_agcal$pesticidings |> 
  mutate(year = year(pesticiding_date),
         plot = factor(plot_id, levels = plots_by_treatment)) |> 
  count(year, plot) |> 
  complete(year, plot) |> 
  arrange(plot) |>
  filter(!str_starts(plot, "L") | is.na(plot)) |> # no lakeland
  left_join(crop_lookup, by = c("year", "plot")) |> 
  mutate(value = glue("{actual_crop} {n}")) |> 
  pivot_wider(names_from = plot, id_cols = year) |> 
  arrange(year)

xt_pest_year_plot |> clipr::write_clip()

xl_agcal$pesticidings |> count(name, rate, rate_unit) |> clipr::write_clip()


  # yield_compare_4 --------------------------------------------------

master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)

# master_counts <- master_yield |> 
#   filter(year %in% 2015:2023, !(crop %in% c("c_silage", "c silage"))) |> 
#   count(plot, year) |>
#   mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment)) |> 
#   rename(master_n = n) 

master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

# not robust the way it's implemented now, but still valid
db_adj_harvestings <- db_harvestings |> left_join(
  db_directlosses |>
    summarize(direct_loss = sum(loss_area), .by = harvesting_id),
  by = "harvesting_id"
) |> left_join(
  db_systematiclosses |>
    summarize(sys_loss = sum(loss_fraction), .by = c(harvesting_id, loss_category)),
  by = "harvesting_id"
) |> mutate(harvest_area = (harvest_area - coalesce(direct_loss, 0)) * (1 - coalesce(sys_loss, 0))) |> 
  get_yield(product)

# adj silage for loss
db_adj_silage_harvestings <- db_silage_harvestings |> left_join(
  db_silage_directlosses |> 
    summarize(total_loss = sum(loss_area),
              .by = harvesting_id),
  by = "harvesting_id"
) |>
  left_join(
    # works for now since no duplicate loss types
    db_silage_sysloss |> summarize(total_frac = sum(loss_fraction), .by = c(harvesting_id, loss_type)), by = "harvesting_id"
  ) |> 
  mutate(harvest_area = (harvest_area - coalesce(total_loss, 0)) * (1 - coalesce(total_frac, 0))) |> 
  get_yield("corn silage")

harvest_for_qa <- bind_rows(db_adj_harvestings,
                            db_biomassings |> get_biomass(),
                            db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date)),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

# extra
harvest_for_qa_exclosure <- bind_rows(db_adj_harvestings,
                                      db_biomassings |> filter(method == "exclosure") |> get_biomass(),
                                      db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date), as.numeric(str_sub(harvesting_id, 2, 5))),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, 
                                                             suppressWarnings(as.numeric(str_sub(id, start = 18, end = 18))),
                                                             row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

yield_compare_4 <- master_yield_for_qa |> filter(product != "pasture", site == "ARL") |> 
  full_join(harvest_for_qa_exclosure |> filter(product != "pasture"),
            by = join_by("year" == "year", "plot" == "plot_id",
                         "cut" == "cut", "product" == "product")) |> 
  select(id, site, year, block, system, phase, cycle, plot, cut, crop, product, mst_dm, percent_moisture,
         bu_t_ac, everything()) |> 
  mutate(plot = factor(plot, levels = plots_by_treatment)) |> 
  arrange(year, plot, product, cut) |>
  mutate(
    # for excel coloring
    bu_t_ac = as.numeric(bu_t_ac),
    delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac), NA),
                     abs(harvest_tons_dm_per_acre - bu_t_ac),
                     abs(biomass_tons_dm_per_acre - bu_t_ac), 
                     Inf),
    delta = case_when(delta < .001 ~ NA,
                      .default = delta),
    relative_delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac) / bu_t_ac, NA),
                              abs(harvest_tons_dm_per_acre - bu_t_ac) / (bu_t_ac),
                              abs(biomass_tons_dm_per_acre - bu_t_ac) / (bu_t_ac), 
                              Inf),
    relative_delta = case_when(relative_delta < .01 ~ NA,
                               .default = relative_delta)
  )

yield_compare_4 |> write_clip()

yield_compare_4 |> filter(id == "H1993_A110MMX_OS_2")
db_harvestings |> filter(harvesting_id == "H2006_A206MMX_CN_1")
db_harvestingdetails |> filter(harvesting_id == "H2006_A206MMX_CN_1") |> pull(comments)

db_harvestings |> filter(harvesting_id == "H1991_A103MMX_OT_1") |> 
  get_yield("oat grain") |>
  View()

db_harvestings |> filter(harvesting_id == "H2008_A204MMX_CN_1")
yield_compare_4 |> filter(id == "H2008_A204MMX_CN_1")

db_harvestingdetails |>
  filter(harvesting_id == "H2015_A111MMX_AF_2")

# yield_compare_5 ---------------------------------------------------------

master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)

# master_counts <- master_yield |> 
#   filter(year %in% 2015:2023, !(crop %in% c("c_silage", "c silage"))) |> 
#   count(plot, year) |>
#   mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment)) |> 
#   rename(master_n = n) 

master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

# not robust the way it's implemented now, but still valid
db_adj_harvestings <- db_harvestings |> left_join(
  db_directlosses |>
    summarize(direct_loss = sum(loss_area), .by = harvesting_id),
  by = "harvesting_id"
) |> left_join(
  db_systematiclosses |>
    summarize(sys_loss = sum(loss_fraction), .by = c(harvesting_id, loss_category)),
  by = "harvesting_id"
) |> mutate(harvest_area = (harvest_area - coalesce(direct_loss, 0)) * (1 - coalesce(sys_loss, 0))) |> 
  get_yield(product)

# adj silage for loss
db_adj_silage_harvestings <- db_silage_harvestings |> left_join(
  db_silage_directlosses |> 
    summarize(total_loss = sum(loss_area),
              .by = harvesting_id),
  by = "harvesting_id"
) |>
  left_join(
    # works for now since no duplicate loss types
    db_silage_sysloss |> summarize(total_frac = sum(loss_fraction), .by = c(harvesting_id, loss_type)), by = "harvesting_id"
  ) |> 
  mutate(harvest_area = (harvest_area - coalesce(total_loss, 0)) * (1 - coalesce(total_frac, 0))) |> 
  get_yield("corn silage")

harvest_for_qa <- bind_rows(db_adj_harvestings,
                            db_biomassings |> get_biomass(),
                            db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date)),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

# extra
harvest_for_qa_exclosure <- bind_rows(db_adj_harvestings,
                                      db_biomassings |> filter(method == "exclosure") |> get_biomass(),
                                      db_adj_silage_harvestings) |> 
  mutate(
    id =  coalesce(harvesting_id, biomassing_id),
    product = coalesce(product, biomass),
    year = coalesce(year(harvest_date), year(biomass_date), as.numeric(str_sub(harvesting_id, 2, 5))),
    plot_id = coalesce(factor(plot_id, levels = plots_by_treatment), str_c("A", str_sub(sideplot, 1, 3)))) |> 
  group_by(year, plot_id) |> 
  mutate(cut = if_else(product == "corn silage", 1, coalesce(cut, 
                                                             suppressWarnings(as.numeric(str_sub(id, start = 18, end = 18))),
                                                             row_number()))) |>
  select(id, year, plot_id, product, cut, percent_moisture, harvest_tons_dm_per_acre, corrected_bu_per_acre,
         biomass_tons_dm_per_acre, harvest_lbs, biomass_grams)

yield_compare_5 <- master_yield_for_qa |> filter(product != "pasture", site == "ARL") |> 
  full_join(harvest_for_qa_exclosure |> filter(product != "pasture"),
            by = join_by("year" == "year", "plot" == "plot_id",
                         "cut" == "cut", "product" == "product")) |> 
  select(id, site, year, block, system, phase, cycle, plot, cut, crop, product, mst_dm, percent_moisture,
         bu_t_ac, everything()) |> 
  mutate(plot = factor(plot, levels = plots_by_treatment)) |> 
  arrange(year, plot, product, cut) |>
  mutate(
    # for excel coloring
    bu_t_ac = as.numeric(bu_t_ac),
    delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac), NA),
                     abs(harvest_tons_dm_per_acre - bu_t_ac),
                     abs(biomass_tons_dm_per_acre - bu_t_ac), 
                     Inf),
    delta = case_when(delta < .001 ~ NA,
                      .default = delta),
    relative_delta = coalesce(if_else(product != "wheat straw", abs(corrected_bu_per_acre - bu_t_ac) / bu_t_ac, NA),
                              abs(harvest_tons_dm_per_acre - bu_t_ac) / (bu_t_ac),
                              abs(biomass_tons_dm_per_acre - bu_t_ac) / (bu_t_ac), 
                              Inf),
    relative_delta = case_when(relative_delta < .01 ~ NA,
                               .default = relative_delta)
  )

yield_compare_5 |> write_clip()

yield_compare_5 |> filter(id == "H2019_A102MMX_OS_1")



# Pasture Compare ---------------------------------------------------------

master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)

# master_counts <- master_yield |> 
#   filter(year %in% 2015:2023, !(crop %in% c("c_silage", "c silage"))) |> 
#   count(plot, year) |>
#   mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment)) |> 
#   rename(master_n = n) 
master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

master_past <- master_yield_for_qa |> filter(crop == "past", site == "ARL") |> select(-product) |>
  mutate(plot = readr::parse_number(as.character(plot)))

# Pasture Yield Compare ---------------------------------------------------

# master version


# arl version
arl_past_yields <- xl_arl |> map(\(x) x |> filter(crop == "p") |>
                                   select(plot, any_of("date"), num, moisture, yield, any_of("bio"))) |> 
  list_rbind(names_to = "year") |> 
  rename(exclosure = bio) # bio in the arl sheet really means exclosure

# our version
pasture_harvest_for_qa <- db_harvestings |> filter(product == "pasture") |> get_yield() |> 
  # mutate(plot = as.numeric(str_sub(plot_id,start = 2))) |> 
  arrange(harvest_date) |> 
  group_by(plot_id, year(harvest_date)) |>
  mutate(
    cut = row_number()
  ) |> 
  ungroup() |>
  get_yield() |> 
  select(harvesting_id, harvest_date, plot_id, harvest_area, cut, percent_moisture, harvest_lbs, harvest_tons_dm_per_acre) |>
  rename(
    plot = plot_id,
    our_harvest_yield = harvest_tons_dm_per_acre,
         id = harvesting_id,
         date = harvest_date,
         area = harvest_area) |> 
  add_column(type = "harvest")

# exclosures
pasture_exclosure_for_qa <- db_biomassings |> filter(biomass == "pasture", method == "exclosure") |> 
  # mutate(plot = as.numeric(str_sub(plot_id, start = 2))) |>
  get_biomass() |> 
  select(biomassing_id, biomass_date, plot_id, cut, biomass_area,
         percent_moisture, biomass_grams, biomass_tons_dm_per_acre) |>
  rename(
    plot = plot_id,
    our_exclosure_yield = biomass_tons_dm_per_acre,
         id = biomassing_id,
         date = biomass_date,
         area = biomass_area) |>
  add_column(type = "exclosure")

pasture_harvest_and_exclosure_for_qa <- bind_rows(pasture_exclosure_for_qa, pasture_harvest_for_qa) |> 
  relocate(type, .after = cut) |> 
  relocate(our_exclosure_yield, .after = our_harvest_yield) |>
  relocate(biomass_grams, .after = harvest_lbs) |> 
  mutate(year = year(date))

# yields
master_yield <- xl_master |> clean_names() |> select(site:rfq4) |>
  mutate(across(where(is.character), \(x) na_if(x, "."))) |> 
  pivot_longer(matches(".*[1-4]"), names_pattern = "(.*)([1-4])",
               names_to = c(".value", "cut")) |> 
  filter(!is.na(crop)) # some 0 yields for crop NA. Just assuming these are no harvests (for cut 4)

master_yield_for_qa <- master_yield |> 
  filter(between(year, 1990, 2023),
         !is.na(bu_t_ac) & bu_t_ac != 0) |> 
  mutate(plot = factor(str_c("A", plot), levels = plots_by_treatment),
         cut = as.numeric(cut),
         product = case_match(crop,
                              "fc"~"filler corn",
                              "c"~"corn",
                              "sb"~"soybean",
                              c("a0", "a1", "a2", "dsa")~"alfalfa",
                              c("of")~"oatlage",
                              c("os")~"oat straw",
                              c("og")~"oat grain",
                              c("past")~"pasture",
                              c("rc")~"red clover", 
                              c("wg")~"wheat grain",
                              c("wheatlage", "wl")~"wheatlage",
                              c("ws")~"wheat straw",
                              c("c silage", "c_silage")~"corn silage",
                              .default = crop),
         mst_dm = case_when(product %in% c("oatlage", "wheat straw", "wheatlage", "alfalfa", "oat straw", "pasture", "corn silage") ~ (100 - as.numeric(mst_dm)),
                            .default = as.numeric(mst_dm))) |> 
  arrange(year, plot, cut) |> 
  select(-rfv, -rfq)

masture_pasture_yield_for_qa <- master_yield_for_qa |>
  mutate(type = case_when(
    year < 1993 | year == 2020~"harvest",
                          year > 2003~"exclosure"))

masture_pasture_yield_for_qa |> 
  mutate(plot = as.character(plot)) |>
  filter(site == "ARL", product == "pasture") |>
  full_join(pasture_harvest_and_exclosure_for_qa, by = join_by(type == type,
                                                               plot == plot,
                                                               cut == cut,
                                                               year == year)) |>
  arrange(year) |>
  clipr::write_clip()






