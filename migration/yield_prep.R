library(stringr)
library(dplyr)
library(tibble)
library(glue)
library(janitor)
library(purrr)
library(tidyr)
library(rlang)

load("data/wip_20241122.Rdata")
load("data/master_20240926.Rdata")
load("data/core_20241104.Rdata")
load("data/agcal_20241119.Rdata")

c_ideal_percent_moisture = 15.5
c_bushel = 56
wg_ideal_percent_moisture = 13.5
wg_bushel = 60
sb_ideal_percent_moisture = 13
sb_bushel = 60
ws_ideal_percent_moisture = 13
ws_bushel = 60
b_ideal_percent_moisture = 14.5
b_bushel = 48
# barley straw ideal moisture?/bushel?
oat_ideal_percent_moisture = 14
oat_bushel = 32 # ? is this right? 2019 oat harvest uses this
kg_to_lbs = 2.2046226
acre_to_ft2 = 43560
m2_to_ft2 = 10.7639
m_to_ft = 3.28084

ei_plots <- xl_core$subplots$plot |> unique() |> str_sub(2) |> as.numeric()

# workshopping custom id
# section: 0 - main
# section: 789
#          456
#          123
get_harvest_id <- function(year, plot, section, product, cut = 1, site = "A") {
  
  # section_code <- case_match(section,
  #            c("s", "S", "south", "South") ~ "123",
  #            c("w", "W", "west", "West") ~ "147",
  #            c("n", "N", "north", "North") ~ "789",
  #            c("e", "E", "east", "East") ~ "369",
  #            c("NW") ~ "777",
  #            c("NE") ~ "999",
  #            c("SW") ~ "111",
  #            c("SE") ~ "333",
  #            c("Main") ~ "0",
  #            .default = "")
  
  section_code <- case_match(section,
                             c("s", "S", "south", "South") ~ "SS",
                             c("w", "W", "west", "West") ~ "WW", # EI
                             c("n", "N", "north", "North") ~ "NN",
                             c("e", "E", "east", "East") ~ "EE", # EI
                             c("NW") ~ "NW",
                             c("NE") ~ "NE",
                             c("SW") ~ "SW",
                             c("SE") ~ "SE",
                             c("East 15'") ~ "ES", # Corn Silage
                             c("West 15'") ~ "WS", # Corn Silage
                             c("Macro") ~ "MA",
                             c("Micro") ~ "MI",
                             c("Main", "main", "MAIN") ~ "MM",
                             .default = "XX")
  
  product_code <- case_match(product,
                             c("corn")~"CN",
                             c("corn silage")~"CS",
                             c("wheat", "wheat grain", "w/cl")~"WG",
                             c("soybean")~"SB",
                             c("wheat straw")~"WS",
                             c("barley")~"BY",
                             c("alfalfa", "dsA", "A1", "A2", "o/A", "ORG A1", "Ai", "Aii", "a", "o/a", "O/A")~"AF",
                             c("red_clover")~"RC",
                             c("berseem_clover")~"BC",
                             c("oats")~"OT",
                             c("oat straw")~"OS",
                             c("pasture", "P")~"PT",
                             c("prairie")~"PR",
                             .default = "XX")
  
  return(glue("H{year}_{site}{plot}{section}{coordinate}_{product}_{cut}",
              plot = replace_na(as.character(plot), "XXX"), # handle when plot is missing
              section = section_code,
              coordinate = "X", # coordinate is not applicable in harvests
              product = product_code))
}

get_biomassing_id <- function(year, plot, section, coordinate, biomass,
                              cut = 1, site = "A", method = "quadrat",component = "shoots") {
  # is it the main plot, or is it subplot for the EI experiments
  section_code <- case_match(section,
                             c("s", "S", "south", "South") ~ "SS",
                             c("w", "W", "west", "West") ~ "WW",
                             c("n", "N", "north", "North") ~ "NN",
                             c("e", "E", "east", "East") ~ "EE",
                             c("NW") ~ "NW",
                             c("NE") ~ "NE",
                             c("SW") ~ "SW",
                             c("SE") ~ "SE",
                             c("Main", "main", "MAIN") ~ "MM",
                             .default = "XX")
  
  # where in the plot
  coordinate_code <- case_match(coordinate,
                                c("South", "S", "SOUTH", "south")~"S",
                                c("Central", "Center", "C", "CENTRAL", "center", "central")~"C",
                                c("North", "N", "NORTH", "north")~"N",
                                c("Paddock 1", "paddock 1", "1")~"1",
                                c("Paddock 2", "paddock 2", "2")~"2",
                                c("Paddock 3", "paddock 3", "3")~"3",
                                c("Paddock 4", "paddock 4", "4")~"4",
                                c("Paddock 5", "paddock 5", "5")~"5",
                                c("Paddock 6", "paddock 6", "6")~"6",
                                c("Paddock 7", "paddock 7", "7")~"7",
                                .default = "X")
  biomass_code <- case_match(biomass,
                             c("rye")~"RY",
                             c("wheat")~"WT",
                             c("weeds", "weed")~"WD",
                             c("residue")~"RS",
                             c("alfalfa", "dsA", "A1", "A2", "o/A", "ORG A1")~"AF",
                             c("red_clover", "red clover")~"RC",
                             c("clover")~"CV",
                             c("berseem_clover", "berseem clover", "bc")~"BC",
                             c("oats", "oat")~"OT",
                             c("pasture")~"PT",
                             c("prairie")~"PR",
                             c("wheat straw", "wheat_straw")~"WS", 
                             c("quack")~"QK",
                             .default = "XX")
  
  cut_code <- case_when(is.numeric(cut)~as.character(cut),
                        is.na(cut)~"X",
                        .default = as.character(cut))
  
  method_code <- case_when(method %in% c("exclosure") | biomass_code == "PT"~"EX",
                           method %in% c("quadrat")~"QT",
                           method %in% c("undercutting")~"UC",
                           .default = "XX")
  
  component_code <- case_match(component,
                               "shoots"~"ST",
                               "roots"~"RT",
                               .default = "XX")
  
  return(glue("B{year}_{site}{plot}{section}{coordinate}_{biomass}_{cut}_{method}{component}",
              section = section_code,
              coordinate = coordinate_code,
              biomass = biomass_code,
              cut = cut_code,
              method = method_code,
              component = component_code))
}

get_canopeo_id <- function(year, plot, section, coordinate, biomass = "XX", cut = 1, site = "A") {
  # is it the main plot, or is it subplot for the EI experiments
  section_code <- case_match(section,
                             c("s", "S", "south", "South") ~ "SS",
                             c("w", "W", "west", "West") ~ "WW",
                             c("n", "N", "north", "North") ~ "NN",
                             c("e", "E", "east", "East") ~ "EE",
                             c("NW") ~ "NW",
                             c("NE") ~ "NE",
                             c("SW") ~ "SW",
                             c("SE") ~ "SE",
                             c("Main", "main", "MAIN") ~ "MM",
                             .default = "XX")
  
  # where in the plots
  coordinate_code <- case_match(coordinate,
                                c("South", "S", "SOUTH", "south")~"S",
                                c("Central", "Center", "C", "CENTRAL", "center")~"C",
                                c("North", "N", "NORTH", "north")~"N",
                                c("Paddock 1", "paddock 1", "1")~"1",
                                c("Paddock 2", "paddock 2", "2")~"2",
                                c("Paddock 3", "paddock 3", "3")~"3",
                                c("Paddock 4", "paddock 4", "4")~"4",
                                c("Paddock 5", "paddock 5", "5")~"5",
                                c("Paddock 6", "paddock 6", "6")~"6",
                                c("Paddock 7", "paddock 7", "7")~"7",
                                .default = "X")
  
  biomass_code <- case_match(biomass,
                             c("corn")~"CN",
                             c("corn silage")~"CS",
                             c("wheat")~"WG",
                             c("soybean")~"SB",
                             c("wheat straw", "wheat_straw")~"WS",
                             c("wheat grain")~"WG",
                             c("barley")~"BY",
                             c("alfalfa", "dsA", "A1", "A2", "o/A", "ORG A1")~"AF",
                             c("clover")~"CV",
                             c("red_clover", "red clover")~"RC",
                             c("berseem_clover", "berseem clover")~"BC",
                             c("rye")~"RY",
                             c("weeds")~"WD",
                             c("residue")~"RS",
                             c("oats", "oat")~"OT",
                             c("pasture")~"PT",
                             c("prairie")~"PR",
                             .default = "XX")
  
  return(glue("C{year}_{site}{plot}{section}{coordinate}_{biomass}_{cut}",
              section = section_code,
              coordinate = coordinate_code,
              biomass = biomass_code, # fill in later? biomass_code
              cut = cut))
}

# coordinate shouldn't be relevant for harvests
# loss_num can have multiple losses for each cut, i.e. over multiple alfalfa cuts, maybe different reasons for many areas
# share harvesting loss id between systematic and direct
get_harvestingloss_id <- function(year, plot, section,
                                  coordinate = "X", 
                                  product = "XX",
                                  cut = 1,
                                  site = "A",
                                  loss_num = 1) {
  # is it the main plot, or is it subplot for the EI experiments
  section_code <- case_match(section,
                             c("s", "S", "south", "South") ~ "SS",
                             c("w", "W", "west", "West") ~ "WW",
                             c("n", "N", "north", "North") ~ "NN",
                             c("e", "E", "east", "East") ~ "EE",
                             c("NW") ~ "NW",
                             c("NE") ~ "NE",
                             c("SW") ~ "SW",
                             c("SE") ~ "SE",
                             c("East 15'") ~ "ES", # Corn Silage
                             c("West 15'") ~ "WS", # Corn Silage
                             c("Macro") ~ "MA",
                             c("Micro") ~ "MI",
                             c("Main", "main", "MAIN") ~ "MM",
                             .default = "XX")
  
  # where in the plots
  coordinate_code <- case_match(coordinate,
                                c("South", "S", "SOUTH", "south")~"S",
                                c("Central", "Center", "C", "CENTRAL", "center")~"C",
                                c("North", "N", "NORTH", "north")~"N",
                                c("Paddock 1", "paddock 1", "1")~"1",
                                c("Paddock 2", "paddock 2", "2")~"2",
                                c("Paddock 3", "paddock 3", "3")~"3",
                                c("Paddock 4", "paddock 4", "4")~"4",
                                c("Paddock 5", "paddock 5", "5")~"5",
                                c("Paddock 6", "paddock 6", "6")~"6",
                                c("Paddock 7", "paddock 7", "7")~"7",
                                .default = "X")
  product_code <- case_match(product,
                             c("corn")~"CN",
                             c("corn silage")~"CS",
                             c("wheat")~"WG",
                             c("soybean")~"SB",
                             c("wheat straw", "wheat_straw")~"WS",
                             c("wheat grain")~"WG",
                             c("barley")~"BY",
                             c("alfalfa", "dsA", "A1", "A2", "o/A", "ORG A1")~"AF",
                             c("red_clover")~"RC",
                             c("berseem_clover")~"BC",
                             c("rye")~"RY",
                             c("weeds")~"WD",
                             c("residue")~"RS",
                             c("oats", "oat")~"OT",
                             c("pasture")~"PT",
                             c("prairie")~"PR",
                             .default = "XX")
  
  return(glue("L{year}_{site}{plot}{section}{coordinate}_{product}_{cut}_{loss_num}",
              section = section_code,
              coordinate = "X", # fill in later?
              product = product_code, 
              cut = cut, # it should be relevant, but none seen yet
              loss_num = loss_num))
}


stitch_notes <- function(notes, ml_notes) {
  case_when(
    is.na(notes) & is.na(ml_notes) ~ NA,
    !is.na(notes) & is.na(ml_notes)~ glue('General: "{notes}"', notes = notes),
    is.na(notes) & !is.na(ml_notes) ~ ml_notes,
    .default = glue('General: "{notes}"|{ml_notes}',
                    notes = notes,
                    ml_notes = ml_notes)
  )
}

get_yield <- function(dat, product = NULL) {
  corn_grains <- c("corn")
  wheat_grains <- c("wheat", "wheat_grain")
  wheat_straws <- c("wheat_straw", "wheat straw")
  soybeans <- c("soybean") 
  barleys <- c("barley")
  oats <- c("oat", "oats", "oat grain")
  
  cols_needed <- c("percent_moisture", "harvest_lbs", "harvest_area")
  missing_cols <- cols_needed[!(cols_needed %in% names(dat))]
  
  if(!is_empty(missing_cols)) {
    rlang::abort(message = glue("`dat` missing columns: {str_flatten(missing_cols, collapse = ', ')}"),
                 missing_cols = missing_cols)
  }
  
  dat |> mutate(
    harvest_lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
    harvest_tons_dm = harvest_lbs_dm / 2000,
    acre_frac = harvest_area / acre_to_ft2,
    harvest_tons_dm_per_acre = harvest_tons_dm / acre_frac,
    ideal_moisture = case_match(product %||% str_to_lower(crop),
                                corn_grains~c_ideal_percent_moisture,
                                wheat_grains~wg_ideal_percent_moisture,
                                wheat_straws~ws_ideal_percent_moisture,
                                soybeans~sb_ideal_percent_moisture,
                                barleys~b_ideal_percent_moisture,
                                oats~oat_ideal_percent_moisture),
    bushel_lbs = case_match(product %||% str_to_lower(crop),
                            corn_grains~c_bushel,
                            wheat_grains~wg_bushel,
                            wheat_straws~ws_bushel,
                            soybeans~sb_bushel,
                            barleys~b_bushel,
                            oats~oat_bushel),
    corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - ideal_moisture)),
    corrected_bu = corrected_lbs / bushel_lbs,
    corrected_lbs_per_acre = corrected_lbs / acre_frac,
    corrected_tons_per_acre = corrected_lbs_per_acre / 2000,
    corrected_bu_per_acre = corrected_bu / acre_frac)
}

get_biomass <- function(dat, biomass = NULL) {
  
  # if percent_moisture is missing, can assume that biomass_grams was dried
  cols_needed <- c("biomass_grams", "biomass_area")
  cols_optional <- c("percent_moisture")
  missing_cols <- cols_needed[!(cols_needed %in% names(dat))]
  
  if(!is_empty(missing_cols)) {
    rlang::abort(message = glue("`dat` missing columns: {str_flatten(missing_cols, collapse = ', ')}"),
                 missing_cols = missing_cols)
  }
  
  # if percent_moisture is missing, can assume that biomass_grams was dried
  dat |> mutate(
    biomass_lbs_dm = biomass_grams / 1000 * kg_to_lbs * (100 - get0("percent_moisture", ifnotfound = 0)) / 100,
    biomass_tons_dm = biomass_lbs_dm / 2000,
    acre_frac = biomass_area / acre_to_ft2,
    biomass_tons_dm_per_acre = biomass_tons_dm / acre_frac)
}


harvesting_cols <- c("harvesting_id", "harvest_date", 
                     "plot", "crop",
                     "harvest_area", "percent_moisture",
                     "harvest_lbs")

ei_harvesting_cols <- c("harvesting_id", "harvest_date", 
                        "subplot", "crop",
                        "harvest_area", "percent_moisture",
                        "harvest_lbs")

cs_harvesting_cols <- c("harvesting_id", "harvest_date", 
                        "sideplot", "crop",
                        "harvest_area", "percent_moisture",
                        "harvest_lbs")

supp_harvesting_cols <- c("harvesting_id", 
                          "harvest_length", "harvest_width", # dimensions
                          "rrl_id", "bushel_lbs",
                          "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                          "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                          "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                          "num_bales",
                          "wagon_weight", "wagon_color", "trailer_weight",
                          "comments")

supp_ei_harvesting_cols <- supp_harvesting_cols

supp_cs_harvesting_cols <- c("harvesting_id", 
                             "harvest_length", "harvest_width", # dimensions
                             "rrl_id",
                             # "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                             # "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                             # "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                             # "num_bales",
                             # "wagon_weight", "wagon_color",
                             "comments")

loss_cols <- c( "harvestingloss_id", "harvesting_id",
                "loss_area") # direct loss

supp_loss_cols <- c("harvestingloss_id",
                    # area is the main attibute
                       "loss_width", "loss_length", # ft
                       "loss_rows", "loss_location", "loss_reason",
                       "loss_comments")

# systematic loss, may decide to aggregate by loss category for 1 fraction
sysloss_cols <- c("systematicharvestingloss_id", "harvesting_id", "loss_fraction", "loss_category")

supp_sysloss_cols <- c("systematicharvestingloss_id",
                        "assessment_total_area", "assessment_loss_area",
                        # "assessment_date", # not used yet but possibly important
                        "assessment_loss_length","assesssment_loss_width", # ft
                        "assessment_loss_rows", "sysloss_location", "sysloss_reason",
                        "sysloss_comments")

# thus far loss is the same between main and ei
ei_loss_cols <- loss_cols
supp_ei_loss_cols <- supp_loss_cols
ei_sysloss_cols <- sysloss_cols
supp_ei_sysloss_cols <- supp_sysloss_cols


biomassing_cols <- c("biomassing_id","biomass_date","biomass_area", # ft^2
                     "plot", "coordinate","cut", "percent_moisture",
                     "biomass", "biomass_grams", "method", "component")

ei_biomassing_cols <- str_replace(biomassing_cols, "plot", "subplot")

supp_biomassing_cols <- c("biomassing_id", 
                          "biomass_length", "biomass_width", # ft
                          "rrl_id",
                          "bucket_lbs",
                          "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                          "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                          "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                          "comments")


supp_ei_biomassing_cols <- c("biomassing_id", "biomass_length", "biomass_width",
                          "rrl_id",
                          "bucket_lbs",
                          "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                          "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                          "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                          "comments")

canopeo_cols <- c("canopeo_id", "coverage_date",
                  "plot", "coordinate", 
                  "biomass", 
                  "percent_cover")

ei_canopeo_cols <- c("canopeo_id", "coverage_date",
                     "subplot", "coordinate", 
                     "biomass", # probably should be tracked... some design decision tbd because complicated mix of lots
                     "percent_cover")

# supp canopeo
supp_canopeo_cols <- c("canopeo_id", "comments")
supp_ei_canopeo_cols <- c("canopeo_id", "comments")
# there was some info about height of photos and such... maybe in supp

# priarie tables

fuel_harvesting_cols <- c("harvesting_id", "harvest_date", 
               "fuel_plot", "crop",
               "harvest_area", "percent_moisture",
               "harvest_lbs")

supp_fuel_harvesting_cols <- c("harvesting_id", 
                               "harvest_length", "harvest_width", # dimensions
                               "rrl_id",
                               "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                               "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                               "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                               "num_bales",
                               "wagon_weight", "wagon_color", "trailer_weight",
                               "comments")

fuel_115_harvesting_cols <- c("harvest_date", 
                              "plot", "section", 
                              "crop","percent_moisture",
                              "harvest_area", "harvest_lbs")

supp_fuel_115_harvesting_cols <- supp_fuel_harvesting_cols |> 
  discard(\(x) x %in% c("harvesting_id", "rrl_id"))

# Template Section --------------------------------------------------------

# # Assemble Tables ---------------------------------------------------------
# 
# ## Core --------------------------------------------------------------------
# 
# # harvests
# tbl_2021_harvests <- bind_rows()
# supp_2021_harvests <- bind_rows()
# 
# # biomassings
# tbl_2021_bio <- bind_rows()
# supp_2021_bio <- bind_rows()
# 
# # canopeo
# tbl_2021_can <- bind_rows()
# supp_2021_can <- bind_rows()
# 
# # losses
# tbl_2021_loss <- bind_rows()
# supp_2021_loss <- bind_rows()
# tbl_2021_sysloss <- bind_rows()
# supp_2021_sysloss <- bind_rows()
# 
# ## EI ----------------------------------------------------------------------
# 
# # harvests
# tbl_2021_ei_harvests <- bind_rows()
# supp_2021_ei_harvests <- bind_rows()
# 
# # biomassings
# tbl_2021_ei_bio <- bind_rows()
# supp_2021_ei_bio <- bind_rows()
# 
# # canopeo
# tbl_2021_ei_can <- bind_rows()
# supp_2021_ei_can <- bind_rows()
# 
# # losses
# tbl_2021_ei_loss <- bind_rows()
# supp_2021_ei_loss <- bind_rows()
# tbl_2021_ei_sysloss <- bind_rows()
# supp_2021_ei_sysloss <- bind_rows()
# 
# ## Silage ------------------------------------------------------------------
# 
# # harvests
# tbl_2021_silage <- bind_rows()
# supp_2021_silage <- bind_rows()
# 
# ## Biofuel -----------------------------------------------------------------
# 
# # harvests
# tbl_2021_prairie <- bind_rows()
# supp_2021_prairie <- bind_rows()


