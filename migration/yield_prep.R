library(stringr)
library(dplyr)
library(tibble)
library(glue)
library(janitor)
library(purrr)
library(tidyr)
library(rlang)

load("data/wip_20241102.Rdata")
load("data/master_20240926.Rdata")

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
kg_to_lbs = 2.2046226
acre_to_ft2 = 43560
m2_to_ft2 = 10.7639

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
                             c("Main", "main") ~ "MM",
                             .default = "XX")
  
  product_code <- case_match(product,
                             c("corn")~"cn",
                             c("corn silage")~"cs",
                             c("wheat")~"wg",
                             c("soybean")~"sb",
                             c("wheat straw")~"ws",
                             c("wheat grain")~"wg",
                             c("barley")~"by",
                             c("alfalfa", "dsA", "A1", "A2", "o/A", "ORG A1")~"af",
                             c("red_clover")~"rc",
                             c("berseem_clover")~"bc",
                             c("oats")~"ot",
                             c("pasture")~"pt",
                             .default = "xx")
  
  return(glue("H{year}_{site}{plot}{section}{coordinate}_{product}_{cut}",
              plot = replace_na(as.character(plot), "XXX"),
              section = section_code,
              coordinate = "X", # coordinate is not applicable in harvests
              product = product_code))
}

get_biomassing_id <- function(year, plot, section, coordinate, biomass, cut = 1, site = "A") {
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
                                c("South", "S", "SOUTH")~"S",
                                c("Central", "Center", "C", "CENTRAL")~"C",
                                c("North", "N", "NORTH")~"N",
                                c("Paddock 1", "paddock 1", "1")~"1",
                                c("Paddock 2", "paddock 2", "2")~"2",
                                c("Paddock 3", "paddock 3", "3")~"3",
                                c("Paddock 4", "paddock 4", "4")~"4",
                                c("Paddock 5", "paddock 5", "5")~"5",
                                c("Paddock 6", "paddock 6", "6")~"6",
                                c("Paddock 7", "paddock 7", "7")~"7",
                                .default = "X")
  biomass_code <- case_match(biomass,
                             c("rye")~"ry",
                             c("weeds")~"wd",
                             c("residue")~"rs",
                             c("alfalfa")~"al",
                             c("red_clover")~"rc",
                             c("berseem_clover")~"bc",
                             c("oats")~"ot",
                             c("pasture")~"pt",
                             .default = "xx")
  
  return(glue("B{year}_{site}{plot}{section}{coordinate}_{biomass}_{cut}",
              section = section_code,
              coordinate = coordinate_code,
              biomass = biomass_code))
}

stitch_notes <- function(notes, ml_notes) {
  ifelse(is.na(notes),
         NA, 
         glue('General: "{notes}"{other_notes}',
              other_notes = ifelse(is.na(ml_notes),
                                   "",
                                   paste0(" | ", ml_notes))))
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
                                barleys~b_ideal_percent_moisture),
    bushel_lbs = case_match(product %||% str_to_lower(crop),
                            corn_grains~c_bushel,
                            wheat_grains~wg_bushel,
                            wheat_straws~ws_bushel,
                            soybeans~sb_bushel,
                            barleys~b_bushel),
    corrected_lbs = harvest_lbs * ((100 - percent_moisture) / (100 - ideal_moisture)),
    corrected_bu = corrected_lbs / bushel_lbs,
    corrected_lbs_per_acre = corrected_lbs / acre_frac,
    corrected_tons_per_acre = corrected_lbs_per_acre / 2000,
    corrected_bu_per_acre = corrected_bu / acre_frac)
}


harvesting_cols <- c("harvesting_id", "harvest_date", 
                     "plot", "crop",
                     "harvest_area", "percent_moisture",
                     "harvest_lbs")

ei_harvesting_cols <- c("harvesting_id", "harvest_date", 
                        "subplot", "crop",
                        "harvest_area", "percent_moisture",
                        "harvest_lbs")

supp_harvesting_cols <- c("harvesting_id", 
                          "harvest_length", "harvest_width", # dimensions
                          "rrl_id",
                          "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                          "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                          "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                          "num_bales",
                          "wagon_weight", "wagon_color",
                          "comments")

supp_ei_harvesting_cols <- c("harvesting_id", 
                          "harvest_length", "harvest_width", # dimensions
                          "rrl_id",
                          "bag_weight", "wet_bag_weight", "dry_bag_weight", # weights of bag themselves
                          "wet_weight_w_bag", "dry_weight_w_bag", # grab sample with the bag
                          "wet_weight_no_bag", "dry_weight_no_bag", # grab sample without bag
                          "num_bales",
                          "wagon_weight", "wagon_color",
                          "comments")

loss_harvesting_cols <- c("harvesting_id", "plot", "section_description",
                          "loss_width", "loss_length", "loss_area", 
                          "loss_reason")

loss_details_cols <- c("harvesting_")

biomassing_cols <- c("biomassing_id","biomass_date","biomass_area",
                     "plot", "coordinate","cut", 
                     "biomass", "biomass_grams")

supp_biomassing_cols <- c("biomassing_id", "biomass_length", "biomass_width",
                          "bag_weight", "comments")

