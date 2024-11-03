
# Pesticidings ------------------------------------------------------------

library(janitor)
library(dplyr)
load("data/agcal_20241011.Rdata")
raw_2023_pest <-  xl_agcal$`2023_pesticidings` |> clean_names()

raw_2023_pest |> filter(!is.na(category))

raw_2023_pest |> count(name)

raw_2023_pest |> 
  mutate(category = case_match(name,
                               c("Aztec 4.67 G", "Strelius II")~"insecticide",
                               c("Buccaneer 5 extra", "2,4-D LV6")~"herbicide",
                               c("Compadre")~"adjuvant"))
