# limings

load("data/agcal_20241010.Rdata")


pre_2023_lime <- xl_agcal$`2023_fertilizings` |> filter(fertilizer_type == "lime")
pre_2023_lime |> mutate(grading = str_extract(fertilizer, "\\((.*)\\)", 1)) |>
  select(date, plot, grading, rate, rate_unit) |> 
  clipr::write_clip(sep = ", ")

raw_2023_fert <- xl_agcal$`2023_fertilizings` |> clean_names()
raw_2023_fert |> filter(fertilizer_type != "lime") |> 
  separate_wider_delim(fertilizer, names = c("N", "P2O5", "K2O", "S", "Ca"), "-", ) |> 
  relocate(fertilizer_type, .before = "N") |> 
  add_column(Mg = 0,.after="Ca") |> 
  mutate(across(c(N:Mg), ~case_match(fertilizer_type,
                             "cpm"~"-",
                             .default = as.character(.x),
                        .ptype = ""))) |> 
  mutate(date = glue("{month(date)}/{day(date)}/{year(date)}")) |> 
  clipr::write_clip(sep = ", ") 





?separate_wider_delim
