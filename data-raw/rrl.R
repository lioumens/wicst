library(stringr)

# pastures from 2017 ------------------------------------------------------

raw_rrl_2017_past <- read.csv("~/OneDrive - UW-Madison/Database development/RRL/2017_WICST_FORAGE_RRL_FULL.csv") |>
  clean_names()

# rrl_2017_pasture |> 
#   mutate(
#     plot = as.numeric(str_split_i(sample_description, pattern = "\\s", i = 2)),
#     date_sampled = lubridate::mdy(date_sampled, tz = "UTC")
#   ) |> select(plot, moisture, date_sampled) |> 
#   count(plot)


pre_rrl_2017_past <- raw_rrl_2017_past |> separate_wider_regex(cols = sample_description,
                                         patterns = c("Plot ",
                                                      plot = "[0-9]*",
                                                      ".*? ", sample_date = "[0-9/]*\\s*$"),
                                         cols_remove = FALSE) |> 
  mutate(
    plot = as.numeric(plot),
    sample_date = lubridate::mdy(
      str_replace(str_trim(sample_date), "/2016", "/2017"),
      tz = "UTC")
  ) |> arrange(plot, sample_date) |> 
  # assume cuts are by row number
  mutate(cut = row_number(), .by = c(plot))

tbl_rrl_2017_past <- pre_rrl_2017_past |> 
  select(sample_date, plot, cut, moisture, dry_matter)

raw_rrl_2019 <- read.csv("~/OneDrive - UW-Madison/Database development/RRL/RockRiverLabExport_all_2019_crop_nutrients.csv") |> 
  clean_names()

raw_rrl_2019 |> filter(plot %in% c(102, 212, 313, 407)) |> View()




