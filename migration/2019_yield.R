raw_2019_cs <- xl_snap$`2019_harvests_corn_silage` |> clean_names()

# raw_2021_cs |> names()
pre_2019_cs <- raw_2019_cs |>
  filter(subplot == "Main") |>
  mutate(
    harvesting_id = get_harvest_id(year = 2019,
                                   plot = plot,
                                   section = "main",
                                   product = "corn silage"),
    harvest_date = harvest_date,
    plot = plot,
    crop = "corn silage",
    harvest_area = plot_area_ft,
    harvest_length = length_ft,
    harvest_width = width_ft,
    harvest_lbs = wet_wt_lbs,
    # rrl_id = rrl_id,
    percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, NA)
  )

raw_2019_cs |> 
  filter(subplot == "Main") |>
  mutate(dd = yield_tons_dm_acre / 1.22627737) |> select(plot, yield_tons_dm_acre, dd)


tbl_2020_cs <-  pre_2020_cs |> 
  select(any_of(harvesting_cols))

# dupe
dupe_2020_cs <- pre_2020_cs |>
  mutate(lbs_dm = harvest_lbs * (100 - percent_moisture) / 100,
         acre_frac = harvest_area / acre_to_ft2,
         lbs_dm_per_acre = lbs_dm / acre_frac,
         tons_dm_per_acre = lbs_dm_per_acre / 2000,
         my_acre_frac = harvest_area / 415 * 510 / acre_to_ft2,
         my_lbs_dm_per_acre = lbs_dm / my_acre_frac,
         my_tons_dm_per_acre = my_lbs_dm_per_acre / 2000)
dupe_2020_cs |> View()