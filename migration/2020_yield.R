

# corn silage -------------------------------------------------------------

raw_2020_cs <- xl_snap$`2020_harvests_corn_silage` |> clean_names()

# raw_2021_cs |> names()
pre_2020_cs <- raw_2020_cs |>
  mutate(
    harvesting_id = get_harvest_id(year = 2020,
                                   plot = ml_plot,
                                   section = "main",
                                   product = "corn silage"),
    harvest_date = harvest_date,
    plot = ml_plot,
    crop = "corn silage",
    harvest_area = plot_area,
    harvest_length = plot_length_ft,
    harvest_width = plot_width_ft,
    harvest_lbs = plot_wt_lbs,
    # rrl_id = rrl_id,
    percent_moisture = moisture_rrl * 100,
    # loss_area = ml_harvest_loss_m2 * m2_to_ft2,
    comments = stitch_notes(notes, NA)
  )


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




