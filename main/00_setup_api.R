kobo_token(username = "ds1987",
           password = askpass::askpass(),
           url = "https://kf.kobotoolbox.org/")

#usethis::edit_r_environ(scope = "project")

kobo_library <- kobo_asset_list()

SCAPES_hh_id <- kobo_library %>%
  filter(name == "SCAPES - Household") %>%
  pull(uid)

SCAPES_hh_submissions <- kobo_asset(SCAPES_hh_id)

SCAPES_hh_df <- kobo_data(SCAPES_hh_submissions) %>%
  dm::dm_flatten_to_tbl(.start = main,
                        .join = left_join,
                        .recursive = TRUE)

SCAPES_i_id <- kobo_library %>%
  filter(name == "SCAPES - Individual") %>%
  pull(uid)

SCAPES_i_submissions <- kobo_asset(SCAPES_i_id)

SCAPES_i_df <- kobo_data(SCAPES_i_submissions) %>%
  dm::dm_flatten_to_tbl(.start = main,
                        .join = left_join,
                        .recursive = TRUE)

# Manual
i_csv <- read_csv2(here("data", "i_data", "i_2023-12-12.csv"), col_names = TRUE)
