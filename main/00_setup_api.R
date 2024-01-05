username = "ds1987"
password = "3iK26CP!mfH&-5C"
url = "https://kf.kobotoolbox.org/"

kobo_token(username = username,
           password = password,
           url = url)

# usethis::edit_r_environ(scope = "project")

kobo_library <- kobo_asset_list()

SCAPES_hh_id <- kobo_library %>%
  filter(name == "SCAPES - Household") %>%
  pull(uid)

SCAPES_hh_submissions <- kobo_asset(SCAPES_hh_id)

SCAPES_hh_dm <- kobo_data(SCAPES_hh_submissions)

dm_draw(SCAPES_hh_dm)

write_rds(SCAPES_hh_dm, file = here("data", "h_data", paste0("h_data_dm_", Sys.Date(), ".rds")))

SCAPES_i_id <- kobo_library %>%
  filter(name == "SCAPES - Individual") %>%
  pull(uid)

SCAPES_i_submissions <- get_subs(uid = SCAPES_i_id)

x = SCAPES_i_submissions
size = 100
paginate = FALSE
page_size = NULL
chunk_size = NULL
SCAPES_i_df <- kobo_data(SCAPES_i_submissions) %>%
  dm::dm_flatten_to_tbl(.start = main,
                        .join = left_join,
                        .recursive = TRUE)
