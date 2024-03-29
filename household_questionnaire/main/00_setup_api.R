#username = "ds1987"
#url = "https://kf.kobotoolbox.org/"

# kobo_token(username = username,
#           password = password,
#           url = url)

# usethis::edit_r_environ(scope = "project")

kobo_library <- kobo_asset_list()

# Household ---------------------------------------------------------------

SCAPES_hh_id <- kobo_library %>%
  filter(name == "SCAPES - Household") %>%
  pull(uid)

SCAPES_hh_submissions <- kobo_asset(SCAPES_hh_id)

SCAPES_hh_dm <- kobo_data(SCAPES_hh_submissions)

# downloads the media from the Kobotoolbox server for this questionnaire
SCAPES_media(questionnaire = SCAPES_hh_submissions, media_folder = here("household_questionnaire", "data", "h_data", "media"), force_overwrite = FALSE)

dm_draw(SCAPES_hh_dm)

write_rds(SCAPES_hh_dm, file = here("household_questionnaire", "data", "h_data", paste0("h_data_dm_", Sys.Date(), ".rds")))

# Individual --------------------------------------------------------------

SCAPES_i_id <- kobo_library %>%
  filter(name == "SCAPES - Individual") %>%
  pull(uid)

SCAPES_i_submissions <- kobo_asset(SCAPES_i_id)

SCAPES_i_dm <- kobo_data(SCAPES_i_submissions)

SCAPES_media(questionnaire = SCAPES_i_submissions, media_folder = here("household_questionnaire", "data", "i_data", "media"), force_overwrite = FALSE)

dm_draw(SCAPES_i_dm)

write_rds(SCAPES_i_dm, file = here("household_questionnaire", "data", "i_data", paste0("i_data_dm_", Sys.Date(), ".rds")))

