# Authenticate to OneDrive
# If you are running from the .Rmd file you may need to do this first
one_drive <- get_business_onedrive()

# This is the shared DATA file on Sagan's OneDrive contact her for access
# This function looks at the items shared with you and retains the DATA folder
od_shared_data <- one_drive$list_shared_items() %>%
  keep(~ .x$properties$name == "DATA")
# Make it the only level of the environment instead of a list
od_data <- od_shared_data[[1]]

# Read in the files that we will save
hh_df_clean <- read_rds(here("household_questionnaire", "data", "h_data", "hh_df_list_cleaned.rds"))
i_df_clean <- read_rds(here("household_questionnaire", "data", "i_data", "i_df_list_cleaned.rds"))

od_data$save_rds(hh_df_clean, "Human Subjects Data/hh_questionnaire/hh_df_clean.rds")
od_data$save_rds(i_df_clean, "Human Subjects Data/i_questionnaire/i_df_clean.rds")

# Upload media
hh_media <- here("household_questionnaire", "data", "h_data", "media")
zip(here("household_questionnaire", "data", "h_data", "media.zip"), files = hh_media, include_directories = FALSE, mode = "cherry-pick")
od_data$upload(here("household_questionnaire", "data", "h_data", "media.zip"), dest = "Human Subjects Data/hh_questionnaire/media.zip")

i_media <-  here("household_questionnaire", "data", "i_data", "media")
zip(here("household_questionnaire", "data", "i_data", "media.zip"), files = i_media, include_directories = FALSE, mode = "cherry-pick")
od_data$upload(here("household_questionnaire", "data", "i_data", "media.zip"), dest = "Human Subjects Data/i_questionnaire/media.zip")
