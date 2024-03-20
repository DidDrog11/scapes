
# Import questionnaire ----------------------------------------------------
if(!file.exists(here("household_questionnaire", "questionnaire", "i_questionnaire_current.xlsx"))) {
  dir.create(here("household_questionnaire", "questionnaire"))
  drive_download("https://docs.google.com/spreadsheets/d/12YWDQxcQwth4-c7-gDpAEoLgUVx25yfm/edit?usp=drive_link&ouid=109198876678825407497&rtpof=true&sd=true",
                 path = here("household_questionnaire", "questionnaire", "i_questionnaire_current.xlsx"),
                 overwrite = TRUE)
}

i_questions <- read_xlsx(path = here("household_questionnaire", "questionnaire", "i_questionnaire_current.xlsx"))

# Load in I df -----------------------------------------------------------

i_data_files <- list.files(here("household_questionnaire", "data", "i_data"))[str_detect(list.files(here("household_questionnaire", "data", "i_data")), "data_dm")] %>%
  sort(decreasing = TRUE)

i_dm <- read_rds(here("household_questionnaire", "data", "i_data", i_data_files[1]))

i_df_main <- i_dm$main %>%
  select(date, interviewer_id, `_index`, household_id, participant_id, participant_id_under_12, id_confirmation, participant_id_confirm_child, id_correction, participant_id_correction_chil,
         over_18, age_individual, age_under_12, age_in_months, yob_individual, first_name, child_first_name, surname, child_surname,
         informed_consent_adult, blood_sample_consent_adult, assent_under_18, blood_sample_assent_child, blood_spot, confirm_blood_spot,
         sex_individual, sex_under_12, ethnicity_individual, ethnicity_individual_other, religion_individual, religion_individual_other,
         education, education_other,
         community_pob, country_of_birth, state_of_birth, region_of_birth, village_of_birth, year_arrived,
         resident_months, resident_elsewhere, non_resident_reason, return_reason, sleep_elsewhere, sleep_elsewhere_location,
         relationship, children_ever, n_children, children_alive, children_in_household,
         income, income_other, # need to check this variable, shows different online
         income_individual,
         field_entry, field_work_freq, field_work_seasonality,
         forest_entry, forest_entry_purpose, forest_entry_purpose_other, # need to check this variable, shows different online
         forest_entry_freq, forest_work_seasonality,
         meat_consumption, meat_consumption_type, meat_consumption_other, meat_consumption_freq, bushmeat_consumption, bushmeat_animals, bushmeat_other, bushmeat_freq,
         meat_avoided, meat_avoided_animals, current_rodent_consumption, past_rodent_consumption, rodent_eat_reason,
         rodent_eat_other, rodent_consumption_hunger, sell_rodent, sell_rodent_location, sell_rodent_cities, sell_rodent_other, sell_rodent_importance, sell_rodent_condition, sell_mastomys, sell_mastomys_condition,
         administered_health_cultural, prepared_health_cultural,
         rc_year_p1, rc_year_p2, rc_year_p3, rc_year_p4,
         excreta_year, excreta_location, excreta_location_other, excreta_cleaning, excreta_cleaning_method, excreta_cleaning_method_other,
         rodent_diseases, rodent_diseases_name, rodent_diseases_other, 
         lassa_disease_aware, lassa_source, lassa_source_other, human_lassa_infection, human_lassa_infection_other,
         rodent_contact_lassa, rodent_contact_lassa_other, rodent_species_lassa, rodent_species_lassa_explicit, 
         lassa_community, lassa_diagnosis, lassa_diagnosis_year, lassa_diagnosis_cause, lassa_diagnosis_cause, lassa_diagnosis_rodent
  ) %>% #this includes variables that can be merged, as below
  mutate(
    participant_id = case_when(id_confirmation == "yes" | participant_id_confirm_child == "yes" ~ coalesce(participant_id, participant_id_under_12),
                               TRUE ~ coalesce(id_correction, participant_id_correction_chil, participant_id, participant_id_under_12)), # Coalesce the id's of children and adults into one
    id_confirmed = coalesce(id_confirmation, participant_id_confirm_child),
    age = round(case_when(is.na(age_in_months) ~ coalesce(age_individual, age_under_12),
                    TRUE ~ age_in_months/12), 2),
    sex = coalesce(sex_individual, sex_under_12),
    first_name = coalesce(first_name, child_first_name),
    surname = coalesce(surname, child_surname),
    q_consent = coalesce(informed_consent_adult, assent_under_18),
    b_consent = coalesce(blood_sample_consent_adult, blood_sample_assent_child),
    ethnicity = coalesce(ethnicity_individual, ethnicity_individual_other),
    religion = coalesce(religion_individual, religion_individual_other),
    education = coalesce(education, education_other)
  ) %>%
  select(-any_of(c("participant_id_under_12", "id_correction", "participant_id_correction_chil",
                   "id_confirmation", "participant_id_confirm_child",
                   "age_in_months", "age_individual", "age_under_12",
                   "sex_individual", "sex_under_12",
                   "child_first_name",
                   "child_surname",
                   "informed_consent_adult", "assent_under_18",
                   "blood_sample_assent_child", "blood_sample_consent_adult",
                   "ethnicity_individual", "ethnicity_individual_other",
                   "religion_individual", "religion_individual_other",
                   "education_other"))) %>%
  mutate(id = paste(household_id, participant_id, sep = "-")) %>%
  relocate(id, .after = participant_id)


# Health and cultural uses of rodents ------------------------------------

i_dm_hc_use <- i_dm$rep_health_cultural %>%
  select(`_parent_index`, `_index`, name_health_cultural, rodent_name_detail, part_of_rodent, part_of_rodent_other, purpose_of_rodent, prepare_rodent, prepare_rodent_other, administering_rodent, administering_rodent_other)

i_dm_hc_use_rename <- tibble(original = names(i_dm_hc_use),
                                new = c("_index", "hc_use_id", "practice_name", "rodent_name", "rodent_part", "rodent_part_other", "purpose", "prepare", "prepare_other", "administer", "administer_other"))

i_df_hc_use <- i_dm_hc_use %>%
  rename(!!setNames(as.character(i_dm_hc_use_rename$original), as.character(i_dm_hc_use_rename$new)))


# Health and cultural uses of rodents preparation -----------------

i_dm_hc_prep <- i_dm$rep_health_cultural_prep %>%
  select(`_parent_index`, `_index`, name_health_cultural_prep, rodent_name_detail_prep, part_of_rodent_prep, part_of_rodent_other_prep, purpose_of_rodent_prep, prepare_rodent_prep, prepare_rodent_other_prep, administering_rodent_prep, administering_rodent_prep_other)

i_dm_hc_prep_rename <- tibble(original = names(i_dm_hc_prep),
                              new = c("_index", "hc_use_id", "practice_name", "rodent_name", "rodent_part", "rodent_part_other", "purpose", "prepare", "prepare_other", "administer", "administer_other"))

i_df_hc_prep <- i_dm_hc_prep %>%
  rename(!!setNames(as.character(i_dm_hc_prep_rename$original), as.character(i_dm_hc_prep_rename$new)))

# Rename images -----------------------------------------------------------
i_images <- i_df_main %>%
  select(any_of(c("_index", "id",
                  "rc_year_p1", "rc_year_p2", "rc_year_p3", "rc_year_p4",
                  "consent_p1", "blood_spot"))) %>%
  pivot_longer(cols = matches("rc|consent|blood"), values_to = "image_filename", names_to = "image_id") %>%
  drop_na(image_filename)

rename_i_images <- tibble(cur_flocation = list.files(here("household_questionnaire", "data", "i_data", "media"), full.names = TRUE),
                          image_filename = str_split(list.files(here("household_questionnaire", "data", "i_data", "media")), "_", simplify = TRUE)[, 2]) %>%
  full_join(i_images %>%
              select(id, image_id, image_filename), by = "image_filename") %>%
  mutate(new_filename = paste0(here("household_questionnaire", "data", "i_data", "media"), "/", id, "_", image_id, ".jpg")) %>%
  select(cur_flocation, image_filename, new_filename)

file.rename(rename_i_images$cur_flocation, rename_i_images$new_filename)


# Combine df into a list --------------------------------------------------

i_df <- list(individual_main = i_df_main,
             individual_hc_use = i_df_hc_use,
             individual_hc_prep = i_df_hc_prep)

write_rds(i_df, here("household_questionnaire", "data", "i_data", "i_df_list.rds"))
