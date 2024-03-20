
# Import questionnaire ----------------------------------------------------
if(!file.exists(here("household_questionnaire", "questionnaire", "hh_questionnaire_current.xlsx"))) {
  dir.create(here("household_questionnaire", "questionnaire"))
  drive_download("https://docs.google.com/spreadsheets/d/1YVVEsuOYtSrbmsW-Z30TvXHQWtTWgKre/edit?usp=sharing&ouid=109198876678825407497&rtpof=true&sd=true",
                 path = here("household_questionnaire", "questionnaire", "hh_questionnaire_current.xlsx"),
                 overwrite = TRUE)
}

h_questions <- read_xlsx(path = here("household_questionnaire", "questionnaire", "hh_questionnaire_current.xlsx"))

# Load in H df -----------------------------------------------------------

h_data_files <- list.files(here("household_questionnaire", "data", "h_data"))[str_detect(list.files(here("household_questionnaire", "data", "h_data")), "data_dm")] %>%
  sort(decreasing = TRUE)

h_dm <- read_rds(here("household_questionnaire", "data", "h_data", h_data_files[1]))

h_df_main <- h_dm$main %>%
  select(any_of(c("start", "date", "interviewer_id", "_index", "household_id", "multiple_family_household", "n_people", "compound", "n_in_other_households",
                  "other_household_activities", "household_ethnicity", "household_religion", "n_compound", "n_individual_buildings", "building_owned",
                  "n_single_room", "n_multi_room", "rodent_removal_home_method", "rodent_remove_use", "rodent_mitigation_method", "cats", "cats_inside",
                  "toilet", "livestock", "livestock_animals", "meal_location", "storage_cooked_containers", "storage_packaged_containers", "storage_seed_crop_containers_c",
                  "storage_seed_crop_containers_g", "food_resources", "food_resources_frequency", "food_sleep", "food_sleep_frequency", "food_day", "food_day_frequency",
                  "household_items", "latitude", "longitude", "surroundings_household", "surroundings_outside_other", "notes")))


# Demographics of household members ---------------------------------------

h_dm_household <- h_dm$household_members %>%
  select(`_parent_index`, `_index`, sex_person, age_person, age_baby, permanent_transient)

household_demo_rename <- tibble(original = names(h_dm_household),
                                new = c("_index", "hh_resident_id", "sex", "age", "age_child", "resident"))

h_df_household <- h_dm_household %>%
  rename(!!setNames(as.character(household_demo_rename$original), as.character(household_demo_rename$new))) %>%
  mutate(age = round(case_when(is.na(age) ~ age_child/12,
                               TRUE ~ age), 2)) %>%
  select(-age_child)


# Buildings ---------------------------------------------------------------

h_dm_single <- h_dm$single_room_repeat %>%
  relocate(c(`_parent_index`, `_index`), .before = 1) %>%
  select(`_parent_index`, `_index`, building_purpose_single, building_location_single, animals_in_structure, roof_single, walls_single, door_single, window_single,
         ceiling_single, storage_items_ceiling_single, floor_single, n_household_sleep_single, n_other_household_sleep_single, rodents_enter_building_single,
         rodents_in_building_single, rodents_in_building_evidence_single, rodent_sleeping_contact_single, mastomys_single, mastomys_timing_single,
         mastomys_season_single, sleep_in_kitchen_single, food_inside_single, cooked_inside_single, rodent_damage_kitchen_single, rodent_damage_kitchen_items_single,
         n_sleep_in_store_single, cooked_food_in_store_single, stored_inside_single, rodent_damage_store_items_single)

household_room_rename_single <- tibble(original = names(h_dm_single),
                                       new = c("_index", "single_room_id", "building_use", "building_location", "animal_structure", "roof", "wall", "door", "window",
                                               "ceiling", "ceiling_store", "floor", "n_sleep", "n_sleep_ext", "rodents_enter",
                                               "rodents_building", "rodents_building_evidence", "rodent_sleep_contact", "mastomys", "mastomys_timing",
                                               "mastomys_season",  "n_sleep_kitchen", "food_inside", "cooked_inside", "rodent_damage_kitchen", "rodent_damage_kitchen_item",
                                               "n_sleep_store", "cooked_in_store", "items_stored", "rodent_damage_store_item"))

h_df_single <- h_dm_single %>%
  rename(!!setNames(as.character(household_room_rename_single$original), as.character(household_room_rename_single$new)))

h_dm_multi <- h_dm$multi_room_repeat %>%
  select(`_parent_index`, `_index`, building_purpose, building_location, roof, walls, door, window, ceiling, storage_items_ceiling, floor, internal_door_fit,
         rooms_in_building, n_household_sleep, sleep_same_room, n_other_household_sleep_multi, rodents_enter_building, rodents_in_building, rodents_in_building_evidence,
         rodent_faeces_multi_sleep, rodent_sleeping_contact_multi, mastomys_multi, mastomys_timing_multi, mastomys_season_multi, designated_kitchen, n_sleep_in_kitchen,
         rodent_damage_kitchen, rodent_damage_kitchen_items, designated_store, cooked_food_in_store, n_sleep_in_store, rodent_damage_store, rodent_damage_store_items)

household_room_rename_multi <- tibble(original = names(h_dm_multi),
                                      new = c("_index", "multi_room_id", "building_use", "building_location", "roof", "wall", "door", "window", "ceiling", "ceiling_store",
                                              "floor", "internal_door_fit", "n_rooms", "n_sleep", "sleep_same_room", "n_sleep_ext", "rodents_enter", "rodents_building",
                                              "rodents_building_evidence", "rodent_faeces", "rodent_sleep_contact", "mastomys", "mastomys_timing", "mastomys_season", 
                                              "kitchen", "n_sleep_kitchen", "rodent_damage_kitchen", "rodent_damage_kitchen_item", "designated_store", "cooked_in_store",
                                              "n_sleep_store","rodent_damage_store", "rodent_damage_store_item"))
h_df_multi <- h_dm_multi %>%
  rename(!!setNames(as.character(household_room_rename_multi$original), as.character(household_room_rename_multi$new)))

h_df_building <- bind_rows(h_df_single, h_df_multi) %>%
  relocate(c(`_index`, single_room_id, multi_room_id), .before = 1)


# Fields ------------------------------------------------------------------

h_dm_field <- h_dm$field_repeat %>%
  relocate(c(`_parent_index`, `_index`), .before = 1) %>%
  select(`_parent_index`, `_index`, field_ownership, field_years, field_shared, field_type, field_crop, rat_field_evidence, mastomys_field, mastomys_timing_field,
         mastomys_season_field, predation, pesticide_use, rodent_removal_field, rodent_remove_use_field,
         rodent_mitigation_field, field_location, field_position)

household_field_rename <- tibble(original = names(h_dm_field),
                                 new = c("_index", "field_id", "owned", "years_rented", "external_workers", "field_type", "field_crop", "rodents_field_evidence",
                                         "mastomys", "mastomys_timing", "mastomys_season", "predation", "pesticide", "rodent_removal", "rodent_removal_use",
                                         "rodent_mitigation", "field_location", "field_position"))

h_df_field <- h_dm_field %>%
  rename(!!setNames(as.character(household_field_rename$original), as.character(household_field_rename$new)))                                 


# Cultural health practices -----------------------------------------------

h_dm_culture <- h_dm$rats_cultural %>%
  relocate(c(`_parent_index`, `_index`), .before = 1) %>%
  select(`_parent_index`, `_index`, rodent_culture_health_name, rodent_name_detail, part_of_rodent, purpose_of_rodent, prepare_rodent)

household_cultural_rename <- tibble(original = names(h_dm_culture),
                                    new = c("_index", "culture_id", "name", "rodent", "part", "purpose", "preparation"))

h_df_culture <- h_dm_culture %>%
  rename(!!setNames(as.character(household_cultural_rename$original), as.character(household_cultural_rename$new)))   

# Images ------------------------------------------------------------------

h_dm_images_outside <- h_dm$outside_photos %>%
  select(`_parent_index`, `_index`, photo_outside, photo_outside_name)

h_dm_images_inside <- h_dm$inside_photos %>%
  select(`_parent_index`, `_index`, photo_inside, photo_inside_name)

photo_rename <- tibble(original = c(names(h_dm_images_inside), names(h_dm_images_outside)),
                       new = c(c("_index", "image_id", "image_filename", "image_description"),
                               c("_index", "image_id", "image_filename", "image_description")))

h_df_images <- bind_rows(h_dm_images_outside %>%
                          rename_with(~ photo_rename$new[match(., photo_rename$original)], everything()) %>%
                          mutate(image_id = paste0(image_id, "_out"),
                                 setting = "Outside"),
                        h_dm_images_inside %>%
                          rename_with(~ photo_rename$new[match(., photo_rename$original)], everything()) %>%
                          mutate(image_id = paste0(image_id, "_in"),
                                 setting = "Inside"))

rename_hh_images <- tibble(cur_flocation = list.files(here("household_questionnaire", "data", "h_data", "media"), full.names = TRUE),
                           image_filename = str_split(list.files(here("household_questionnaire", "data", "h_data", "media")), "_", simplify = TRUE)[, 2]) %>%
  full_join(h_df_images %>%
              left_join(h_df_main %>%
                          select(`_index`, household_id), by = "_index") %>%
              select(household_id, image_id, image_filename), by = "image_filename") %>%
  mutate(new_filename = paste0(here("household_questionnaire", "data", "h_data", "media"), "/", household_id, "_", image_id, ".jpg")) %>%
  select(cur_flocation, image_filename, new_filename)

file.rename(rename_hh_images$cur_flocation, rename_hh_images$new_filename)

# Combine df into a list --------------------------------------------------

hh_df <- list(household_main = h_df_main,
              household_demographics = h_df_household,
              household_buildings = h_df_building,
              household_fields = h_df_field,
              household_culture = h_df_culture,
              household_images = h_df_images)

write_rds(hh_df, here("household_questionnaire", "data", "h_data", "hh_df_list.rds"))
