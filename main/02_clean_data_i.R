i_df_list <- read_rds(here("data", "i_data", "i_df_list.rds"))

# Remove those missing a participant ID
excluded <- i_df_list$individual_main %>%
  filter(is.na(participant_id))

i_df_list$individual_main <- i_df_list$individual_main %>%
  filter(!is.na(participant_id))

# Load cleaned hh questionnaire for comparisons
hh_df_list <- read_rds(here("data", "h_data", "hh_df_list_cleaned.rds"))


# Check unique numbering of individuals ------------------------------------

check_unique_id_i <- function(df =  i_df_list$individual_main) {
  
  individual_id <- paste(df$household_id, df$participant_id, sep = "-")
  
  if(nrow(df) == length(unique(individual_id))) {
    message(crayon::green("All individual IDs are unique"))
    df$participant_id <- individual_id
    message(crayon::green("Individual IDs (combined household and participant ID) have overwritten participant IDs"))
  } else {
    message(crayon::red("Individual IDs are not unique, needs correction prior to overwriting the participant ID. Duplicated individual IDs are returned."))
    
    duplicated_ids <- df %>%
      select(household_id, participant_id, `_index`) %>%
      mutate(individual_id = paste(household_id, participant_id, sep = "-")) %>%
      filter(individual_id %in% individual_id[duplicated(individual_id)])
    
    return(duplicated_ids)
    
  }
  
}

check_unique_id_i(df =  i_df_list$individual_main)


# Check individual questionnaire household IDs match hh questionnaire -----

check_match <- function(df = i_df_list$individual_main) {
  
  no_h_questionnaires <- vector()
  no_i_questionnaires <- vector()
  
  matched_hh <- tibble(in_i = as.character(df$household_id),
                       matched_hh = as.character(df$household_id) %in% as.character(hh_df_list$household_main$household_id))
  matched_i <-  tibble(in_hh = as.character(hh_df_list$household_main$household_id),
                       matched_i = as.character(hh_df_list$household_main$household_id) %in% as.character(df$household_id)) %>%
    left_join(df %>%
                group_by(household_id) %>%
                summarise(n = n()),
              by = c("in_hh" = "household_id"))
  
  if(all(matched_hh$matched_hh) == TRUE) {
    
    message(crayon::green("Check 1: PASS Individual questionnaires come from sampled households\n\n"),
            crayon::green("All completed individual questionnaires have completed household level questionnaires based on their household ID\n"))
    
  } else {
    
    message(crayon::red("Check 1: FAIL Individual questionnaires come from unsampled households\n\n"),
            crayon::red("There is a mismatch between completed individual questionnaires and household level questionnaires based on their household ID\n"),
            crayon::red(paste("Household questionnaires from", matched_hh %>% filter(matched_hh == FALSE) %>% distinct(in_i) %>% nrow(), "household IDs (in the individual data) are missing\n")),
            crayon::red(paste("This has resulted in", matched_hh %>% filter(matched_hh == FALSE) %>% nrow(), "individual questionnaires being unable to be linked to household level data\n")),
            crayon::red("The household IDs of those without household questionnaires are stored in `match_questionnaires$no_h_questionnaires`\n"))
  }
  
  no_h_questionnaires <- matched_hh %>%
    filter(matched_hh == FALSE) %>%
    distinct(in_i) %>%
    pull(in_i)
  
  if(all(matched_i$matched_i) == TRUE) {
    
    message(crayon::green("Check 2: PASS Households have at least one associated individual questionnaire\n\n"),
            crayon::green("The following table shows the number of individuals who have completed questionnaires within a household\nThe aim is for 3 individuals per household\n"),
            crayon::green(paste0(capture.output(data.frame(matched_i %>%
                                          rename("n_individuals" = n) %>%
                                          group_by(n_individuals) %>%
                                          summarise(n_questionnaires = n()))), collapse = "\n")))
    
  } else {
    
    message(crayon::red("Check 2: FAIL Some households have no associated individual questionnaires\n\n"),
            crayon::red(paste0("Households come from the following villages: ", combine_words(unique(str_split(matched_i$in_hh, "-", simplify = TRUE)[, 1]))), "\n"),
            crayon::red(paste0("Households with no individual questionnaires are shown below:\n")),
            crayon::red(paste0(capture.output(data.frame(matched_i %>%
                                                           filter(is.na(n)) %>%
                                                           mutate(village = str_sub(in_hh, end = 3)) %>%
                                                           group_by(village) %>%
                                                           summarise(n_households = n()))), collapse = "\n")),
            crayon::red("\nThe household IDs of those without individual questionnaires are stored in `match_questionnaires$no_i_questionnaires`"))
    
    no_i_questionnaires <- matched_i %>%
      filter(is.na(n)) %>%
      arrange(in_hh) %>%
      pull(in_hh)
    
  }
  
  return(list(no_h_questionnaires = no_h_questionnaires,
              no_i_questionnaires = no_i_questionnaires))
  
}

match_questionnaires <- check_match(df = i_df_list$individual_main)


# Checking completeness ---------------------------------------------------
# Missing age
missing_age <- i_df_list$individual_main %>%
  filter(is.na(age)) %>%
  pull(id)
# Missing names
missing_names <- i_df_list$individual_main %>%
  filter(is.na(first_name)|is.na(surname)) %>%
  pull(id)
# Missing sex
missing_sex <- i_df_list$individual_main %>%
  filter(is.na(sex)) %>%
  pull(id)

# Check DBS association ---------------------------------------------------

# Missing blood spot id
missing_dbs_id <- i_df_list$individual_main %>%
  filter(b_consent == "yes" & is.na(confirm_blood_spot)) %>%
  pull(id)
# Missing dbs image
missing_dbs_img <-i_df_list$individual_main %>%
  filter(b_consent == "yes" & is.na(blood_spot)) %>%
  pull(id)

# Check if DBS ID matches the expected ID for those consenting to DBS
dbs_id_check <- function(df = i_df_list$individual_main) {
  
  df <- df %>%
    filter(b_consent == "yes")
  
  participant_id <- df$id
  
  dbs_id <- df$confirm_blood_spot
  
  comparable <- (
    # Compare village section of the DBS ID
    str_to_lower(str_sub(participant_id, 1, 3)) == str_to_lower(str_sub(participant_id, 1, 3)) &
    # Compare household number section of the DBS ID
    str_extract(participant_id, "(?<=\\D|^)\\d{1,3}") == str_extract(dbs_id, "(?<=\\D|^)\\d{1,3}") &
    # Compare participant number section of the DBS ID after removing any additional letters
    str_extract(participant_id, "\\d{1,3}(?=$)") == str_extract(str_replace(dbs_id, "\\D*$", ""), "\\d{1,3}(?=$)")
  )
  
  dbs_mismatch <- participant_id[!comparable | is.na(comparable)]
  
  if(length(dbs_mismatch) < 1) {
    
    message(crayon::green("No mismatches between expected DBS names and participant IDs have been detected"))
    
  } else {
    
    message(crayon::red(paste0(length(dbs_mismatch), " mismatches between DBS names and participant IDs have been detected\n",
                               "These mismatches have been stored in `dbs_mismatch`")))
  }
  
  return(dbs_mismatch)
  
}

dbs_mismatch <- dbs_id_check(df = i_df_list$individual_main)

# Then to check these have been logged in the inventory document


# Check place of birth and residence -----------------------------------------
# Clean state names within place of birth
# Function to clean and standardize state names
# All ChatGPT function - can't take any credit
clean_state_names <- function(states, valid_states = nigeria_states) {
  # Function to find the closest match for a given string in a list
  find_closest_match <- function(str, valid_list) {
    distances <- sapply(valid_list, stringdist::stringdistmatrix, str)
    closest_index <- which.min(distances)
    return(valid_list[closest_index])
  }
  
  # Clean and standardize each state name
  cleaned_states <- sapply(states, function(state) {
    if (is.na(state)) {
      return(state)
    } else {
      # Extract the first word as the state name
      state_name <- strsplit(state, " ")[[1]][1]
      
      # Check if the state name is in the valid list
      if (state_name %in% valid_states) {
        # State name is valid, add "State" if missing
        return(paste(state_name, "State"))
      } else {
        # State name is not valid, find the closest match in the valid list
        closest_match <- find_closest_match(state_name, valid_states)
        # Exclude appending "State" if the closest match is "Federal Capital Territory"
        if (closest_match == "Federal Capital Territory") {
          return(state)
        } else {
          # Return the closest match with "State"
          return(paste(closest_match, "State"))
        }
      }
    }
  })
  
  return(cleaned_states)
}

pob <- i_df_list$individual_main %>%
  select(id, age, date, community_pob, country_of_birth, state_of_birth, region_of_birth, village_of_birth, year_arrived, resident_months, resident_elsewhere) %>%
  mutate(abbreviation = str_sub(id, end = 3)) %>%
  left_join(village_state %>%
              select(abbreviation, village, lga, state)) %>%
  mutate(state = clean_state_names(state)) %>%
  mutate(village_of_birth = case_when(community_pob == "yes" ~ village,
                                      community_pob == "no" ~ str_to_title(village_of_birth),
                                      is.na(community_pob) ~ as.character(NA)),
         region_of_birth = case_when(community_pob == "yes" ~ lga,
                                     community_pob == "no" ~ str_to_title(region_of_birth),
                                     is.na(community_pob) ~ as.character(NA)),
         state_of_birth =  case_when(community_pob == "yes" ~ clean_state_names(state),
                                     community_pob == "no" & !is.na(state_of_birth) ~ clean_state_names(str_to_title(state_of_birth)),
                                     is.na(community_pob) ~ as.character(NA)),
         country_of_birth = case_when(community_pob == "yes" ~ "Nigeria",
                                      community_pob == "no" ~ str_to_title(country_of_birth),
                                      is.na(community_pob) ~ as.character(NA)),
         time_since_arrival = case_when(is.na(community_pob) ~ as.numeric(NA),
                                        community_pob == "yes" ~ as.numeric(NA),
                                        community_pob == "no" & year_arrived >= 1900 & year_arrived <= year(Sys.Date()) ~ year(as.Date(date, "%Y_%m_%d")) - year_arrived,  # If year_arrived is a valid year, calculate time of residency
                                        community_pob == "no" & year_arrived <= 120 ~ year_arrived,  # If year_arrived is less or equal to 120, assume it's years since arrival
                                        community_pob == "no" & str_sub(year_arrived, end = 4) >= 1900 & str_sub(year_arrived, end = 4) <= year(Sys.Date()) ~  year(as.Date(date, "%Y_%m_%d")) - as.numeric(str_sub(year_arrived, end = 4)),
                                        TRUE ~ as.numeric(NA)),  # Handle any other cases with NA
         tsa_compatible = case_when(is.na(time_since_arrival)|is.na(age) ~ NA,
                                    time_since_arrival <= age ~ TRUE,
                                    time_since_arrival > age ~ FALSE,
                                    TRUE ~ FALSE),
         length_of_residence = case_when(is.na(time_since_arrival)|is.na(tsa_compatible) ~ as.numeric(age),
                                         tsa_compatible == TRUE ~ as.numeric(time_since_arrival),
                                         TRUE ~ as.numeric(NA)),
         # defining permanent resident as <= 6 months away from the village
         permanent_resident = case_when(resident_months >= 6 ~ TRUE,
                                        resident_months < 6 ~ FALSE,
                                        is.na(resident_months) ~ NA)) %>%
  select(id, age, community_pob,
         village_birth = village_of_birth, region_birth = region_of_birth, state_birth = state_of_birth, country_birth = country_of_birth,
         village_residence = village, region_residence = lga, state_residence = state, period_residence = length_of_residence,
         permanent_residence = permanent_resident, months_residence_in_year = resident_months, other_residence = resident_elsewhere)


# Check RCS associated ----------------------------------------------------
check_rcs <- function(df = i_df_list$individual_main) {
  
  rcs <- df %>%
    filter(is.na(age)|age >= 12) %>%
    select(id, rc_year_p1, rc_year_p2, rc_year_p3, rc_year_p4) %>%
    mutate(rcs_present = case_when(!is.na(rc_year_p1)|!is.na(rc_year_p2)|!is.na(rc_year_p3)|!is.na(rc_year_p4) ~ TRUE,
                                   TRUE ~ FALSE))
  
  missing_rcs <- rcs %>%
    filter(rcs_present == FALSE) %>%
    pull(id)
  
  if(sum(rcs$rcs_present) == nrow(rcs)) {
    
    message(crayon::green("All individuals with an age and over 12 years old have an associated Rodent Contact Sheet"))
    
  } else {
    
    message(crayon::red(paste0(nrow(rcs %>%
                                      filter(rcs_present == FALSE)),
                               " individuals are missing an associated Rodent Contact Sheet\n")),
            crayon::red("The IDs for these individuals are stored in `missing_rcs`"))
    
  }
  
  return(missing_rcs)
  
}

missing_rcs <- check_rcs(df = i_df_list$individual_main)


# Cleaning XXXX -----------------------------------------------------------



# Identify missing data ---------------------------------------------------

missing_data <- list(missing_h_questionnaire = match_questionnaires$no_h_questionnaires,
                     missing_i_questionnaire = match_questionnaires$no_i_questionnaires,
                     missing_age = missing_age,
                     missing_names = missing_names,
                     missing_sex = missing_sex,
                     missing_dbs_id = missing_dbs_id,
                     duplicated_dbs_id = dbs_mismatch,
                     missing_dbs_image = missing_dbs_img,
                     missing_rcs_image = missing_rcs) %>%
  enframe(name = "missing") %>%
  unnest(cols = value) %>%
  rename(id = value) %>%
  left_join(i_df_list$individual_main %>%
              select(id, interviewer_id, date) %>%
              mutate(interviewer_id = as_factor(interviewer_id))) %>%
  arrange(id, interviewer_id, date, missing)

write_csv(missing_data, here("data", "missing", paste0("missing_", Sys.Date(), ".csv")))

# Save clean dataframe ----------------------------------------------------

clean_i <- i_df_list$individual_main %>%
  left_join(pob %>%
              select(-any_of(c("community_pob", "age"))), by = "id") %>%
  select(`_index`, household_id, participant_id, id, id_confirmed, date, interviewer_id, q_consent, b_consent,
         age, sex, ethnicity, religion, education,
         village_residence, region_residence, state_residence, permanent_residence, months_residence_in_year, other_residence,
         community_pob, village_birth, region_birth, state_birth, country_birth, period_residence,
         non_resident_reason, return_reason, sleep_elsewhere, sleep_elsewhere_location,
         relationship, children_ever, n_children, children_alive, children_in_household,
         income, income_other, income_individual,
         field_entry, field_work_freq, field_work_seasonality,
         forest_entry, forest_entry_purpose, forest_entry_purpose_other, forest_entry_freq, forest_work_seasonality,
         any_of(starts_with("meat_")), any_of(starts_with("bushmeat_")), current_rodent_consumption, past_rodent_consumption,
         rodent_eat_reason, rodent_eat_other, rodent_consumption_hunger,
         any_of(starts_with("sell_")), any_of(starts_with("excreta_")), administered_health_cultural, prepared_health_cultural,
         any_of(contains("disease")), any_of(contains("lassa")),
         blood_spot, any_of(starts_with("rc"))) %>%
  mutate_all(~ if (is.labelled(.)) as_factor(.) else .) %>%
  zap_label()
         
clean_i_list <- list(individual_main = clean_i,
                     individual_hc_use = i_df_list$individual_hc_use,
                     individual_hc_prep = i_df_list$individual_hc_prep)

write_rds(clean_i_list, here("data", "i_data", "i_df_list_cleaned.rds"))

