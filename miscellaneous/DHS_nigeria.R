#dhs_household <- read_sav(here("data", "additional", "NGHR7BFL.SAV"))
#dhs_household_member <- read_sav(here("data", "additional", "NGPR7BFL.SAV"))
dhs_individual <- read_sav(here("miscellaneous", "data", "NGIR7BFL.SAV"))
dhs_mens <- read_sav(here("miscellaneous", "data", "NGMR7AFL.SAV"))

#variable_names <- tibble(id = names(dhs_household),
#                         name = labelled::get_variable_labels(dhs_household, unlist = TRUE))

#variable_names_2 <- tibble(id = names(dhs_household_member),
#                         name = labelled::get_variable_labels(dhs_household_member, unlist = TRUE))

variable_names_3 <- tibble(id = names(dhs_individual),
                           name = labelled::get_variable_labels(dhs_individual, unlist = TRUE))

variable_names_4 <- tibble(id = names(dhs_mens),
                           name = labelled::get_variable_labels(dhs_mens, unlist = TRUE))

female_var_ids <- variable_names_3 %>%
  filter(str_detect(name, 
                    "State|Respondent's current age|Highest educational level"))

male_var_ids <- variable_names_4 %>%
  filter(str_detect(name,
                    "State|Current age|Educational level"))

nigeria_dhs <- bind_rows(
  dhs_individual %>%
    select(female_var_ids$id, V705, V025) %>%
    mutate(sex = factor("Female", levels = c("Female", "Male")),
           age = as.integer(V012),
           state = as_factor(SSTATE),
           education = as_factor(V106),
           occupation = as_factor(V705),
           setting = as_factor(V025)),
  dhs_mens %>%
    select(male_var_ids$id, MV717, MV025) %>%
    mutate(sex = factor("Male", levels = c("Female", "Male")),
           age = as.integer(MV012),
           state = as_factor(SMSTATE),
           education = as_factor(MV106),
           occupation = as_factor(MV717),
           setting = as_factor(MV025))) %>%
  mutate(id = row_number(),
         sampled_pop = case_when(str_detect(state, "Benue|Cross River|Ebonyi") ~ factor("Yes", levels = c("Yes", "No")),
                                 TRUE ~ factor("No", levels = c("Yes", "No")))) %>%
  select(id, sex, age, state, education, occupation, setting, sampled_pop)

dhs_occupation_match <- tibble(occupation = unique(nigeria_dhs$occupation)) %>%
  mutate(isco.manual = c("Street and Related Sales and Service Workers",
                         NA,
                         "Professionals",
                         "Agriculture",
                         "Craft and Related Trades Workers",
                         "Elementary Occupations",
                         "Business and Administration Associate Professionals",
                         "Not working",
                         "Business and Administration Associate Professionals",
                         NA,
                         NA,
                         "Not working",
                         "Agriculture"))

nigeria_dhs <- nigeria_dhs %>%
  left_join(dhs_occupation_match %>%
              select(occupation, isco.manual), 
            by = "occupation")

write_rds(nigeria_dhs, here("data", "additional", "nigeria_dhs.rds"))         
