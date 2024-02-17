library(tidyverse)
library(lubridate)

# Function to estimate the time required to survey households
# and create a visualization of the survey schedule.
#
# study_start: The start date and time of the study (in a specific time zone).
# winter_break: A vector containing the start and end dates of the winter break (in a specific time zone).
# arrange_hh: The time required for arranging households (in minutes).
# working_day: The duration of a working day (in hours).
# RAs: The number of research assistants.
# hh_n: The number of households.
# i_target: The target number of interviews.
# vil_n: The number of villages.
# tt_be: Time required for travel between villages in state Benue (in minutes).
# tt_eb: Time required for travel between villages in state Ebonyi (in minutes).
# tt_cr: Time required for travel between villages in state Cross River (in minutes).
# start_day: The start time of a working day (in HH:MM:SS format).
# end_day: The end time of a working day (in HH:MM:SS format).
# rest_days: A character vector specifying which days are considered rest days.
#
# The function returns a list containing two elements:
#   - A tibble with details of the survey schedule for all villages.
#   - A ggplot2 visualization of the survey progress over time.

time_estimate <- function(study_start = as_datetime("2023-12-08 07:00:00", tz = "Africa/Lagos"),
                          winter_break = c(as_datetime("2023-12-17", tz = "Africa/Lagos"), as_datetime("2024-01-06", tz = "Africa/Lagos")),
                          arrange_hh = minutes(30),
                          consent = minutes(20),
                          dbs = minutes(30),
                          hh_questionnaire = minutes(180),
                          i_questionnaire = minutes(60),
                          rc_sheet = minutes(20),
                          u_12_questionnaire = minutes(10),
                          working_day = hours(6),
                          RAs = 4,
                          hh_n = 60,
                          i_target = 1500,
                          vil_n = 9,
                          tt_be = minutes(15),
                          tt_eb = minutes(15),
                          tt_cr = minutes(15),
                          start_day = hours(8),
                          end_day = hms("19:00:00"),
                          rest_days = c("Sunday", "Monday")) {
  
  # Import necessary libraries
  library(tidyverse)
  library(lubridate)
  
  time_each_hh <- arrange_hh + consent + dbs + hh_questionnaire + i_questionnaire + rc_sheet + u_12_questionnaire
  
  time_each_village <- time_each_hh * hh_n
  
  t <- tibble()
  end_int <- tibble(hh = 0,
                    end_prev = NA,
                    start_cur = NA,
                    end_cur = NA)
  
  sample_houses <- function(village_name = "village_1", state = "ebonyi", t0) {
    
    for(i in 1:hh_n) {
      
      # When do we want to interview this household
      current_time <- if(i == 1) t0 else max(end_int$end_cur, na.rm = TRUE)
      # Is it possible to complete the interview the same day
      same_day <- if_else(current_time + time_each_hh < as_datetime(as.Date(current_time), tz = "Africa/Lagos") + end_day + minutes(30), TRUE, FALSE)
      
      int_start_time <- NA  # Initialize int_start_time
      
      if(same_day == TRUE) {
        int_start_time <- current_time  # Assign value in case of same day interview
      } else {
        
        int_start_time <- if(as_date(current_time) == as_date(winter_break)[1] - days(1)) as_datetime(as_date(winter_break[2]) + start_day + tt_be, tz = "Africa/Lagos") else
          if(weekdays(current_time) == "Saturday") as_datetime(as_date(current_time) + days(3), tz = "Africa/Lagos") + start_day + tt_be else #skip 3 days to Tuesday if Saturday
            as_datetime(as_date(current_time) + days(1), tz = "Africa/Lagos") + start_day + tt_be #interviews resume the next day
        
      }
      
      int_end_time <- int_start_time + time_each_hh
      
      # How many interviews are occurring concurrently
      n_simul <- if(as_date(current_time) >= as_date(study_start) + full_capacity) RAs else 2
      
      end_int <- bind_rows(end_int,
                           tibble(
                             "hh" = if(i == 1) seq(i, i + (n_simul - 1), length.out = n_simul) else
                               seq(max(end_int$hh) + 1, max(end_int$hh) + (n_simul), length.out = n_simul),
                             "end_prev" = rep(current_time, n_simul),
                             "start_cur" = rep(int_start_time, n_simul),
                             "end_cur" = rep(int_end_time, n_simul)
                           ))
      
      if(max(end_int$hh >= 60)) break
    }
    
    tibble(end_int,
           village = village_name,
           state = state) %>%
      filter(hh > 0)
    
  }
  
  benue_1 <- sample_houses(village_name = "zugu", state = "benue", t0 = study_start)
  benue_2 <- sample_houses(village_name = "dyegh", state = "benue",
                           t0 = if(max(benue_1$end_cur) >= as_datetime(as_date(max(benue_1$end_cur)) + hours(12), tz = "Africa/Lagos"))
                             max(benue_1$end_cur) + days(3) else
                               max(benue_1$end_cur) + days(2))
  benue_3 <- sample_houses(village_name = "ikyogbakpev", state = "benue",
                           t0 = if(max(benue_2$end_cur) >= as_datetime(as_date(max(benue_2$end_cur)) + hours(12), tz = "Africa/Lagos"))
                             max(benue_2$end_cur) + days(3) else
                               max(benue_2$end_cur) + days(2))
  cross_river_1 <- sample_houses(village_name = "okimbongha", state = "cross_river", t0 = as_datetime(as_date(max(benue_3$end_cur) + days(2)) + hours(6), tz = "Africa/Lagos"))
  cross_river_2 <- sample_houses(village_name = "ogamanna", state = "cross_river",
                                 t0 = if(max(cross_river_1$end_cur) >= as_datetime(as_date(max(cross_river_1$end_cur)) + hours(12), tz = "Africa/Lagos"))
                                   max(cross_river_1$end_cur) + days(3) else
                                     max(cross_river_1$end_cur) + days(2))
  cross_river_3 <- sample_houses(village_name = "ofonekom", state = "cross_river",
                                 t0 = if(max(cross_river_2$end_cur) >= as_datetime(as_date(max(cross_river_2$end_cur)) + hours(12), tz = "Africa/Lagos"))
                                   max(cross_river_2$end_cur) + days(3) else
                                     max(cross_river_2$end_cur) + days(2))
  ebonyi_1 <- sample_houses(village_name = "ezeakataka", state = "ebonyi", t0 = as_datetime(as_date(max(cross_river_3$end_cur) + days(2)) + hours(6)))
  ebonyi_2 <- sample_houses(village_name = "enyandulogu", state = "ebonyi",
                            t0 = if(max(ebonyi_1$end_cur) >= as_datetime(as_date(max(ebonyi_1$end_cur)) + hours(12), tz = "Africa/Lagos"))
                              max(ebonyi_1$end_cur) + days(3) else
                                max(ebonyi_1$end_cur) + days(2))
  ebonyi_3 <- sample_houses(village_name = "offianka", state = "ebonyi",
                            t0 = if(max(ebonyi_2$end_cur) >= as_datetime(as_date(max(ebonyi_2$end_cur)) + hours(12), tz = "Africa/Lagos"))
                              max(ebonyi_2$end_cur) + days(3) else
                                max(ebonyi_2$end_cur) + days(2))
  
  all_villages <- bind_rows(benue_1, benue_2, benue_3,
                            cross_river_1, cross_river_2, cross_river_3,
                            ebonyi_1, ebonyi_2, ebonyi_3) %>%
    mutate(village = str_to_sentence(village), 
           village = fct_inorder(village),
           cumulative_count = row_number(),
           state = str_to_sentence(str_replace_all(state, "_", " ")))
  
  visualise <- ggplot(all_villages) +
    geom_line(aes(x = end_cur, y = hh, group = village, colour = village)) +
    geom_line(aes(x = end_cur, y = cumulative_count, colour = village)) +
    geom_hline(aes(yintercept = 60), linetype = "dashed") +
    annotate(geom = "rect", xmin = winter_break[1], xmax = winter_break[2], ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.6) +
    guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
    theme_bw() +
    labs(x = "Date",
         y = "Households surveyed",
         colour = "Village",
         title = paste0("Combined interview time = ", round(as.numeric(time_each_hh, units = "hours"), 1), " hours"))
  
  return(list(all_villages, visualise))
  
}



current_estimate <- time_estimate()
shorter_questionnaires <- time_estimate(hh_questionnaire = minutes(90),
                                        i_questionnaire = minutes(30),http://127.0.0.1:41367/graphics/plot_zoom_png?width=2048&height=1090
                                        rc_sheet = minutes(30))
additional_RA <- time_estimate(hh_questionnaire = minutes(90),
                               i_questionnaire = minutes(30),
                               rc_sheet = minutes(30),
                               RAs = 5)


hh_data <- read_rds(here("data", "h_data", "hh_df_list.rds"))$household_main

# Extract relevant information from hh_data
observed_data <- hh_data %>%
  select(household_id, start) %>%
  mutate(abbreviation = substr(household_id, 1, 3)) %>%
  left_join(village_state %>%
              select(abbreviation, village, state), by = "abbreviation") %>%
  mutate(village = fct_inorder(village)) %>%
  arrange(start) %>%
  mutate(cumulative_count = row_number()) %>%
  group_by(village) %>%
  mutate(hh = row_number())

# Overlay on modelled estimate
current_estimate[[2]] +
  geom_line(data = observed_data, aes(x = start, y = hh, group = village, colour = village), lwd = 1) +
  geom_line(data = observed_data, aes(x = start, y = cumulative_count, colour = village), lwd = 1, inherit.aes = FALSE) +
  geom_vline(xintercept = as_datetime(Sys.Date()), linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d") 

shorter_questionnaires[[2]] +
  geom_line(data = observed_data, aes(x = start, y = hh, group = village, colour = village), lwd = 1) +
  geom_line(data = observed_data, aes(x = start, y = cumulative_count, colour = village), lwd = 1, inherit.aes = FALSE) +
  geom_vline(xintercept = as_datetime(Sys.Date()), linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d") 
