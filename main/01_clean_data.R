hh_df_list <- read_rds(here("data", "h_data", "hh_df_list.rds"))


# Check unique numbering of households ------------------------------------

check_unique_id <- function(df =  hh_df_list$household_main) {
 
  if(nrow(df) == length(unique(df$household_id))) message(crayon::green("All household IDs are unique"))
  
  return(df)
}

check_unique_id(df =  hh_df_list$household_main)

# Cleaning Coordinates ----------------------------------------------------

clean_coordinates <- function(df = hh_df_list$household_main) {
  
  # Get the coordinates, community and id
  coords <- tibble(`_index` = df$`_index`,
                   id = df$household_id,
                   latitude = df$latitude,
                   longitude = df$longitude)
  
  coords_spat <- vect(coords, geom = c("latitude", "longitude"), crs = project_CRS)
  
  # Coordinates are within Nigeria
  Nigeria <- gadm(country = "NGA", level = 0, path = here("data", "spatial"))
  
  
  # Test if the coordinates fall within the bounding box for each village (village location within 10km radius)
  in_nigeria <- coords_spat[Nigeria]
  
  message(paste0("Coordinate check 1:\n"), 
          crayon::green(paste(bold(nrow(in_nigeria)), "records have coordinates within Nigeria.\n")),
          crayon::yellow(paste(bold(nrow(coords_spat) - nrow(in_nigeria)), "records have coordinates falling outside of Nigeria.\n")),
          crayon::red(paste(bold(sum(is.na(df$latitude) | is.na(df$longitude))), "records have missing coordinates.")))
  
  out_nigeria <- coords_spat[!is.related(coords_spat, Nigeria, "intersects"), ]
  
  # Initialize counters
  decimal_place_count <- 0
  swap_coordinates_count <- 0
  other_errors_count <- 0
  
  # Produce a df of coordinates that are potential errors
  potential_errors <- data.frame()
  
  # Check 1 is for a missplaced decimal point
  # Try different factors to correct these
  factors <- 10^(0:12)
  
  for (index in out_nigeria$`_index`) {
    # Extract coordinates for the specific record
    record <- coords[coords$`_index` == index, c("latitude", "longitude")]
    
    correction_made <- FALSE
    
    # Check latitude value range and correct if high
    if (record$latitude >= 14.0) {
      
      for (factor in factors) {
        
        corrected_latitude <- record$latitude / factor
        
        # Check if corrected latitude falls within the desired range
        if (corrected_latitude >= 4.0 && corrected_latitude <= 14.0) {
          
          # Correct potential decimal point mistake
          coords[coords$`_index` == index, "latitude"] <- corrected_latitude
          
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected decimal point mistake (latitude high)"))
          
          correction_made <- TRUE
          
          break  # Break the loop if correction is successful
        }
      }
    }
    # Check latitude value range and correct if low
    if (record$latitude <= 4.0) {
      
      for (factor in factors) {
        
        corrected_latitude <- record$latitude / factor
        
        # Check if corrected latitude falls within the desired range
        if (corrected_latitude >= 4.0 && corrected_latitude <= 14.0) {
          
          # Correct potential decimal point mistake
          coords[coords$`_index` == index, "latitude"] <- corrected_latitude
          
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected decimal point mistake (latitude low)"))
          
          correction_made <- TRUE
          
          break  # Break the loop if correction is successful
        }
      }
    }
    # Check longitude value range and correct if high
    if (record$longitude >= 15.0) {
      
      for (factor in factors) {
        
        corrected_longitude <- record$longitude / factor
        
        # Check if corrected longitude falls within the desired range
        if (corrected_longitude >= 2.0 && corrected_longitude <= 15.0) {
          
          # Correct potential decimal point mistake
          coords[coords$`_index` == index, "longitude"] <- corrected_longitude
          
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected decimal point mistake (longitude high)"))
          
          correction_made <- TRUE
          
          break  # Break the loop if correction is successful
        }
      }
    }
    # Check latitude value range and correct if low
    if (record$latitude <= 2.0) {
      # Try different factors to correct the latitude
      factors <- 10^(0:12)
      
      for (factor in factors) {
        
        corrected_longitude <- record$longitude / factor
        
        # Check if corrected latitude falls within the desired range
        if (corrected_longitude >= 4.0 && corrected_longitude <= 14.0) {
          
          # Correct potential decimal point mistake
          coords[coords$`_index` == index, "longitude"] <- corrected_longitude
          
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected decimal point mistake (longitude low)"))
          
          correction_made <- TRUE
          
          break  # Break the loop if correction is successful
        }
      }
    }
    
    # Check for swapped lat and long
    if (record$latitude >= 4.0 && record$latitude <= 14.0 && record$longitude >= 2.0 && record$longitude <= 15.0) {
      # Swap latitude and longitude
      coords[coords$`_index` == index, c("latitude", "longitude")] <- record[c("longitude", "latitude")]
      
      # Convert the modified coords to SpatVect
      modified_coords_spat <- vect(coords, geom = c("latitude", "longitude"), crs = project_CRS)
      
      # Check if the swapped coordinates are within country bounds
      if (is.related(modified_coords_spat[coords_spat$`_index` == index, ], Nigeria, "intersects")) {
        
        potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected latitude and longitude swap"))
        
        correction_made <- TRUE
        
      } else {
        
        # Revert the swap if the coordinates are still outside country bounds
        coords[coords$`_index` == index, c("latitude", "longitude")] <- record
        
      }
    }
  }
  
  dec_place <- potential_errors[str_detect(potential_errors$reason, "corrected decimal point"), ]
  swapped_coords <- potential_errors[str_detect(potential_errors$reason, "corrected latitude and longitude swap"), ]
  
  if(nrow(dec_place) >= 1) {
    
    message(paste("\nMisspecified decimal place\n",
                crayon::green(paste(nrow(dec_place), "Issues associated with _index", bold(dec_place %>%
                                                                                             str_glue_data("{`_index`}")),
                                    "would be corrected by fixing the decimal point.\n"))))
    
    decimal_place_count <- nrow(dec_place) 
    
  }
  
  if(nrow(dec_place) >= 1) print(knitr::kable(dec_place, format = "markdown"))
  
  if(nrow(swapped_coords) >= 1) {
    
    message(paste("\nSwapped Coordinates\n",
                crayon::blue(paste(nrow(swapped_coords), "Issues associated with _index", bold(swapped_coords %>%
                                                                                                 str_glue_data("{`_index`}")),
                                   "have been corrected by swapping latitude and longitude.\n"))))
    
    swap_coordinates_count <- nrow(swapped_coords)
    
  }
  
  if(nrow(swapped_coords) >= 1) print(knitr::kable(swapped_coords, format = "markdown"))
  
  # Create remaining_errors if needed
  remaining_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "other potential error"))
  
  
  if(nrow(remaining_errors) >= 1) {
    
    message(crayon::red(paste(bold(other_errors_count), "errors remain")))
    
    other_errors_count <- nrow(remaining_errors)
    
  }

  return(coords)
}

coords <- clean_coordinates(df = hh_df_list$household_main)

# Get the row indices in hh_df_list$household_main corresponding to coords
rows_to_update <- match(coords$`_index`, hh_df_list$household_main$`_index`)

# Update the relevant rows in hh_df_list$household_main with the values from coords
hh_df_list$household_main[rows_to_update, c("latitude", "longitude")] <- coords[, c("latitude", "longitude")]

# Cleaning XXXX -----------------------------------------------------------



# Save clean dataframe ----------------------------------------------------

write_rds(hh_df_list, here("data", "h_data", "hh_df_list_cleaned.rds"))

