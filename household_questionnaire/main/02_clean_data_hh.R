hh_df_list <- read_rds(here("household_questionnaire", "data", "h_data", "hh_df_list.rds"))


# Check unique numbering of households ------------------------------------

# Description: This function checks whether the household IDs in the dataset are unique.
#              If duplicates are found, it returns the duplicate records for further inspection.

# Arguments:
#   - df: The dataframe containing household data. Default is hh_df_list$household_main.

# Returns:
#   - NULL if all household IDs are unique.
#   - Dataframe containing duplicate records if duplicates are found.

check_unique_id <- function(df = hh_df_list$household_main) {
  duplicate_indices <- duplicated(df$household_id) | duplicated(df$household_id, fromLast = TRUE)
  if (any(duplicate_indices)) {
    duplicate_records <- df$`_index`[duplicate_indices, ]
    message(crayon::red("FAIL: Duplicate household IDs found. Check duplicate_records for details."))
    return(duplicate_records)
  } else {
    message(crayon::green("PASS: All household IDs are unique"))
    return(NULL)
  }
}

# Execute the function to check for unique household IDs
duplicate_records <- check_unique_id(df = hh_df_list$household_main)


# Cleaning Coordinates ----------------------------------------------------
# Sub-function 1: Extract Coordinates
extract_coordinates <- function(df) {
  # Extract coordinates from the dataframe
  coords <- df %>% 
    dplyr::select(`_index`, id = household_id, latitude, longitude) %>%
    # Remove rows with missing latitude or longitude values
    dplyr::filter(!is.na(latitude) & !is.na(longitude)) %>%
    # Create a SpatVector
    vect(geom = c("longitude", "latitude"), crs = project_CRS)
  return(coords)
}


# Sub-function 2: Check Coordinates
check_coordinates <- function(coords, Nigeria, lga) {
  coords_spat <- coords
  in_nigeria <- coords_spat[Nigeria]
  out_nigeria <- coords_spat[!is.related(coords_spat, Nigeria, "intersects"), ]
  
  cat(paste("Number of records inside Nigeria: ", nrow(in_nigeria), "\n"))
  cat(paste("Number of records outside Nigeria: ", nrow(out_nigeria), "\n"))
  
  # Add LGA information to the coordinates
  coords_spat$lga <- coords_spat %>%
    mutate(abbreviation = str_split(coords_spat$id, "-", simplify = TRUE)[, 1]) %>%
    left_join(village_state %>%
                select(abbreviation, lga),
              by = "abbreviation") %>%
    pull(lga)
  # Check if coordinates fall within the correct LGA
  in_lga <- coords_spat[coords_spat$lga == terra::extract(lga, coords_spat)$NAME_2]
  out_lga <- coords_spat[coords_spat$lga != terra::extract(lga, coords_spat)$NAME_2 | is.na(terra::extract(lga, coords_spat)$NAME_2)]
  
  list(coords_spat = coords_spat, in_nigeria = in_nigeria, out_nigeria = out_nigeria, in_lga = in_lga, out_lga = out_lga)
}

# Sub-function 3: Correct Errors
correct_errors <- function(coords, factors, Nigeria) {
  potential_errors <- data.frame()
  updated_coords <- coords$coords_spat  # Store the updated coordinates in a new object
  for (index in coords$out_nigeria$`_index`) {
    record <- coords$coords[coords$coords$`_index` == index]
    # Initialize a flag to track if any correction is made for this record
    correction_made <- FALSE
    # Check latitude value range and correct if high or low
    if (crds(record)[,2] >= 14.0 || crds(record)[,2] <= 4.0) {
      for (factor in factors) {
        corrected_latitude <- crds(record)[,2] / factor
        # Check if corrected latitude falls within the desired range
        if (corrected_latitude >= 4.0 && corrected_latitude <= 14.0) {
          # Create a new SpatVector with the updated latitude
          updated_coords <- crds(coords$coords_spat)
          updated_coords[coords$coords_spat$`_index` == index] <- c(crds(record)[,1], corrected_latitude)
          updated_spatvector <- vect(updated_coords, crs = project_CRS) %>%
            bind_spat_cols(coords$coords_spat)
          # Replace the SpatVector in coords with the updated one
          coords$coords_spat <- updated_spatvector
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = if (crds(record)[,2] >= 14.0) "corrected decimal point mistake (latitude high)" else "corrected decimal point mistake (latitude low)"))
          correction_made <- TRUE
          break
        }
      }
    }
    # Check longitude value range and correct if high or low
    if (crds(record)[,1] >= 15.0 || crds(record)[,1] <= 2.0) {
      for (factor in factors) {
        corrected_longitude <- crds(record)[,1] / factor
        # Check if corrected longitude falls within the desired range
        if (corrected_longitude >= 2.0 && corrected_longitude <= 15.0) {
          # Create a new SpatVector with the updated longitude
          updated_coords <- crds(coords$coords_spat)
          updated_coords[coords$coords_spat$`_index` == index] <- c(corrected_longitude, crds(record)[, 2])
          updated_spatvector <- vect(updated_coords, crs = project_CRS) %>%
            bind_spat_cols(coords$coords_spat)
          # Replace the SpatVector in coords with the updated one
          coords$coords_spat <- updated_spatvector
          potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = if (crds(record)[,1] >= 15.0) "corrected decimal point mistake (longitude high)" else "corrected decimal point mistake (longitude low)"))
          correction_made <- TRUE
          break
        }
      }
    }
    # Check for swapped lat and long
    if (!is.na(crds(record)[,2]) && !is.na(crds(record)[,1]) && 
        crds(record)[,2] >= 4.0 && crds(record)[,2] <= 14.0 && 
        crds(record)[,1] >= 2.0 && crds(record)[,1] <= 15.0) {
      # Create a new SpatVector with swapped latitude and longitude
      updated_coords <- tibble(x = crds(record)[, c("y")],
                               y = crds(record)[, c("x")])
      updated_spatvector <- vect(updated_coords, geom = c("x", "y"), crs = project_CRS) %>%
        bind_spat_cols(record)
      # Check if the swapped coordinates are within country bounds
      if (is.related(updated_spatvector, Nigeria, "intersects")) {
        coords$coords_spat <- bind_spat_rows(coords$coords[coords$coords$`_index` != index],
                                        updated_spatvector)
        potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "corrected latitude and longitude swap"))
        correction_made <- TRUE
      }
      # If not within bounds, revert to original coordinates
      else {
        coords$coords_spat <- coords$coords_spat
      }
    }
    # If no correction is made for this record, consider it as an "other error"
    if (!correction_made) {
      potential_errors <- bind_rows(potential_errors, tibble("_index" = index, reason = "other potential error"))
    }
  }
  # Extract the corrected coordinates and potential errors
  corrected_coords <- list(
    coords_spat = coords$coords_spat,
    in_nigeria = coords$in_nigeria,
    out_nigeria = coords$out_nigeria,
    potential_errors = potential_errors
  )
  return(corrected_coords)
}

# Sub-function 4: Update Coordinates
update_coordinates <- function(coords = coords_spat, df) {
  # Match the row indices between the original dataframe and the corrected coordinates
  rows_to_update <- match(coords$`_index`, df$`_index`)
  # Extract the corrected latitude and longitude from the coords object
  updated_latitude <- crds(coords)[, 2]  # Column number 2 corresponds to latitude
  updated_longitude <- crds(coords)[, 1] # Column number 1 corresponds to longitude
  # Update the relevant rows in the original dataframe with the corrected coordinates
  df[rows_to_update, c("latitude", "longitude")] <- list(updated_latitude, updated_longitude)
  # Return the updated dataframe
  return(df)
}

# Sub-function 5: Log Messages
log_messages <- function(coords, checked_coords, corrected_coords, unique_errors) {
  # Coordinate Check 1
  cat("Coordinate Check 1:\n")
  cat(crayon::green(paste(bold(nrow(checked_coords$in_nigeria)), "records have coordinates within Nigeria.\n")))
  cat(crayon::yellow(paste(bold(nrow(coords) - nrow(checked_coords$in_nigeria)), "records have coordinates falling outside of Nigeria.\n")))
  cat(crayon::red(paste(bold(nrow(df) - nrow(coords)), "records have missing coordinates.\n")))
  # Coordinate Check 2
  cat("\nCoordinate Check 2:\n")
  cat(crayon::red(paste(bold(nrow(checked_coords$out_lga)), "records have coordinates outside of the correct LGAs.\n")))
  # Coordinate Check 3
  cat("\nCoordinate Check 3:\n")
  if (unique_errors == 0) {
    cat(crayon::green("No latitude and longitude swapping detected.\n"))
  } else {
    cat(crayon::yellow("No latitude and longitude swapping detected after correction.\n"))
  }
  # Coordinate Check 4
  cat("\nCoordinate Check 4:\n")
  cat(crayon::yellow(paste(bold(nrow(corrected_coords$potential_errors)), "misplaced decimal points were detected and corrected.\n")))
  # Summary
  corrected_errors <- sum(grepl("corrected", corrected_coords$potential_errors$reason))
  remaining_errors <- unique_errors - corrected_errors
  if (remaining_errors == 0) {
    cat(crayon::green("All errors have been corrected.\n"))
  } else {
    cat(crayon::red(paste(remaining_errors, "errors remain.\n")))
    if (remaining_errors > 1) {
      cat(crayon::red("Indexes of errors: ", paste(corrected_coords$potential_errors$`_index`, collapse = ", "), "\n"))
    } else {
      cat(crayon::red("Index of error: ", corrected_coords$potential_errors$`_index`, "\n"))
    }
  }
}

# Combine Functions
clean_coordinates <- function(df = hh_df_list$household_main) {
  # Extract coordinates from the dataframe
  coords <- extract_coordinates(df)
  # Define administrative boundaries
  Nigeria <- gadm(country = "NGA", level = 0, path = here("project_wide_data", "spatial"))
  lga <- gadm(country = "NGA", level = 2, path = here("project_wide_data", "spatial"))
  # Check coordinates against country boundary and administrative boundaries
  checked_coords <- check_coordinates(coords, Nigeria, lga)
  # Correct errors in the coordinates
  # Factors to correct
  factors <- 10^(0:12)
  corrected_coords <- correct_errors(checked_coords, factors, Nigeria)
  # Define unique_errors before calling log_messages
  unique_errors <- nrow(corrected_coords$potential_errors)
  # Log messages about the coordinates
  log_messages(coords, checked_coords, corrected_coords, unique_errors)
  # Update the original dataframe with the corrected coordinates
  updated_df <- update_coordinates(corrected_coords$coords_spat, df)
  # Return the updated dataframe
  return(updated_df)
}

coords <- clean_coordinates(df = hh_df_list$household_main)

# Cleaning XXXX -----------------------------------------------------------



# Save clean dataframe ----------------------------------------------------

write_rds(hh_df_list, here("household_questionnaire", "data", "h_data", "hh_df_list_cleaned.rds"))

