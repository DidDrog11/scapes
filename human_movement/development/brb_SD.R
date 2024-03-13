library(here)
library(lubridate)
library(adehabitatHR)
library(terra)
library(tidyterra)
library(tidyverse)
library(units)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("buffer", "terra")

project_crs <- "EPSG:4326"
SD_crs <- "EPSG:32610"

gps <- read_csv(here("human_movement", "data", "test", "week_test.csv")) %>%
  mutate(utc_datetime = ymd_hms(Time)) %>%
  group_by(Region) %>%
  mutate(local_datetime = with_tz(ymd_hms(`Local Time`), tzone = unique(Region))) %>%
  ungroup() %>%
  rename("y" = Latitude,
         "x" = Longitude,
         "altitude" = `Altitude(m)`,
         "speed" = `Speed(km/h)`,
         "heading" = Course,
         "distance" = `Distance(m)`,
         "satellites" = `Visible Satellites`,
         "satellites_cn22" = `Satellites(CN>22)`) %>%
  mutate(altitude = as_units(altitude, "m"),
         speed = as_units(speed, "km/h"),
         distance = as_units(distance, "m"),
         INDEX = row_number()) %>%
  select(INDEX, utc_datetime, local_datetime, x, y, altitude, speed, heading, distance, satellites, satellites_cn22, HDOP) %>%
  filter(local_datetime <= ymd_hms("2024-03-10 03:09:55", tz = "America/Los_Angeles"))

## I think it easier to pass a clean dataframe onto the ltraj functions
## gps timepoints are the expected timepoints given a polling time of 90s
gps_timepoints <- tibble(timepoint = seq(min(gps$local_datetime), max(gps$local_datetime), by = 90)) %>%
  mutate(index = row_number())

gps_matched <- gps %>%
  mutate(closest_expected = gps_timepoints$timepoint[findInterval(local_datetime, gps_timepoints$timepoint)],  # Find the closest timepoint
         closest_expected_i = gps_timepoints$index[findInterval(local_datetime, gps_timepoints$timepoint)], # Extract the index of this timepoint
         closest_diff = as.numeric(difftime(local_datetime, closest_expected, units = "secs")),  # Calculate time difference
         adjusted_closest = case_when(closest_diff > 45 ~ gps_timepoints$timepoint[closest_expected_i + 1], # If timediff >45 seconds associate with next expected timepoint record
                                      TRUE ~ closest_expected),
         adjusted_i = gps_timepoints$index[findInterval(adjusted_closest, gps_timepoints$timepoint)],
         adjusted_diff = as.numeric(difftime(local_datetime, adjusted_closest, units = "secs")))

gps_vect <- vect(gps, geom = c("x", "y"), crs = "EPSG:4326") %>%
  project(SD_crs)

gps_df <- sf::st_as_sf(gps_vect) %>%
  sf::st_coordinates()

## Create a grid based on GPS points
## Buffer by 1km
ext <- ext(buffer(gps_vect, 1000))
r_SD <- rast(ext, res = 50, crs = SD_crs)
# requires raster for the conversion
# library(raster)
p_SD <- as(as.points(r_SD), "Spatial")
# sp may be loaded by another package, if not it's needed for this
# library(sp)
pi_SD <- sp::SpatialPixels(p_SD)

## as.ltraj requires 
## xy = a dataframe with x and y coordinates of the relocations
## date =  a vector of class POSIXct giving the date for each relocation
## typeII = TRUE a trajectory of type 2 (time-recorded)
## proj4string = CRS storing the projection information
d <- as.ltraj(xy = gps_df,
         date = gps$local_datetime,
         id = "site",
         typeII = TRUE,
         proj4string = CRS(SD_crs))
summary(d)
plot(d)

## Set time before first date time
start_time <- min(gps$local_datetime) - 90

## use setNA to place missing values in the ltraj object
## requires
## ltraj = ltraj object (i.e., d)
## date.ref = the start date/time of the relocations
## dt = the time lag between relocations
## tol = the tolerance of the imprecision in the time timing of data collection
## units = a character string for the time units
polling = 90
tol_s = 45 # I'm interpreting this as we allow the tolerance for the records to be 45s either side of the expected polling

d2 <- setNA(d, start_time, polling, tol = tol_s, units = "sec")
summary(d2)

## Set NAs based on start time for every 90 seconds
d3 <- sett0(d2, start_time, 90, correction.xy = c("none"), tol = tol_s, units = "sec")
summary(d3)


# Utilisation distribution ------------------------------------------------
# Set Tmax as the maximum time between steps (in seconds) - set as 25 minutes
# Maximum threshold between points before they were considered uncorrelated set as 3 hr based on typical reported activity times. 
# Is this an activity from home to somewhere and back? If so 9 may be more suitable in our settings (i.e., day in the field)
tmax <- (60*25)
# Set minimum distance between relocations (anything less is immobile - account for GPS error; use coordinate units)
lmin <- 10
# Set minimum smoothing parameter, should be equal to SD of GPS error or resolution of habitat map
# We can calculate this from our data in Nigeria, may be worth doing a couple of calibrations (i.e., in Abakaliki, a field, a forest and a local house)
hmin <- 5
# If not using habitat, set cell (higher is smaller cells)
cell <- 100
# Set maximum time allowed to spend outside patch before considering having left
# Set as 10 minutes
maxT <- 60*10
# Set filtershort
## If TRUE, track segments shorter than Lmin are assumed to correspond to resting
## If FALSE, short segments are taken into account when not associate with resting
filtershort <- TRUE

## Estimate the diffusion component
diffusion <- BRB.D(d3, Tmax = tmax, Lmin = lmin, habitat = NULL, activity = NULL)
diffusion

## Estimate utilisation distribution
ud <- BRB(d3, D = diffusion, Tmax = tmax, Lmin = lmin, hmin = hmin, type = "UD",
          b = FALSE, same4all = FALSE, extent = 0.1, grid = 370)

summary(ud)

SD_dens <- tibble(x = ud@coords[, 1],
                  y = ud@coords[, 2],
                  density = ud$dens)

dens_vect <- SD_dens %>%
  vect(geom = c("x", "y"), crs = SD_crs)

ggplot() +
  geom_spatvector(data = dens_vect, aes(colour = density))

dens_rast <- terra::rasterize(dens_vect, r_SD, field = "density", fun = "max")

## Plot utilisation distribution
vud <- getvolumeUD(ud)
image(vud)

## PDF for UD
ud_vect <- as.data.frame.estUD(ud) %>%
  vect(geom = c("x", "y"), crs = SD_crs)

