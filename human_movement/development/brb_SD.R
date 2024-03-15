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
library(raster)
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
# Maximum threshold between points before they were considered uncorrelated set as 3 hr based on typical reported activity times. 
# Is this an activity from home to somewhere and back? If so 9 may be more suitable in our settings (i.e., day in the field)
tmax <- (60*60*3)
# Set minimum distance between relocations (anything less is immobile - account for GPS error; use coordinate units)
lmin <- 50
# Set minimum smoothing parameter, should be equal to SD of GPS error or resolution of habitat map
# We can calculate this from our data in Nigeria, may be worth doing a couple of calibrations (i.e., in Abakaliki, a field, a forest and a local house)
hmin <- 100
# Tau
tau <- 90
# If not using habitat, set cell (higher is smaller cells)
cell <- 200
# Set maximum time allowed to spend outside patch before considering having left
# Set as 10 minutes
maxt <- 60*10
# Set filtershort
## If TRUE, track segments shorter than Lmin are assumed to correspond to resting
## If FALSE, short segments are taken into account when not associate with resting
filtershort <- FALSE

## Estimate the diffusion component
diffusion <- BRB.D(d3, Tmax = tmax, Lmin = lmin, habitat = NULL, activity = NULL)
diffusion

## Estimate utilisation distribution
ud <- BRB(d3, D = diffusion, Tmax = tmax, tau = tau, Lmin = lmin, hmin = hmin, maxt = maxt, type = "UD",
          b = FALSE, same4all = FALSE, extent = 0.1, grid = 370)
id <- BRB(d3, D = diffusion, Tmax = tmax, tau = tau, Lmin = lmin, hmin = hmin, maxt = maxt, type = "ID",
          b = FALSE, same4all = FALSE, extent = 0.1, grid = 370)
rd <- BRB(d3, D = diffusion, Tmax = tmax, tau = tau, Lmin = lmin, hmin = hmin, maxt = maxt, type = "RD",
          b = FALSE, same4all = FALSE, extent = 0.1, grid = 370)

summary(ud)
summary(id)
summary(rd)

vol_ud <- getvolumeUD(ud) %>%
  rast()
rast_ud <- rast(ud)
rast_ud[rast_ud == 0] <- NA
names(vol_ud) <- "Utilisation Distribution"
names(rast_ud) <- "Utilisation Distribution"
crs(vol_ud) <- SD_crs
crs(rast_ud) <- SD_crs
vol_id <- getvolumeUD(id) %>%
  rast()
rast_id <- rast(id)
rast_id[rast_id == 0] <- NA
names(vol_id) <- "Intensity Distribution"
names(rast_id) <- "Intensity Distribution"
crs(vol_id) <- SD_crs
crs(rast_id) <- SD_crs
vol_rd <- getvolumeUD(rd) %>%
  rast()
rast_rd <- rast(rd)
rast_rd[rast_rd == 0] <- NA
names(vol_rd) <- "Recursion Distribution"
names(rast_rd) <- "Recursion Distribution"
crs(vol_rd) <- SD_crs
crs(rast_rd) <- SD_crs

comb_rast_ud <- c(rast_ud, rast_id, rast_rd)

decomposing_rast_ud <- ggplot() + 
  geom_spatraster(data = comb_rast_ud) +
  geom_spatraster_contour(data = comb_rast_ud, bins = 5) +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~lyr) +
  coord_sf() +
  labs(fill = "Probability density",
     caption = "Utilisation distribution = probability density that an animal is found at a point\n
       Intensity distribution = average residency time\n
       Recursion distribution = reflecting the number of visits") +
  theme_minimal()

comb_vol_ud <- c(vol_ud, vol_id, vol_rd)
  
decomposing_vol_ud <- ggplot() + 
  geom_spatraster(data = comb_vol_ud) +
  geom_spatraster_contour(data = comb_vol_ud, bins = 5) +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~lyr) +
  coord_sf() +
  labs(fill = "UD component",
       caption = "UD component = percentage of the smallest home range containing this pixel\n
       Utilisation distribution = probability density that an animal is found at a point\n
       Intensity distribution = average residency time\n
       Recursion distribution = reflecting the number of visits") +
  theme_minimal()

plot_lims <- as.data.frame(gps_df) %>%
  summarise(x = median(X),
            y = median(Y)) %>%
  vect(geom = c("x", "y"), crs = SD_crs) %>%
  buffer(5000) %>%
  ext()

zoomed_vol_ud <- decomposing_vol_ud +
  coord_sf(xlim = plot_lims[1:2], ylim = plot_lims[3:4])

## Plot ud with overlay of observed locations
dens_plot <- decomposing_vol_ud +
  geom_spatvector(data = gps_vect, colour = "white", alpha = 0.2)


# Limit to 8-8 only -------------------------------------------------------
gps_day <- gps_vect %>%
  filter(hour(local_datetime) >= 8 &
           hour(local_datetime) <= 20)

gps_day_df <- sf::st_as_sf(gps_day) %>%
  sf::st_coordinates()

d_1 <- as.ltraj(xy = gps_day_df,
              date = gps_day$local_datetime,
              id = "site",
              typeII = TRUE,
              proj4string = CRS(SD_crs))
summary(d_1)
plot(d_1)

d2_1 <- setNA(d_1, start_time, polling, tol = tol_s, units = "sec")
summary(d2_1)

## Set NAs based on start time for every 90 seconds
d3_1 <- sett0(d2_1, start_time, 90, correction.xy = c("none"), tol = tol_s, units = "sec")
summary(d3_1)

## Estimate the diffusion component
diffusion_1 <- BRB.D(d3_1, Tmax = tmax, Lmin = lmin, habitat = NULL, activity = NULL)
diffusion_1

## Estimate utilisation distribution
ud_1 <- BRB(d3_1, D = diffusion, Tmax = tmax, Lmin = lmin, hmin = hmin, type = "UD",
            b = FALSE, same4all = FALSE, extent = 0.1, grid = 370)

summary(ud_1)

## Obtain predictions from ud
SD_dens_1 <- tibble(x = ud_1@coords[, 1],
                    y = ud_1@coords[, 2],
                    density = ud_1$dens)

dens_vect_1 <- SD_dens_1 %>%
  vect(geom = c("x", "y"), crs = SD_crs)

dens_rast_1 <- terra::rasterize(dens_vect_1, r_SD, field = "density", fun = "max")

dens_rast_1[dens_rast_1 == 0] <- NA

## Plot ud with overlay of observed locations
dens_plot_1 <- ggplot() +
  geom_spatraster(data = dens_rast_1, aes(fill = max)) +
  geom_spatvector(data = gps_day, colour = "white", alpha = 0.2) +
  scale_fill_viridis_c(na.value = NA)

gps_vect$modelled <- terra::extract(dens_rast, gps_vect)[, 2]

ggplot() + 
  geom_histogram(data = tibble(dens = gps_vect$modelled), aes(x = dens))