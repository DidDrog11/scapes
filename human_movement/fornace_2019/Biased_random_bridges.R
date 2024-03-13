##########################################################################
########################## Biased random bridges #########################
##########################################################################

require(stringr)
require(adehabitatHR)
require(sp)
require(lattice)
require(gmodels)
require(spatstat)
require(maptools)
require(raster)
require(gstat)
require(spacetime)

# additional libraries
library(here)

############################### DATA #################################

## Load data - this is randomly jittered GPS data for example only
# GPS data should include coordinates and date and time 

gps <- read.csv(here("human_movement", "fornace_2019", "gps_test.csv"))

## Create a grid based on GPS points or study site
ext <- raster::extent(0, (max(gps$X)+1000), 0, (max(gps$Y)+1000))
r <- raster::raster(ext, res=10)
p <- as(r, "SpatialPixels")

######################## Create trajectory #########################
  
## Set date and time
dt <- as.POSIXct(gps$dt, format = "%Y/%m/%d %H:%M")

## Separate coordinates
coord <- data.frame(gps$X, gps$Y)
  
## Create ltraj separating data by individual and trip
# Type II for time recorded track
d <- as.ltraj(coord, dt, id="site", typeII = TRUE)
summary(d)
plot(d)

## Set time before first date time
start.date <- min(dt) - 60
  
## Set NAs every 60 sec, tolerance 30 seconds
d2 <- setNA(d, start.date, 60, tol=30, units="sec")
  
## Create reference start time each minute
refda <- strptime("00:01:00", "%H:%M:%S")
  
## Set NAs based on reference time for every 60 seconds
d3 <- sett0(d2, refda, 60, correction.xy=c("none"), tol=30, units="sec")
summary(d3)
  

################## SETTING PARAMETERS AND MODEL #####################
  
## Set grid for study location
  
  # Set Tmax as the maximum time between steps (in seconds) - set as 3 hours
  tmax <- (60*60*3)
  
  # Set minimum distance between relocations (anything less is immobile - account for GPS error; use coordinate units)
  lmin <- 10
  
  # Set minimum smoothing parameter, should be equal to SD of GPS error or resolution of habitat map
  hmin <- 30
  
  # If not using habitat, set cell (higher is smaller cells)
  cell <- 100
  
  # Set maximum time allowed to spend outside patch before considering having left
  maxT <- 60*10
  
  # Set filtershort
  ## If TRUE, track segments shorter than Lmin are assumed to correspond to resting
  ## If FALSE, short segments are taken into account when not associate with resting
  filtershort <- TRUE
  
## Estimate the diffusion component
diffusion <- BRB.D(d3, Tmax = tmax, Lmin = lmin, habitat= NULL, activity = NULL)
diffusion
  
## Estimate utilisation distribution
ud <- BRB(d3, D=diffusion, Tmax = tmax, Lmin=lmin, hmin=hmin, type = "UD",
            b = FALSE, same4all = FALSE, extent = 0.1, grid= 50)
summary(ud)
image(ud)
  
## Plot utilisation distribution
vud <- getvolumeUD(ud)
image(vud)
  
## PDF for UD
ud.df <- as.data.frame.estUD(ud)
