Sys.time()
###############################################################################
# This script takes WY16 and WY17 .nc files and creates a map of 
# DOY that 261 DD is met across the entire domain using lapply
###############################################################################

###############################################################################
# Libraries to Open
###############################################################################

library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RNetCDF)
library(weathermetrics)
library(viridis)
library(DescTools)
library(abind)
library(date)


###############################################################################
# Custom Functions
###############################################################################

# This function is used to create the Degree Day column.  It's input is a data frame
# that contains daily high and low temperatures for a calendar year at one location.
# This will be set up to take in 2 files names for high and low

DD_Calculation_2files <- function(file_name_min, file_name_max) {
  DD <- matrix(-9999, nrow = 365, ncol=1)
  DD <- data.frame(DD)
  for(i in 1:365) {
    high <- as.numeric(file_name_max[i])
    low <- as.numeric(file_name_min[i])
    DD_Calc <- DD_S(low, high)
    DD[i,1] <- (DD_Calc)
  }
  return(DD)
}

#  This function takes the input of a sum, difference and a variable
#  fk1, and calculates the number of "heat days" for that day.
#  This is used in the DD_S (Degree Day Sine) function (below) only to help
#  calculate "degree days"

#  This is the Single and Double Sine Curve method
#  (Baskerville & Emin 1969, Ecology 50:514-517)

sinec <- function(sum, diff, fk1) {
  two_pi <- 2*pi
  half_pi <- pi/2
  d2 <- fk1-sum
  theta <- atan2(d2, sqrt(diff * diff - d2 * d2))
  
  if(d2 < 0 & theta > 0) {
    theta <- theta - pi
  }
  heat <- (diff * cos(theta) - d2 * (half_pi - theta)) / two_pi
}

#  DD_S is the Degree Day Calculation using the Sine Method:
#  This function takes the input of a high temperature and a low
#  temperature, and calculates the number of "degree days" for that day.
#  The high and low will be from one day at one station in units of
#  Fahrenheit.

#  This is the Single and Double Sine Curve method
#  (Baskerville & Emin 1969, Ecology 50:514-517)

DD_S <- function(Low_Temp , High_Temp) {
  t_hi <- 88
  t_lo <- 50
  min <- Low_Temp
  max <- High_Temp
  
  if (min > t_hi) {
    heat <- t_hi - t_lo
  }
  else {
    if (max <= t_lo){
      heat <- 0
    }
    else {
      fk1 <- 2 * t_lo
      diff <- max - min
      sum <- max + min
      if (min >= t_lo) {
        heat <- (sum - fk1)/2
      }
      else {
        heat <- sinec(sum, diff, fk1)
      }
      if(max > t_hi) {
        fk1 <- 2 * t_hi
        zheat <- heat
        heat <- sinec(sum, diff, fk1)
        heat <- zheat - heat
      }
    }
  }
  return(heat)    
}

###############################################################################
# Scripts to compile and plot
###############################################################################

# Open files to extract min and max across domain

WRF_WY2016 <- nc_open("WRF-NARR-1km-WY2016.nc")
WRF_WY2017 <- nc_open("WRF-NARR-1km-WY2017.nc")

# Pull out max and min for 2016

# Extract the WY2016 max/min values for whole domain
tmax_WY16 <- ncvar_get(WRF_WY2016, "TMAX")
tmin_WY16 <- ncvar_get(WRF_WY2016, "TMIN")

# Extract the WY2017 max/min values for whole domain
tmax_WY17 <- ncvar_get(WRF_WY2017, "TMAX")
tmin_WY17 <- ncvar_get(WRF_WY2017, "TMIN")

# Extract and combine values to create CY2016 (366 days)
# CAL DOY
# Oct 1 = DOY 274 (275 in LY)
# Dec 31 = DOY 365 (366 in LY)
# WY DOY
# Oct 1 = DOY 1
# Dec 31 = DOY 91
# Jan 1 = DOY 92
# Sept 30 DOY 365 (366 in LY)

# Extract CY2016 Mins
jan_sep_min_16 <- tmin_WY16[,,92:366]
oct_dec_min_16 <- tmin_WY17[,,1:91]
min_2016 <- c(jan_sep_min_16, oct_dec_min_16)

# Extract CY2016 Maxs
jan_sep_max_16 <- tmax_WY16[,,92:366]
oct_dec_max_16 <- tmax_WY17[,,1:91]
max_2016 <- c(jan_sep_max_16, oct_dec_max_16)

# Combine to get a calendar year
min_CY2016 <- abind(jan_sep_min_16, oct_dec_min_16, rev.along=1)
max_CY2016 <- abind(jan_sep_max_16, oct_dec_max_16, rev.along=1)

# Convert from Kelvin to Fahrenheit

minF_CY2016 <- 9/5*(min_CY2016 - 273.15) + 32
maxF_CY2016 <- 9/5*(max_CY2016 - 273.15) + 32

Sys.time() # Less than 1 minute

# Calculate Degree Day for each location on each calendar day

Sys.time()
DD_all <- mapply(DD_S, minF_CY2016, maxF_CY2016)
dim(DD_all)
DD_reshape_all <- array(DD_all, dim=dim(minF_CY2016))
dim(DD_reshape_all)
Sys.time() # Takes 3-8 mins

# Calculate the cumsum for each year at each location. 

Sys.time()
DD_cumsum <- apply(DD_reshape_all, c(1,2), cumsum) # [366,348,327]
dim(DD_cumsum)
Sys.time() # less than 1 min


# Make a matrix for DOY261DD

loc_1 <- 1
loc_2 <- 1
FEG_DOY_261 <- array(0,c(348,327,1))

for(loc_1 in seq(from=1, to=348, by=1)) {
  for(loc_2 in seq(from=1, to=327, by=1)) {
    cumsum_temp <- DD_cumsum[1:366,loc_1,loc_2]
    DOY_261 <- as.numeric((which(cumsum_temp>=261)[1]))
    FEG_DOY_261[loc_1,loc_2,1] <- DOY_261
  }
}
Sys.time()

# Plot DOY261DD

x_values <- 1:dim(FEG_DOY_261)[1]
y_values <- 1:dim(FEG_DOY_261)[2]
z_values <- FEG_DOY_261[,,]

image(x_values,y_values,z_values,
      xlab = "Latitude",
      ylab = "Longitude")
  

# Make a matrix for DOY510DD

loc_1 <- 1
loc_2 <- 1
FEG_DOY_510 <- array(0,c(348,327,1))

for(loc_1 in seq(from=1, to=348, by=1)) {
  for(loc_2 in seq(from=1, to=327, by=1)) {
    cumsum_temp <- DD_cumsum[1:366,loc_1,loc_2]
    DOY_510 <- as.numeric((which(cumsum_temp>=510)[1]))
    FEG_DOY_510[loc_1,loc_2,1] <- DOY_510
  }
}

# Plot DOY510DD

x_values <- 1:dim(FEG_DOY_510)[1]
y_values <- 1:dim(FEG_DOY_510)[2]
z_values <- FEG_DOY_510[,,]

image(x_values,y_values,z_values,
      xlab = "Latitude",
      ylab = "Longitude")

