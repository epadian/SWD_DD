# This code will pull in all the water year netCDF files from R2,
# isolate the max and min and reassemble them into calendar year 
# netCDF files.

# Libraries needed

library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(RNetCDF)
library(weathermetrics)
library(viridis)
library(DescTools)
library(abind)
library(date)



# Read in netCDF files as a function where I give it 2 files

WY_to_CY <- function(WY8_12, WY1_7, CY_Creating)
  # WY_8_12 is the water year .nc file that contains October - December files
  # WY_1_7 is te water year .nc file that contains January - September files
  # CY_Creating should be 4 number year that matches the year in WY1_7
  
# Go to the directory?
    
# Open the files
  WY8_12_temp <- nc_open("WY8_12")
  WY1_7_temp <- nc_open("WY1_7")
  
# Extract the max/min 8-12 values for whole domain
  tmax_temp1 <- ncvar_get(WY8_12_temp, "TMAX")
  tmin_temp1 <- ncvar_get(WY8_12_temp, "TMIN")

# Extract the max/min 1-7 values for whole domain
  tmax_temp2 <- ncvar_get(WY1_7_temp, "TMAX")
  tmin_temp2 <- ncvar_get(WY1_7_temp, "TMIN")

#Establish what DOYs we are working with
  # Oct 1 = WY_DOY 1
  # Dec 31 = WY_DOY 92
  # Jan 1 = WY_DOY 93
  # Sept 30 = WY_DOY 365 or 366
  
  if (length(tmax_temp1)+length(tmax_temp2) == 365) {
        days = 365
 }

  if (length(tmax_temp1)+length(tmax_temp2) == 366) { 
        days = 366
    }

  print(days)

# Extract Mins

  jan_sep_min_temp <- tmin_temp2[,,93:days]
  oct_dec_min_temp <- tmin_temp1[,,1:92]
  min_temp <- c(tmin_temp2, tmin_temp2)

# Extract Maxs
  jan_sep_max_temp <- tmax_temp2[,,93:days]
  oct_dec_max_temp <- tmax_temp1[,,1:92]
  max_temp <- c(tmax_temp2, tmax_temp1)

# Combine to get a calendar year
  min_CY <- abind(jan_sep_min_temp, oct_dec_min_temp, rev.along=1)
  max_CY <- abind(jan_sep_max_temp, oct_dec_max_temp, rev.along=1)

# Convert from Kelvin to Fahrenheit
  minF_CY <- 9/5*(min_CY - 273.15) + 32
  maxF_CY <- 9/5*(max_CY - 273.15) + 32

# Reassemble into one netCDF per year that contains max and mins
  # path and file name, set dname
  ncpath <- #TBD
  ncname <- paste("CY",CY_Creating, sep = "_")  
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  # not sure if i need this: dname <- "tmp"  # note: tmp means temperature (not temporary)
  # define dimensions
  londim <- ncdim_def("lon","position_east",as.double(1:348)) 
  latdim <- ncdim_def("lat","position_north",as.double(1:327)) 
  mindim <- ncdim_def("min",tunits3,as.double(minF_CY))
  maxdim <- ncdim_def("max",tunits3,as.double(maxF_CY))

