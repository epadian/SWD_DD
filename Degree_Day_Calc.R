# This script is meant to be run on R2 using the YYYY.nc files located in
# scratch/LEAF/Padian to calculate the custom GDD at each location for each
# year. The resulting file should be a [348,327,366] where 348 is the x or 
# longitude direction, 327 is the y or latitude direction, and 366 is one
# one column with daily GDD values at each location for that year. The output
# file should be located on R2 in scratch/LEAF/Padian/DD. A cumulative
# summation script will follow. 

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

leap <- function(y) {
  if(y %% 400 == 0) {
    return(TRUE)
  }
  if(y %% 100 == 0) {
    return(FALSE)
  }
  if(y %% 4 == 0) {
    return(TRUE)
  }
  return(FALSE)
}



DD_path <- "/scratch/leaf/Padian"
#DD_path <- "~/Desktop/ISDA_Code_2021/DD_SWD"
# set to one, two or all years
years <- c(2012,2013,2014)


for(year in years) {
  print(year)
  file_path <- sprintf("%s/%d.nc",DD_path, year)
  year_temp <- nc_open(file_path)
  
  maxs <- ncvar_get(year_temp, "T2_max")
  mins <- ncvar_get(year_temp, "T2_min")

  DD_temp <- mapply(DD_S, mins, maxs)
  DD_temp2 <- array(DD_temp, dim=dim(maxs))
  print(dim(DD_temp2))
  
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  
  tdim <- ncdim_def("Calendar Day", sprintf("days since %d-01-01 00:00:00", year), 1:t)
  xdim <- ncdim_def("x", "kilometers-ish", 1:348)
  ydim <- ncdim_def("y", "kilometers-ish", 1:327)
  dims = list(xdim, ydim, tdim)
  DD_var <- ncvar_def("DD", "GDD (unitless)", dims)
 
  GDD <- nc_create(sprintf("%dGDD.nc", year), list(DD_var))
  ncvar_put(GDD, DD_var, DD_temp2)
  nc_close(GDD)
}

###### Cumulative Summation ######



for(year in years) {
  print(year)
  file_path <- sprintf("%s/%dGDD.nc",DD_path, year)
  GDD_nc_temp <- nc_open(file_path)
  
  GDD_temp <- ncvar_get(GDD_nc_temp, "DD")
  
  CS_GDD_temp <- apply(GDD_temp, c(1,2), cumsum)
    # ^^ This produces [365 348 327]
  #CS_GDD_temp <- mapply(cumsum, GDD_temp) # I TRIED
  #CS_GDD_temp2 <- array(CS_GDD_temp, dim=dim(GDD_temp)) # YOU WIN
  #dim(CS_GDD_temp2)
  
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  
  tdim <- ncdim_def("Calendar Day", sprintf("days since %d-01-01 00:00:00", year), 1:t)
  xdim <- ncdim_def("x", "kilometers-ish - xlat", 1:348)
  ydim <- ncdim_def("y", "kilometers-ish- ylat", 1:327)
  dims = list(tdim, xdim, ydim)
  CDD_var <- ncvar_def("Cumulative Summation GDD", "Cumulative GDD (unitless)", dims)
  
  CS_GDD <- nc_create(sprintf("%dCGDD.nc", year), list(CDD_var))
  ncvar_put(CS_GDD, CDD_var, CS_GDD_temp)
  nc_close(CS_GDD)
}
