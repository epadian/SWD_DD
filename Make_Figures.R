# This script pulls in XXXXCSGDD.nc files located on R2 @ login
# /scratch/leaf/Padian and creates various figures.

# Libraries needed
library(ncdf4)
library(ggplot2)

# Custom function used to determine how many days in the year
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

# Paths for local computer and R2 
#DD_path <- "/scratch/leaf/Padian"
DD_path <- "~/Desktop/ISDA_Code_2021/DD_SWD"
# Years to perform calculations on (example: c(1987,1988...,2016))
years <- c(2014)

for(year in years) {
  print("Creating all lines plot for")
  print(year)
  file_path <- sprintf("%s/%dCSGDD.nc",DD_path, year)
  year_temp <- nc_open(file_path)
  CSGDD_temp <- ncvar_get(year_temp, "Cumulative Summation GDD")
  
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  
  a <- 1
  b <- 1
  
  #file_path2 <- sprintf("%s/%dlines_jpgtest.jpg",DD_path, year)
  #jpeg(filename=sprintf("%s/%dlines_COME_ON.jpg",DD_path, year), 
     # width = 480, height = 480)
  #png(filename=sprintf("%s/%dall_lines.png",DD_path, year), 
  #     width = 480, height = 480)
  
  pdf(file=sprintf("%s/%dall_lines.pdf",DD_path, year))
  
  plot(year_temp$dim$t$vals, CSGDD_temp[a,b,1:t], 
       main = "all locations", 
       sub = year, 
       xlab='Date', 
       ylab='Cumulative Degree Days',
       type='l', col='red')
  
  for(a in seq(from=2, to=348, by=1)) {
    for(b in seq(from=2, to=327, by=1)) {
      lines(year_temp$dim$t$vals, CSGDD_temp[a,b,1:t], col='red')
    }
  }
  dev.off()
}


# This plot is a threshold DOY for the whole domain

# Plot Day of Year that 261 Degree Days is met each year

for(year in years) {
  print("Creating a map of 261 DD theshold being met for")
  print(year)
  file_path <- sprintf("%s/%dCSGDD.nc",DD_path, year)
  year_temp <- nc_open(file_path)
  CSGDD_temp <- ncvar_get(year_temp, "Cumulative Summation GDD")
  
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  
  loc_1 <- 1
  loc_2 <- 1
  DOY261 <- array(0,c(348,327,1))
  
  for(loc_1 in seq(from=1, to=348, by=1)) {
    for(loc_2 in seq(from=1, to=327, by=1)) {
      cumsum_temp <- CSGDD_temp[loc_1,loc_2, 1:t]
      DOY_temp <- as.numeric((which(cumsum_temp>=261)[1]))
      DOY261[loc_1,loc_2,1] <- DOY_temp
    }
  }
x_values <- 1:dim(DOY261)[1]
y_values <- 1:dim(DOY261)[2]
z_values <- DOY261[,,1]


threshold <- 261

png(filename=sprintf("%s/%d_thresh%d.png",DD_path, year, threshold),
    width=480, height=480)

image(x_values,y_values,z_values,
      xlab = "East-West",
      ylab = "North-South")

dev.off()

}
