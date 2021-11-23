library(ncdf4)
library(ggplot2)

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

#DD_path <- "/scratch/leaf/Padian"
DD_path <- "~/Desktop/ISDA_Code_2021/DD_SWD"
# set to one, two or all years
years <- c(2014)

### Make a line graph of the average DOY that 261 occurs across each year

for(year in years) {
  print(year)
  file_path <- sprintf("%s/%dCSGDD.nc",DD_path, year)
  year_temp <- nc_open(file_path)
  CSGDD_temp <- ncvar_get(year_temp, "Cumulative Summation GDD")
  
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  
  year_dim <- as.numeric(length(years))
  threshold <- 261
  loc_1 <- 1
  loc_2 <- 1
  i <- 1
  DOY261 <- array(0,c(348,327,1))
  averages_261 <- array(0,c(year_dim,2))
  
  for(loc_1 in seq(from=1, to=348, by=1)) {
    for(loc_2 in seq(from=1, to=327, by=1)) {
      cumsum_temp <- CSGDD_temp[loc_1,loc_2, 1:t]
      DOY_temp <- as.numeric((which(cumsum_temp>=261)[1]))
      DOY261[loc_1,loc_2,1] <- DOY_temp
      avg_261 <- colMeans(DOY261, na.rm=TRUE, dims=2)
      averages_261[i,1] <- year
      averages_261[i,2] <- avg_261
      
    }
  }
}    
  
png(filename=sprintf("%s/%d_avg%d.png",DD_path, year, threshold), 
        width = 480, height = 480)

plot(averages_261[,1], averages_261[,2],
     col=red)
