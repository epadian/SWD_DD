# This script pulls in XXXXCSGDD.nc files located on R2 @ login
# /scratch/leaf/Padian adn creates various figures.

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

for(year in years) {
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
  file_path2 <- sprintf("%s/%dlines.jpg",DD_path, year)
  
  png(file_path2)
  
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
  file_path2 <- sprintf("%s/%dlines.png",DD_path, year)
  ggsave(file_path2, plot=last_plot())
  dev.off()
}
