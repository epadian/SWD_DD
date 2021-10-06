library(ncdf4)

# adding in some words to commit
#run_tests = F


# wrapper for normal printf
printf <- function(...) invisible(print(sprintf(...)))

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

test_leap <- function() {
  leaps <- list(1904, 1908, 1912, 2000)
  noleap <- list(1900, 1901)
  for(y in leaps) {
    if(!leap(y)) {
      printf("%d should be a leap year", y)
      stop()
    }
  }
  for(y in noleap) {
    if(leap(y)) {
      printf("%d shouldn't be a leap year", y)
      stop()
    }
  }
}

ktof <- function(x) {
  y <- 9.0/5.0*(x - 273.15) + 32
  return(y)
}

test_ktof <- function() {
  input <- c(310.9278)
  want <- c(100.0)
  for(i in 1:length(input)) {
    got <- ktof(input[i])
    if(round(got, digits=0) != want[i]) {
      printf("got: %f, want: %f", got, want[i])
      stop()
    }
  }
}

if(run_tests) {
  test_leap()
  test_ktof()
  return(0)
}

# set for the wy data
#wy_path <- "."
wy_path <- "/scratch/leaf/wrf_30yr_daily"
# set to one, two or all years
years <- c(2016, 2017)

for(year in years) {
  current_year <- sprintf("%s/forcing_d02_wy%d_daily_summary.nc",wy_path, year)
  next_year <- sprintf("%s/forcing_d02_wy%d_daily_summary.nc",wy_path, year+1)
  printf("extracting data for %d", year)
  printf("jan-sep: %s", current_year)
  printf("oct-dec: %s",  next_year)
  # open this year and next year
  wy_jan_sep <- nc_open(current_year)
  wy_oct_dec <- nc_open(next_year)
  
  # TODO: these definitions are guesses
  xdim <- ncdim_def("x", "kilometers", 1:348)
  ydim <- ncdim_def("y", "kilometers", 1:327)
  t <- 365
  if(leap(year)) {
    t <- 366
  }
  tdim <- ncdim_def("time", sprintf("days since %d-01-01 00:00:00", year), 1:t)
  dims = list(xdim, ydim, tdim)
  tminvar <- ncvar_def("T2_min", "K", dims)
  tmaxvar <- ncvar_def("T2_max", "K", dims)
  
  nc <- nc_create(sprintf("%d.nc", year), list(tminvar, tmaxvar))
  
  for(v in c("T2_min", "T2_max")) {
    x <- ncvar_get(wy_jan_sep, v)
    # There is probably a better way to index here, but I'm unaware of it.
    # TODO: no clue if the 93 is right, needs to be checked.
    if(leap(year)) {
      x <- x[,,93:366]
    } else {
      x <- x[,,93:365]
    }
    y <- ncvar_get(wy_oct_dec, "T2_min")
    # TODO: check indices
    x <- append(x, y[,,1:92])
    # needed?
    x <- ktof(x)
    ncvar_put(nc, v, x)
  }
  nc_close(wy_jan_sep)
  nc_close(wy_oct_dec)
  nc_close(nc)
}
