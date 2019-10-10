# AIS-NetCDF
A guide to merging AIS data with NetCDF data in R.

The metorological in data NetCDF format can be downloaded from [Copernicus](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form) to a NetCDF of ´.nc´ file, either via filling out the form, or using the API (see [this page](https://cds.climate.copernicus.eu/api-how-to)). The following script in R shows how to merge AIS data with the met. data:

```
library(vroom)
library(dplyr)
library(lubridate)
library(ncdf4)
```
Load the AIS data, as done with a small sample below:
```
ais_sample <- vroom("ais_sample.csv") # if you have lots of observations (millions+), vroom is recommended as it works much faster than read.csv()
```

The AIS data is prepared for merging with met. data:
```
ais_dplyr <- ais_sample %>% 
  dplyr::select(-1) %>% # removes column with row numbers in sample data
  mutate(TIME = round_date(TIME, "hour")) %>%  # arbitrary rounding, could be done better
  mutate(LONGITUDE = ifelse(LONGITUDE < 0, LONGITUDE + 360, LONGITUDE)) %>% # as west is everything from 0 to -180 degrees
  group_by(VESSEL_UID) %>% # groups observations by ship (note that there's only one ship in the sample data)
  arrange(., TIME, .by_group = T) %>% # order chronologically (grouped)
  filter(year(TIME) == 2013) %>% # select only observations from 2013, for which we have met. data
  complete(TIME = seq.POSIXt(from = as.POSIXct(min(TIME)), 
                             to = as.POSIXct(max(TIME)), 
                             by="hour")) %>%  # creates rows for hours with lack of AIS reports (in order to match with met. data)
  fill(LATITUDE, LONGITUDE) %>% # fills in missing LON and LAT from the neighbouring row (upwards). One should look for better alternatives with interpolation
  ungroup()
```

Then load the met. data via the file `weather_sample.nc` attached. Printing the variable shows the structure of the file containing met. data.
```
# import .nc-file
ncfile <- nc_open("weather_sample.nc")
ncfile
```

The dimensions are extracted:

```

# space
lat <-  ncvar_get(ncfile, "latitude") # retrieves latitudes in NetCDF file
lon <-  ncvar_get(ncfile, "longitude") # retrieves longitudes in NetCDF file

# time
origin <- ymd_hms("1900-01-01 00:00:00", tz = "UTC") # time is hours since origin
time <- ncvar_get(ncfile, "time") # retrieves timestamps in NetCDF file
time <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")+as.difftime(time,units="hours") # translates to UTC
```

The instances for which both AIS and met. data are present are identified. Note that this will only be a few, as the sample files are quite small.
```
time_df <- data.frame(TIME = as.POSIXct(time)) # in order for inner_join (below on the next line) to work
ais_df <- (inner_join(ais_dplyr, time_df, by = "TIME")) # keeps only rows with both AIS and weather data
```

As the met. data variables are extraxted from the `.nc` file based on the indices, a few functions are made to match latitudes, longitudes and timestamps with the right indices in the `.nc` file:
```
get_lon <- function(LON){
  LON = LON[[1]] # retreives value from data table
  
  ais_lon = round(LON / 0.5) * 0.5 # rounds to match the met. data grid of .5 degrees
  
  ais_lon <- ifelse(ais_lon == 360, 0, ais_lon) # 360 degrees is the same as 0 degrees (greenwich) in the NetCDF files. Only 0 degrees is used, not 360)
  
  lon_ind = which(lon == ais_lon) # retreives the index in the NetCDF file that matches the longitude from the AIS data
  
  return(lon_ind)
}

get_lat <- function(LAT){ # similar to longitude
  LAT = LAT[[1]]
  
  ais_lat = round(LAT / 0.5) * 0.5

  lat_ind = which(lat == ais_lat)
  
  return(lat_ind)
}


get_time <- function(TIME){ # same here. No rounding
  ais_time = TIME[[1]]
  
  time_ind = which(time == ais_time)
  
  return(time_ind)
}

# this retrieves single met. variable for one observation
ncvar_get(ncfile,
          "swh", # the met. variable, arbitrarily chosen
          start = c(get_lon(ais_df[1,4]),
                    get_lat(ais_df[1,3]),
                     get_time(ais_df[1,2])), 
           count = c(1,1,1))
```

The `ncdf4::ncvar_get()` function can be used along with the indexing functions and the `dplyr::mutate()` function to retrieve met. data for all columns in a prepared AIS data set. This works pretty fast on much larger AIS data sets as well (100 000+ lines)
```
ais_dplyr_res <- ais_df %>% 
  rowwise() %>% # in order to retreive the met. variables by row
  mutate(swh = ncvar_get(ncfile, # retreives met. data for every obseration
                            "swh",  # "significant wave height" variable
                            start = c(get_lon(LONGITUDE), 
                                      get_lat(LATITUDE), 
                                      get_time(TIME)), 
                            count = c(1,1,1)))   %>%
  mutate(mwp = ncvar_get(ncfile, # another example with mwp ("mean wave period" variabel)
                         "mwp",
                         start = c(get_lon(LONGITUDE),
                                   get_lat(LATITUDE),
                                   get_time(TIME)),
                         count = c(1,1,1)))


```

