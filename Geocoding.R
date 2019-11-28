### PACKAGES ###
################

library(tidyverse)   # data manipulation
library(jsonlite)    # JSON

### FUNCTIONS ###
#################

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'https://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

### Load Sample Data ###
sample_addresses <- read.csv("sample_addresses.csv") %>%
                    mutate(lon = 0,
                           lat = 0)

### Add Geocoding ###
geo_coded_addresses <- c()

for (i in 1:dim(sample_addresses)[1]) {
  print(i)
  long_lat <- nominatim_osm(paste0(sample_addresses$location2[i],", Toronto, ON"))
  Sys.sleep(1)  # ensure 1 second between API call as per OSM guidelines
  if (dim(long_lat)[1] != 0) {
    sample_addresses$lon[i] = long_lat$lon
    sample_addresses$lat[i] = long_lat$lat
  }
}

