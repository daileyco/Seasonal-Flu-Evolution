# Spatial Data Processing


## Packages
library(sf)
library(dplyr)



## shapefiles from census
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

us.shape.state <- st_read("./01-Data/00-Raw-Data/Spatial/cb_2018_us_state_5m.shp")



## regional classifications
regions <- read.csv("./01-Data/00-Raw-Data/Spatial/Regional_Divisions.csv")





## add regions to shapefile
us.shape.state <- left_join(us.shape.state, regions, by = c("NAME"="State"))




## create subset for only contiguous US
# non.contig <- c(28,40,48,49,50,52,56)
non.contig <- which(us.shape.state$NAME%in%c("Hawaii", 
                                             "United States Virgin Islands", 
                                             "Guam", 
                                             "Commonwealth of the Northern Mariana Islands", 
                                             "American Samoa", 
                                             "Puerto Rico", 
                                             "Alaska"))
us.shape.state.contig <- us.shape.state[-non.contig,]










## save
save(us.shape.state,
     us.shape.state.contig,
     
     regions,
     
     file = "./01-Data/01-Processed-Data/spatial.rdata")



## clean environment
rm(list = ls())
gc()


