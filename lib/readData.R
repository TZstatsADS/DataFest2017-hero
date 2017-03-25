if(!require("readr")) install.packages("readr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("DT")) install.packages("DT")
if(!require("lubridate")) install.packages("lubridate")
if(!require("sp")) install.packages("sp")
if(!require("rgdal")) install.packages("rgdal")
library(readr)
library(dplyr)
library(DT)
library(lubridate)
library(sp)
library(rgdal)

expedia_data=read_tsv("../data/data.txt")
expedia_dest=read_tsv("../data/dest.txt")
names(expedia_data)
dim(expedia_data)
length(unique(expedia_data$user_id))

datatable(sample_n(expedia_data, 10))
datatable(sample_n(expedia_dest, 10))
barplot(table(expedia_data$site_name))

range(expedia_data$date_time)

barplot(table(wday(expedia_data$date_time)))
barplot(table(hour(expedia_data$date_time)))

subset_data=sample_n(expedia_data, 5000)
#make a data frame
coords <- as.data.frame(cbind(as.numeric(subset_data$user_location_longitude),
                              as.numeric(subset_data$user_location_latitude)))
#and into Spatial
coords=coords[complete.cases(coords), ]
points <- SpatialPoints(coords)
#SpatialPolygonDataFrame - I'm using a shapefile of UK counties
worldmap <- readOGR(path.expand("../data/worldmap/"), "TM_WORLD_BORDERS-0.3")
#assume same proj as shapefile!
proj4string(points) <- proj4string(worldmap)
#get county polygon point is in
result <- as.character(over(points, worldmap)$NAME)
table(result)