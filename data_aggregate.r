setwd("~/DataFest2017-hero/data/")

# load packages
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

# read data
expedia_data.cut=read_tsv("./data_cut.txt")
expedia_dest=read_tsv("./dest.txt")

expedia_data.cut$month = month(expedia_data.cut$date_time)

expedia_data.usa = expedia_data.cut[expedia_data.cut$hotel_country == "UNITED STATES OF AMERICA",]
expedia_data.other = expedia_data.cut[!(expedia_data.cut$hotel_country %in% c("UNITED STATES OF AMERICA","CANADA")),]
expedia_data.canada = expedia_data.cut[expedia_data.cut$hotel_country == "CANADA",]

data.aggre = data.frame(aggregate(is_booking ~ user_location_region + hotel_country + month, expedia_data.other, sum))
colnames(data.aggre) = c("user_location_region", "srch_destination", "month", "srch_count")
data.aggre$is_booking = 1
temp = data.frame(aggregate(is_booking ~ user_location_region + hotel_country + month, expedia_data.other, length))
colnames(temp) = c("user_location_region", "srch_destination", "month", "srch_count")
temp$is_booking = 0
data.aggre = bind_rows(data.aggre, temp)

expedia_dest.usa = expedia_dest.cut[grep("United States of America", expedia_dest.cut$srch_destination_name), ]
expedia_dest.usa$region = NA
for (i in 1: length(expedia_dest.usa$region)){
  tmp_str = strsplit(expedia_dest.usa$srch_destination_name[i], ',')[[1]]
  expedia_dest.usa$region[i] = trimws(tmp_str[length(tmp_str)-1],"l")
}

m <- as.matrix(expedia_dest.usa)
m[m == "Texas"] <- "TX"
m[m == "New Mexico"] <- "NM"
m[m == "South Dakota"] <- "ND"
m[m == "Georgia"] <- "GA"
m[m == "Massachusetts"] <- "MA"
m[m == "California"] <- "CA"
m[m == "Nebraska"] <- "NE"
m[m == "Indiana"] <- "IN"
m[m == "New Jersey"] <- "NJ"
m[m == "Iowa"] <- "IA"
m[m == "Alaska"] <- "AK"
m[m == "Florida"] <- "FL"
m[m == "Michigan"] <- "MI"
m[m == "New York"] <- "NY"
m[m == "Oregon"] <- "OR"
m[m == "Wisconsin"] <- "WI"
m[m == "Maine"] <- "ME"
m[m == "North Carolina"] <- "NC"
m[m == "Pennsylvania"] <- "PA"
m[m == "Rhode Island"] <- "RI"
m[m == "Montana"] <- "MT"
m[m == "Illinois"] <- "IL"
m[m == "Tennessee"] <- "TN"
m[m == "Vermont"] <- "VT"
m[m == "Maryland"] <- "MD"
m[m == "South Carolina"] <- "SC"
m[m == "Utah"] <- "UT"
m[m == "Colorado"] <- "CO"
m[m == "Missouri"] <- "MO"
m[m == "West Virginia"] <- "WV"
m[m == "Ohio"] <- "OH"
m[m == "Washington"] <- "WA"
m[m == "Wyoming"] <- "WY"
m[m == "Idaho"] <- "ID"
m[m == "Kansas"] <- "KS"
m[m == "Alabama"] <- "AL"
m[m == "North Dakota"] <- "ND"
m[m == "Minnesota"] <- "MN"
m[m == "Nevada"] <- "NV"
m[m == "Louisiana"] <- "LA"
m[m == "Arizona"] <- "AZ"
m[m == "Mississippi"] <- "MS"
m[m == "Hawaii"] <- "HI"
m[m == "Arkansas"] <- "AR"
m[m == "Oklahoma"] <- "OK"
m[m == "Okalhoma"] <- "OK"
m[m == "New Hampshire"] <- "NH"
m[m == "Kentucky"] <- "KY"
m[m == "Virginia"] <- "VA"
m[m == "District of Columbia"] <- "DC"
m[m == "Delaware"] <- "DE"
m[m == "Eastern Shore Virginia"] <- "VA"
m[m == "Northern Virginia"] <- "VA"
m[m == "Northwest Indiana"] <- "IN"
m[m == "Providence"] <- "NJ"
m[m == "Boothbay"] <- "ME"
m[m == "West Chester Ohio"] <- "OH"
m[m == "Connecticut"] <- "CT"
expedia_dest.usa <- as.data.frame(m)
expedia_data.usa <- merge(expedia_data.usa,expedia_dest.usa,by="srch_destination_id")

temp = data.frame(aggregate(is_booking ~  user_location_region + region + month, expedia_data.usa, length))
colnames(temp) = c("user_location_region", "srch_destination", "month", "srch_count")
temp$is_booking = 0
data.aggre = bind_rows(data.aggre, temp)
temp = data.frame(aggregate(is_booking ~ user_location_region + region + month, expedia_data.usa, sum))
colnames(temp) = c("user_location_region", "srch_destination", "month", "srch_count")
temp$is_booking = 1
data.aggre = bind_rows(data.aggre, temp)

#######canada#######
expedia_dest.canada = expedia_dest.cut[grep("Canada", expedia_dest.cut$srch_destination_name), ]
expedia_dest.canada$region = NA
for (i in 1: length(expedia_dest.canada$region)){
  tmp_str = strsplit(expedia_dest.canada$srch_destination_name[i], ',')[[1]]
  expedia_dest.canada$region[i] = trimws(tmp_str[length(tmp_str)-1],"l")
}
expedia_dest.canada = expedia_dest.canada[expedia_dest.canada$region != "New York",]
expedia_dest.canada = expedia_dest.canada[expedia_dest.canada$region != "California",]
m <- as.matrix(expedia_dest.canada)
m[m == "Ontario"] <- "ON"
m[m == "British Columbia"] <- "BC"
m[m == "Quebec"] <- "QC"
m[m == "Manitoba"] <- "MB"
m[m == "Yukon Territory"] <- "YT"
m[m == "Newfoundland and Labrador"] <- "NL"
m[m == "Northwest Territories"] <- "NT"
m[m == "Nunavut"] <- "NU"
m[m == "New Brunswick"] <- "NB"
m[m == "Alberta"] <- "AB"
m[m == "Saskatchewan"] <- "SK"
m[m == "Nova Scotia"] <- "NS"
m[m == "Prince Edward Island"] <- "PEI"
m[m == "Mont-Tremblant Quebec"] <- "QC"
expedia_dest.canada <- as.data.frame(m)
expedia_data.canada <- merge(expedia_data.canada,expedia_dest.canada,by="srch_destination_id")

###others####
expedia_dest.cut$country = NA
for (i in 1: length(expedia_dest.cut$country)){
  tmp_str = strsplit(expedia_dest.cut$srch_destination_name[i], ',')[[1]]
  expedia_dest.cut$country[i] = trimws(tmp_str[length(tmp_str)],"l")
}

temp = data.frame(aggregate(is_booking ~  user_location_region + region + month, expedia_data.canada, length))
colnames(temp) = c("user_location_region", "srch_destination", "month", "srch_count")
temp$is_booking = 0
data.aggre = bind_rows(data.aggre, temp)
temp = data.frame(aggregate(is_booking ~ user_location_region + region + month, expedia_data.canada, sum))
colnames(temp) = c("user_location_region", "srch_destination", "month", "srch_count")
temp$is_booking = 1
data.aggre = bind_rows(data.aggre, temp)

expedia_dest.usa$srch_destination_latitude <- as.numeric(expedia_dest.usa$srch_destination_latitude) 
expedia_dest.usa$srch_destination_longitude <- as.numeric(expedia_dest.usa$srch_destination_longitude) 

usa.latitude <- aggregate(srch_destination_latitude ~ region, expedia_dest.usa, median)
usa.longtude <- aggregate(srch_destination_longitude ~ region, expedia_dest.usa, median)

expedia_dest.canada$srch_destination_latitude <- as.numeric(expedia_dest.canada$srch_destination_latitude) 
expedia_dest.canada$srch_destination_longitude <- as.numeric(expedia_dest.canada$srch_destination_longitude) 

canada.latitude <- aggregate(srch_destination_latitude ~ region, expedia_dest.canada, median)
canada.longtude <- aggregate(srch_destination_longitude ~ region, expedia_dest.canada, median)

others.latitude <- aggregate(srch_destination_latitude ~ country, expedia_dest.cut, median)
others.longtude <- aggregate(srch_destination_longitude ~ country, expedia_dest.cut, median)

colnames(others.latitude) <- c("region", "srch_destination_latitude")
colnames(others.longtude) <- c("region","srch_destination_longitude")

latitude <- rbind(usa.latitude, canada.latitude, others.latitude)
longtitude <- rbind(usa.longtude, canada.longtude, others.longtude)
locations <- merge(latitude, longtitude, by="region")

mergedata <- merge(data.aggre, locations, by.x = "user_location_region", by.y = "region")
colnames(mergedata)[6:7] = c("user_location_latitude","user_location_longitude")

mergedata <- merge(mergedata, locations, by.x = "srch_destination", by.y = "region")
colnames(mergedata)[8:9] = c("latitude","longitude")

write.csv(mergedata, file="data_aggregate.csv", row.names = FALSE, col.names = TRUE)

testdata = read.csv("./data_aggregate.csv", header = T)
View(testdata)