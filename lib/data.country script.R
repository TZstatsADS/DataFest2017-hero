extract<-expedia_dest$srch_destination_name[1:10]
countryname<-vector(length=10)
#lst<-strsplit(extract[1], ",", fixed = T)
#lst
#length(lst)
#length(lst[[1]])

#str(lst)
for(i in (1:length(extract))){
  if(is.na(extract[i]==T)){
    countryname[i]<-NA
  }
  else{
    lst<-strsplit(extract[i], ",", fixed = T)
    countryname[i]<-lst[[1]][length(lst[[1]])]
  }
}


expedia_dest$country<- vector(length= length(expedia_dest$srch_destination_name))
for(i in (1:length(expedia_dest$srch_destination_name))){
  if(is.na(expedia_dest$srch_destination_name[i]==T)){
    expedia_dest$country[i]<-NA
  }
  else{
    lst<-strsplit(expedia_dest$srch_destination_name[i], ",", fixed = T)
    expedia_dest$country[i]<-lst[[1]][length(lst[[1]])]
  }
}

expedia_dest$country2<- vector(length= length(expedia_dest$srch_destination_name))
for(i in (1:length(expedia_dest$country))){
  if(is.na(expedia_dest$country[i]==T)){
    expedia_dest$country2[i]<-NA
  }
  else{
    lst<-strsplit(expedia_dest$country[i], "(", fixed = T)
    expedia_dest$country2[i]<-lst[[1]][1]
  }
}


expedia_dest$country2[expedia_dest$country2==" United States "]<-" United States of America"
top10 = head(sort(table(expedia_dest$country2), decreasing = TRUE), n=10)
top.country<-names(top10)
activity.type<-NULL
activity.names<-NULL
for(i in 6:144){
  activity.names<-c(activity.names,strsplit(colnames(expedia_dest)[i],"_",fixed=T)[[1]][3])
  activity.type<-c(activity.type,strsplit(colnames(expedia_dest)[i],"_",fixed=T)[[1]][2])
}

data.country<-data.frame(rep(NA,1390),rep(NA,1390))
colnames(data.country)<-c("Activity","Type")

lcountry<-NULL
lpopular<-NULL
lactivity<-NULL
ltype<-NULL

for (country in top.country){
  lcountry<-c(lcountry,rep(country,139))
  popular.mean<-colMeans(10^(expedia_dest[expedia_dest$country2==country,6:144]))
  lpopular<-c(lpopular,sort(popular.mean,decreasing = TRUE))
  lactivity<-c(lactivity,activity.names[order(popular.mean,decreasing=TRUE)])
  ltype<-c(ltype,activity.type[order(popular.mean,decreasing=TRUE)])
}
data.country$Country<-lcountry
data.country$Popular<-lpopular
data.country$Activity<-lactivity
data.country$Type<-ltype
data.country$Type<-factor(data.country$Type)

#data.country<-expedia_dest[,c(6:144,146)]

treemap(data.country[data.country$Country==" United States of America",], index="Activity",vSize="Popular",vColor="Type")
library(treemap)





##########server.R#######
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

#expedia_data=read_tsv("./Downloads/ASADataFest2017/data.txt")
#expedia_dest=read_tsv("./Downloads/ASADataFest2017/dest.txt")
data.country = read.csv("/Users/duanshiqi/Desktop/data.country.csv",header=TRUE,as.is=T)
# expedia_data$month <- format(expedia_data$date_time, '%m')
# names(expedia_data)
# dim(expedia_data)
# length(unique(expedia_data$user_id))
# 
# datatable(sample_n(expedia_data, 10))
# datatable(sample_n(expedia_dest, 10))
# barplot(table(expedia_data$site_name))
# 
# range(expedia_data$date_time)
# 
# barplot(table(wday(expedia_data$date_time)))
# barplot(table(hour(expedia_data$date_time)))
# 
# subset_data=sample_n(expedia_data, 5000)
# #make a data frame
# coords <- as.data.frame(cbind(as.numeric(subset_data$user_location_longitude),
#                               as.numeric(subset_data$user_location_latitude)))
# #and into Spatial
# coords=coords[complete.cases(coords), ]
# points <- SpatialPoints(coords)
# #SpatialPolygonDataFrame - I'm using a shapefile of UK counties
# worldmap <- readOGR(path.expand("../data/worldmap/"), "TM_WORLD_BORDERS-0.3")
# #assume same proj as shapefile!
# proj4string(points) <- proj4string(worldmap)
# #get county polygon point is in
# result <- as.character(over(points, worldmap)$NAME)
# table(result)

shinyServer(function(input,output){
  
  ## Tree Map
  output$treemap<-renderPlot({
    #selcet a year and a one of the five categories
    sub_country<-data.country[data.country$Country==input$country_name,]
    sub_country<-sub_country[1:input$activity_number,]
    
    #sub_country[nrow(sub_country)+1,3:7]<-colSums(sub_country[,3:7])
    #for(i in 3:7){
    #sub_country[,i]<-sub_country[,i]/sub_country[nrow(sub_country),i]
    #}
    #sub_country<-sub_country[1:nrow(sub_country)-1,]
    
    sub_country$label<-paste(sub_country$Activity,", ",sub_country$Type,", ",round(100*sub_country$Popular),"%",sep="")
    treemap(sub_country, index='label', vSize="Popular", vColor="Type", type="categorical", palette="RdYlBu",aspRatio=30/30,drop.unused.levels = FALSE, position.legend="none")
  })
  ## end Tree Map
  
})



########ui.R #########
## Packages

packages.used <- 
  c("geosphere", # For spatial methods  
    "threejs",   # threejs is used for 3-D interactive Earth Visualization
    "rworldmap", # For creating earth map
    "leaflet",   # Leaflet for R provides functions to control and integrate Leaflet, a JavaScript library for interactive maps, within R.
    "rgeos",      # Provides functions for handling operations on topologies.
    "raster",     # For raster image
    "DT",         # For creating interactive tables
    "ggplot2",
    "sp"   ,       # For Spatial processing of data
    "ggmap",       # To reverse geocode Long/Lat
    "knitr",        # TO enable 3-D visualization embedding in the HTML page
    "rglwidget",
    "rgl",
    "plyr",
    "reshape2",
    "maptools",
    "shiny",
    "googleVis",
    "dplyr",
    "plotly",
    "RColorBrewer",
    "treemap",
    "gplots"
  )

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages
library("gplots")
library("plyr")
library("dplyr")
library("reshape2")
library("geosphere")
library("threejs")
library("rworldmap")
library("leaflet")
library("rgeos")
library("raster")
library("DT")
library("ggplot2")
library("sp")
library("ggmap")
library("knitr")
#library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")


### Tree Map
tabPanel("Popular Activities",
         titlePanel("Popular Activities in the Destination Country"),
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId = "Country_name",
                         label  = "Select the destination country",
                         choices = c(' United States of America',' Canada', ' Italy',' United Kingdom',' Germany',' France',' Spain',' Mexico',' Australia',' Japan'),
                         selected =' United States of America'),
             #              sliderInput(
             #                inputId = "activity_number",
             #                label = "Select the number of activities",
             #                value = 1, min =1, max =50),
             sliderInput(inputId = "activity_number",
                         label = "Select the number of activities",
                         value = 1,min = 1,max = 50),
             
             width = 3
           ),
           
           mainPanel(
             plotOutput("treemap",width = "100%", height = 600),
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, 
                           top = 600, left = 20, right = "auto", bottom = "auto",
                           width = 350, height = "auto",
                           plotOutput("ggplot",width="100%",height="250px")
             )
           )
         )
)
### end Tree Map

