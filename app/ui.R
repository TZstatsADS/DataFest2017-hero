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
library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")

## Load Data
# data_country = read.csv("data.country.csv",header=TRUE,as.is=T)
# data_hotel = read.csv("data.country.csv",header=TRUE,as.is=T)
# data_aggregate = read.csv("data_aggregate.csv",header=TRUE,as.is=T)
# data.region = read.csv("data.region.csv",header=TRUE,as.is=T)

## UI Function

ui<- navbarPage(
  
  ## link to css.file
  theme = "bootstrap2.css",
  
  ## Project Title
  "TRAVELLING WITH EXPEDIA",
  
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("TRAVELLING WITH EXPEDIA"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
  ),
  
  ## 2D Map tab
  tabPanel("2D Map",
           titlePanel("Popular Travel Destinations"),
           
           leafletOutput("mymap",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("2D Explorer"),
                         
                         radioButtons(inputId = "is_booking",
                                      label  = "Choose click/book",
                                      choices = c('click','book'),
                                      selected ='book'),
                         sliderInput(inputId = "month",
                                     label = "Select a month",
                                     value = 12, min =1, max =12),
                         sliderInput(inputId = "num_destination",
                                     label = "Top travel destinations",
                                     value = 20,min = 1,max = 50),
                         selectInput(inputId = "user_location_region",
                                     label  = "Select user region",
                                     choices = c('CA','NY', 'TX','FL','ON','IL','WA','NJ','BC','PA'),
                                     selected ='CA')
                         
           )
  ),
  
  ## end 2D Map tab
  
  ##Regional Findings tabset
             tabPanel("Regional Findings",
                      tabsetPanel(
                        
                        ##Continent & Region
                        tabPanel("Regional Statistics",
                                 titlePanel("Continent & Region"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput(inputId = "traveler",
                                                 label = "Select the number of travelers ",
                                                 value = 1, min =1, max =18),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("regional_import",height = "400px"),
                                     plotOutput("continent_import", height = "600px")
                                     
                                   )
                                   
                                 )
                        ),
                        
                        ## tree map
                        tabPanel("Popular Activities",
                                 titlePanel("Popular Activities in the Destination Country"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "country_name",
                                                 label  = "Select the destination country",
                                                 choices = c(' United States of America',' Canada', ' Italy',' United Kingdom',' Germany',' France',' Spain',' Mexico',' Australia',' Japan'),
                                                 selected =' United States of America'),
                                     #              sliderInput(
                                     #                inputId = "activity_number",
                                     #                label = "Select the number of activities",
                                     #                value = 1, min =1, max =50),
                                     sliderInput(inputId = "activity_number",
                                                 label = "Select the number of events",
                                                 value = 10,min = 1,max = 50),
                                     
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
                        ),
                        ### end Tree Map
                        
                        ### Radar Chart
                        tabPanel("Hotel Analysis",
                                 titlePanel("Top Hotels in a specific destination"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput(inputId = "destination",
                                                 label  = "Select the destination",
                                                 value = 100408, min =7997, max =196566132
                                     ),
                                     sliderInput(
                                       inputId = "topK",
                                       label = "Select a K",
                                       value = 10, min =1, max =30),
                                     width = 3
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("radarchart",width = "100%", height = 600),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                   draggable = TRUE, 
                                                   top = 600, left = 20, right = "auto", bottom = "auto",
                                                   width = 350, height = "auto",
                                                   plotOutput("radarPlot",width="100%",height="250px")
                                     )
                                   )
                                 )
                        )
                        ### end Radarchart
                        
                      )
             )
)