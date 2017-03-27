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


#Load the data
# data_country = read.csv("data.country.csv",header=TRUE,as.is=T)
# data_hotel = read.csv("data.country.csv",header=TRUE,as.is=T)
# data_aggregate = read.csv("data_aggregate.csv",header=TRUE,as.is=T)
# data.region = read.csv("data.region.csv",header=TRUE,as.is=T)

## map creation preprocess
data(wrld_simpl) # Basic country shapes
bgcolor = "#000000"
arc_colors = c("#ffdbdb","#c4e0ff","#e8fff0","#ffe9bf","pink","orange")
map_pal = data.frame(AnnualAggregate = c("red"),Chocolate = c("blue"),Coffee = c("green"),COCOA = c("#ffe9bf"),Spices = c("pink"),Tea = c("orange"))
names(map_pal)[1] = "Annual Aggregate"
## end preprocess map

## Server function

server<- function(input, output){
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project looks into Expedia User's Travelling preference<br/><br/><br/><br/>Group Member: Ji Shen, Minghao Dong, Shiqi Duan, Dawei Zhao, Peiran Fang")
  })
  
  ## ggplot
  output$ggplot <- renderPlot({
    
    ##### subset dataframe
    temp = data_country
    temp = subset(temp,Country == as.character(input$country_name))
    temp = arrange(temp,desc(Popular))[1:input$activity_number,]
    temp$Popular<-temp$Popular/sum(temp$Popular)
    colnames(temp)
    maxValue = log(max(temp$Popular))
    ##### end subset
    
    g = ggplot(data = temp, aes(x = Activity, y = Popular))+
      labs(x="Events")+
      geom_bar(stat='identity',position = "dodge")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
      theme(legend.position="none") + 
      theme(legend.background = element_rect(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + geom_bar(stat = "identity", aes(fill=temp$Popular)) + scale_fill_gradient(low = "#0033FF", high = "#AB00FF") + scale_x_discrete(limits = temp$Activity) 
    g
    #print(g,vp = viewport(angle=-90))
    
  })
  ## end ggplot
  
  ## 2D map
  output$mymap <- renderLeaflet({
    ## Control Icon size and looks
    levelIcon <- iconList(
      level1 = makeIcon("trade-icon_1.png", iconAnchorX = 19, iconAnchorY = 19),
      level2 = makeIcon("trade-icon_2.png", iconAnchorX = 19, iconAnchorY = 19),
      level3 = makeIcon("trade-icon_3.png", iconAnchorX = 19, iconAnchorY = 19),
      level4 = makeIcon("trade-icon_4.png", iconAnchorX = 19, iconAnchorY = 19),
      level5 = makeIcon("trade-icon_5.png", iconAnchorX = 19, iconAnchorY = 19),
      level6 = makeIcon("trade-icon_6.png", iconAnchorX = 19, iconAnchorY = 19),
      level7 = makeIcon("trade-icon_7.png", iconAnchorX = 19, iconAnchorY = 19),
      level8 = makeIcon("trade-icon_8.png", iconAnchorX = 19, iconAnchorY = 19)
    )
    Icon = makeIcon(iconAnchorX = 19, iconAnchorY = 19,
                    iconWidth = 38, iconHeight = 38)
    ## subset the data
    US = data.frame(Country = "US",longitude = -95.71289,latitude = 37.09024)
    ##### subset dataframe
    tmp = data_aggregate
    tmp = subset(tmp,user_location_region == as.character(input$user_location_region))
    tmp = subset(tmp,month == as.integer(input$month))
    tmp = subset(tmp,is_booking == as.numeric(as.character(input$is_booking) == 'book'))
    tmp = arrange(tmp,desc(srch_count))[1:50,]
    min = min(tmp$srch_count, na.rm = TRUE)
    tmp = tmp[1:input$num_destination,]
    rank = 1:nrow(tmp)
    max = max(tmp$srch_count, na.rm = TRUE)
    
    Log = paste("level",floor(log((tmp$srch_count) - min + 1, base =1.0001)/log(max - min + 1, base =1.0001) * 7 + 1),sep = "")
    tmp$rank = paste(tmp$srch_destination,"<br/>",
                     "ranks No.",rank,"<br/>",
                     "count",tmp$srch_count,"<br/>",sep = "",
                     "<a href='https://en.wikipedia.org/wiki/",tmp$srch_destination,"'>Wikipedia Page</a>","<br/>",
                     "<a href='https://www.expedia.com/search-results?q=",tmp$srch_destination,"&_xpid=11905%7C1&rfrr=mercury'>Expedia Discover Page</a>"
    )
    index = match(input$user_location_region,c('CA','NY','TX','FL','ON','IL','WA','NJ','BC','PA'))
    Colors = c("#231d65","#276d98","#2586a4","#3c6049","#216957","#4abf8c","#9eae1e","#eff09e")
    Labels = paste("Level:",1:8)
    ##### end subset      
    leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
      addMarkers(popup=~rank,icon = ~levelIcon[Log])%>%
      addMarkers(data = US, 
                 popup=~Country,icon = ~Icon)%>%  
      setView(lng=-30,lat=28,zoom=2)%>%#put US in the centre
      addLegend("topright", colors = Colors, labels = Labels,
                title = "Search Frequency <br/>From Small to Large",
                opacity = 1)
  })
  ## end 2D map
  
  ## Hotel Analysis
  output$radarchart<-renderPlot({
    #select a destination and a topK
    data = data_hotel[data_hotel$srch_destination_id == as.integer(input$destination), ]
    data = head(data[order(data$is_booking),], as.integer(input$topK))
    m = as.matrix(data)
    m[m == "VC"] = 5
    m[m == "C"] = 4
    m[m == "M"] = 3
    m[m == "F"] = 2
    m[m == "VF"] = 1
    m[m == "VL"] = 5
    m[m == "L"] = 4
    m[m == "H"] = 2
    m[m == "VH"] = 1
    data = as.data.frame(m)
    data = data[complete.cases(data),]
    data_tmp=subset(data, select = c("prop_starrating","distance_band","hist_price_band","popularity_band"))
    data_tmp$prop_starrating <- as.integer(data_tmp$prop_starrating)
    data_tmp$distance_band <- as.integer(data_tmp$distance_band)
    data_tmp$hist_price_band <- as.integer(data_tmp$hist_price_band)
    data_tmp$popularity_band <- as.integer(data_tmp$popularity_band)
    data_mean = colMeans(data_tmp)
    tmp=rbind(rep(5,4), rep(0,4))
    colnames(tmp) <- c("prop_starrating","distance_band","hist_price_band","popularity_band")
    data_mean = as.data.frame(rbind(tmp, data_mean))
    
    colnames(data_mean) <- c("starrating","distance","hist_price","popularity")
    
    # Custom the radarChart !
    radarchart( data_mean  , axistype=1 , 
                
                #custom polygon
                pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=0:5, cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                seg = 5
    )
  })
  ## end Hotel Analysis
  
  ## Tree Map
  output$treemap<-renderPlot({
    #selcet a year and a one of the five categories
    sub_country<-data_country[data_country$Country==input$country_name,]
    sub_country<-sub_country[1:input$activity_number,]
    sub_country$label<-paste(sub_country$Activity,", ",sub_country$Type,", ",round(100*sub_country$Popular/sum(sub_country$Popular),2),"%",sep="")
    treemap(sub_country, index='label', vSize="Popular", vColor="Type", type="categorical", palette="RdYlBu",aspRatio=30/30,drop.unused.levels = FALSE, position.legend="none")
  })
  ## end Tree Map
  
  ## continent analysis
  output$continent_import <- renderPlot({
    
    data.region$Value<-as.numeric(data.region$Value)
    title <- paste("Where",input$traveler,"Traveler(s) Like to Go",sep = " ")
    temp <- filter(data.region, data.region$Traveler == input$traveler)
    temp_1<-aggregate(Value ~ Region, temp, sum)
    pie(temp_1$Value, labels = temp_1$Region,main=title)
  })
  ## continent analysis
  
  ## Regional analysis
  output$regional_import <- renderPlotly({
    temp <- filter(data.region,
                   data.region$Traveler == input$traveler)
    
    p <- plot_ly()
    
    for (i in  unique (temp$Region)){
      temp_1 <- filter(temp , temp$Region ==i)
      temp_1$value <- as.numeric(temp_1$Value)
      temp_1 <- group_by(temp_1,Month)%>% summarise(value = log10(sum(value)))
      p <- add_trace(p, x = temp_1$Month, y = temp_1$value, mode = "lines+markers", name = i)
      
    }
    p
  })
  ## Regional analysis
  
  }
