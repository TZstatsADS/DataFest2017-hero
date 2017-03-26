# Library
library(fmsb)
library(readr)

topK = 10
destination = 100408

# Create data:
# expedia_data.cut=read_tsv("./data_cut.txt")
# expedia_dest=read_tsv("./dest.txt")
data = subset(expedia_data.cut, select = c("hotel_country","srch_destination_id", "hotel_id", "prop_starrating","distance_band","hist_price_band","popularity_band"))
expedia_data.cut$hotel_id <- as.numeric(as.character(expedia_data.cut$hotel_id))
hotel_list = unique(data)
booking_hotel = aggregate(is_booking ~ hotel_id + srch_destination_id, expedia_data.cut, sum)
hotel_list.merged = merge(hotel_list, booking_hotel, by = c("srch_destination_id", "hotel_id"), all.x = T)
write.csv(hotel_list.merged, file="data_hotel.csv", row.names = FALSE)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data = hotel_list.merged[hotel_list.merged$srch_destination_id == destination, ]
data = head(data[order(data$is_booking),], topK)
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
