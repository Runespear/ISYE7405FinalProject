rm(list=ls())

# Load Necessry Packages, use require("pacman") if you don't have it
library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate","maps","leaps","matrixStats","ggplot2","tmap")
p_load("sf","raster","dplyr","spData","tmap","leaflet","tidyverse","maps","mapview")
p_load("spDataLarge","htmltools")
webshot::install_phantomjs()
# Set path to data folder
setwd(here())
DATAFOLDERPATH = "./data"
OUTPUTFOLDERPATH = file.path(DATAFOLDERPATH,"output")
CHUNKSFOLDERPATH = file.path(DATAFOLDERPATH,"chunks")

# Create folder for output
dir.create("./data/output",showWarnings=FALSE)


# Read Data
TaxiStandP = file.path(CHUNKSFOLDERPATH,"STAND_SUMMARY.csv")
TaxiStand = read_csv(file = TaxiStandP)
Z=rep(97,63)
Data <- structure(list(Name = structure(1:63, .Label = paste0("T",sprintf("%s",1:63)), 
                                        class = "factor"), 
                       Latitude = as.numeric(unlist(TaxiStand$Latitude)), 
                       Longitude = as.numeric(unlist(TaxiStand$Longitude)), 
                       Altitude = as.numeric(unlist(Z))), 
                  .Names = c("Name", "Latitude", "Longitude", "Altitude"), 
                  class = "data.frame", row.names = c(NA, -63L))


rawdata=Data

Data$lat <- jitter(Data$Latitude, factor = 0.0001)
Data$lon <- jitter(Data$Longitude, factor = 0.0001)
coordinates(Data) <- ~ lon + lat
proj4string(Data) <- "+init=epsg:4326"
#m=mapview::mapview(Data,popup="Name") 
m <-leaflet(Data)
m <-addTiles(m)
#########################################################################
# Total Trips in Red
#########################################################################
mc <-addCircleMarkers(m,radius=2 + 0.002*TaxiStand$TotalRides/min(TaxiStand$TotalRides),color='red')
palette <- colorNumeric(
  palette = "YlGnBu",
  domain = TaxiStand$TotalTime
)
my_title <- tags$p(tags$style("p {color: red; font-size:22px}"),
                   tags$b("Total Trips"))

mc<-mc %>% addControl(my_title, position = "bottomleft" )
mapshot(mc, file = file.path(OUTPUTFOLDERPATH,"totaltrips.png"))
#########################################################################
# Total Time Taken in blue
##########################################################################
mk <- addCircleMarkers(m,radius=2 + 0.002*TaxiStand$TotalTime/min(TaxiStand$TotalTime),color='blue')
my_title <- tags$p(tags$style("p {color: blue; font-size:22px}"),
                   tags$b("Total Time"))
mk<-mk %>% addControl(my_title, position = "bottomleft" )
mapshot(mk, file = file.path(OUTPUTFOLDERPATH,"totaltime.png"))
##########################################################################
# Average Time per trip in green
##########################################################################
mt <- addCircleMarkers(m,radius=2 + TaxiStand$AVG_TRIP_TIME/min(TaxiStand$AVG_TRIP_TIME),color='purple')
my_title <- tags$p(tags$style("p {color: purple; font-size:22px}"),
                   tags$b("Average Time Per Trip"))
mt<-mt %>% addControl(my_title, position = "bottomleft" )
mapshot(mt, file = file.path(OUTPUTFOLDERPATH,"avgtriptime.png"))

