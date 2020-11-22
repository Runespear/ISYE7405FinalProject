rm(list=ls())
install.packages("maps")
install.packages('leaps')
install.packages("matrixStats")
install.packages("ggplot2")
install.packages("tmap")
library(ggplot2)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

library(tidyverse)
library(maps)
# Load Necessry Packages, use require("pacman") if you don't have it
library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate")
# Set path to data folder
DATAFOLDERPATH = "./data/"

# Create folder for output
#dir.create("./data/output",showWarnings=FALSE)

install.packages("mapview")
library(mapview)
# Read Data
TaxiStandP = file.path(DATAFOLDERPATH,"chunks","STAND_LOCATIONS.csv")
TaxiStand = read_csv(file = TaxiStandP)
Z=rep(97,63)
Data <- structure(list(Name = structure(1:63, .Label = c("T1", "T2", "T3","T4","T5","T6","T7","T8","T9","T10",
                                                         "T11", "T12", "T13","T14","T15","T16","T17","T18","T19","T20",
                                                         "T21", "T22", "T23","T24","T25","T26","T27","T28","T29","T30",
                                                         "T31", "T32", "T33","T34","T35","T36","T37","T38","T39","T40",
                                                         "T41", "T42", "T43","T44","T45","T46","T47","T48","T49","T50",
                                                         "T51", "T52", "T53","T54","T55","T56","T57","T58","T59","T60","T61","T62","T63"), 
                                        class = "factor"), 
                       Latitude = as.numeric(unlist(TaxiStand[,3])), 
                       Longitude = as.numeric(unlist(TaxiStand[,2])), 
                       Altitude = as.numeric(unlist(Z))), 
                  .Names = c("Name", "Latitude", "Longitude", "Altitude"), 
                  class = "data.frame", row.names = c(NA, -63L))

#Data <- structure(list(Name = structure(1:3, .Label = c("M1", "M2", "M3"), 
#                                        class = "factor"), 
#                       Latitude = c(41.1485L, 41.1412L, 41.1447L), 
#                       Longitude = c(-8.5857L, -8.6140L, -8.6065L), 
#                       Altitude = c(97L, 97L, 108L)), 
#                  .Names = c("Name", "Latitude", "Longitude", "Altitude"), 
#                  class = "data.frame", row.names = c(NA, -3L))
rawdata=Data

Data$lat <- jitter(Data$Latitude, factor = 0.0001)
Data$lon <- jitter(Data$Longitude, factor = 0.0001)
coordinates(Data) <- ~ lon + lat
proj4string(Data) <- "+init=epsg:4326"
m=mapview(Data,popup="Name") 


m+leaflet() %>%addMarkers(~Longitude, ~Latitude, label=~Name, labelOptions=labelOptions(noHide=T),data=rawdata)
m@map %>%addMarkers(~Longitude, ~Latitude, label=~Name, labelOptions=labelOptions(noHide=F),data=rawdata)

