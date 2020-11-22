###############################################################
# Stratified Analysis of Taxi Stands and Weeks
# Predicting Taxi–Passenger Demand Using Streaming Data
# By Luis Moreira-Matias; João Gama; Michel Ferreira; João Mendes-Moreira; Luis Damas
# IEEE Transactions on Intelligent Transportation Systems 2013
# https://ieeexplore.ieee.org/document/6532415/authors#authors

#NOTE THAT STUDY PERIOD IS DIFFERENT, SO DIFFERENT TABLE II
###############################################################

# Load Necessry Packages, use require("pacman") if you don't have it
library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate","ggplot2","tidyverse","remotes")

# Set working directory to this script's location for RSTUDIO only
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(here())

# Start Clock 
ptm <- proc.time()

#################################################################
# Read NewTaxiData
#################################################################

# Set path to data folder
DATAFOLDERPATH = "./data"
OUTPUTFOLDERPATH = "./data/output"
CHUNKSFOLDERPATH = "./data/chunks"

# Create folder for output
dir.create("./data/output",showWarnings=FALSE)


# Read Data
NewDataXY <- read_csv(file.path(DATAFOLDERPATH,"NewDataXY.csv"))
# Drop non taxi stands
NewDataXY <- NewDataXY[which(!is.na(NewDataXY$ORIGIN_STAND)),]
Coordinates <- read_csv(file.path(CHUNKSFOLDERPATH,"STAND_LOCATIONS.csv"))

# Get the total number of trips for each stand
stand.summary <- aggregate(NewDataXY[,c("TotalTime","vones")],list(NewDataXY$ORIGIN_STAND),sum)
stand.summary <- stand.summary  %>% 
  rename(
    STAND = Group.1,  
    TotalRides = vones
  )
stand.summary$AVG_TRIP_TIME <- stand.summary$TotalTime/stand.summary$TotalRides
stand.summary <- merge(x=stand.summary,y=Coordinates,by="STAND")
stand.summary <- stand.summary  %>% 
  rename(
    Longitude = Xstart,  
    Latitude = Ystart
  )
#write_csv(stand.summary,file.path(CHUNKSFOLDERPATH,"STAND_SUMMARY.csv"))

#################################################################
# Calculate Waiting Times per stand
#################################################################

stand.lag <- NewDataXY %>%
  arrange(ORIGIN_STAND,TIMESTAMP) %>%
  group_by(ORIGIN_STAND) %>%
  mutate(WAIT = (TIMESTAMP - lag(TIMESTAMP))/60)

stand.lag <- drop_na(stand.lag,WAIT)
nr <- nrow(stand.lag)
n <- nr/6
k = split(stand.lag, rep(1:ceiling(nr/n), each=n, length.out=nr))
for (i in 1:6){
  fname = paste("WAITINGTIMES_part", toString(i),".csv" ,sep="")
  write_csv(k[[i]],file.path(CHUNKSFOLDERPATH,fname))
}


# Calculate Mean waiting times per stand
stand.lag.summary <- aggregate(stand.lag[,c("WAIT")],list(stand.lag$ORIGIN_STAND),mean)
stand.lag.summary <- stand.lag.summary  %>% 
  rename(
    STAND = Group.1
  )
stand.lag.summary <- merge(x=stand.summary,y=stand.lag.summary,by="STAND")

write_csv(stand.lag.summary,file.path(CHUNKSFOLDERPATH,"STAND_LAG_SUMMARY.csv"))


#stand.lag.summary <- stand.lag.summary  %>% 
# rename(
#    Longitude = Xstart,  
#    Latitude = Ystart
#  )
#write_csv(stand.lag.summary,file.path(CHUNKSFOLDERPATH,"STAND_LAG_SUMMARY.csv"))
