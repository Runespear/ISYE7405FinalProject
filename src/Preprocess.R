###############################################################
# Script to Preprocess train.csv
# Predicting Taxi–Passenger Demand Using Streaming Data
# By Luis Moreira-Matias; João Gama; Michel Ferreira; João Mendes-Moreira; Luis Damas
# IEEE Transactions on Intelligent Transportation Systems 2013
# https://ieeexplore.ieee.org/document/6532415/authors#authors

#NOTE THAT STUDY PERIOD IS DIFFERENT, SO DIFFERENT TABLE II
###############################################################

#################################################################
# Environment Setup
#################################################################

# Clear Environment
#rm(list=ls())

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
# Data Processing
#################################################################

# Set path to data folder
DATAFOLDERPATH = "./data"
OUTPUTFOLDERPATH = "./data/output"

# Create folder for output
dir.create("./data/output",showWarnings=FALSE)


# Read Data
TaxiDataPath = file.path(DATAFOLDERPATH,"train.csv")
TaxiData = read_csv(file = TaxiDataPath)
# Remove MISSING_DATA entries, then drop
TaxiData = TaxiData[which(!TaxiData$MISSING_DATA),]
TaxiData$MISSING_DATA <- NULL
# POLYLINE has GPS ticks every 15 min
# We count number of ticks by number of commas
# Case 0: "[]", 0 ticks, 0 commas
# Case 1: "[[1,1]]", 1 tick, 1 comma
# General Case: NumTicks = floor((NumCommas+1)/2)
# Floor is used to handle 0 case, since it will be the only non natural number (0.5)

# Count the number of ticks for each entry in parallel
TotalTicks <- mclapply(TaxiData$POLYLINE,str_count,pattern=",")
# Compute the time in minutes of each POLYLINE entry, floor is to handle 0 tick case 
TotalTime <- floor( (unlist(TotalTicks)+1)/2)*(15/60)

# We sum up a vector of ones to get the total number of rides
l=dim(TaxiData)[1]
vones=rep(1, l)
NewTaxiData=data.frame(TaxiData,TotalTime,vones)

# Sum up the TotaTime and TotalRides for each unique TAXI_ID
summary.data <- aggregate(NewTaxiData[,c("TotalTime","vones")],list(NewTaxiData$TAXI_ID),sum)
summary.data$TotalRides <- summary.data$vones
summary.data$vones <- NULL

#########################################################################
# Convert TIMESTAMP to weekdays and weekends and 
# Partition 0000 to 0800, 0800 to 1600, 1600 to 0000
# NOTE USE PORTUGAL TIMZEONE GMT +1 or WEST
##########################################################################
NewTaxiData$DAYOFWEEK <- weekdays(anytime(NewTaxiData$TIMESTAMP))
NewTaxiData$WEEKEND <- str_detect(NewTaxiData$DAYOFWEEK,"Sunday") | str_detect(NewTaxiData$DAYOFWEEK,"Saturday")
NewTaxiData$DATETIME <- as.POSIXct(NewTaxiData$TIMESTAMP, origin="1970-01-01",tz="Portugal")

NewTaxiData$EARLYSHIFT <- hour(NewTaxiData$DATETIME) < 8
NewTaxiData$MIDSHIFT <- hour(NewTaxiData$DATETIME) >= 8 & hour(NewTaxiData$DATETIME) < 16
NewTaxiData$LATESHIFT <- hour(NewTaxiData$DATETIME) >= 16


write_csv(NewTaxiData,file.path(DATAFOLDERPATH,"NewTaxiData.csv"))


#####################
# Script Ends Here
#####################
# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
