###############################################################
# Script to replicate Table II in 
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
rm(list=ls())

# Load Necessry Packages, use require("pacman") if you don't have it
library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here")

# Set working directory to this script's location for RSTUDIO only
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(here())

# Start Clock 
ptm <- proc.time()

#################################################################
# Data Processing
#################################################################

# Set path to data folder
DATAFOLDERPATH = "./data/"

# Read Data
TaxiDataPath = file.path(DATAFOLDERPATH,"train.csv")
TaxiData = read_csv(file = TaxiDataPath)

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

TotalTimePD = summary.data$TotalTime
TotalRidesPD = summary.data$TotalRides

mTTPD = mean(TotalTimePD)
mTRPD = mean(TotalRidesPD)
sdTTPD = sd(TotalTimePD)
sdTRPD = sd(TotalRidesPD)

#########################################################################
# Generate Table II: Max,Min,Mean,SD of TotalRides and TotalTime
##########################################################################
TableII.TotalTime <- c(max(TotalTimePD), min(TotalTimePD),mTTPD,sdTTPD )
TableII.TotalRides <- c(max(TotalRidesPD), min(TotalRidesPD),mTRPD,sdTRPD)
TableII <- data.frame("ServicesPerDriver"=TableII.TotalRides,"TotalCruiseTime"=TableII.TotalTime)
row.names(TableII) <- c("Max","Min","Mean","SD")

# Display TableII in R
TableII
# Export Table in LATEX format, 0 decimal place precision 
TableII.LATEX <- xtable(TableII,digits=c(0,0,0))
print(TableII.LATEX,include.rownames=TRUE)

#####################
# Script Ends Here
#####################
# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
