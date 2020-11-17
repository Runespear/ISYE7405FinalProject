#detach()
rm(list=ls())

library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel")

#################################################################
# Set working directory to this script's location
#################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Start Clock 
ptm <- proc.time()

# Read Data
TaxiData = read_csv(file = 'train.csv')
ColPol=TaxiData[,'POLYLINE']
l=dim(ColPol)[1]
TotalTime=vector(,l)

# Last column has GPS ticks every 15 min
# We count number of ticks by number of commas
# Case: "[]", 0 ticks, 0 commas
# Case: "[[1,1]]", 1 tick, 1 comma
# Case: "[[1,1],[2,2]]", 2 ticks, 3 commas
# Case: "[[1,1],[2,2],[3,3]]", 3 ticks, 5 commas
# General Case: NumTicks = floor((NumCommas+1)/2)
# Floor is used to handle 0 case, since it will be the only non natural number (0.5)

# Count the number of ticks for each entry in parallel
TotalTicks <- mclapply(TaxiData$POLYLINE,str_count,pattern=",")
# Compute the time in minutes of each POLYLINE entry, floor is to handle 0 tick case 
TotalTime <- floor( (unlist(TotalTicks)+1)/2)*(15/60)

# We sum up a vector of ones to get the total number of rides
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

# Display Max,Min,Mean,SD of TotalRides and TotalTime


TableII.TotalTime <- c(max(TotalTimePD), min(TotalTimePD),mTTPD,sdTTPD )
TableII.TotalRides <- c(max(TotalRidesPD), min(TotalRidesPD),mTRPD,sdTRPD)
TableII <- data.frame("ServicesPerDriver"=TableII.TotalRides,"TotalCruiseTime"=TableII.TotalTime)
row.names(TableII) <- c("Max","Min","Mean","SD")

# Display TableII
TableII

# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
