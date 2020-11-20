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
print(TableII.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableIILATEX.txt"))


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

#########################################################################
# CREATE SUBSETS BASED ON TIME OF DAY 
# GENERATE TABLE I
##########################################################################

EARLYSHIFT.TaxiData <- NewTaxiData[which(NewTaxiData$EARLYSHIFT),]
MIDSHIFT.TaxiData <- NewTaxiData[which(NewTaxiData$MIDSHIFT),]
LATESHIFT.TaxiData <- NewTaxiData[which(NewTaxiData$LATESHIFT),]

Workdays.EARLYSHIFT.TaxiData <- EARLYSHIFT.TaxiData[which(!EARLYSHIFT.TaxiData$WEEKEND),]
Workdays.MIDSHIFT.TaxiData <- MIDSHIFT.TaxiData[which(!MIDSHIFT.TaxiData$WEEKEND),]
Workdays.LATESHIFT.TaxiData <- LATESHIFT.TaxiData[which(!LATESHIFT.TaxiData$WEEKEND),]

Weekends.EARLYSHIFT.TaxiData <- EARLYSHIFT.TaxiData[which(EARLYSHIFT.TaxiData$WEEKEND),]
Weekends.MIDSHIFT.TaxiData <- MIDSHIFT.TaxiData[which(MIDSHIFT.TaxiData$WEEKEND),]
Weekends.LATESHIFT.TaxiData <- LATESHIFT.TaxiData[which(LATESHIFT.TaxiData$WEEKEND),]

EARLYSHIFT.summary.data <- aggregate(EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(EARLYSHIFT.TaxiData$TAXI_ID),sum)
MIDSHIFT.summary.data <- aggregate(MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(MIDSHIFT.TaxiData$TAXI_ID),sum)
LATESHIFT.summary.data <- aggregate(LATESHIFT.TaxiData[,c("TotalTime","vones")],list(LATESHIFT.TaxiData$TAXI_ID),sum)

Workdays.EARLYSHIFT.summary.data <- aggregate(Workdays.EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.EARLYSHIFT.TaxiData$TAXI_ID),sum)
Workdays.MIDSHIFT.summary.data <- aggregate(Workdays.MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.MIDSHIFT.TaxiData$TAXI_ID),sum)
Workdays.LATESHIFT.summary.data <- aggregate(Workdays.LATESHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.LATESHIFT.TaxiData$TAXI_ID),sum)

Weekends.EARLYSHIFT.summary.data <- aggregate(Weekends.EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.EARLYSHIFT.TaxiData$TAXI_ID),sum)
Weekends.MIDSHIFT.summary.data <- aggregate(Weekends.MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.MIDSHIFT.TaxiData$TAXI_ID),sum)
Weekends.LATESHIFT.summary.data <- aggregate(Weekends.LATESHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.LATESHIFT.TaxiData$TAXI_ID),sum)



TableI.TotalServicesEmerged <- c(sum(NewTaxiData[which(!NewTaxiData$WEEKEND),]$vones), sum(NewTaxiData[which(NewTaxiData$WEEKEND),]$vones),sum(NewTaxiData$vones))
TableI.AverageEARLY <- c(mean(Workdays.EARLYSHIFT.summary.data$vones),mean(Weekends.EARLYSHIFT.summary.data$vones),mean(EARLYSHIFT.summary.data$vones))
TableI.AverageMID <- c(mean(Workdays.MIDSHIFT.summary.data$vones),mean(Weekends.MIDSHIFT.summary.data$vones), mean(MIDSHIFT.summary.data$vones))
TableI.AverageLATE <- c(mean(Workdays.LATESHIFT.summary.data$vones), mean(Weekends.LATESHIFT.summary.data$vones), mean(LATESHIFT.summary.data$vones))

TableI <- data.frame("TotalServicesEmerged"=TableI.TotalServicesEmerged,"AverageEarly"=TableI.AverageEARLY,"AverageMID"=TableI.AverageMID,"AverageLATE"=TableI.AverageLATE)
row.names(TableI) <- c("Workdays","Weekends","AllDaytypes")

TableI.LATEX <- xtable(TableI,digits=c(0,0,0,0,0))


print(TableI.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableILATEX.txt"))

###############################################
# Plot histogram of service time distribution
###############################################

p<-NewTaxiData %>% 
  mutate(x_new = ifelse(TotalTime > 60, 60, TotalTime)) %>% 
  ggplot(aes(x_new)) +
  geom_histogram(binwidth = 1, col = "black", fill = "cornflowerblue") + 
  labs(title = "Service Time Distribution",subtitle=">60 binned to 60") + xlab("Service Time/min") + ylab("Count")
png(file.path(OUTPUTFOLDERPATH,"Fig4.png"),width=1080,height=720,type="cairo")
print(p)
dev.off()

#####################
# Script Ends Here
#####################
# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
