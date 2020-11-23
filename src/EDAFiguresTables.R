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
# Read NewTaxiData
#################################################################

# Set path to data folder
DATAFOLDERPATH = "./data"
OUTPUTFOLDERPATH = "./data/output"

# Create folder for output
dir.create("./data/output",showWarnings=FALSE)


# Read Data
NewTaxiDataPath = file.path(DATAFOLDERPATH,"NewTaxiData.csv")
NewTaxiData = read_csv(file = NewTaxiDataPath)

####################################################
# Plot histogram of service time distribution Fig4
####################################################
k<- NewTaxiData %>% 
  mutate(x_new = ifelse(TotalTime > 60, 60, TotalTime))
k$x_new <- sort(k$x_new)
k$x_cum <- cumsum(k$x_new/sum(k$x_new))
ymax <- 1.55e+05
p<- ggplot(k, aes(x_new)) +
  geom_histogram(binwidth = 1, col = "black", fill = "cornflowerblue") +
  labs(title = "Service Time Distribution",subtitle=">60 binned to 60") + xlab("Service Time/min") + ylab("Count") + 
  geom_line(aes(x=x_new,y=x_cum*ymax), color="red") +
  scale_y_continuous(name = 'Count', sec.axis = sec_axis(~./ymax, 
                                                                   name = "Cumulative percentage [%]"))
png(file.path(OUTPUTFOLDERPATH,"Fig4.png"),width=1080,height=720,type="cairo")
print(p)
dev.off()

k1 <- NewTaxiData
k1$TotalTime <- sort(k1$TotalTime)
k1$x_cum <- cumsum(k1$TotalTime/sum(k1$TotalTime))
p1<- ggplot(k1, aes(TotalTime)) +
  geom_histogram(binwidth = 1, col = "black", fill = "cornflowerblue") +
  labs(title = "Service Time Distribution all",subtitle="No truncation") + xlab("Service Time/min") + ylab("Count") + 
  geom_line(aes(x=TotalTime,y=x_cum*ymax), color="red") +
  scale_y_continuous(name = 'Count', sec.axis = sec_axis(~./ymax, 
                                                         name = "Cumulative percentage [%]"))
png(file.path(OUTPUTFOLDERPATH,"FigALL.png"),width=1080,height=720,type="cairo")
print(p1)
dev.off()


#########################################################################
# Generate Table II: Max,Min,Mean,SD of TotalRides and TotalTime
##########################################################################
# Use filtered
NewTaxiData <- read_csv(file.path(DATAFOLDERPATH,"NewTaxiDataFiltered.csv")) 
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

TableII.TotalTime <- c(max(TotalTimePD), min(TotalTimePD),mTTPD,sdTTPD )
TableII.TotalRides <- c(max(TotalRidesPD), min(TotalRidesPD),mTRPD,sdTRPD)
TableII <- data.frame("ServicesPerDriver"=TableII.TotalRides,"TotalCruiseTime"=TableII.TotalTime)
row.names(TableII) <- c("Max","Min","Mean","SD")

# Display TableII in R

sink(file.path(OUTPUTFOLDERPATH,"TableII.txt"))
print(TableII)
sink()
# Export Table in LATEX format, 0 decimal place precision 
TableII.LATEX <- xtable(TableII,digits=c(0,0,0))
print(TableII.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableIILATEX.txt"))



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

sink(file.path(OUTPUTFOLDERPATH,"TableI.txt"))
print(TableI)
sink()

print(TableI.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableILATEX.txt"))



#####################
# Script Ends Here
#####################
# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
