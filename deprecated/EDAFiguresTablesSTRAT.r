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
#setwd(here())

# Start Clock 
ptm <- proc.time()

#################################################################
# Read NewTaxiData
#################################################################

# Set path to data folder
DATAFOLDERPATH = "../data"
OUTPUTFOLDERPATH = "../data/output"

# Create folder for output
dir.create("../data/output",showWarnings=FALSE)


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


NewTaxiData$E1 <- hour(NewTaxiData$DATETIME) < 1
NewTaxiData$E2 <- hour(NewTaxiData$DATETIME) >= 1 & hour(NewTaxiData$DATETIME) < 2
NewTaxiData$E3 <- hour(NewTaxiData$DATETIME) >= 2 & hour(NewTaxiData$DATETIME) < 3
NewTaxiData$E4 <- hour(NewTaxiData$DATETIME) >= 3 & hour(NewTaxiData$DATETIME) < 4
NewTaxiData$E5 <- hour(NewTaxiData$DATETIME) >= 4 & hour(NewTaxiData$DATETIME) < 5
NewTaxiData$E6 <- hour(NewTaxiData$DATETIME) >= 5 & hour(NewTaxiData$DATETIME) < 6
NewTaxiData$E7 <- hour(NewTaxiData$DATETIME) >= 6 & hour(NewTaxiData$DATETIME) < 7
NewTaxiData$E8 <- hour(NewTaxiData$DATETIME) >= 7 & hour(NewTaxiData$DATETIME) < 8

NewTaxiData$M1 <- hour(NewTaxiData$DATETIME) >= 8 & hour(NewTaxiData$DATETIME) < 9
NewTaxiData$M2 <- hour(NewTaxiData$DATETIME) >= 9 & hour(NewTaxiData$DATETIME) < 10
NewTaxiData$M3 <- hour(NewTaxiData$DATETIME) >= 10 & hour(NewTaxiData$DATETIME) < 11
NewTaxiData$M4 <- hour(NewTaxiData$DATETIME) >= 11 & hour(NewTaxiData$DATETIME) < 12
NewTaxiData$M5 <- hour(NewTaxiData$DATETIME) >= 12 & hour(NewTaxiData$DATETIME) < 13
NewTaxiData$M6 <- hour(NewTaxiData$DATETIME) >= 13 & hour(NewTaxiData$DATETIME) < 14
NewTaxiData$M7 <- hour(NewTaxiData$DATETIME) >= 14 & hour(NewTaxiData$DATETIME) < 15
NewTaxiData$M8 <- hour(NewTaxiData$DATETIME) >= 15 & hour(NewTaxiData$DATETIME) < 16

NewTaxiData$L1 <- hour(NewTaxiData$DATETIME) >= 16 & hour(NewTaxiData$DATETIME) < 17
NewTaxiData$L2 <- hour(NewTaxiData$DATETIME) >= 17 & hour(NewTaxiData$DATETIME) < 18
NewTaxiData$L3 <- hour(NewTaxiData$DATETIME) >= 18 & hour(NewTaxiData$DATETIME) < 19
NewTaxiData$L4 <- hour(NewTaxiData$DATETIME) >= 19 & hour(NewTaxiData$DATETIME) < 20
NewTaxiData$L5 <- hour(NewTaxiData$DATETIME) >= 20 & hour(NewTaxiData$DATETIME) < 21
NewTaxiData$L6 <- hour(NewTaxiData$DATETIME) >= 21 & hour(NewTaxiData$DATETIME) < 22
NewTaxiData$L7 <- hour(NewTaxiData$DATETIME) >= 22 & hour(NewTaxiData$DATETIME) < 23
NewTaxiData$L8 <- hour(NewTaxiData$DATETIME) >= 23

# #################################################################### NEW TABLE I
E1.TaxiData <- NewTaxiData[which(NewTaxiData$E1),]
E2.TaxiData <- NewTaxiData[which(NewTaxiData$E2),]
E3.TaxiData <- NewTaxiData[which(NewTaxiData$E3),]
E4.TaxiData <- NewTaxiData[which(NewTaxiData$E4),]
E5.TaxiData <- NewTaxiData[which(NewTaxiData$E5),]
E6.TaxiData <- NewTaxiData[which(NewTaxiData$E6),]
E7.TaxiData <- NewTaxiData[which(NewTaxiData$E7),]
E8.TaxiData <- NewTaxiData[which(NewTaxiData$E8),]

M1.TaxiData <- NewTaxiData[which(NewTaxiData$M1),]
M2.TaxiData <- NewTaxiData[which(NewTaxiData$M2),]
M3.TaxiData <- NewTaxiData[which(NewTaxiData$M3),]
M4.TaxiData <- NewTaxiData[which(NewTaxiData$M4),]
M5.TaxiData <- NewTaxiData[which(NewTaxiData$M5),]
M6.TaxiData <- NewTaxiData[which(NewTaxiData$M6),]
M7.TaxiData <- NewTaxiData[which(NewTaxiData$M7),]
M8.TaxiData <- NewTaxiData[which(NewTaxiData$M8),]

L1.TaxiData <- NewTaxiData[which(NewTaxiData$L1),]
L2.TaxiData <- NewTaxiData[which(NewTaxiData$L2),]
L3.TaxiData <- NewTaxiData[which(NewTaxiData$L3),]
L4.TaxiData <- NewTaxiData[which(NewTaxiData$L4),]
L5.TaxiData <- NewTaxiData[which(NewTaxiData$L5),]
L6.TaxiData <- NewTaxiData[which(NewTaxiData$L6),]
L7.TaxiData <- NewTaxiData[which(NewTaxiData$L7),]
L8.TaxiData <- NewTaxiData[which(NewTaxiData$L8),]


Workdays.E1.TaxiData <- E1.TaxiData[which(!E1.TaxiData$WEEKEND),]
Workdays.E2.TaxiData <- E2.TaxiData[which(!E2.TaxiData$WEEKEND),]
Workdays.E3.TaxiData <- E3.TaxiData[which(!E3.TaxiData$WEEKEND),]
Workdays.E4.TaxiData <- E4.TaxiData[which(!E4.TaxiData$WEEKEND),]
Workdays.E5.TaxiData <- E5.TaxiData[which(!E5.TaxiData$WEEKEND),]
Workdays.E6.TaxiData <- E6.TaxiData[which(!E6.TaxiData$WEEKEND),]
Workdays.E7.TaxiData <- E7.TaxiData[which(!E7.TaxiData$WEEKEND),]
Workdays.E8.TaxiData <- E8.TaxiData[which(!E8.TaxiData$WEEKEND),]

Workdays.M1.TaxiData <- M1.TaxiData[which(!M1.TaxiData$WEEKEND),]
Workdays.M2.TaxiData <- M2.TaxiData[which(!M2.TaxiData$WEEKEND),]
Workdays.M3.TaxiData <- M3.TaxiData[which(!M3.TaxiData$WEEKEND),]
Workdays.M4.TaxiData <- M4.TaxiData[which(!M4.TaxiData$WEEKEND),]
Workdays.M5.TaxiData <- M5.TaxiData[which(!M5.TaxiData$WEEKEND),]
Workdays.M6.TaxiData <- M6.TaxiData[which(!M6.TaxiData$WEEKEND),]
Workdays.M7.TaxiData <- M7.TaxiData[which(!M7.TaxiData$WEEKEND),]
Workdays.M8.TaxiData <- M8.TaxiData[which(!M8.TaxiData$WEEKEND),]

Workdays.L1.TaxiData <- L1.TaxiData[which(!L1.TaxiData$WEEKEND),]
Workdays.L2.TaxiData <- L2.TaxiData[which(!L2.TaxiData$WEEKEND),]
Workdays.L3.TaxiData <- L3.TaxiData[which(!L3.TaxiData$WEEKEND),]
Workdays.L4.TaxiData <- L4.TaxiData[which(!L4.TaxiData$WEEKEND),]
Workdays.L5.TaxiData <- L5.TaxiData[which(!L5.TaxiData$WEEKEND),]
Workdays.L6.TaxiData <- L6.TaxiData[which(!L6.TaxiData$WEEKEND),]
Workdays.L7.TaxiData <- L7.TaxiData[which(!L7.TaxiData$WEEKEND),]
Workdays.L8.TaxiData <- L8.TaxiData[which(!L8.TaxiData$WEEKEND),]


Weekends.E1.TaxiData <- E1.TaxiData[which(E1.TaxiData$WEEKEND),]
Weekends.E2.TaxiData <- E2.TaxiData[which(E2.TaxiData$WEEKEND),]
Weekends.E3.TaxiData <- E3.TaxiData[which(E3.TaxiData$WEEKEND),]
Weekends.E4.TaxiData <- E4.TaxiData[which(E4.TaxiData$WEEKEND),]
Weekends.E5.TaxiData <- E5.TaxiData[which(E5.TaxiData$WEEKEND),]
Weekends.E6.TaxiData <- E6.TaxiData[which(E6.TaxiData$WEEKEND),]
Weekends.E7.TaxiData <- E7.TaxiData[which(E7.TaxiData$WEEKEND),]
Weekends.E8.TaxiData <- E8.TaxiData[which(E8.TaxiData$WEEKEND),]

Weekends.M1.TaxiData <- M1.TaxiData[which(M1.TaxiData$WEEKEND),]
Weekends.M2.TaxiData <- M2.TaxiData[which(M2.TaxiData$WEEKEND),]
Weekends.M3.TaxiData <- M3.TaxiData[which(M3.TaxiData$WEEKEND),]
Weekends.M4.TaxiData <- M4.TaxiData[which(M4.TaxiData$WEEKEND),]
Weekends.M5.TaxiData <- M5.TaxiData[which(M5.TaxiData$WEEKEND),]
Weekends.M6.TaxiData <- M6.TaxiData[which(M6.TaxiData$WEEKEND),]
Weekends.M7.TaxiData <- M7.TaxiData[which(M7.TaxiData$WEEKEND),]
Weekends.M8.TaxiData <- M8.TaxiData[which(M8.TaxiData$WEEKEND),]

Weekends.L1.TaxiData <- L1.TaxiData[which(L1.TaxiData$WEEKEND),]
Weekends.L2.TaxiData <- L2.TaxiData[which(L2.TaxiData$WEEKEND),]
Weekends.L3.TaxiData <- L3.TaxiData[which(L3.TaxiData$WEEKEND),]
Weekends.L4.TaxiData <- L4.TaxiData[which(L4.TaxiData$WEEKEND),]
Weekends.L5.TaxiData <- L5.TaxiData[which(L5.TaxiData$WEEKEND),]
Weekends.L6.TaxiData <- L6.TaxiData[which(L6.TaxiData$WEEKEND),]
Weekends.L7.TaxiData <- L7.TaxiData[which(L7.TaxiData$WEEKEND),]
Weekends.L8.TaxiData <- L8.TaxiData[which(L8.TaxiData$WEEKEND),]



E1.summary.data <- aggregate(E1.TaxiData[,c("TotalTime","vones")],list(E1.TaxiData$TAXI_ID),sum)
E2.summary.data <- aggregate(E2.TaxiData[,c("TotalTime","vones")],list(E2.TaxiData$TAXI_ID),sum)
E3.summary.data <- aggregate(E3.TaxiData[,c("TotalTime","vones")],list(E3.TaxiData$TAXI_ID),sum)
E4.summary.data <- aggregate(E4.TaxiData[,c("TotalTime","vones")],list(E4.TaxiData$TAXI_ID),sum)
E5.summary.data <- aggregate(E5.TaxiData[,c("TotalTime","vones")],list(E5.TaxiData$TAXI_ID),sum)
E6.summary.data <- aggregate(E6.TaxiData[,c("TotalTime","vones")],list(E6.TaxiData$TAXI_ID),sum)
E7.summary.data <- aggregate(E7.TaxiData[,c("TotalTime","vones")],list(E7.TaxiData$TAXI_ID),sum)
E8.summary.data <- aggregate(E8.TaxiData[,c("TotalTime","vones")],list(E8.TaxiData$TAXI_ID),sum)

M1.summary.data <- aggregate(M1.TaxiData[,c("TotalTime","vones")],list(M1.TaxiData$TAXI_ID),sum)
M2.summary.data <- aggregate(M2.TaxiData[,c("TotalTime","vones")],list(M2.TaxiData$TAXI_ID),sum)
M3.summary.data <- aggregate(M3.TaxiData[,c("TotalTime","vones")],list(M3.TaxiData$TAXI_ID),sum)
M4.summary.data <- aggregate(M4.TaxiData[,c("TotalTime","vones")],list(M4.TaxiData$TAXI_ID),sum)
M5.summary.data <- aggregate(M5.TaxiData[,c("TotalTime","vones")],list(M5.TaxiData$TAXI_ID),sum)
M6.summary.data <- aggregate(M6.TaxiData[,c("TotalTime","vones")],list(M6.TaxiData$TAXI_ID),sum)
M7.summary.data <- aggregate(M7.TaxiData[,c("TotalTime","vones")],list(M7.TaxiData$TAXI_ID),sum)
M8.summary.data <- aggregate(M8.TaxiData[,c("TotalTime","vones")],list(M8.TaxiData$TAXI_ID),sum)

L1.summary.data <- aggregate(L1.TaxiData[,c("TotalTime","vones")],list(L1.TaxiData$TAXI_ID),sum)
L2.summary.data <- aggregate(L2.TaxiData[,c("TotalTime","vones")],list(L2.TaxiData$TAXI_ID),sum)
L3.summary.data <- aggregate(L3.TaxiData[,c("TotalTime","vones")],list(L3.TaxiData$TAXI_ID),sum)
L4.summary.data <- aggregate(L4.TaxiData[,c("TotalTime","vones")],list(L4.TaxiData$TAXI_ID),sum)
L5.summary.data <- aggregate(L5.TaxiData[,c("TotalTime","vones")],list(L5.TaxiData$TAXI_ID),sum)
L6.summary.data <- aggregate(L6.TaxiData[,c("TotalTime","vones")],list(L6.TaxiData$TAXI_ID),sum)
L7.summary.data <- aggregate(L7.TaxiData[,c("TotalTime","vones")],list(L7.TaxiData$TAXI_ID),sum)
L8.summary.data <- aggregate(L8.TaxiData[,c("TotalTime","vones")],list(L8.TaxiData$TAXI_ID),sum)


Workdays.E1.summary.data <- aggregate(Workdays.E1.TaxiData[,c("TotalTime","vones")],list(Workdays.E1.TaxiData$TAXI_ID),sum)
Workdays.E2.summary.data <- aggregate(Workdays.E2.TaxiData[,c("TotalTime","vones")],list(Workdays.E2.TaxiData$TAXI_ID),sum)
Workdays.E3.summary.data <- aggregate(Workdays.E3.TaxiData[,c("TotalTime","vones")],list(Workdays.E3.TaxiData$TAXI_ID),sum)
Workdays.E4.summary.data <- aggregate(Workdays.E4.TaxiData[,c("TotalTime","vones")],list(Workdays.E4.TaxiData$TAXI_ID),sum)
Workdays.E5.summary.data <- aggregate(Workdays.E5.TaxiData[,c("TotalTime","vones")],list(Workdays.E5.TaxiData$TAXI_ID),sum)
Workdays.E6.summary.data <- aggregate(Workdays.E6.TaxiData[,c("TotalTime","vones")],list(Workdays.E6.TaxiData$TAXI_ID),sum)
Workdays.E7.summary.data <- aggregate(Workdays.E7.TaxiData[,c("TotalTime","vones")],list(Workdays.E7.TaxiData$TAXI_ID),sum)
Workdays.E8.summary.data <- aggregate(Workdays.E8.TaxiData[,c("TotalTime","vones")],list(Workdays.E8.TaxiData$TAXI_ID),sum)

Workdays.M1.summary.data <- aggregate(Workdays.M1.TaxiData[,c("TotalTime","vones")],list(Workdays.M1.TaxiData$TAXI_ID),sum)
Workdays.M2.summary.data <- aggregate(Workdays.M2.TaxiData[,c("TotalTime","vones")],list(Workdays.M2.TaxiData$TAXI_ID),sum)
Workdays.M3.summary.data <- aggregate(Workdays.M3.TaxiData[,c("TotalTime","vones")],list(Workdays.M3.TaxiData$TAXI_ID),sum)
Workdays.M4.summary.data <- aggregate(Workdays.M4.TaxiData[,c("TotalTime","vones")],list(Workdays.M4.TaxiData$TAXI_ID),sum)
Workdays.M5.summary.data <- aggregate(Workdays.M5.TaxiData[,c("TotalTime","vones")],list(Workdays.M5.TaxiData$TAXI_ID),sum)
Workdays.M6.summary.data <- aggregate(Workdays.M6.TaxiData[,c("TotalTime","vones")],list(Workdays.M6.TaxiData$TAXI_ID),sum)
Workdays.M7.summary.data <- aggregate(Workdays.M7.TaxiData[,c("TotalTime","vones")],list(Workdays.M7.TaxiData$TAXI_ID),sum)
Workdays.M8.summary.data <- aggregate(Workdays.M8.TaxiData[,c("TotalTime","vones")],list(Workdays.M8.TaxiData$TAXI_ID),sum)

Workdays.L1.summary.data <- aggregate(Workdays.L1.TaxiData[,c("TotalTime","vones")],list(Workdays.L1.TaxiData$TAXI_ID),sum)
Workdays.L2.summary.data <- aggregate(Workdays.L2.TaxiData[,c("TotalTime","vones")],list(Workdays.L2.TaxiData$TAXI_ID),sum)
Workdays.L3.summary.data <- aggregate(Workdays.L3.TaxiData[,c("TotalTime","vones")],list(Workdays.L3.TaxiData$TAXI_ID),sum)
Workdays.L4.summary.data <- aggregate(Workdays.L4.TaxiData[,c("TotalTime","vones")],list(Workdays.L4.TaxiData$TAXI_ID),sum)
Workdays.L5.summary.data <- aggregate(Workdays.L5.TaxiData[,c("TotalTime","vones")],list(Workdays.L5.TaxiData$TAXI_ID),sum)
Workdays.L6.summary.data <- aggregate(Workdays.L6.TaxiData[,c("TotalTime","vones")],list(Workdays.L6.TaxiData$TAXI_ID),sum)
Workdays.L7.summary.data <- aggregate(Workdays.L7.TaxiData[,c("TotalTime","vones")],list(Workdays.L7.TaxiData$TAXI_ID),sum)
Workdays.L8.summary.data <- aggregate(Workdays.L8.TaxiData[,c("TotalTime","vones")],list(Workdays.L8.TaxiData$TAXI_ID),sum)


Weekends.E1.summary.data <- aggregate(Weekends.E1.TaxiData[,c("TotalTime","vones")],list(Weekends.E1.TaxiData$TAXI_ID),sum)
Weekends.E2.summary.data <- aggregate(Weekends.E2.TaxiData[,c("TotalTime","vones")],list(Weekends.E2.TaxiData$TAXI_ID),sum)
Weekends.E3.summary.data <- aggregate(Weekends.E3.TaxiData[,c("TotalTime","vones")],list(Weekends.E3.TaxiData$TAXI_ID),sum)
Weekends.E4.summary.data <- aggregate(Weekends.E4.TaxiData[,c("TotalTime","vones")],list(Weekends.E4.TaxiData$TAXI_ID),sum)
Weekends.E5.summary.data <- aggregate(Weekends.E5.TaxiData[,c("TotalTime","vones")],list(Weekends.E5.TaxiData$TAXI_ID),sum)
Weekends.E6.summary.data <- aggregate(Weekends.E6.TaxiData[,c("TotalTime","vones")],list(Weekends.E6.TaxiData$TAXI_ID),sum)
Weekends.E7.summary.data <- aggregate(Weekends.E7.TaxiData[,c("TotalTime","vones")],list(Weekends.E7.TaxiData$TAXI_ID),sum)
Weekends.E8.summary.data <- aggregate(Weekends.E8.TaxiData[,c("TotalTime","vones")],list(Weekends.E8.TaxiData$TAXI_ID),sum)

Weekends.M1.summary.data <- aggregate(Weekends.M1.TaxiData[,c("TotalTime","vones")],list(Weekends.M1.TaxiData$TAXI_ID),sum)
Weekends.M2.summary.data <- aggregate(Weekends.M2.TaxiData[,c("TotalTime","vones")],list(Weekends.M2.TaxiData$TAXI_ID),sum)
Weekends.M3.summary.data <- aggregate(Weekends.M3.TaxiData[,c("TotalTime","vones")],list(Weekends.M3.TaxiData$TAXI_ID),sum)
Weekends.M4.summary.data <- aggregate(Weekends.M4.TaxiData[,c("TotalTime","vones")],list(Weekends.M4.TaxiData$TAXI_ID),sum)
Weekends.M5.summary.data <- aggregate(Weekends.M5.TaxiData[,c("TotalTime","vones")],list(Weekends.M5.TaxiData$TAXI_ID),sum)
Weekends.M6.summary.data <- aggregate(Weekends.M6.TaxiData[,c("TotalTime","vones")],list(Weekends.M6.TaxiData$TAXI_ID),sum)
Weekends.M7.summary.data <- aggregate(Weekends.M7.TaxiData[,c("TotalTime","vones")],list(Weekends.M7.TaxiData$TAXI_ID),sum)
Weekends.M8.summary.data <- aggregate(Weekends.M8.TaxiData[,c("TotalTime","vones")],list(Weekends.M8.TaxiData$TAXI_ID),sum)

Weekends.L1.summary.data <- aggregate(Weekends.L1.TaxiData[,c("TotalTime","vones")],list(Weekends.L1.TaxiData$TAXI_ID),sum)
Weekends.L2.summary.data <- aggregate(Weekends.L2.TaxiData[,c("TotalTime","vones")],list(Weekends.L2.TaxiData$TAXI_ID),sum)
Weekends.L3.summary.data <- aggregate(Weekends.L3.TaxiData[,c("TotalTime","vones")],list(Weekends.L3.TaxiData$TAXI_ID),sum)
Weekends.L4.summary.data <- aggregate(Weekends.L4.TaxiData[,c("TotalTime","vones")],list(Weekends.L4.TaxiData$TAXI_ID),sum)
Weekends.L5.summary.data <- aggregate(Weekends.L5.TaxiData[,c("TotalTime","vones")],list(Weekends.L5.TaxiData$TAXI_ID),sum)
Weekends.L6.summary.data <- aggregate(Weekends.L6.TaxiData[,c("TotalTime","vones")],list(Weekends.L6.TaxiData$TAXI_ID),sum)
Weekends.L7.summary.data <- aggregate(Weekends.L7.TaxiData[,c("TotalTime","vones")],list(Weekends.L7.TaxiData$TAXI_ID),sum)
Weekends.L8.summary.data <- aggregate(Weekends.L8.TaxiData[,c("TotalTime","vones")],list(Weekends.L8.TaxiData$TAXI_ID),sum)

TableI.TotalServicesEmerged <- c(sum(NewTaxiData[which(!NewTaxiData$WEEKEND),]$vones), sum(NewTaxiData[which(NewTaxiData$WEEKEND),]$vones),sum(NewTaxiData$vones))



TableI.AverageE1 <- c(mean(Workdays.E1.summary.data$vones),mean(Weekends.E1.summary.data$vones),mean(E1.summary.data$vones))
TableI.AverageE2 <- c(mean(Workdays.E2.summary.data$vones),mean(Weekends.E2.summary.data$vones),mean(E2.summary.data$vones))
TableI.AverageE3 <- c(mean(Workdays.E3.summary.data$vones),mean(Weekends.E3.summary.data$vones),mean(E3.summary.data$vones))
TableI.AverageE4 <- c(mean(Workdays.E4.summary.data$vones),mean(Weekends.E4.summary.data$vones),mean(E4.summary.data$vones))
TableI.AverageE5 <- c(mean(Workdays.E5.summary.data$vones),mean(Weekends.E5.summary.data$vones),mean(E5.summary.data$vones))
TableI.AverageE6 <- c(mean(Workdays.E6.summary.data$vones),mean(Weekends.E6.summary.data$vones),mean(E6.summary.data$vones))
TableI.AverageE7 <- c(mean(Workdays.E7.summary.data$vones),mean(Weekends.E7.summary.data$vones),mean(E7.summary.data$vones))
TableI.AverageE8 <- c(mean(Workdays.E8.summary.data$vones),mean(Weekends.E8.summary.data$vones),mean(E8.summary.data$vones))

TableI.AverageM1 <- c(mean(Workdays.M1.summary.data$vones),mean(Weekends.M1.summary.data$vones),mean(M1.summary.data$vones))
TableI.AverageM2 <- c(mean(Workdays.M2.summary.data$vones),mean(Weekends.M2.summary.data$vones),mean(M2.summary.data$vones))
TableI.AverageM3 <- c(mean(Workdays.M3.summary.data$vones),mean(Weekends.M3.summary.data$vones),mean(M3.summary.data$vones))
TableI.AverageM4 <- c(mean(Workdays.M4.summary.data$vones),mean(Weekends.M4.summary.data$vones),mean(M4.summary.data$vones))
TableI.AverageM5 <- c(mean(Workdays.M5.summary.data$vones),mean(Weekends.M5.summary.data$vones),mean(M5.summary.data$vones))
TableI.AverageM6 <- c(mean(Workdays.M6.summary.data$vones),mean(Weekends.M6.summary.data$vones),mean(M6.summary.data$vones))
TableI.AverageM7 <- c(mean(Workdays.M7.summary.data$vones),mean(Weekends.M7.summary.data$vones),mean(M7.summary.data$vones))
TableI.AverageM8 <- c(mean(Workdays.M8.summary.data$vones),mean(Weekends.M8.summary.data$vones),mean(M8.summary.data$vones))

TableI.AverageL1 <- c(mean(Workdays.L1.summary.data$vones),mean(Weekends.L1.summary.data$vones),mean(L1.summary.data$vones))
TableI.AverageL2 <- c(mean(Workdays.L2.summary.data$vones),mean(Weekends.L2.summary.data$vones),mean(L2.summary.data$vones))
TableI.AverageL3 <- c(mean(Workdays.L3.summary.data$vones),mean(Weekends.L3.summary.data$vones),mean(L3.summary.data$vones))
TableI.AverageL4 <- c(mean(Workdays.L4.summary.data$vones),mean(Weekends.L4.summary.data$vones),mean(L4.summary.data$vones))
TableI.AverageL5 <- c(mean(Workdays.L5.summary.data$vones),mean(Weekends.L5.summary.data$vones),mean(L5.summary.data$vones))
TableI.AverageL6 <- c(mean(Workdays.L6.summary.data$vones),mean(Weekends.L6.summary.data$vones),mean(L6.summary.data$vones))
TableI.AverageL7 <- c(mean(Workdays.L7.summary.data$vones),mean(Weekends.L7.summary.data$vones),mean(L7.summary.data$vones))
TableI.AverageL8 <- c(mean(Workdays.L8.summary.data$vones),mean(Weekends.L8.summary.data$vones),mean(L8.summary.data$vones))



TableI <- data.frame("TotalServicesEmerged"=TableI.TotalServicesEmerged,"AverageE1"=TableI.AverageE1,"AverageE2"=TableI.AverageE2,"AverageE3"=TableI.AverageE3,
                     "AverageE4"=TableI.AverageE4,"AverageE5"=TableI.AverageE5,"AverageE6"=TableI.AverageE6,"AverageE7"=TableI.AverageE7,"AverageE8"=TableI.AverageE8,
                     "AverageM1"=TableI.AverageM1,"AverageM2"=TableI.AverageM2,"AverageM3"=TableI.AverageM3,
                     "AverageM4"=TableI.AverageM4,"AverageM5"=TableI.AverageM5,"AverageM6"=TableI.AverageM6,"AverageM7"=TableI.AverageM7,"AverageM8"=TableI.AverageM8,
                     "AverageL1"=TableI.AverageL1,"AverageL2"=TableI.AverageL2,"AverageL3"=TableI.AverageL3,
                     "AverageL4"=TableI.AverageL4,"AverageL5"=TableI.AverageL5,"AverageL6"=TableI.AverageL6,"AverageL7"=TableI.AverageL7,"AverageL8"=TableI.AverageL8)
row.names(TableI) <- c("Workdays","Weekends","AllDaytypes")

TableI.LATEX <- xtable(TableI,digits=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

sink(file.path(OUTPUTFOLDERPATH,"TableI2ed.txt"))
print(TableI)
sink()

print(TableI.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableILATEXSTRSAT.txt"))






#####################
# Script Ends Here
#####################
# Check time taken
TimeTaken <- proc.time() - ptm
TimeTaken
