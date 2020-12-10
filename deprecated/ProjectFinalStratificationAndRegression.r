rm(list=ls())
DATAFOLDERPATH = "../data"
#OUTPUTPATH = file.path(DATAFOLDERPATH,"output")
CHUNKSPATH = file.path(DATAFOLDERPATH,"chunks")
#dir.create(OUTPUTPATH,showWarnings=FALSE)
#dir.create(CHUNKSPATH,showWarnings=FALSE)
library("pacman")
#OUTPUTPATH = file.path(DATAFOLDERPATH,"output")
#CHUNKSPATH = file.path(DATAFOLDERPATH,"chunks")
#dir.create(OUTPUTPATH,showWarnings=FALSE)
#dir.create(CHUNKSPATH,showWarnings=FALSE)p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate","foreach","dplyr","readr","stringr",'nlme',"lme4","Metrics","rpart","rpart.plot","stargazer")


fileslist = c()
for (i in 1:6){
  fname = paste("WAITINGTIMES_part",toString(i),".csv",sep="")
  fileslist[i] = file.path(CHUNKSPATH,fname)
}
NewDataWaiting <- lapply(fileslist, read_csv)
NewDataWaiting <- do.call(rbind , NewDataWaiting)
NewDataWaiting<-NewDataWaiting[!(is.na(NewDataWaiting$ORIGIN_STAND)==TRUE),]



DistX=NewDataWaiting['Xend']-NewDataWaiting['Xstart']
DistY=NewDataWaiting['Yend']-NewDataWaiting['Ystart']
Dist=sqrt(DistX^2+DistY^2)
NewDataWaiting=data.frame(NewDataWaiting,Dist)
names(NewDataWaiting)[21] <- "Distance"


############################################################### Stratification by Hour
NewDataWaiting$E1 <- hour(NewDataWaiting$DATETIME) < 1
NewDataWaiting$E2 <- hour(NewDataWaiting$DATETIME) >= 1 & hour(NewDataWaiting$DATETIME) < 2
NewDataWaiting$E3 <- hour(NewDataWaiting$DATETIME) >= 2 & hour(NewDataWaiting$DATETIME) < 3
NewDataWaiting$E4 <- hour(NewDataWaiting$DATETIME) >= 3 & hour(NewDataWaiting$DATETIME) < 4
NewDataWaiting$E5 <- hour(NewDataWaiting$DATETIME) >= 4 & hour(NewDataWaiting$DATETIME) < 5
NewDataWaiting$E6 <- hour(NewDataWaiting$DATETIME) >= 5 & hour(NewDataWaiting$DATETIME) < 6
NewDataWaiting$E7 <- hour(NewDataWaiting$DATETIME) >= 6 & hour(NewDataWaiting$DATETIME) < 7
NewDataWaiting$E8 <- hour(NewDataWaiting$DATETIME) >= 7 & hour(NewDataWaiting$DATETIME) < 8

NewDataWaiting$M1 <- hour(NewDataWaiting$DATETIME) >= 8 & hour(NewDataWaiting$DATETIME) < 9
NewDataWaiting$M2 <- hour(NewDataWaiting$DATETIME) >= 9 & hour(NewDataWaiting$DATETIME) < 10
NewDataWaiting$M3 <- hour(NewDataWaiting$DATETIME) >= 10 & hour(NewDataWaiting$DATETIME) < 11
NewDataWaiting$M4 <- hour(NewDataWaiting$DATETIME) >= 11 & hour(NewDataWaiting$DATETIME) < 12
NewDataWaiting$M5 <- hour(NewDataWaiting$DATETIME) >= 12 & hour(NewDataWaiting$DATETIME) < 13
NewDataWaiting$M6 <- hour(NewDataWaiting$DATETIME) >= 13 & hour(NewDataWaiting$DATETIME) < 14
NewDataWaiting$M7 <- hour(NewDataWaiting$DATETIME) >= 14 & hour(NewDataWaiting$DATETIME) < 15
NewDataWaiting$M8 <- hour(NewDataWaiting$DATETIME) >= 15 & hour(NewDataWaiting$DATETIME) < 16

NewDataWaiting$L1 <- hour(NewDataWaiting$DATETIME) >= 16 & hour(NewDataWaiting$DATETIME) < 17
NewDataWaiting$L2 <- hour(NewDataWaiting$DATETIME) >= 17 & hour(NewDataWaiting$DATETIME) < 18
NewDataWaiting$L3 <- hour(NewDataWaiting$DATETIME) >= 18 & hour(NewDataWaiting$DATETIME) < 19
NewDataWaiting$L4 <- hour(NewDataWaiting$DATETIME) >= 19 & hour(NewDataWaiting$DATETIME) < 20
NewDataWaiting$L5 <- hour(NewDataWaiting$DATETIME) >= 20 & hour(NewDataWaiting$DATETIME) < 21
NewDataWaiting$L6 <- hour(NewDataWaiting$DATETIME) >= 21 & hour(NewDataWaiting$DATETIME) < 22
NewDataWaiting$L7 <- hour(NewDataWaiting$DATETIME) >= 22 & hour(NewDataWaiting$DATETIME) < 23
NewDataWaiting$L8 <- hour(NewDataWaiting$DATETIME) >= 23


# #################################################################### NEW TABLE I
# E1.TaxiData <- NewDataWaiting[which(NewDataWaiting$E1),]
# E2.TaxiData <- NewDataWaiting[which(NewDataWaiting$E2),]
# E3.TaxiData <- NewDataWaiting[which(NewDataWaiting$E3),]
# E4.TaxiData <- NewDataWaiting[which(NewDataWaiting$E4),]
# E5.TaxiData <- NewDataWaiting[which(NewDataWaiting$E5),]
# E6.TaxiData <- NewDataWaiting[which(NewDataWaiting$E6),]
# E7.TaxiData <- NewDataWaiting[which(NewDataWaiting$E7),]
# E8.TaxiData <- NewDataWaiting[which(NewDataWaiting$E8),]
# 
# M1.TaxiData <- NewDataWaiting[which(NewDataWaiting$M1),]
# M2.TaxiData <- NewDataWaiting[which(NewDataWaiting$M2),]
# M3.TaxiData <- NewDataWaiting[which(NewDataWaiting$M3),]
# M4.TaxiData <- NewDataWaiting[which(NewDataWaiting$M4),]
# M5.TaxiData <- NewDataWaiting[which(NewDataWaiting$M5),]
# M6.TaxiData <- NewDataWaiting[which(NewDataWaiting$M6),]
# M7.TaxiData <- NewDataWaiting[which(NewDataWaiting$M7),]
# M8.TaxiData <- NewDataWaiting[which(NewDataWaiting$M8),]
# 
# L1.TaxiData <- NewDataWaiting[which(NewDataWaiting$L1),]
# L2.TaxiData <- NewDataWaiting[which(NewDataWaiting$L2),]
# L3.TaxiData <- NewDataWaiting[which(NewDataWaiting$L3),]
# L4.TaxiData <- NewDataWaiting[which(NewDataWaiting$L4),]
# L5.TaxiData <- NewDataWaiting[which(NewDataWaiting$L5),]
# L6.TaxiData <- NewDataWaiting[which(NewDataWaiting$L6),]
# L7.TaxiData <- NewDataWaiting[which(NewDataWaiting$L7),]
# L8.TaxiData <- NewDataWaiting[which(NewDataWaiting$L8),]
# 
# 
# Workdays.E1.TaxiData <- E1.TaxiData[which(!E1.TaxiData$WEEKEND),]
# Workdays.E2.TaxiData <- E2.TaxiData[which(!E2.TaxiData$WEEKEND),]
# Workdays.E3.TaxiData <- E3.TaxiData[which(!E3.TaxiData$WEEKEND),]
# Workdays.E4.TaxiData <- E4.TaxiData[which(!E4.TaxiData$WEEKEND),]
# Workdays.E5.TaxiData <- E5.TaxiData[which(!E5.TaxiData$WEEKEND),]
# Workdays.E6.TaxiData <- E6.TaxiData[which(!E6.TaxiData$WEEKEND),]
# Workdays.E7.TaxiData <- E7.TaxiData[which(!E7.TaxiData$WEEKEND),]
# Workdays.E8.TaxiData <- E8.TaxiData[which(!E8.TaxiData$WEEKEND),]
# 
# Workdays.M1.TaxiData <- M1.TaxiData[which(!M1.TaxiData$WEEKEND),]
# Workdays.M2.TaxiData <- M2.TaxiData[which(!M2.TaxiData$WEEKEND),]
# Workdays.M3.TaxiData <- M3.TaxiData[which(!M3.TaxiData$WEEKEND),]
# Workdays.M4.TaxiData <- M4.TaxiData[which(!M4.TaxiData$WEEKEND),]
# Workdays.M5.TaxiData <- M5.TaxiData[which(!M5.TaxiData$WEEKEND),]
# Workdays.M6.TaxiData <- M6.TaxiData[which(!M6.TaxiData$WEEKEND),]
# Workdays.M7.TaxiData <- M7.TaxiData[which(!M7.TaxiData$WEEKEND),]
# Workdays.M8.TaxiData <- M8.TaxiData[which(!M8.TaxiData$WEEKEND),]
# 
# Workdays.L1.TaxiData <- L1.TaxiData[which(!L1.TaxiData$WEEKEND),]
# Workdays.L2.TaxiData <- L2.TaxiData[which(!L2.TaxiData$WEEKEND),]
# Workdays.L3.TaxiData <- L3.TaxiData[which(!L3.TaxiData$WEEKEND),]
# Workdays.L4.TaxiData <- L4.TaxiData[which(!L4.TaxiData$WEEKEND),]
# Workdays.L5.TaxiData <- L5.TaxiData[which(!L5.TaxiData$WEEKEND),]
# Workdays.L6.TaxiData <- L6.TaxiData[which(!L6.TaxiData$WEEKEND),]
# Workdays.L7.TaxiData <- L7.TaxiData[which(!L7.TaxiData$WEEKEND),]
# Workdays.L8.TaxiData <- L8.TaxiData[which(!L8.TaxiData$WEEKEND),]
# 
# 
# Weekends.EARLYSHIFT.TaxiData <- EARLYSHIFT.TaxiData[which(EARLYSHIFT.TaxiData$WEEKEND),]
# Weekends.MIDSHIFT.TaxiData <- MIDSHIFT.TaxiData[which(MIDSHIFT.TaxiData$WEEKEND),]
# Weekends.LATESHIFT.TaxiData <- LATESHIFT.TaxiData[which(LATESHIFT.TaxiData$WEEKEND),]
# 
# EARLYSHIFT.summary.data <- aggregate(EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(EARLYSHIFT.TaxiData$TAXI_ID),sum)
# MIDSHIFT.summary.data <- aggregate(MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(MIDSHIFT.TaxiData$TAXI_ID),sum)
# LATESHIFT.summary.data <- aggregate(LATESHIFT.TaxiData[,c("TotalTime","vones")],list(LATESHIFT.TaxiData$TAXI_ID),sum)
# 
# Workdays.EARLYSHIFT.summary.data <- aggregate(Workdays.EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.EARLYSHIFT.TaxiData$TAXI_ID),sum)
# Workdays.MIDSHIFT.summary.data <- aggregate(Workdays.MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.MIDSHIFT.TaxiData$TAXI_ID),sum)
# Workdays.LATESHIFT.summary.data <- aggregate(Workdays.LATESHIFT.TaxiData[,c("TotalTime","vones")],list(Workdays.LATESHIFT.TaxiData$TAXI_ID),sum)
# 
# Weekends.EARLYSHIFT.summary.data <- aggregate(Weekends.EARLYSHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.EARLYSHIFT.TaxiData$TAXI_ID),sum)
# Weekends.MIDSHIFT.summary.data <- aggregate(Weekends.MIDSHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.MIDSHIFT.TaxiData$TAXI_ID),sum)
# Weekends.LATESHIFT.summary.data <- aggregate(Weekends.LATESHIFT.TaxiData[,c("TotalTime","vones")],list(Weekends.LATESHIFT.TaxiData$TAXI_ID),sum)
# 
# 
# 
# TableI.TotalServicesEmerged <- c(sum(NewTaxiData[which(!NewTaxiData$WEEKEND),]$vones), sum(NewTaxiData[which(NewTaxiData$WEEKEND),]$vones),sum(NewTaxiData$vones))
# TableI.AverageEARLY <- c(mean(Workdays.EARLYSHIFT.summary.data$vones),mean(Weekends.EARLYSHIFT.summary.data$vones),mean(EARLYSHIFT.summary.data$vones))
# TableI.AverageMID <- c(mean(Workdays.MIDSHIFT.summary.data$vones),mean(Weekends.MIDSHIFT.summary.data$vones), mean(MIDSHIFT.summary.data$vones))
# TableI.AverageLATE <- c(mean(Workdays.LATESHIFT.summary.data$vones), mean(Weekends.LATESHIFT.summary.data$vones), mean(LATESHIFT.summary.data$vones))
# 
# TableI <- data.frame("TotalServicesEmerged"=TableI.TotalServicesEmerged,"AverageEarly"=TableI.AverageEARLY,"AverageMID"=TableI.AverageMID,"AverageLATE"=TableI.AverageLATE)
# row.names(TableI) <- c("Workdays","Weekends","AllDaytypes")
# 
# TableI.LATEX <- xtable(TableI,digits=c(0,0,0,0,0))
# 
# sink(file.path(OUTPUTFOLDERPATH,"TableI.txt"))
# print(TableI)
# sink()
# 
# print(TableI.LATEX,include.rownames=TRUE,file=file.path(OUTPUTFOLDERPATH,"TableILATEX.txt"))
# 
# 
# 
# 


####################################### Create Boolean Vectors
index=NewDataWaiting$WEEKEND==TRUE
NewDataWaiting$WEEKEND[index]=1
index=NewDataWaiting$EARLYSHIFT==TRUE
NewDataWaiting$EARLYSHIFT[index]=1
index=NewDataWaiting$MIDSHIFT==TRUE
NewDataWaiting$MIDSHIFT[index]=1
index=NewDataWaiting$LATESHIFT==TRUE
NewDataWaiting$LATESHIFT[index]=1

index=NewDataWaiting$E1==TRUE
NewDataWaiting$E1[index]=1
index=NewDataWaiting$E2==TRUE
NewDataWaiting$E2[index]=1
index=NewDataWaiting$E3==TRUE
NewDataWaiting$E3[index]=1
index=NewDataWaiting$E4==TRUE
NewDataWaiting$E4[index]=1
index=NewDataWaiting$E5==TRUE
NewDataWaiting$E5[index]=1
index=NewDataWaiting$E6==TRUE
NewDataWaiting$E6[index]=1
index=NewDataWaiting$E7==TRUE
NewDataWaiting$E7[index]=1
index=NewDataWaiting$E8==TRUE
NewDataWaiting$E8[index]=1

index=NewDataWaiting$M1==TRUE
NewDataWaiting$M1[index]=1
index=NewDataWaiting$M2==TRUE
NewDataWaiting$M2[index]=1
index=NewDataWaiting$M3==TRUE
NewDataWaiting$M3[index]=1
index=NewDataWaiting$M4==TRUE
NewDataWaiting$M4[index]=1
index=NewDataWaiting$M5==TRUE
NewDataWaiting$M5[index]=1
index=NewDataWaiting$M6==TRUE
NewDataWaiting$M6[index]=1
index=NewDataWaiting$M7==TRUE
NewDataWaiting$M7[index]=1
index=NewDataWaiting$M8==TRUE
NewDataWaiting$M8[index]=1


index=NewDataWaiting$L1==TRUE
NewDataWaiting$L1[index]=1
index=NewDataWaiting$L2==TRUE
NewDataWaiting$L2[index]=1
index=NewDataWaiting$L3==TRUE
NewDataWaiting$L3[index]=1
index=NewDataWaiting$L4==TRUE
NewDataWaiting$L4[index]=1
index=NewDataWaiting$L5==TRUE
NewDataWaiting$L5[index]=1
index=NewDataWaiting$L6==TRUE
NewDataWaiting$L6[index]=1
index=NewDataWaiting$L7==TRUE
NewDataWaiting$L7[index]=1
index=NewDataWaiting$L8==TRUE
NewDataWaiting$L8[index]=1


################################################### REGRESSION ANALYSIS THREE FIRST MODELS (NO STRATIFICATION BY HOURS)
NewDataWaitingSt=NewDataWaiting
NewDataWaitingSt$Distance=scale(NewDataWaitingSt$Distance)
NewDataWaitingSt$Xstart=scale(NewDataWaitingSt$Xstart)
NewDataWaitingSt$Ystart=scale(NewDataWaitingSt$Ystart)


TotalTime=NewDataWaitingSt$TotalTime
Xend=NewDataWaitingSt$Xend
Yend=NewDataWaitingSt$Yend
WaitingTime=NewDataWaitingSt$WAIT
Distance=NewDataWaitingSt$Distance
Xstart=NewDataWaitingSt$Xstart
Ystart=NewDataWaitingSt$Ystart
EARLYSHIFT=NewDataWaitingSt$EARLYSHIFT
MIDSHIFT=NewDataWaitingSt$MIDSHIFT
LATESHIFT=NewDataWaitingSt$LATESHIFT
WEEKEND=NewDataWaitingSt$WEEKEND


NewData1=data.frame(TotalTime,Distance,Xstart,Ystart,EARLYSHIFT,MIDSHIFT,WEEKEND)
NewData2=data.frame(WaitingTime,Distance,Xstart,Ystart,EARLYSHIFT,MIDSHIFT,WEEKEND)
NewData3=data.frame(TotalTime,WaitingTime, Distance,Xstart,Ystart,EARLYSHIFT,MIDSHIFT,WEEKEND)
set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData1), 0.8*nrow(NewData1))
trainingData <- NewData1[trainingRowIndex, ]  # model training data
testData  <- NewData1[-trainingRowIndex, ]   # test data

fitTrain1=lm(TotalTime ~ Distance+Xstart+Ystart+EARLYSHIFT
             +MIDSHIFT+WEEKEND,data=trainingData)
anova(fitTrain1)
summary(fitTrain1)
fitTrain1
print(xtable(anova(fitTrain1), type = "latex"), file = "TableProject1.tex")
print(xtable(summary(fitTrain1), type = "latex"), file = "TableProject2.tex")

distPred <- predict(fitTrain1, testData[-1])
rmse(testData$TotalTime,distPred)
mape(testData$TotalTime,distPred)
Dif=testData[2]-distPred

Difabs=lapply(Dif,abs)
lapply(Difabs,mean)
lapply(Difabs,sd)




set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData2), 0.8*nrow(NewData2))
trainingData <- NewData2[trainingRowIndex, ]  # model training data
testData  <- NewData2[-trainingRowIndex, ]   # test data

fitTrain2=lm(WaitingTime ~ Distance+Xstart+Ystart+EARLYSHIFT
             +MIDSHIFT+WEEKEND,data=trainingData)
anova(fitTrain2)
summary(fitTrain2)
fitTrain2

print(xtable(anova(fitTrain2), type = "latex"), file = "TableProject3.tex")
print(xtable(summary(fitTrain2), type = "latex"), file = "TableProject4.tex")

stargazer(fitTrain2, "Linear Regression Analysis for Waiting Time",align=TRUE)





fitTrain22=lm(WaitingTime ~Xstart+Ystart+EARLYSHIFT
              +MIDSHIFT,data=trainingData)
anova(fitTrain22)
summary(fitTrain22)
fitTrain22
distPred <- predict(fitTrain22, testData[-1])
rmse(testData$WaitingTime,distPred)
mape(testData$WaitingTime,distPred)
Dif=testData[2]-distPred

Difabs=lapply(Dif,abs)
lapply(Difabs,mean)
lapply(Difabs,sd)

print(xtable(summary(fitTrain22), type = "latex"), file = "TableProject5.tex")



set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData3), 0.8*nrow(NewData3))
trainingData <- NewData3[trainingRowIndex, ]  # model training data
testData  <- NewData3[-trainingRowIndex, ]   # test data

fitTrain3=lm(TotalTime ~ WaitingTime+Distance+Xstart+Ystart+EARLYSHIFT
             +MIDSHIFT+WEEKEND,data=trainingData)
anova(fitTrain3)
summary(fitTrain3)
fitTrain3

distPred <- predict(fitTrain3, testData[-1])
rmse(testData$TotalTime,distPred)
mape(testData$TotalTime,distPred)

Dif=testData[1]-distPred

Difabs=lapply(Dif,abs)
lapply(Difabs,mean)
lapply(Difabs,sd)


print(xtable(anova(fitTrain3), type = "latex"), file = "TableProject6.tex")
print(xtable(summary(fitTrain3), type = "latex"), file = "TableProject7.tex")


################################################### REGRESSION ANALYSIS STRATIFICATION BY HOURS
TotalTime=NewDataWaitingSt$TotalTime
WaitingTime=NewDataWaitingSt$WAIT
Distance=NewDataWaitingSt$Distance
Xstart=NewDataWaitingSt$Xstart
Ystart=NewDataWaitingSt$Ystart
Xend=NewDataWaitingSt$Xend
Yend=NewDataWaitingSt$Yend
E1=NewDataWaitingSt$E1
E2=NewDataWaitingSt$E2
E3=NewDataWaitingSt$E3
E4=NewDataWaitingSt$E4
E5=NewDataWaitingSt$E5
E6=NewDataWaitingSt$E6
E7=NewDataWaitingSt$E7
E8=NewDataWaitingSt$E8
M1=NewDataWaitingSt$M1
M2=NewDataWaitingSt$M2
M3=NewDataWaitingSt$M3
M4=NewDataWaitingSt$M4
M5=NewDataWaitingSt$M5
M6=NewDataWaitingSt$M6
M7=NewDataWaitingSt$M7
M8=NewDataWaitingSt$M8
L1=NewDataWaitingSt$L1
L2=NewDataWaitingSt$L2
L3=NewDataWaitingSt$L3
L4=NewDataWaitingSt$L4
L5=NewDataWaitingSt$L5
L6=NewDataWaitingSt$L6
L7=NewDataWaitingSt$L7
L8=NewDataWaitingSt$L8
WEEKEND=NewDataWaitingSt$WEEKEND

NewData4=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData4), 0.8*nrow(NewData4))
trainingData <- NewData4[trainingRowIndex, ]  # model training data
testData  <- NewData4[-trainingRowIndex, ]   # test data

fitTrain4=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain4)
summary(fitTrain4)
fitTrain4


NewData5=data.frame(TotalTime,Distance,Xstart,Xend,Ystart,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData5), 0.8*nrow(NewData5))
trainingData <- NewData5[trainingRowIndex, ]  # model training data
testData  <- NewData5[-trainingRowIndex, ]   # test data

fitTrain5=lm(TotalTime ~ Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain5)
summary(fitTrain5)
fitTrain5

distPred <- predict(fitTrain5, testData[-1])
rmse(testData$TotalTime,distPred)
mape(testData$TotalTime,distPred)

Dif=testData[1]-distPred

Difabs=lapply(Dif,abs)
lapply(Difabs,mean)
lapply(Difabs,sd)


stargazer(fitTrain5,"Linear Regression Analysis for Total Trip Time (Stratification)",align=TRUE)



NewData6=data.frame(WaitingTime,Distance,Xstart,Xend,Ystart,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)
set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData6), 0.8*nrow(NewData6))
trainingData <- NewData6[trainingRowIndex, ]  # model training data
testData  <- NewData6[-trainingRowIndex, ]   # test data

fitTrain6=lm(WaitingTime ~ Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain6)
summary(fitTrain6)
fitTrain6

stargazer(fitTrain6,"Linear Regression Analysis for Waiting Time (Stratification)",align=TRUE)

stargazer(fitTrain1,fitTrain3,"Linear Regression Analysis for Total Trip Time",align=TRUE)

############################################################################### Regression within Clusters


###################Clusters for Total Time

###Cluster 1 Total Time: 3,16,28,48,55,62
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==3|NewDataWaitingSt$ORIGIN_STAND==16
                    |NewDataWaitingSt$ORIGIN_STAND==28|NewDataWaitingSt$ORIGIN_STAND==48
                    |NewDataWaitingSt$ORIGIN_STAND==55|NewDataWaitingSt$ORIGIN_STAND==62)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain7=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain7)
summary(fitTrain7)
fitTrain7

###Cluster 2 Total Time: 1,6,7,10,17,19,20,23,26,27,29,30,31,32,33,34,35,36,37,42,46,47,49,51,52,54,56,58,60
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==1|NewDataWaitingSt$ORIGIN_STAND==6
                   |NewDataWaitingSt$ORIGIN_STAND==7|NewDataWaitingSt$ORIGIN_STAND==10
                   |NewDataWaitingSt$ORIGIN_STAND==17|NewDataWaitingSt$ORIGIN_STAND==19
                   |NewDataWaitingSt$ORIGIN_STAND==20|NewDataWaitingSt$ORIGIN_STAND==23
                   |NewDataWaitingSt$ORIGIN_STAND==26|NewDataWaitingSt$ORIGIN_STAND==27
                   |NewDataWaitingSt$ORIGIN_STAND==29|NewDataWaitingSt$ORIGIN_STAND==30
                   |NewDataWaitingSt$ORIGIN_STAND==31|NewDataWaitingSt$ORIGIN_STAND==32
                   |NewDataWaitingSt$ORIGIN_STAND==33|NewDataWaitingSt$ORIGIN_STAND==34
                   |NewDataWaitingSt$ORIGIN_STAND==35|NewDataWaitingSt$ORIGIN_STAND==36
                   |NewDataWaitingSt$ORIGIN_STAND==37|NewDataWaitingSt$ORIGIN_STAND==42
                   |NewDataWaitingSt$ORIGIN_STAND==46|NewDataWaitingSt$ORIGIN_STAND==47
                   |NewDataWaitingSt$ORIGIN_STAND==49|NewDataWaitingSt$ORIGIN_STAND==51
                   |NewDataWaitingSt$ORIGIN_STAND==52|NewDataWaitingSt$ORIGIN_STAND==54
                   |NewDataWaitingSt$ORIGIN_STAND==56|NewDataWaitingSt$ORIGIN_STAND==58|NewDataWaitingSt$ORIGIN_STAND==60)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain8=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain8)
summary(fitTrain8)
fitTrain8


###Cluster 3 Total Time: 59
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==59)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain9=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain9)
summary(fitTrain9)
fitTrain9

###Cluster 4 Total Time: 2,4,5,18,22,25,39,41,44,45,50
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==2|NewDataWaitingSt$ORIGIN_STAND==4
                   |NewDataWaitingSt$ORIGIN_STAND==5|NewDataWaitingSt$ORIGIN_STAND==18
                   |NewDataWaitingSt$ORIGIN_STAND==22|NewDataWaitingSt$ORIGIN_STAND==25
                   |NewDataWaitingSt$ORIGIN_STAND==39|NewDataWaitingSt$ORIGIN_STAND==41
                   |NewDataWaitingSt$ORIGIN_STAND==44|NewDataWaitingSt$ORIGIN_STAND==45
                   |NewDataWaitingSt$ORIGIN_STAND==50)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain10=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain10)
summary(fitTrain10)
fitTrain10

###Cluster 5 Total Time: 9,11,15,63
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==9|NewDataWaitingSt$ORIGIN_STAND==11
                   |NewDataWaitingSt$ORIGIN_STAND==15|NewDataWaitingSt$ORIGIN_STAND==63)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain11=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
             +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain11)
summary(fitTrain11)
fitTrain11

###Cluster 6 Total Time: 8,43
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==8|NewDataWaitingSt$ORIGIN_STAND==43)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain12=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain12)
summary(fitTrain12)
fitTrain12

###Cluster 7 Total Time: 12,13,14,21,24,40,53,57,61
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==12|NewDataWaitingSt$ORIGIN_STAND==13
                   |NewDataWaitingSt$ORIGIN_STAND==14|NewDataWaitingSt$ORIGIN_STAND==21
                   |NewDataWaitingSt$ORIGIN_STAND==24|NewDataWaitingSt$ORIGIN_STAND==38
                   |NewDataWaitingSt$ORIGIN_STAND==40|NewDataWaitingSt$ORIGIN_STAND==53
                   |NewDataWaitingSt$ORIGIN_STAND==57|NewDataWaitingSt$ORIGIN_STAND==61)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(TotalTime,Distance,Xstart,Ystart,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain13=lm(TotalTime ~ Distance+Xstart+Ystart+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7,data=trainingData)
anova(fitTrain13)
summary(fitTrain13)
fitTrain13



###################Clusters for Waiting Time

###Cluster 1 Waiting Time: 2,22,29,31,39,44,45,50,59,62
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==2|NewDataWaitingSt$ORIGIN_STAND==22
                   |NewDataWaitingSt$ORIGIN_STAND==29|NewDataWaitingSt$ORIGIN_STAND==31
                   |NewDataWaitingSt$ORIGIN_STAND==39|NewDataWaitingSt$ORIGIN_STAND==44
                   |NewDataWaitingSt$ORIGIN_STAND==45|NewDataWaitingSt$ORIGIN_STAND==50
                   |NewDataWaitingSt$ORIGIN_STAND==59|NewDataWaitingSt$ORIGIN_STAND==62)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain14=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain14)
summary(fitTrain14)
fitTrain14

###Cluster 2 Waiting Time: 5
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==5)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain15=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain15)
summary(fitTrain15)
fitTrain15

###Cluster 3 Waiting Time: 1,3,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,
##28,30,32,33,34,35,36,37,38,40,42,47,49,51,52,53,54,55,56,57,58,60,61,63
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==1|NewDataWaitingSt$ORIGIN_STAND==3
                   |NewDataWaitingSt$ORIGIN_STAND==6|NewDataWaitingSt$ORIGIN_STAND==7
                   |NewDataWaitingSt$ORIGIN_STAND==9|NewDataWaitingSt$ORIGIN_STAND==10
                   |NewDataWaitingSt$ORIGIN_STAND==11|NewDataWaitingSt$ORIGIN_STAND==12
                   |NewDataWaitingSt$ORIGIN_STAND==13|NewDataWaitingSt$ORIGIN_STAND==14
                   |NewDataWaitingSt$ORIGIN_STAND==15|NewDataWaitingSt$ORIGIN_STAND==16
                   |NewDataWaitingSt$ORIGIN_STAND==17|NewDataWaitingSt$ORIGIN_STAND==18
                   |NewDataWaitingSt$ORIGIN_STAND==19|NewDataWaitingSt$ORIGIN_STAND==20
                   |NewDataWaitingSt$ORIGIN_STAND==21|NewDataWaitingSt$ORIGIN_STAND==23
                   |NewDataWaitingSt$ORIGIN_STAND==24|NewDataWaitingSt$ORIGIN_STAND==25
                   |NewDataWaitingSt$ORIGIN_STAND==26|NewDataWaitingSt$ORIGIN_STAND==27
                   |NewDataWaitingSt$ORIGIN_STAND==28|NewDataWaitingSt$ORIGIN_STAND==30
                   |NewDataWaitingSt$ORIGIN_STAND==32|NewDataWaitingSt$ORIGIN_STAND==33
                   |NewDataWaitingSt$ORIGIN_STAND==34|NewDataWaitingSt$ORIGIN_STAND==35
                   |NewDataWaitingSt$ORIGIN_STAND==36|NewDataWaitingSt$ORIGIN_STAND==37
                   |NewDataWaitingSt$ORIGIN_STAND==38|NewDataWaitingSt$ORIGIN_STAND==40
                   |NewDataWaitingSt$ORIGIN_STAND==42|NewDataWaitingSt$ORIGIN_STAND==47
                   |NewDataWaitingSt$ORIGIN_STAND==49|NewDataWaitingSt$ORIGIN_STAND==51
                   |NewDataWaitingSt$ORIGIN_STAND==52|NewDataWaitingSt$ORIGIN_STAND==53
                   |NewDataWaitingSt$ORIGIN_STAND==54|NewDataWaitingSt$ORIGIN_STAND==55
                   |NewDataWaitingSt$ORIGIN_STAND==56|NewDataWaitingSt$ORIGIN_STAND==57
                   |NewDataWaitingSt$ORIGIN_STAND==58|NewDataWaitingSt$ORIGIN_STAND==60
                   |NewDataWaitingSt$ORIGIN_STAND==61|NewDataWaitingSt$ORIGIN_STAND==63)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain16=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain16)
summary(fitTrain16)
fitTrain16

###Cluster 4 Waiting Time: 8,41,43
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==8|NewDataWaitingSt$ORIGIN_STAND==41
                   |NewDataWaitingSt$ORIGIN_STAND==43)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain17=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain17)
summary(fitTrain17)
fitTrain17

###Cluster 5 Waiting Time: 48
##wE DISCARD THIS
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==48)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain18=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain18)
summary(fitTrain18)
fitTrain18

###Cluster 6 Waiting Time: 4,46
DataCluster=subset(NewDataWaitingSt, NewDataWaitingSt$ORIGIN_STAND==4|NewDataWaitingSt$ORIGIN_STAND==46)

TotalTime=DataCluster$TotalTime
WaitingTime=DataCluster$WAIT
Distance=DataCluster$Distance
Xstart=DataCluster$Xstart
Ystart=DataCluster$Ystart
Xend=DataCluster$Xend
Yend=DataCluster$Yend
E1=DataCluster$E1
E2=DataCluster$E2
E3=DataCluster$E3
E4=DataCluster$E4
E5=DataCluster$E5
E6=DataCluster$E6
E7=DataCluster$E7
E8=DataCluster$E8
M1=DataCluster$M1
M2=DataCluster$M2
M3=DataCluster$M3
M4=DataCluster$M4
M5=DataCluster$M5
M6=DataCluster$M6
M7=DataCluster$M7
M8=DataCluster$M8
L1=DataCluster$L1
L2=DataCluster$L2
L3=DataCluster$L3
L4=DataCluster$L4
L5=DataCluster$L5
L6=DataCluster$L6
L7=DataCluster$L7
L8=DataCluster$L8
WEEKEND=DataCluster$WEEKEND

NewData=data.frame(WaitingTime,Distance,Xstart,Ystart,Xend,Yend,E1,E2,E3,E4,E5,E6,E7,E8,M1,M2,M3,M4,M5,M6,M7,M8,L1,L2,L3,L4,L5,L6,L7,WEEKEND)

set.seed(100)
trainingRowIndex <- sample(1:nrow(NewData), 0.8*nrow(NewData))
trainingData <- NewData[trainingRowIndex, ]  # model training data
testData  <- NewData[-trainingRowIndex, ]   # test data

fitTrain19=lm(WaitingTime~Distance+Xstart+Xend+Ystart+Yend+E1+E2+E3+E4+E5+E6+E7+E8
              +M1+M2+M3+M4+M5+M6+M7+M8+L1+L2+L3+L4+L5+L6+L7+WEEKEND,data=trainingData)
anova(fitTrain19)
summary(fitTrain19)
fitTrain19










