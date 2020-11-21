library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate","foreach")
# Set path to data folder
setwd(here())

# Start Clock 
ptm <- proc.time()

###################################################################
DATAFOLDERPATH = "./data/"
OUTPUTPATH = file.path(DATAFOLDERPATH,"output")
dir.create(OUTPUTPATH,showWarnings=FALSE)

###################################################################
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
# Create folder for output

TaxiDataA = file.path(DATAFOLDERPATH,"train.csv")
TaxiData = read_csv(file = TaxiDataA)
TaxiData = TaxiData[which(!TaxiData$MISSING_DATA),]
TaxiData$MISSING_DATA <- NULL
ColPol=TaxiData[,'POLYLINE']
l=length(ColPol)
nm=nrow(TaxiData)
TotalTime=vector(,l)
####################################################################
Xstart=rep(NA,nm)
Ystart=rep(NA,nm)
Xend=rep(NA,nm)
Yend=rep(NA,nm)
for (i in 1:nm){
  A=TaxiData[i,'POLYLINE']
  B=str_split(A,",")
  Inter=data.frame(B)
  len=nrow(Inter)
  p1=1
  p2=1
  Xstart[i]=as.double(numextract(Inter[1,]))
  Xend[i]=as.double(numextract(Inter[len-1,]))
  Ystart[i]=as.double(numextract(Inter[2,]))
  Yend[i]=as.double(numextract(Inter[len,]))
}


NewDataXY=data.frame(TaxiData,Xstart,Xend,Ystart,Yend)

###########################################################################
TimeTaken <- proc.time() - ptm
TimeTaken






#sum(TotalTime)
#min(TotalTime)
#max(TotalTime)
#Vec <- rep(1, l)
#TotalTimeINmin=TotalTime/60
#NewData=data.frame(TaxiData,TotalTimeINmin,Vec)
#cc=TaxiData[colnames(unique(TaxiData[,"TAXI_ID"])),]
#cc=unique(TaxiData[,"TAXI_ID"])
#lID=length(cc)
#sum(subset(NewData, NewData$TAXI_ID==cc[1])$TotalTime)
#sum(subset(NewData, NewData$TAXI_ID==cc[1])$Vec)
#dc='14.005'
#dcn=as.double(dc)
#sss=data.frame(Inter)
#sss[1]

#x=numextract(sss[1])
#str_split(A,',')

