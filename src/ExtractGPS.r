###################################################################
# Parses Polyline in NewTaxiDataFiltered.csv
# Adds in start and end X,Y coordinates
###################################################################
library("pacman")
p_load("readr","dplyr","stringr","rstudioapi","parallel","xtable","here","anytime")
p_load("pracma","lubridate","foreach")
# Set path to data folder
setwd(here())

# Start Clock 
ptm <- proc.time()

###################################################################
DATAFOLDERPATH = "./data"
OUTPUTPATH = file.path(DATAFOLDERPATH,"output")
CHUNKSPATH = file.path(DATAFOLDERPATH,"chunks")
dir.create(OUTPUTPATH,showWarnings=FALSE)
dir.create(CHUNKSPATH,showWarnings=FALSE)

###################################################################
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
# Create folder for output

NewTaxiDataFiltered = file.path(DATAFOLDERPATH,"NewTaxiDataFiltered.csv")
NewTaxiDataFiltered = read_csv(file = NewTaxiDataFiltered)

###################################################################
ColPol=NewTaxiDataFiltered[,'POLYLINE']
l=length(ColPol)
nm=nrow(NewTaxiDataFiltered)
TotalTime=vector(,l)
####################################################################
Xstart=rep(NA,nm)
Ystart=rep(NA,nm)
Xend=rep(NA,nm)
Yend=rep(NA,nm)
for (i in 1:nm){
  #print(i)
  A=NewTaxiDataFiltered[i,'POLYLINE']
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


NewDataXY=data.frame(NewTaxiDataFiltered,Xstart,Xend,Ystart,Yend)
NewDataXY$POLYLINE<- NULL
write_csv(NewDataXY,file.path(DATAFOLDERPATH,"NewDataXY.csv"))

nr <- nrow(NewDataXY)
n <- nr/6
k = split(NewDataXY, rep(1:ceiling(nr/n), each=n, length.out=nr))
for (i in 1:6){
  fname = paste("NewDataXY_part", toString(i),".csv" ,sep="")
  write_csv(k[[i]],file.path(CHUNKSPATH,fname))
}



# Join this file with NewTaxiDataFiltered.csv after filtering < 3
XYticks <- NewDataXY[c("TRIP_ID","Xstart","Xend","Ystart","Yend")]
#write_csv(XYticks,file.path(DATAFOLDERPATH,"XYticks.csv"))
# Split file into smaller chunks


nr <- nrow(XYticks)
n <- nr/3
k = split(XYticks, rep(1:ceiling(nr/n), each=n, length.out=nr))
for (i in 1:3){
  fname = paste("XYticks_part", toString(i),".csv" ,sep="")
  write_csv(k[[i]],file.path(CHUNKSPATH,fname))
}

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

