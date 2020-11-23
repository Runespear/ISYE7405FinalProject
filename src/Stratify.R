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
p_load("reshape","lattice","dtwclust")

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
  dplyr::rename(
    STAND = 'Group.1',
    TotalRides = 'vones'
  )
stand.summary$AVG_TRIP_TIME <- stand.summary$TotalTime/stand.summary$TotalRides
stand.summary <- merge(x=stand.summary,y=Coordinates,by="STAND")
stand.summary <- stand.summary  %>% 
  dplyr::rename(
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
  dplyr::rename(
    STAND = Group.1
  )
stand.lag.summary <- merge(x=stand.summary,y=stand.lag.summary,by="STAND")

write_csv(stand.lag.summary,file.path(CHUNKSFOLDERPATH,"STAND_LAG_SUMMARY.csv"))


# xyplot( WAIT ~ TIMESTAMP ,groups=ORIGIN_STAND, data=stand.lag,t="l")
# Plot histogram of waiting times for each station
get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}
histvec = c()

############################
# Waiting Times Histogram
############################
step = 30
for (i in 1:ceil(63/step)){
  test.stand.lag <- stand.lag[which( (i-1)*step < stand.lag$ORIGIN_STAND & stand.lag$ORIGIN_STAND <= i*step ),]
  p<-ggplot(test.stand.lag,aes(x=WAIT))+geom_histogram(binwidth=5,fill="white",colour="black")+
    facet_wrap(~ORIGIN_STAND, scales = "free") + labs(title="Interarrival Times Histogram by station")
  pname <- paste("HIST_WAIT_part",toString(i),".png",sep="")
  png(file.path(OUTPUTFOLDERPATH,pname),width=1920,height=1080,type="cairo")
  print(p)
  dev.off()
  #histvec <- c(histvec,get_hist(p))
}
############################
# Travel Times Histogram
############################
step = 30
for (i in 1:ceil(63/step)){
  test.stand.lag <- stand.lag[which( (i-1)*step < stand.lag$ORIGIN_STAND & stand.lag$ORIGIN_STAND <= i*step ),]
  p<-ggplot(test.stand.lag,aes(x=TotalTime))+geom_histogram(binwidth=5,fill="white",colour="black")+
    facet_wrap(~ORIGIN_STAND, scales = "free") + labs(title="Travel Times Histogram by station")
  pname <- paste("HIST_TOTALTIME_part",toString(i),".png",sep="")
  png(file.path(OUTPUTFOLDERPATH,pname),width=1920,height=1080,type="cairo")
  print(p)
  dev.off()
  #histvec <- c(histvec,get_hist(p))
}




