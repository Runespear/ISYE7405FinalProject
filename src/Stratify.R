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
p_load("reshape","lattice","dtwclust","ggfortify")
p_load("FactoMineR","factoextra","HistDAWass")

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


#############################################
# Hierarchical Clustering with L2Wasserstein
#############################################
station.list <- unique(stand.lag$ORIGIN_STAND)
num.station <- length(station.list)

WAIT.histvec <- vector("list",num.station)
TotalTime.histvec <- vector("list",num.station)
for (i in station.list){
  strat <- as.data.frame(stand.lag[which(stand.lag$ORIGIN_STAND==i),c("TotalTime","WAIT")])
  WAIT.histvec[[i]] <- data2hist(strat$WAIT)
  TotalTime.histvec[[i]] <- data2hist(strat$TotalTime)
}

WAIT.MatH <- new("MatH", nrows=num.station, ncols=1, ListOfDist=WAIT.histvec, names.rows=station.list, names.cols="density")
TotalTime.MatH <- new("MatH", nrows=num.station, ncols=1, ListOfDist=TotalTime.histvec, names.rows=station.list, names.cols="density")

# Loop to see quality increase
# Clustering for Waiting Times
WAIT.df = data.frame()
for(i in 3:10) {
  WAIT.df = rbind(WAIT.df, data.frame(n_clust = i, quality = WH_kmeans(WAIT.MatH, k=i)$quality))
}
WAIT.p<-ggplot(WAIT.df, aes(x=n_clust, y=quality)) + geom_point(size=4) + geom_line() +labs(title="WAIT Cluster Quality")
pname <- paste("WAITQuality.png")
png(file.path(OUTPUTFOLDERPATH,pname),width=1080,height=720,type="cairo",res=100)
print(WAIT.p)
dev.off()
WAIT.decision <- WH_kmeans(WAIT.MatH, k=6)
WAIT.decision.clusters <- unname(WAIT.decision$solution$IDX)

# Clusterin for TotalTime
TotalTime.df = data.frame()
for(i in 3:10) {
  TotalTime.df = rbind(TotalTime.df, data.frame(n_clust = i, quality = WH_kmeans(TotalTime.MatH, k=i)$quality))
}
TotalTime.p<-ggplot(TotalTime.df, aes(x=n_clust, y=quality)) + geom_point(size=4) + geom_line() +labs(title="TotalTime Cluster Quality")
pname <- paste("TotalTimeQuality.png")
png(file.path(OUTPUTFOLDERPATH,pname),width=1080,height=720,type="cairo",res=100)
print(TotalTime.p)
dev.off()
TotalTime.decision <- WH_kmeans(TotalTime.MatH, k=7)
TotalTime.decision.clusters <- unname(TotalTime.decision$solution$IDX)

clusters.df <- data.frame("ORIGIN_STAND"=station.list,"TotalTime.clusters"=TotalTime.decision.clusters,"WAIT.clusters"=WAIT.decision.clusters)
cluster.fname <- "cluster_assignment.csv"
cluster.path <- file.path(CHUNKSFOLDERPATH,cluster.fname)
write_csv(clusters.df,cluster.path,cluster.path)

#cluster.TABLE <- xtable(clusters.df,digits=c(0,0,0,0))

sink(file.path(OUTPUTFOLDERPATH,"clusterTABLE.txt"))
print(xtable(clusters.df,digits=c(0,0,0,0)),include.rownames=FALSE,file=file.path(OUTPUTFOLDERPATH,"clusterTABLE.txt"))
sink()




break
############################
# Waiting Times Histogram
############################
step = 30
for (i in 1:ceil(63/step)){
  test.stand.lag <- stand.lag[which( (i-1)*step < stand.lag$ORIGIN_STAND & stand.lag$ORIGIN_STAND <= i*step ),]
  p<-ggplot(test.stand.lag,aes(x=WAIT))+geom_histogram(binwidth=5,fill="white",colour="black")+
    facet_wrap(~ORIGIN_STAND, scales = "free") + labs(title="Interarrival Times Histogram by station")
  pname <- paste("HIST_WAIT_part",toString(i),".png",sep="")
  png(file.path(OUTPUTFOLDERPATH,pname),width=3840,height=2160,type="cairo",res=100)
  print(p)
  dev.off()
  #histvec <- c(histvec,get_hist(p))
}
############################
# Travel Times Histogram
############################
step = 30
#test.stand.lag.vec <- c()
for (i in 1:ceil(63/step)){
  test.stand.lag <- stand.lag[which( (i-1)*step < stand.lag$ORIGIN_STAND & stand.lag$ORIGIN_STAND <= i*step ),]
  p<-ggplot(test.stand.lag,aes(x=TotalTime))+geom_histogram(binwidth=5,fill="white",colour="black")+
    facet_wrap(~ORIGIN_STAND, scales = "free") + labs(title="Travel Times Histogram by station")
  pname <- paste("HIST_TOTALTIME_part",toString(i),".png",sep="")
  png(file.path(OUTPUTFOLDERPATH,pname),width=3840,height=2160,type="cairo",res=100)
  print(p)
  dev.off()
  #test.stand.lag.vec <- c(test.stand.lag,test.stand.lag)
}


###########################
# PCA of NewTaxiDataXY
###########################

# Exclude non numeric data
PCAINPUT <- subset(NewDataXY, select=c(TotalTime,Xstart,Xend,Ystart,Yend,TotalTime))
pca_res <- prcomp(PCAINPUT, scale. = TRUE)

ap <- autoplot(pca_res, data = NewDataXY, colour = 'DAYOFWEEK',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
png(file.path(OUTPUTFOLDERPATH,"PCA.png"),width=1920,height=1080,res=100)
print(ap)
dev.off()

PCAINPUT2 <- subset(NewDataXY, select=c(Xstart,Xend,Ystart,Yend))
pca_res2 <- prcomp(PCAINPUT2, scale. = TRUE)

ap <- autoplot(pca_res2, data = NewDataXY, colour = 'DAYOFWEEK',
               loadings = TRUE, loadings.colour = 'blue',
               loadings.label = TRUE, loadings.label.size = 3)
png(file.path(OUTPUTFOLDERPATH,"PCA_no_TOTALTIME.png"),width=1920,height=1080,res=100)
print(ap)
dev.off()

