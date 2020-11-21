###################################################################
# Extracts Starting Location of Stations from Chunks
# NewDataXY_part1 to 6
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

fileslist = c()
for (i in 1:6){
  fname = paste("NewDataXY_part",toString(i),".csv",sep="")
  fileslist[i] = file.path(CHUNKSPATH,fname)
}

NewDataXY <- lapply(fileslist, read_csv)
NewDataXY <- do.call(rbind , NewDataXY)

#####################################################################
# Find the GPS locations of the stands
#####################################################################

summary.stands <- aggregate(NewDataXY[,c("Xstart","Ystart")],list(NewDataXY$ORIGIN_STAND),mean)
summary.stands <- summary.stands  %>% 
  rename(
    STAND = Group.1  
  )
write_csv(summary.stands,file.path(CHUNKSPATH,"STAND_LOCATIONS.csv"))
write_csv(NewDataXY.file.path(DATAFOLDERPATH,"NewDataXY.csv"))