
## Mini project Advanced GI Science for Earth and Environment
## Period 4, Academic Year 2015-2016
## Case 1 - Random Animal Trajectories
##M ark ten Vregelaar – Stijn Wijdeven – Joris Wever – Erwin van den Berg – Bob Houtkooper
## 29-02-2016

## Import modules
library(rgl)
library(sp)
library(rgdal)
library(rgeos)
library(spacetime)
library(lattice)
library(maptools)
library(plyr)
library(raster)
## Source functions
source('Functions/create_random_traj.R')
source('Functions/maxspeed2009.R')
source('Functions/getbeardata.R')
source('Functions/LineDensityRaster.R')
## Create directories
data_dir = 'Data'
output_dir = 'Output'
functions_dir = 'Functions'

dir_list = list(data_dir, output_dir, functions_dir)
for (i in dir_list)
{if (!file.exists(i)){
  dir.create(i)
} 
}

## Load the data
load("Data/mating2009.Rdata")
DEM<- readGDAL("Data/DEM.tif") 
data2007 <- read.table("Data/August2007.txt", header = T, sep=",")
data2007$GMT_date <- NULL
data2007$LMT_date <- as.POSIXct(data2007$LMT_date, 
                                  format='%d-%m-%Y %H:%M:%S')


Koski2007 <- subset(data2007, PubName == "Koski (2310)")
Koski2007 <- Koski2007[order(Koski2007$LMT_date),]      # order on data_time
tstartkoski <- min(Koski2007$LMT_date)
Koski2007<-getbeardata(data2007,Pubname = "Koski (2310)",tstart = tstartkoski )

mating2009$LMT_date <- as.POSIXct(mating2009$LMT_date, 
                                  format='%d-%m-%Y %H:%M:%S')

Krut2009 <- subset(mating2009, PubName == "Krut (2937)")
tstartkrut <- min(Krut2009$LMT_date)
Krut2009<-getbeardata(mating2009,Pubname = "Krut (2937)",tstart = tstartkrut )


mspeedKrut <- maxspeed2009('Krut') 

mypoints<-Krut2009
#mypoints<-subset(mypoints,tspannum>450 & tspannum<700)
mypoints<-subset(mypoints,tspannum>690 & tspannum<700)
## Compute random trajectories
V=mspeedKrut*1000+100
Rtrajectories<-list()

for (i in seq(1:100)){
  
  Rtrajectory<-create_random_traj(mypoints,V,4)
  Rtrajectories<-c(Rtrajectories,Rtrajectory)
  
  
}


plot(mypoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/5),bbox(mypoints)[,2][1]+(V/5)),axes=T,
     ylim=c(bbox(mypoints)[2][1]-(V/5),bbox(mypoints)[,2][2]+(V/5)),xlab='X (meters)', ylab= 'Y (meters)',
     main='Random trajectory between known points')
lapply(Rtrajectories,function(x) lines(x[1]@coords,add=T,col='red'))

#plot(mypoints,add=T,col='blue')
## Create line density surface of trajectories 
LineDensity <- LineDensityRaster(Rtrajectories,500)

plot(LineDensity, col=colorRampPalette(c("white", "orangered", "black"))(101))
plot(mypoints,add=T,pch=19,col='yellow')
lapply(Rtrajectories,function(x) lines(x[1]@coords,add=T,col=rgb(0,0,0,alpha=0.15)))
## Evaluate results

## Visualize results

