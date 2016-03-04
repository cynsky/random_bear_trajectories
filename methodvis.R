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



mspeedKrut <- maxspeed2009('Krut') 

xyt<-Krut2009
#mypoints<-subset(mypoints,tspannum>450 & tspannum<700)
xyt<-subset(xyt,tspannum>698.5 & tspannum<700)
mypoints<-xyt
## Compute random trajectories
V=2400


tinterval<-3
allpoints<-xyt[length(xyt),]
xy<-c()
time<-c()
tvector<- sample(seq(1:(tinterval-1)))

plot(mypoints,xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
     ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
     main='Random trajectory between known points')
apoints<-xyt[1,]
      
for(j in seq(1:(length(xyt)-1))){
  startpoint<-xyt[j,]
  endpoint<- xyt[j+1,]
  newpoints<-xyt[j,]
  npoint<-startpoint
  last_t<-0
  t<-(xyt[j+1,]@data$tspannum-xyt[j,]@data$tspannum)/tinterval
  
  for(i in tvector){
    if (i<last_t)
    { endpoint<-npoint
    }
    if (i>last_t)
    { startpoint<-npoint
    }
    plot(apoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
         ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
         main='Random trajectory between known points')
    plot(mypoints,add=T)
    
    
    buffer1<-gBuffer(startpoint,width=(V*(t*i)),quadsegs=7)
    
    plot(buffer1,add=T)
    
    buffer2<-gBuffer(endpoint,width=(V*(t*(tinterval-i))),quadsegs=7)
    
    plot(apoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
         ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
         main='Random trajectory between known points')
    plot(mypoints,add=T)
    
    plot(buffer1,add=T)
    plot(buffer2,border='red',add=T)
    
    PpA<-gIntersection(buffer1,buffer2)
    
    plot(apoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
         ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
         main='Random trajectory between known points')
    
    plot(npoint,col='blue',add=T,size=2)
    plot(buffer1,add=T)
    plot(buffer2,border='red',add=T)
    plot(PpA,add=T,col='green')
    plot(mypoints,add=T)
    
    npoint<-spsample(PpA,1,type = 'random')
    npoint<-SpatialPointsDataFrame(npoint,data=as.data.frame(xyt@data$tspannum[j]+(i*t)))
    
    plot(apoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
         ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
         main='Random trajectory between known points')
    plot(PpA,add=T,col='green')
    plot(mypoints,add=T)
    plot(npoint,col='blue',add=T,size=2)
    
    plot(apoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
         ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
         main='Random trajectory between known points')
    plot(mypoints,add=T)
    plot(npoint,col='blue',add=T,size=2)
    
    names(npoint)<-'tspannum'
    newpoints<-spRbind(newpoints,npoint)
    apoints<-spRbind(apoints,npoint)
    last_t<-i
  }
  xy<-rbind(xy,newpoints@coords)
  time <- c(time,newpoints@data$tspannum)
  allpoints<-SpatialPointsDataFrame(xy[order(time),],data=as.data.frame(c(time[order(time)])))
}

allpoints<-SpatialPointsDataFrame(xy[order(time),],data=as.data.frame(c(time[order(time)])))
names(allpoints)<- 'tspannum'
allpoints<-spRbind(allpoints,xyt[length(xyt),])

plot(allpoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
     ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
     main='Random trajectory between known points')
plot(mypoints,add=T)

lines(allpoints[1]@coords,add=T)


