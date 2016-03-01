mypoints <-  SpatialPointsDataFrame(cbind(Koski2007$Locale_E, Koski2007$Locale_N), data =Koski2007,
                                    proj4string=CRS("+init=epsg:2400"))
mypoints@data$tspannum<- as.numeric(mypoints@data$tspan)
mypoints<-mypoints[1:10,]

# 74:150 bij V<-2000
#tinterval <- 3
mypoints<-SpatialPointsDataFrame(mypoints@coords,as.data.frame(mypoints@data$tspannum))
names(mypoints)<-'tspannum'


V<-5000
tinterval <- 2
allpoints<-mypoints[length(mypoints),]
xy<-c()
time<-c()

tvector<- sample(seq(1:(tinterval-1)))
plot(mypoints,col='blue',xlim=c(bbox(mypoints)[1][1]-(V/3),bbox(mypoints)[,2][1]+(V/3)),axes=T,
     ylim=c(bbox(mypoints)[2][1]-(V/3),bbox(mypoints)[,2][2]+(V/3)),xlab='X (meters)', ylab= 'Y (meters)',
     main='Random trajectory between known points')
for(j in seq(1:(length(mypoints)-1))){
  startpoint<-mypoints[j,]
  endpoint<- mypoints[j+1,]
  newpoints<-mypoints[j,]
  npoint<-startpoint
  last_t<-0
  t<-(mypoints[j+1,]@data$tspannum-mypoints[j,]@data$tspannum)/tinterval
  
  for(i in tvector){
    if (i<last_t)
    { endpoint<-npoint
      }
    if (i>last_t)
    { startpoint<-npoint
      }
    buffer1<-gBuffer(startpoint,width=(V*(t*i)),quadsegs=7)
    buffer2<-gBuffer(endpoint,width=(V*(t*(tinterval-i))),quadsegs=7)
    plot(buffer1,add=T)
    plot(buffer2,add=T,border='red')
    PpA<-gIntersection(buffer1,buffer2) 
    plot(PpA,add=T,col='green')
    npoint<-spsample(PpA,1,type = 'random')
    npoint<-SpatialPointsDataFrame(npoint,data=as.data.frame(mypoints@data$tspannum[j]+(i*t)))
    names(npoint)<-'tspannum'
    newpoints<-spRbind(newpoints,npoint)
    last_t<-i
  
  }
  xy<-rbind(xy,newpoints@coords)
  time <- c(time,newpoints@data$tspannum)
}

allpoints<-SpatialPointsDataFrame(xy[order(time),],data=as.data.frame(c(time[order(time)])))
names(allpoints)<- 'tspannum'
allpoints<-spRbind(allpoints,mypoints[length(mypoints),])

plot(allpoints,add=T,col='blue',pch='*')
lines(allpoints@coords)
plot(mypoints,add=T,col='red')


## spplot
spplot(mypoints,zcol='tspannum',colorkey = list(
  right = list( # see ?levelplot in package trellis, argument colorkey:
    fun = draw.colorkey, 
    args = list(
      key = list(
        at = seq(0, max(mypoints@data$tspannum), 10), # colour breaks
        col = bpy.colors(length(seq(0, max(mypoints@data$tspannum), 10))), # colours
        labels = list(
          at = c(0, median(seq(0, max(mypoints@data$tspannum), 10)),740), 
          labels = c("0 hour", "370 hour", "740 hour")
        )
      )
    )
  )
))

#### original function
V<-100
tinterval <- 16
allpoints<-mypoints[length(mypoints),]
xy<-c()
time<-c()

for(j in seq(1:(length(mypoints)-1))){
  npoint<-mypoints[j,]
  newpoints<-mypoints[j,]
  plot(mypoints,col='blue')
  t<-(mypoints[j+1,]@data$tspannum-mypoints[j,]@data$tspannum)/tinterval
  m=1
  for(i in seq(tinterval,2,-1)){
    
    buffer1<-gBuffer(npoint,width=(V*t),quadsegs=7)
    buffer2<-gBuffer(mypoints[j+1,],width=(V*t)*i,quadsegs=7)
    PpA<-gIntersection(buffer1,buffer2) 
    npoint<-spsample(PpA,1,type = 'random')
    npoint<-SpatialPointsDataFrame(npoint,data=as.data.frame(mypoints@data$tspannum[j]+m*(t)))
    names(npoint)<-'tspannum'
    newpoints<-spRbind(newpoints,npoint)
    m <- m+1
    
  }
  xy<-rbind(xy,newpoints@coords)
  time <- c(time,newpoints@data$tspannum)
}

allpoints<-SpatialPointsDataFrame(xy,data=as.data.frame(c(time)))
names(allpoints)<- 'tspannum'
allpoints<-spRbind(allpoints,mypoints[length(mypoints),])

plot(allpoints)
lines(allpoints@coords)
plot(mypoints,add=T,col='red')
