
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


## Source functions
source('Functions/create_random_traj.R')


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
data2007$LMT_date <- as.POSIXct(data2007$LMT_date, format='%d-%m-%Y %H:%M:%S')

## Pre-processing
# delete obvious outlier caused by GPS error
data2007 <- subset(data2007, Locale_E > 1400000)

# SUBSETS and ordering
Koski2007 <- subset(data2007, PubName == "Koski (2310)")
Koski2007 <- Koski2007[order(Koski2007$LMT_date),]      # order on data_time

tstart <- min(Koski2007$LMT_date)
Koski2007$tspan <- difftime(Koski2007$LMT_date, tstart, units="hours")

lKoski <- Lines(Line(cbind(Koski2007$Locale_E, Koski2007$Locale_N)),"1")
lKoski <- SpatialLines(list(lKoski), proj4string = CRS("+init=epsg:2400"))
lKoski <- SpatialLinesDataFrame(lKoski, data=data.frame(ID="1",name="Koski"), match.ID = T)

mypoints <-  SpatialPointsDataFrame(cbind(Koski2007$Locale_E, Koski2007$Locale_N), data =Koski2007,
                                    proj4string=CRS("+init=epsg:2400"))
mypoints@data$tspannum<- as.numeric(mypoints@data$tspan)

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

## Compute random trajectories
# xyt = spatiotemporal points, V = Speed, t = time interval
for (i in seq(1:100)){
  
  create_random_traj(xyt,V,t)
  
  
  
}

## Create line density surface of trajectories 
## Evaluate results

## Visualize results