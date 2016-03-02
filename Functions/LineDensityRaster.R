LineDensityRaster <- function(listOfSpatialPointsDF,cellfactor){
  #' Standardise values
  #'      Arg:
  #'          ListOfSpatialPointsDF(list): A list of SpatialPointsDataframes containing the coordinates of the points, thus the paths, between given points.
  #'      Return:
  #'        	A raster containing the amount of lines per cell(SpatialPointsDataFrame) 
  
  
  
  
  # convert the spPointsDF to a spLinesDF
  pathList <- c()
  iterations <- 0
  
  for(i in listOfSpatialPointsDF){
    iterations <- iterations + 1 
    
    line_obj <- Lines(Line(i@coords), "1")
    line_sp <- SpatialLines(list(line_obj), proj4string=CRS("+init=epsg:2400"))
    line_df <- SpatialLinesDataFrame(line_sp,
                                     data=data.frame(ID=iterations,name="Koski", data=i@data))
    pathList <- c(pathList, line_df)
  }
  
  
  # Rtrajectories[[1]]@proj4string
 
  # specify output extent based on input extents
  outRasterExtent <- vector()
  
  for( i in 1:length(pathList)){
    if(i == 1){
      outRasterExtent['xmin'] <- xmin(pathList[[i]])
      outRasterExtent['xmax'] <- xmax(pathList[[i]])
      outRasterExtent['ymin'] <- ymin(pathList[[i]])
      outRasterExtent['ymax'] <- ymax(pathList[[i]])
    } else {
      if(extent(pathList[[i]])[1] < outRasterExtent['xmin']){
        outRasterExtent['xmin'] <- extent(pathList[[i]])[1] }
      if(extent(pathList[[i]])[2] > outRasterExtent['xmax']){
        outRasterExtent['xmax'] <- extent(pathList[[i]])[2] }
      if(extent(pathList[[i]])[3] < outRasterExtent['ymin']){
        outRasterExtent['ymin'] <- extent(pathList[[i]])[3] }
      if(extent(pathList[[i]])[4] > outRasterExtent['ymax']){
        outRasterExtent['ymax'] <- extent(pathList[[i]])[4] }
    }
  }
  outRasterExtent <- extent(outRasterExtent['xmin'],
                            outRasterExtent['xmax'],
                            outRasterExtent['ymin'],
                            outRasterExtent['ymax'])
  
  # empty Line Density Raster
  LineDensity <- raster(outRasterExtent, crs = projection(pathList[1]), vals=0, 
                        ncols=(outRasterExtent[2]/cellfactor) - (outRasterExtent[1]/cellfactor), #xmax - xmin
                        nrows=(outRasterExtent[4]/cellfactor) - (outRasterExtent[3]/cellfactor)) #ymax - ymin
  
  # create raster
  for(i in pathList){
    
    # create single raster per line
    singleRaster <- raster(outRasterExtent, crs = projection(i),
                           ncols=(outRasterExtent[2]/cellfactor) - (outRasterExtent[1]/cellfactor), #xmax - xmin
                           nrows=(outRasterExtent[4]/cellfactor) - (outRasterExtent[3]/cellfactor))
    
    # loop over each cell in the single raster and find if it intersects with a line.
    # If it does, assign a value of 1
    # If it doesn't, assign a value of 0
    overlay <- sapply(1:ncell(singleRaster), function(j) {
      tmp_rst <- singleRaster
      tmp_rst[j] <- 1
      tmp_shp <- rasterToPolygons(tmp_rst)
      
      if (gIntersects(i, tmp_shp)) {
        crp <- crop(i, tmp_shp)
        crp_length <- gLength(crp)
        if(crp_length > 0){
          return(1)}
      } else {
        return(0)
      }
    })
    singleRaster[] <- overlay
    
    # add the single raster to the final raster
    LineDensity <- LineDensity + singleRaster
  }
  
  
  return(LineDensity)
}