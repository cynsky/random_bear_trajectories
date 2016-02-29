create_random_traj <- function(xyt,V,t){
  #' Standardise values
  #'      Arg:
  #'          xyt(DF): dataframe with spatiotemporal points: 1st colomm x coordinate, 2nd colomn y coordinate and 3rd colomn time. 
  #'          V(int) = maximum Speed
  #'          t(float) = time interval
  #'      Return:
  #'        	A random trajectory(SpatialPointsDataFrame) 
  
  # Takes the input raster and makes the scores between 0 and 1
  out_raster <- calc(in_raster, function(x) x/maxValue(in_raster))
  return(out_raster)
}