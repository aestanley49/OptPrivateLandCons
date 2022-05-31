###########################################################################################
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###########################################################################################
### ### ### Source of base code: https://github.com/hurlbertlab/core-transient-simulation/blob/52ab2aa2d163b5d5dd1e4a8a6aacb1c66d1e4f76/Code/CTSim/R/make_landscape.r
### ### ### From package: https://github.com/hurlbertlab/core-transient-simulation/tree/52ab2aa2d163b5d5dd1e4a8a6aacb1c66d1e4f76


library("gstat")
library("raster")
library("sp")

library("landscapemetrics") 

make_landscape = function(x=NULL, y=NA, mod=NULL, d=NA, prop=NA, draw_plot=F){
  
  # Catch error if no dimensions specified
  if(is.null(x)&is.na(y)){ stop('Must supply grid dimensions.') }
  if(is.na(y)&length(x)==2){
    y = x[2]
    x = x[1]
  } 
  if(is.na(y)) { stop('Must supply grid dimensions.') }
  
  # Define coordinates
  locs = expand.grid(1:x, 1:y)
  names(locs) = c('x','y')
  
  # Define spatial model with an expected value of 0 and no spatial trend (by universal kriging)
  # See vgm() in gstat package for explanation of spatial correlation models
  # Range defaults to 1/3 of the smallest grid dimension
  if(is.null(mod)){
    mod = gstat::vgm(psill=1, model='Gau', range=ifelse(is.na(d), min(x/3,y/3), d))
  }
  spatial_model = gstat::gstat(formula=z~1, locations=~x+y, dummy=T, beta=0, model=mod)
      # for ordinary and simple kriging use the formula z~1
      # 
  
  # Define proportion habitat B if unspecified
  if(is.na(prop)) prop = 0.25
  
  # Simulate values at locations
  values = predict(spatial_model, locs, nsim=1)
  
  # Convert to spatial data and raster grid
  sp::gridded(values) = ~x+y
  values_grid = raster::raster(values)
  
  # Threshold to binary habitat based on proportion of habitat in each type
  threshold = raster::quantile(values_grid, prop)
  binary_grid = values_grid > threshold
  binary_grid[binary_grid==0] = -1
  
  # Draw landscape
  if(draw_plot) plot(binary_grid)
  
  # Return landscape
  binary_grid
}


tryme <- make_landscape(20, 20, d = 10, prop = .55, draw_plot = TRUE)


### q - how decide:
  # which model type to use?
    # The sill is the value of semivariance beyond the estimated range, i.e., the variability that cannot be attributed to spatial autocorrelation. Note that some theoretical models assume that there is no sill (e.g., an exponential model; Fig. 5.3) while others assume that there is no nugget (i.e., the intercept 1⁄4 0)
  # d will determine level of autocorrelation? "The range indicates the distance up which the spatial dependence occurs, such that beyond the range, the data are no longer spatially autocorrelated"


