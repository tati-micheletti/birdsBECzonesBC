forestSuccessionSuccession <- function(sim) {

  # assuming ageMap has zeros on it, this increases index to 1
  sim$vegMap <- sim$vegMapBeacons
  sim$trajMap <- sim$trajMapBeacons
  ageMap.v <- round(raster::getValues(sim$ageMap)) + 1
  trajMap.v <- raster::getValues(sim$trajMap)
  sim$vegMap <- raster::setValues(sim$vegMap, sim$trajObj[cbind(ageMap.v, trajMap.v)])
  return(invisible(sim))
  
}