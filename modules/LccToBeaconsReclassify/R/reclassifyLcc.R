# Make a factor map, allowing for character labels

reclassifyLcc <- function(sim = sim, 
                          vegMap = sim$vegMap, 
                          lcc05VegTable = sim$lcc05VegTable, 
                          lcc05VegReclass = sim$lcc05VegReclass, 
                          lcc05TrajTable = sim$lcc05TrajTable){

sim$vegMapBeacons <- raster::ratify(raster::reclassify(vegMap, lcc05VegTable))

base::levels(sim$vegMapBeacons) <- data.frame(
  ID = lcc05VegReclass$VEG.reclass,
  Class = lcc05VegReclass$Description
) %>%
  .[raster::levels(sim$vegMapBeacons)[[1]]$ID,]

indices <- c(1, lcc05VegTable[, 1][fastmatch::fmatch(1:11, lcc05VegTable[, 2])] + 1)
setColors(sim$vegMapBeacons, n = 12) <- getColors(sim$vegMap)[[1]][indices]

sim$trajMapBeacons <- raster::reclassify(sim$vegMap, lcc05TrajTable)
setColors(sim$trajMapBeacons, n = 12) <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(12)

return(sim)

}