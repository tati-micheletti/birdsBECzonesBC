fireSpread <- function(sim) {
  
  ### create burn map that tracks fire locations over time
  
  sim$maxFiresCumul <- 7 # used in legend scale
  
  sim$Fires <- raster(extent(sim$vegMap), ncol = ncol(sim$vegMap),
                      nrow = nrow(sim$vegMap), vals = 0) %>%
    mask(sim$vegMap)
  
  #sim$Fires <- as(sim$Fires, "RasterLayerSparse")
  setColors(sim$Fires, n = P(sim)$nFires + 1) <-
    c("#FFFFFF", rev(heat.colors(P(sim)$nFires)))
  
  sim$FiresCumul <- sim$Fires
  
  return(invisible(sim))
}