forestAgeAge <- function(sim) {
  
  quickPlot::setColors(sim$ageMap, n = 201) <- colorRampPalette(c("LightGreen", "DarkGreen"))(50)
  sim$ageMap <- setValues(sim$ageMap, pmin(200, getValues(sim$ageMap) +
                                             params(sim)$forestAge$returnInterval))
  if (exists("Fires", envir = envir(sim))) {
    sim$ageMap[sim$Fires>0] <- 0
  }
  quickPlot::setColors(sim$ageMap, n = 201) <- colorRampPalette(c("LightGreen", "darkgreen"))(50)
  
  return(invisible(sim))
}
