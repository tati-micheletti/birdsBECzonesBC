outputageForest <- function(sim) {
  # store the output interval forest cover rasters
  sim$ageForest[[time(sim)]] <- sim$ageMap
  return(invisible(sim))
}