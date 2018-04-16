outputForestCover <- function(sim) {
  # store the output interval forest cover rasters
  sim$forestCover[[time(sim)]] <- sim$vegMap
  return(invisible(sim))
}