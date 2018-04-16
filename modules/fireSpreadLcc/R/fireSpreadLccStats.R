fireSpreadLccStats <- function(sim) {
  npix <- sim[[P(sim)$burnStatsName]]
  sim[[P(sim)$burnStatsName]] <- c(npix, length(which(values(sim$Fires) > 0)))
  return(invisible(sim))
}
