fireSpreadLccBurn <- function(sim) {
  fireSpreadProb <- reclassify(x = sim$vegMap,
                               rcl = cbind(1:11,
                                           c(0.225, 0.225, 0.21, 0.15, 0.15,
                                             0.18, 0.1, 0.1, 0, 0, 0)*
                                             P(sim)$drought))
  
  nFires <- rnbinom(n = 1, size = P(sim)$fireSize, mu = P(sim)$nFires*P(sim)$drought)
  sim$Fires <- SpaDES.tools::spread(fireSpreadProb, # CHANGE TO SPREAD2?
                                    loci = as.integer(sample(1:ncell(fireSpreadProb), nFires)),
                                    spreadProb = fireSpreadProb,
                                    persistance = P(sim)$persistprob,
                                    mask = NULL,
                                    maxSize = 1e8,
                                    directions = 8,
                                    iterations = P(sim)$its,
                                    plot.it = FALSE,
                                    id = TRUE)
  
  sim$Fires[is.na(sim$vegMap)] <- NA
  names(sim$Fires) <- "Fires"
  setColors(sim$Fires, n = nFires + 1) <- c("#FFFFFF", rev(heat.colors(nFires)))
  
  sim$FiresCumul[] <- sim$FiresCumul[] + (sim$Fires[] > 0)
  setColors(sim$FiresCumul) <- c(colorRampPalette(c("orange", "darkred"))(sim$maxFiresCumul))
  
  return(invisible(sim))
}