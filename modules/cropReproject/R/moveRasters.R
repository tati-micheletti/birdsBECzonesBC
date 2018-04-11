moveRasters <- function(sim = sim, croppedRaster = sim$croppedRaster){
  
  for (i in 1:length(croppedRaster)){
    
     assign(names(croppedRaster[i]),croppedRaster[[i]], envir = envir(sim))
  }
  return(sim)
}