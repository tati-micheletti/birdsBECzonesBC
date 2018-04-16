
library(SpaDES)

# set the directories
workDirectory <- getwd()

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
modules <- list("cropReproject", "LccToBeaconsReclassify", "forestAge", "forestSuccessionBeacons", "fireSpreadLcc")

## Set simulation and module parameters

times <- list(start = 2005, end = 2010, timeunit = "year")
parameters <- list(useGdal = TRUE)
objects = list(rasterMap = c(file.path(getwd(),"data","LCC2005_V1_4a.tif"),file.path(getwd(),"data","can_age04_1km.tif")),
               areaLimits = "defined", 
               areaName = "British Columbia", 
               croppedRasterName = c(file.path(paths$outputPath,"vegMap.tif"), file.path(paths$outputPath,"ageMap.tif")),
               .globals = list(.plotInitialTime = 1))

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

# Visualizing outputs
for (i in 2005:2010){
  quickPlot::Plot(mySimOut$ageForest[[i]], title = paste0("Age forest in ",i))}

for (i in 2005:2010){
  plot(mySimOut$forestCover[[i]])}

