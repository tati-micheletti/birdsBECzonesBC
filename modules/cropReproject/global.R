
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
modules <- list("cropReproject")

## Set simulation and module parameters

times <- list(start = 1, end = 1, timeunit = "year")
parameters <- list(useGdal = TRUE)
objects = list(rasterMap = c(file.path(getwd(),"data","can_age04_1km.tif"),file.path(getwd(),"data","LCC2005_V1_4a.tif")),
               areaLimits = "defined", 
               areaName = "British Columbia", 
               croppedRasterName = c(file.path(paths$outputPath,"ageMap"), file.path(paths$outputPath,"vegMap")))

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

