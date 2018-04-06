library(SpaDES)

# set the main working directory
workDirectory <- getwd()

# set the directories
inputDir <-  file.path(workDirectory, "inputs")
moduleDir <- file.path(workDirectory, "modules")
outputDir <- file.path(workDirectory, "outputs")
cacheDir <- file.path(workDirectory, "cache")

setPaths(modulePath = moduleDir, inputPath = inputDir, outputPath = outputDir, cachePath = cacheDir)

times <- list(start = 2005.0, end = 2010.0, timeunit = "year")
parameters <- list(
  .globals = list()
)

modules <- list("forestSuccessionLcc")
paths <- list(
  cachePath = cacheDir,
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = outputDir
)

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

mySimOut <- spades(mySim)

# Plot Canada Map
Plot(mySimOut$lcc05, title = "Land Cover, with simulated area")
# Plot small Yellow polygon showing area simulated
Plot(mySimOut$inputMapPolygon, addTo = "mySimOut$lcc05", gp = gpar(col = "yellow", lwd = 2))

### Simulation overview: note the child modules are initialized
moduleDiagram(mySim)
objectDiagram(mySim)
