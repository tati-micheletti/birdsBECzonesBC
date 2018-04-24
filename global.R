
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
modules <- list("prepingInputs") #, "LccToBeaconsReclassify", "forestAge", "forestSuccessionBeacons", "fireSpreadLcc")

## Set simulation and module parameters

times <- list(start = 2005, end = 2005, timeunit = "year")
parameters <- list(useWholeCountry = FALSE)
objects <- list(url.ageMap = "https://drive.google.com/open?id=1lwszwnFjZ3DQ3BBQ7ikiAlN6FXyy2uNX",
                url.vegMap = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                url.studyArea = paste0("http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d",
                                        "/Boundaries-CensusDivisions2011.zip"),
                tempPath.ageMap = file.path(tempdir(), "ageMap"),
                tempPath.vegMap = file.path(tempdir(), "vegMap"),
                tempPath.studyArea = file.path(tempdir(), "studyArea"),
                specificAreaToCropShapefile = "Vancouver Island",
                dataName = "bird_vri_dat_ready.RData",
                locationDataName = "xy.ss.covar.pkey.method_nomissing.RData")

# dev.useRSGD(TRUE) # do not use Rstudio graphics device
# dev() # opens external (non-RStudio) device, which is faster
# clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE)) #  Error: file.exists(checksumFile) is not TRUE ==> It stops on preparing ageMap.

# Visualizing outputs
for (i in 2005:2010){
  quickPlot::Plot(mySimOut$ageForest[[i]], title = paste0("Age forest in ",i))}

for (i in 2005:2010){
  plot(mySimOut$forestCover[[i]])}

