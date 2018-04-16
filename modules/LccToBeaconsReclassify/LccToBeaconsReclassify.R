stopifnot(packageVersion("SpaDES") >= "1.2.0.9009")

defineModule(sim, list(
  name = "LccToBeaconsReclassify",
  description = "Takes the LCC05 classification of 39 land cover classes, and reclassifies it to the 11 classes of the Beacons succession model.",
  keywords = c("forest succession", "LCC05", "land cover classification 2005", "Beacons"),
  childModules = character(),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut")),
    person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))
  ),
  version = numeric_version("1.1.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c("2005-01-01", NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LccToBeaconsReclassify.Rmd"),
  reqdPkgs = list("raster", "RColorBrewer", "fastmatch", "dplyr"),
  parameters = rbind(
    defineParameter(".Reclassify", "numeric", start(sim), NA, NA, desc = "Time for reclassifying"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")
  ),
  inputObjects = data.frame(
    objectName = "vegMap", objectClass = "RasterLayer",
    sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
    other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("trajMapBeacons", "vegMapBeacons", "trajObj", "lcc05VegTable", "lcc05VegReclass", "lcc05TrajTable"),
    objectClass = c("RasterLayer", "RasterLayer", "matrix", "data.frame", "data.frame", "data.frame"),
    other = rep(NA_character_, 6L), stringsAsFactors = FALSE)
))

doEvent.LccToBeaconsReclassify <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, 
         init = {

    sim <- LccToBeaconsReclassifyInit(sim)

    sim <- scheduleEvent(sim, params(sim)$LccToBeaconsReclassify$.Reclassify,
                         "LccToBeaconsReclassify", "reclassify", eventPriority = 2)
    sim <- scheduleEvent(sim, params(sim)$LccToBeaconsReclassify$.plotInitialTime,
                         "LccToBeaconsReclassify", "plot")
    sim <- scheduleEvent(sim, params(sim)$LccToBeaconsReclassify$.saveInitialTime,
                         "LccToBeaconsReclassify", "save")
  }, 
    reclassify = {
      
    sim <- reclassifyLcc(sim = sim,
                         vegMap = sim$vegMap,
                         lcc05VegTable = sim$lcc05VegTable, 
                         lcc05VegReclass = sim$lcc05VegReclass, 
                         lcc05TrajTable = sim$lcc05TrajTable)
    
   },
        plot = {
          
    Plot(vegMapBeacons, trajMapBeacons, new = TRUE)
    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$LccToBeaconsReclassify$.plotInterval,
                         "LccToBeaconsReclassify", "plot")
  },
        save = {
    # the raster package does not keep colors when writing to a tif file

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$LccToBeaconsReclassify$.saveInterval,
                         "LccToBeaconsReclassify", "save")
  },    
          warning(paste(
        "Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
        "' in module '", events(sim)[1, "moduleName", with = FALSE], "'",
        sep = ""
      ))
  )
  
  return(invisible(sim))
}
