# cropReproject general module

stopifnot(packageVersion("SpaDES") >= "1.3.1.9010")

defineModule(sim, list(
  name = "cropReproject",
  description = paste(
    "A translator module.",
    "It crops and/or reprojects a raster file to a smaller,",
    "cropped RasterLayer, defined by a shapefile that has iknformation on extent and projection.",
    "This was based on the cropReprojectLccAge SpaDES module"
  ),
  keywords = c("translator", "cropping", "raster crop", "shapefile", "crop to shapefile"),
  authors = c(person(c("Eliot", "J","B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
              person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9004", cropReproject = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReproject.Rmd"),
  reqdPkgs = list("SpaDES","quickPlot","sp","raster","sf","rgdal","tools","reproducible","gdalUtils","rgeos","sf"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = c("rasterMap", "areaLimits", "areaName", "filePathTemplate", 
                                "polyMatrix", "areaSize", "croppedRasterName", "funcRast",
                                "cropFormat","useGdal"), 
                 objectClass = c("rasterLayer", "character", "character", "character", 
                                 "matrix", "numeric", "character", "character",
                                 "character","logical"), 
                 desc = c("Raster object/layer/map that you want to crop",
                          "Choose between a 'defined' territory/shapefile/raster or a 'random' polygon.",
                          "Name of the territory to crop the raster to, 'polygon', or 'raster' to chose the type of area to crop to",
                          "File path to the shapefile/raster template to crop from", 
                          "Random polygon matrix, if random polygon is to be used", 
                          "Area size if a random polygon should be used",
                          "File name for the cropped raster",
                          "If not using sf, which raster function should be used: crop or mask?",
                          "Format of the output cropped raster: 'GTiff' for .tif, 'HFA' for .img, and 'ascii' for ESRI .asc",
                          "Should this module use the sf package for cropping?"),
                 sourceURL = rep(NA, times = 10))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = c("rasterMap","croppedRaster"),
                  objectClass = c("rasterLayer","rasterLayer"),
                  desc = c("Raster input which might be a module output if not provided",
                           "Raster object/layer/map cropped to your cropPolygon"))
  )
))

doEvent.cropReproject = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "cropReproject", "crop")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "cropReproject", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "cropReproject", "save")
    },
    crop = {

      if(is.null(sim$rasterMap)|!suppliedElsewhere(sim$rasterMap)){
        invisible(readline(prompt="No raster to crop was provided. A sample raster (LCC2010, 250m) will be downloaded. Press [enter] to continue."))
        sim$rasterMap <- downloadRaster(sim = sim)}
      
      if(!is.null(sim$rasterMap)&!file.exists(sim$rasterMap)){
        invisible(readline(prompt="No raster to crop was provided. A sample raster (LCC2010, 250m) will be downloaded. Press [enter] to continue."))
        sim$rasterMap <- downloadRaster(sim = sim)}
      
      sim$cropReproject$.inputObjects(sim)
      
      if (sim$areaLimits=="defined"){
        
        ifelse (areaName == "polygon",
                sim$croppedRaster <- Cache(cropPolygon, sim = sim, filePathTemplate = sim$filePathTemplate, 
                                                 rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                                                 croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                                                 funcRast = sim$funcRast),
                ifelse(areaName == "raster",
                       sim$croppedRaster <- Cache(cropRaster, sim = sim, filePathTemplate = sim$filePathTemplate, 
                                                       rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                                                       croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat),
                       sim$croppedRaster <- Cache(cropArea, sim = sim, areaName = sim$areaName, filePathTemplate = sim$filePathTemplate, 
                                                     rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                                                     croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                                                  funcRast = sim$funcRast)))
      }
      
      if (sim$areaLimits=="random"){
        
        sim$croppedRaster <- Cache(cropRandomPolygon, polyMatrix = sim$polyMatrix, areaSize = sim$areaSize, 
                                               sim = sim, rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                                               croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                                               funcRast = sim$funcRast)
      }
              
    },
    plot = {
      
      quickPlot::Plot(sim$croppedRaster, title = "Cropped Raster")
    
    },
    save = {
      
      if(!file.exists(sim$croppedRasterName))
      writeRaster(sim$croppedRaster, filename = file.path(ouputPath(sim),"croppedRaster"), format = sim$cropFormat)
    },
 
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  GTiff <- ".tif"
  HFA <-  ".img"
  ascii <- ".asc"
  
  if (is.null(sim$useGdal)|!suppliedElsewhere(sim$useGdal)){
    sim$useGdal <- TRUE # NOT SURE I NEED THIS HERE, AS IT IS SUPPLIED IN THE PARAMETERS. CONFIRM WITH ELIOT
    warning("Using Simple Features for R package (sf).", call. = FALSE)}
  if (is.null(sim$cropFormat)|!suppliedElsewhere(sim$cropFormat)){
    sim$cropFormat <- "GTiff"
    warning("Cropped raster format not provided. Using '.tiff'.", call. = FALSE)}
  if (is.null(sim$croppedRasterName)|!suppliedElsewhere(sim$croppedRasterName)){
    sim$croppedRasterName <- file.path(outputPath(sim),paste0("croppedRaster",get(sim$cropFormat)))
    warning(paste0("Cropped raster name not provided. Using croppedRaster with format ", sim$cropFormat), call. = FALSE)}
  if (is.null(sim$polyMatrix)|!suppliedElsewhere(sim$polyMatrix)){
    sim$polyMatrix <- matrix(c(-122.85, 52.04), ncol = 2)
    warning(paste0("Random polygon coordinates not provided. Using random forested area."), call. = FALSE)}
  if (is.null(sim$areaSize)|!suppliedElsewhere(sim$areaSize)){
    sim$areaSize <- 500000
    warning(paste0("Random polygon area not provided. Using ", format(sim$areaSize, scientific=F), " hectares."), call. = FALSE)}
  if (is.null(sim$areaLimits)|!suppliedElsewhere(sim$areaLimits)){
    sim$areaLimits <- "random"
    warning("No area limits provided. Using a random area to crop.", call. = FALSE)}
  if ((is.null(sim$areaName)&sim$areaLimits=="defined")|
      (!suppliedElsewhere(sim$areaName)&sim$areaLimits=="defined")){
    sim$areaName <- "British Columbia"
    warning(paste0("No defined area provided. Using ",sim$areaName, " area to crop."), call. = FALSE)}
  if (is.null(sim$funcRast)|!suppliedElsewhere(sim$funcRast)){
    sim$funcRast <- "mask"
    warning(paste0("Function for raster cropping not provided, using 'mask'."), call. = FALSE)}
  if (is.null(sim$filePathTemplate)|!suppliedElsewhere(sim$filePathTemplate)){
    sim$filePathTemplate <- file.path(inputPath(sim),"fileTemplate.RData")
    warning("filePathTemplate not provided. Using inputPath of simList and a random polygon template.", call. = FALSE)}

  return(invisible(sim))
}

