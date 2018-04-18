
defineModule(sim, list(
  name = "prepingInputs",
  description = paste("A translator module.",
    "It crops and/or reprojects a raster file to a smaller,",
  "cropped RasterLayer, defined by a shapefile that has information on extent and projection.",
  "The module uses the amazing prepInputs() function from SpaDES.core"),
  keywords = c("translator", "cropping", "raster crop", "shapefile", "crop to shapefile"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9009", prepingInputs = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "prepingInputs.Rmd"),
  reqdPkgs = list("bcmaps", "sf", "dplyr", "RColorBrewer", "raster", "sp"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
       defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
       defineParameter("useWholeCountry", "logical", FALSE, NA, NA, "Should this module be run on the whole country?")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "url.studyArea", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.vegMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.ageMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "tempPath.studyArea", objectClass = "character", desc = "Temporary path to downloaded study area", sourceURL = NA),
    expectsInput(objectName = "tempPath.vegMap", objectClass = "character", desc = "Temporary path to downloaded vegetation map", sourceURL = NA),
    expectsInput(objectName = "tempPath.ageMap", objectClass = "character", desc = "Temporary path to downloaded age map", sourceURL = NA),
    expectsInput(objectName = "specificAreaToCropShapefile", objectClass = "character", desc = "So far only works with vancouver island and a part of the continent", sourceURL = NA),
    expectsInput(objectName = "templateRaster", objectClass = "character", desc = "Template raster for projection and resolution", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "shapefile", desc = "Shapefile to crop to", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "templateRaster", objectClass = "RasterLayer", desc = "Template raster for projection and resolution"),
    createsOutput(objectName = "studyArea", objectClass = "shapefile", desc = "Shapefile to crop to"),
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = "Vegetation map class raster"),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster")
  )
))


doEvent.prepingInputs = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      
      sim$templateRaster <- Cache(prepInputs, url = sim$url.vegMap,
                                             destinationPath = asPath(sim$tempPath.vegMap))
      unlink(sim$tempPath.vegMap, recursive = TRUE)

    if(P(sim)$useWholeCountry==FALSE){
      
      sim$studyArea <- Cache(prepInputs, url = sim$url.studyArea,
                             destinationPath = sim$tempPath.studyArea,
                             rasterToMatch = sim$templateRaster) 
      sim$studyArea <- selectSpecificAreas(sim$studyArea, specificAreas = sim$specificAreaToCropShapefile)
      unlink(sim$tempPath.studydArea, recursive = TRUE)

      sim$vegMap <- Cache(prepInputs, url = sim$url.vegMap,
                          destinationPath = asPath(sim$tempPath.vegMap),
                          rasterToMatch = sim$templateRaster,
                          studyArea = sim$studyArea)
      unlink(sim$tempPath.vegMap, recursive = TRUE)
      
      sim$ageMap <- Cache(prepInputs, url = sim$url.ageMap,
                          destinationPath = asPath(sim$tempPath.ageMap),
                          rasterToMatch = sim$templateRaster,
                          studyArea = sim$studyArea)
      unlink(sim$tempPath.ageMap, recursive = TRUE)
      
    } else {
      
      sim$vegMap <- Cache(prepInputs, url = sim$url.vegMap,
                          destinationPath = asPath(sim$tempPath.vegMap),
                          rasterToMatch = sim$templateRaster)
      unlink(sim$tempPath.vegMap, recursive = TRUE)
      
      sim$ageMap <- Cache(prepInputs, url = sim$url.ageMap,
                          destinationPath = asPath(sim$tempPath.ageMap),
                          rasterToMatch = sim$templateRaster)
      unlink(sim$tempPath.ageMap, recursive = TRUE)

    }
      
      
      # Take out after running !!
Plot(sim$vegMap)
Plot(sim$ageMap)
    },
   
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere(sim$url.studyArea)){

     # sim$studyArea <- "http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d/Boundaries-CensusDivisions2011.zip"
     sim$studyArea <- randomPolygon(matrix(c(-122.85, 52.04), ncol = 2), 500000)

    warning(paste0("You did not provide a study area. Cropping / masking will be performed on a random polygon in BC",
                   "This will be done ONLY if the parameter 'useWholeCountry = FALSE'", call. = FALSE))
    }

    if (!suppliedElsewhere(sim$url.ageMap)){
      url.ageMap <- "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"
      warning(paste("You did not provide an url for age map. The module will use: ", url.ageMap), call. = FALSE)
    }
    
    if (!suppliedElsewhere(sim$url.vegMap)){
      url.vegMap <- "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"
        warning(paste("You did not provide an url for vegetation map. The module will use: ", url.vegMap), call. = FALSE)
    }
    
    if (!suppliedElsewhere(sim$tempPath.ageMap)){
      tempPath.ageMap <- file.path(tempdir(), "ageMap")
      warning(paste("You did not provide an path for age map. The module will use:", tempPath.ageMap), call. = FALSE)
    }
    
    if (!suppliedElsewhere(sim$tempPath.vegMap)){
      tempPath.vegMap <- file.path(tempdir(), "vegMap")
      warning(paste("You did not provide an path for vegetation map. The module will use:", tempPath.vegMap), call. = FALSE)
    }
    
    if (!suppliedElsewhere(sim$tempPath.studyArea)){
      tempPath.studyArea <- file.path(tempdir(), "studyArea")
      warning(paste0("You did not provide an path for study area. The module will use", tempPath.studyArea, 
                      " if this is a relevant object (ie. not using a random polygon as study area)"), call. = FALSE)
    }
    
    if (!suppliedElsewhere(sim$templateRaster)){
      sim$templateRaster <- Cache(prepInputs, url = url.vegMap, destinationPath = asPath(tempPath.vegMap))
      warning(paste0("You did not provide a template raster for the GIS operations. The module will use", 
                     " LCC05 for it"), call. = FALSE)
    }
    
  return(invisible(sim))
}
