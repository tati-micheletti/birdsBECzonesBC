
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
  reqdPkgs = list("sf", "dplyr", "RColorBrewer", "raster", "sp", "reproducible", "data.table","rgdal", "gdalUtils"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("useWholeCountry", "logical", FALSE, NA, NA, "Should this module be run on the whole country?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "url.studyArea", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.vegMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "url.ageMap", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "tempPath.studyArea", objectClass = "character", desc = "Temporary path to downloaded study area", sourceURL = NA),
    expectsInput(objectName = "tempPath.vegMap", objectClass = "character", desc = "Temporary path to downloaded vegetation map", sourceURL = NA),
    expectsInput(objectName = "tempPath.ageMap", objectClass = "character", desc = "Temporary path to downloaded age map", sourceURL = NA),
    expectsInput(objectName = "specificAreaToCropShapefile", objectClass = "character", desc = "So far only works with vancouver island and a part of the continent", sourceURL = NA),
    expectsInput(objectName = "templateRaster", objectClass = "character", desc = "Template raster for projection and resolution", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "shapefile", desc = "Shapefile to crop to", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", desc = "birdData Name not Path", sourceURL = NA),
    expectsInput(objectName = "locationDataName", objectClass = "character", desc = "location file Name not Path", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "templateRaster", objectClass = "RasterLayer", desc = "Template raster for projection and resolution"),
    createsOutput(objectName = "studyArea", objectClass = "shapefile", desc = "Shapefile to crop to"),
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = "Vegetation map class raster"),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster"),
    createsOutput(objectName = "birdData", objectClass = "data.table", desc = "Birds data.table")
  )
))


doEvent.prepingInputs = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      
      sim$templateRaster <- Cache(prepInputs, url = sim$url.vegMap,
                                  destinationPath = asPath(sim$tempPath.vegMap))
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "prepingInputs", "cropReprojectToStudyArea")
      sim <- scheduleEvent(sim, start(sim), "prepingInputs", "cropDataToStudyArea")
      
      
    }, cropReprojectToStudyArea = {
      
      if(P(sim)$useWholeCountry==FALSE){
        
        if(!file.exists(file.path(inputPath(sim), "studyArea.shp"))){
          sim$studyArea <- Cache(prepInputs, url = sim$url.studyArea,
                                 destinationPath = sim$tempPath.studyArea,
                                 rasterToMatch = sim$templateRaster) %>%
            selectSpecificAreas(specificAreas = sim$specificAreaToCropShapefile)
          
          pid <- sapply(slot(sim$studyArea, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
          pdf <- data.frame(ID=1:length(sim$studyArea), row.names = pid)       # Create dataframe with correct rownames
          studyAreaDF <- SpatialPolygonsDataFrame(sim$studyArea, pdf)      # Try coersion again and check class
          writeOGR(obj = studyAreaDF, dsn = file.path(inputPath(sim), "studyArea.shp"), layer = "studyArea.shp", driver = "ESRI Shapefile")
          
        } else {
          
          sim$studyArea <- Cache(prepInputs, 
                                 targetFile = asPath(file.path(inputPath(sim), "studyArea.shp")),
                                 destinationPath = asPath(file.path(inputPath(sim))),
                                 rasterToMatch = sim$templateRaster)
          
          studyAreaDF <- readOGR(dsn = file.path(inputPath(sim),"studyArea.shp"))
          
        }
        
        sim$vegMap <- Cache(raster::crop, sim$templateRaster, sim$studyArea)
        sim$vegMap <- Cache(fastMask, sim$vegMap, studyAreaDF) # if not needed, comment lines pid, pdf
        
        Plot(sim$vegMap, title = "Vegetation Map") # Take out after running !! -------------------------------
        
        if(!file.exists(file.path(inputPath(sim), "can_age04_1km.tif"))){
          sim$ageMap <- prepInputs(url = sim$url.ageMap,
                                   targetFile = asPath(file.path(inputPath(sim), "can_age04_1km.tif")),
                                   destinationPath = asPath(file.path(inputPath(sim))),
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$vegMap)
          
        } else {
          
          sim$ageMap <- prepInputs(targetFile = asPath(file.path(inputPath(sim), "can_age04_1km.tif")),
                                   destinationPath = asPath(file.path(inputPath(sim))),
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$vegMap, overwrite = TRUE)}
        #       #%>%
        # #        postProcess(rasterToMatch = sim$vegMap) ==> If this works, I can take out from lines 103 - 116. Maybe put it in the same projection before crop/mask!
        #       
        #       cutlinePath <- file.path(inputPath(sim), "studyArea.shp")
        #       
        # # *** NOTE ***      # In my computer, this is not saving the cropped raster... Maybe memory/processing problems?
        #       gdalUtils::gdalwarp(srcfile = file.path(inputPath(sim), "can_age04_1km.tif"), # Raster file path
        #                dstfile = file.path(inputPath(sim), "can_age04_1km_cropped.tif"), # Cropped raster file name
        #                overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
        #                cutline = cutlinePath, # Shapefile path to use for masking
        #                dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
        #                s_srs = as.character(crs(sim$ageMap)), #Projection from the source raster file
        #                t_srs = as.character(crs(sim$vegMap)), # Projection for the cropped file, it is possible to change projection here
        #                multi = TRUE, # Use multithreaded warping implementation.
        #                of = "GTiff", # Select the output format
        #                crop_to_cutline = TRUE, # Crop the raster to the shapefile
        #                tr = res(sim$vegMap)) # Raster resolution, not sure it needs to be the same from original raster
        #       sim$ageMap <- raster::raster(file.path(inputPath(sim), "can_age04_1km_cropped.tif"))
        #       
        
        Plot(sim$ageMap, title = "Age Map")  #Take out after running !! -------------------------------
        
        
      } else {
      
       sim$vegMap <- Cache(prepInputs, url = sim$url.vegMap,
                           targetFile = asPath(file.path(sim$tempPath.vegMap, "LCC2005_V1_4a.tif")),
                           destinationPath = asPath(sim$tempPath.vegMap))

       sim$ageMap <- Cache(prepInputs, url = sim$url.ageMap,
                           targetFile = asPath(file.path(sim$tempPath.ageMap, "can_age04_1km.tif")),
                           destinationPath = asPath(sim$tempPath.ageMap),
                           rasterToMatch = sim$vegMap)
     }
 

    }, cropDataToStudyArea = {
      
      if (P(sim)$useWholeCountry == FALSE){
        sim$birdData <- loadCroppedData(sim = sim, studyArea = sim$studyArea, 
                                        dataPath = file.path(inputPath(sim)))
      } else {
        
        # NEED TO LOAD BIRD FILE NORMALLY, WITHOUT CROPPING
      }
      
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
    url.ageMap <- "https://drive.google.com/open?id=1lwszwnFjZ3DQ3BBQ7ikiAlN6FXyy2uNX"
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