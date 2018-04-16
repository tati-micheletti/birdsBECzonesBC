cropArea <- function(areaName = sim$areaName, sim = sim, filePathTemplate = sim$filePathTemplate, 
                          rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                          croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                          funcRast = sim$funcRast){

  require(reproducible)
  library(raster)
  library(gdalUtils)
  require(tools)
  require(rgdal)
  require(sf)

  nRasters <- length(rasterMap)
  
  for (i in 1:nRasters){
    rasterMapEach <- raster(rasterMap[i])
    rasterProj <- raster(rasterMap[1])
    
    croppedRasterNameEach <- croppedRasterName[i]

    if(file.exists(croppedRasterNameEach)){next}
    
CAN_adm1 <- raster::getData("GADM", country="CAN", level=1, 
                                  path = dirname(filePathTemplate))

lsfiles <- file.info(dir(dirname(filePathTemplate)))
fileTemplateName <- rownames(lsfiles[order(lsfiles$mtime),][1])
filePathTemplate <- file.path(inputPath(sim), fileTemplateName)

shapefile <- readRDS(filePathTemplate) %>%
 sf::st_as_sf()

    shapefile <-  shapefile[shapefile$NAME_1 == areaName,]

    if (!(sf::st_crs(shapefile)$proj4string==as.character(raster::crs(rasterMapEach)))){
            shapefile <- sf::st_transform(x = shapefile, crs = as.character(raster::crs(rasterMapEach)))}
      
    sf::st_write(obj = shapefile, dsn = outputPath(sim), layer = "reprojectedShapefile", driver = "ESRI Shapefile", delete_dsn = TRUE)
    
     tryCatch(rasterMapEach[] <- rasterMapEach[])
    
    cutlinePath <- file.path(outputPath(sim), "reprojectedShapefile.shp")
    
      if(useGdal==TRUE){
      gdalwarp(srcfile = rasterMap[i], # Raster file path
             dstfile = croppedRasterNameEach, # Cropped raster file name
             overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
             cutline = cutlinePath, # Shapefile path to use for masking
             dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
             s_srs= as.character(crs(rasterMapEach)), #Projection from the source raster file
             t_srs= as.character(crs(rasterProj)), # Projection for the cropped file, it is possible to change projection here
             multi = TRUE, # Use multithreaded warping implementation.
             of = cropFormat, # Select the output format
             crop_to_cutline = TRUE, # Crop the raster to the shapefile
             tr = res(rasterProj)) # Raster resolution, not sure it needs to be the same from original raster

  } else {

    eval(parse(text = paste0("raster::",funcRast,"(rasterMapEach, shapefile, filename=croppedRasterNameEach)"))) # Not sure this works...
    
  }
  
} # End for loop
         
  newRasterMap <- lapply(X = croppedRasterName, FUN = function(dstFile){
    obj <- raster(dstFile)
    return(obj)
  })
  
  require(stringr)
  namesRasters <- str_match(croppedRasterName, "/outputs/(.*?).tif")
  names(newRasterMap) <- namesRasters[,2]

 return(invisible(newRasterMap))
}