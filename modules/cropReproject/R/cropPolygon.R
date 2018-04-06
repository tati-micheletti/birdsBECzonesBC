# cropRaster function

cropPolygon <- function(sim = sim, filePathTemplate = sim$filePathTemplate, 
                        rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                        croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                        funcRast = sim$funcRast){
  library(raster)
  library(gdalUtils)
  require(tools)
  require(rgdal)
  require(sf)
    
  rasterMap <- raster(rasterMap)

     if (tools::file_ext(filePathTemplate)=="rds"){
       shapefile <- readRDS(filePathTemplate)}
     if (tools::file_ext(filePathTemplate)=="shp"){
       shapefile <- readOGR(filePathTemplate)}
  
  if (!(sp::proj4string(shapefile)==as.character(raster::crs(rasterMap)))){
    shapefile <- sp::spTransform(x = shapefile, CRS = raster::crs(rasterMap))}
  
  if(useGdal==TRUE){
    
    tryCatch(rasterMap[] <- rasterMap[])
    
    shapefile <- as(shapefile, "SpatialPolygonsDataFrame")
    rgdal::writeOGR(obj = shapefile, dsn = ".", layer = file.path(outputPath(sim), "reprojectedShapefile"), driver="ESRI Shapefile")
    cutlinePath <- file.path(outputPath(sim), "reprojectedShapefile.shp")
    
      gdalwarp(srcfile = sim$rasterMap, # Raster file path
               dstfile = croppedRasterName, # Cropped raster file name
               overwrite=TRUE, # If you alreday have a raster with the same name and want to overwrite it
               cutline = cutlinePath, # Shapefile path to use for masking
               dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
               s_srs= as.character(crs(rasterMap)), #Projection from the source raster file
               t_srs= as.character(crs(rasterMap)), # Projection for the cropped file, it is possible to change projection here
               multi=TRUE, # Use multithreaded warping implementation.
               of=cropFormat, # Select the output format
               crop_to_cutline = TRUE, # Crop the raster to the shapefile
               tr=res(rasterMap)) # Raster resolution, not sure it needs to be the same from original raster

  } else { # raster package

    eval(parse(text = paste0("raster::",funcRast,"(rasterMap, shapefile, filename=croppedRasterName)")))
  }
     newRasterMap <- raster(croppedRasterName)
     tryCatch(newRasterMap[] <- newRasterMap[]) # Bring the raster to memory (if possible)

  return(invisible(newRasterMap))
}