library(reproducible)

downloadFromWDB <- function(filename)
{
  for (i in 1:nrow(webDatabases::urls))
  {
    if (any(filename == webDatabases::urls$files[[i]]))
    {
      download.file(
        url = paste0(webDatabases::urls$url[[i]], filename),
        destfile = file.path(dataPath, filename),
        method = "auto",
        mode = "wb"
      )
      break
    }
  }
}
  
extractFromArchive <- function(archive, needed)
{
  archivePath <- file.path(dataPath, archive) 
  ext <- tolower(tools::file_ext(archive))
  
  if (ext == "zip")
  {
    fun <- unzip
  }
  else if (ext == "tar")
  {
    fun <- untar
  }
  
  filesInArchive <- fun(archivePath, list = TRUE)$Name
  
  c(
    if (any(needed %in% filesInArchive))
    {
      message(paste("  Extracting from archive:", basename(archive)))
      fun(archivePath, exdir = dataPath, files = needed[needed %in% filesInArchive])
    }
    
    if (any(grepl(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)))
    {
      arch <- grep(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE, value = TRUE)
      c(
        fun(archivePath, exdir = dataPath, files = arch),
        extractFromArchive(archive = arch, needed = needed)
      )
    }
  )
}


smallNamify <- function(name)
{
  file.path(dirname(name), paste0("Small", basename(name)))
}

function(targetFile,
         archive = NULL,
         modulePath,
         moduleName, 
         loadFun = "raster",
         loadPackage = "raster", 
         studyArea = NULL, 
         writeCropped = FALSE, 
         rasterToMatch = NULL,
         rasterInterpMethod = "bilinear",
         rasterDatatype = "INT2U",
         userTags = "stable")
{
  dataPath <- file.path(modulePath, moduleName, "data")
  
  # Here we assume that if dataPath has not been updated checksums don't need to
  # be rerun. This is useful for WEB apps.
  capturedOutput <- capture.output(
    Cache(file.info, asPath(dir(dataPath, full.names = TRUE))),
    type = "message"
  )
  
  notOlderThan <- if (length(capturedOutput) == 0) Sys.time()

  checkSums <- data.table(
    Cache(checksums, 
          module = moduleName, 
          path = modulePath,
          digestPathContent = TRUE,
          notOlderThan = notOlderThan
    )
  )
  
  # Check if the checkSums match, otherwise download or extract the file
  checksums <- checkSums[expectedFile == targetFile,]
  mismatch <- !compareNA(checksums[["result"]], "OK")

  if (!mismatch)
  {
    return(Cache(getFromNamespace(loadFun, loadPackage)(file)))
  }
  else
  {
    if (is.null(archive))
    {
      downloadFromWDB(targetFile)
      
      if (.quickCheck)
      {
        fileSize <- file.size(asPath(targetFile))
        
        if (checksums[["filesize"]] != fileSize)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
      else 
      {
        checkSum <- digest::digest(file = asPath(targetFile), algo = checksums[["algorithm"]])
        
        if (checksums[["checksum"]] != checkSum)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
    }
    else
    {
      checksums <- checkSums[expectedFile == archive,]
      mismatch <- !compareNA(checksums[["result"]], "OK")
      
      if (mismatch)
      {
        downloadFromWDB(archive)
        
        if (.quickCheck)
        {
          fileSize <- file.size(asPath(archive))
          
          if (checksums[["filesize"]] != fileSize)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
        else
        {
          checkSum <- digest::digest(file = asPath(archive), algo = checksums[["algorithm"]])
          
          if (checksums[["checksum"]] != checkSum)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
      }
      
      extractFromArchive(archive = archive, needed = targetFile)
    }
  }
    
  # Load objects
  for (file in c(x = targetFile, studyArea = studyArea))
  {
    assign(x = names(file), value = Cache(getFromNamespace(loadFun, loadPackage)(file)))
  }
  
  objClass <- is(x)
  
  if (!is.null(studyArea) || !is.null(rasterToMatch))
  {
    targetCRS <- 
      if (!is.null(rasterToMatch))
      {
        raster::crs(rasterToMatch)
      }
      else if (!is.null(studyArea))
      {
        raster::crs(studyArea)
      }
      else raster::crs(targetFile)
    
    smallFN <- smallNamify(targetFile)
    
    if (!is.null(studyArea))
    {
      if (!identical(targetCRS, raster::crs(studyArea)))
        studyArea <- sp::spTransform(studyArea, targetCRS)
    }
    
    message("  Cropping, reprojecting")
    
    if ("RasterLayer" %in% objClass || "RasterStack" %in% objClass)
    {
      if (!is.null(studyArea))
      {
        if (!identical(raster::crs(studyArea), raster::crs(x)))
        {
          x <- Cache(raster::crop, x = x, y = sp::spTransform(studyArea, raster::crs(x)))
        }
      }
      
      if (!identical(raster::crs(x), targetCRS))
      {
        x <- Cache(raster::projectRaster, from = x, to = rasterToMatch, method = rasterInterpMethod)
      }
      
      message("  Masking")
      x <- Cache(amc::fastMask, x = x, mask = studyArea)
      
      if (writeCropped)
      {
        raster::writeRaster(x, overwrite = TRUE, format = "GTiff",
                            datatype = rasterDatatype, filename = smallFN)
      }
    } 
    else if ("spatialObjects" %in% objClass)
    {
      if (!suppressWarnings(rgeos::gIsValid(x)))
      {
        xValid <- Cache(raster::buffer, x, dissolve = FALSE, width = 0)
        x <- sp::SpatialPolygonsDataFrame(xValid, data = as.data.frame(x))
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- sp::spTransform(x, targetCRS)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch)
      }
      
      if (writeCropped)
      {
        raster::shapefile(x, overwrite = TRUE, filename = smallFN)
      }
    } 
    else if ("sf" %in% objClass)
    {
      if (!suppressWarnings(sf::st_is_valid(x)))
      {
        x <- Cache(sf::st_buffer, x, dist = 0)
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- sf::st_transform(x, targetCRS)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch)
      }
      # x <- Cache(sf::st_collection_extract, x = x, type = "POLYGON")
      
      if (writeCropped)
      {
        sf::st_write(x, delete_dsn = TRUE, dsn = smallFN)
      }
    }
  }
  
  # Copy to globalenv()
  # list2env(as.list(env), envir = globalenv())
  x
}

