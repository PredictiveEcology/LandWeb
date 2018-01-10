library(reproducible)

downloadFromWebDB <- function(filename)
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
    
    isArchive <- grepl(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)
    
    if (any(isArchive))
    {
      arch <- filesInArchive[isArchive]
      c(
        fun(archivePath, exdir = dataPath, files = arch),
        unlist(
          lapply(arch, extractFromArchive, needed = needed)
        )
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
    Cache(file.info, asPath(dir(dataPath, full.names = TRUE)), userTags = userTags),
    type = "message"
  )
  
  notOlderThan <- if (length(capturedOutput) == 0) Sys.time()

  checkSums <- data.table(
    Cache(checksums, 
          module = moduleName, 
          path = modulePath,
          digestPathContent = TRUE,
          notOlderThan = notOlderThan,
          userTags = userTags
    )
  )
  
  # Check if the checkSums match, otherwise download or extract the file
  checksums <- checkSums[expectedFile == targetFile,]
  mismatch <- !compareNA(checksums[["result"]], "OK")

  if (!mismatch)
  {
    x <- Cache(getFromNamespace(loadFun, loadPackage)(file), userTags = userTags)
  }
  else
  {
    if (is.null(archive))
    {
      downloadFromWebDB(targetFile)
      
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
        downloadFromWebDB(archive)
        
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
    
    assign(x = "x", value = Cache(getFromNamespace(loadFun, loadPackage)(targetFile), userTags = userTags))
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
        studyArea <- Cache(sp::spTransform, x = studyArea, CRSobj = targetCRS, userTags = userTags)
    }
    
    message("  Cropping, reprojecting")
    
    if ("RasterLayer" %in% objClass || "RasterStack" %in% objClass)
    {
      if (!is.null(studyArea))
      {
        if (!identical(raster::crs(studyArea), raster::crs(x)))
        {
          x <- Cache(
            raster::crop,
            x = x,
            y = Cache(sp::spTransform, x = studyArea, CRSobj = raster::crs(x), userTags = userTags),
            userTags = userTags
          )
        }
      }
      
      if (!identical(raster::crs(x), targetCRS))
      {
        x <- Cache(raster::projectRaster, from = x, to = rasterToMatch, method = rasterInterpMethod, userTags = userTags)
      }
      
      message("  Masking")
      x <- Cache(amc::fastMask, x = x, mask = studyArea, userTags = userTags)
      
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
        xValid <- Cache(raster::buffer, x, dissolve = FALSE, width = 0, userTags = userTags)
        x <- Cache(sp::SpatialPolygonsDataFrame, Sr = xValid, data = as.data.frame(x), userTags = userTags)
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sp::spTransform, x = x, CRSobj = targetCRS, userTags = userTags)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = userTags)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = userTags)
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
        x <- Cache(sf::st_buffer, x, dist = 0, userTags = userTags)
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sf::st_transform, x = x, crs = targetCRS, userTags = userTags)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = userTags)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = userTags)
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

