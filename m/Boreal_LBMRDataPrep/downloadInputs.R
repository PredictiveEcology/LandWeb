library(reproducible)

downloadFromWebDB <- function(filename, filepath, dataset = NULL)
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
  
extractFromArchive <- function(archivePath, dataPath = dirname(archivePath), needed, extractedArchives = NULL)
{
  ext <- tolower(tools::file_ext(archivePath))
  
  if (ext == "zip")
  {
    fun <- unzip
  }
  else if (ext == "tar")
  {
    fun <- untar
  }
  
  filesInArchive <- fun(archivePath, list = TRUE)$Name
  
  if (any(needed %in% filesInArchive))
  {
    message(paste("  Extracting from archive:", basename(archivePath)))
    fun(archivePath, exdir = dataPath, files = needed[needed %in% filesInArchive], junkpaths = TRUE)
  }
  
  isArchive <- grepl(tools::file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)
  
  if (any(isArchive))
  {
    arch <- filesInArchive[isArchive]
    extractedArchives <- c(
      extractedArchives,
      fun(archivePath, exdir = dataPath, files = arch, junkpaths = TRUE)
    )
    extractedArchives <- c(
      extractedArchives,
      unlist(
        lapply(arch, extractFromArchive, needed = needed, extractedArchives = extractedArchives)
      )
    )
  }
  
  unique(extractedArchives)
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
         tags = "stable")
{
  dataPath <- file.path(modulePath, moduleName, "data")
  
  targetFile <- basename(targetFile)
  targetFilePath <- file.path(dataPath, targetFile)
  
  # Here we assume that if dataPath has not been updated checksums don't need to
  # be rerun. This is useful for WEB apps.
  capturedOutput <- capture.output(
    Cache(file.info, asPath(dir(dataPath, full.names = TRUE)), userTags = tags),
    type = "message"
  )
  
  notOlderThan <- if (length(capturedOutput) == 0) Sys.time()

  checkSums <- data.table(
    Cache(checksums, 
          module = moduleName, 
          path = modulePath,
          digestPathContent = TRUE,
          checksumFile = asPath(file.path(modulePath, moduleName, "data", "CHECKSUMS.txt")),
          write = FALSE,
          notOlderThan = notOlderThan,
          userTags = tags
    )
  )
  
  # Check if the checkSums match, otherwise download or extract the file
  checksums <- checkSums[expectedFile == targetFile,]
  mismatch <- !compareNA(checksums[["result"]], "OK")

  if (mismatch)
  {
    if (is.null(archive))
    {
      downloadFromWebDB(targetFile, targetFilePath)
      
      if (.quickCheck)
      {
        fileSize <- file.size(asPath(targetFilePath))
        
        if (checksums[["filesize"]] != fileSize)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
      else 
      {
        checkSum <- digest::digest(file = asPath(targetFilePath), algo = checksums[["algorithm"]])
        
        if (checksums[["checksum"]] != checkSum)
          warning("The version downloaded of ", targetFile, " does not match the checksums")
      }
    }
    else
    {
      archive <- basename(archive)
      archivePath <- file.path(dataPath, archive)
      
      checksums <- checkSums[expectedFile == archive,]
      mismatch <- !compareNA(checksums[["result"]], "OK")
      
      if (mismatch)
      {
        downloadFromWebDB(archive, archivePath)
        
        if (.quickCheck)
        {
          fileSize <- file.size(asPath(archivePath))
          
          if (checksums[["filesize"]] != fileSize)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
        else
        {
          checkSum <- digest::digest(file = asPath(archivePath), algo = checksums[["algorithm"]])
          
          if (checksums[["checksum"]] != checkSum)
            warning("The version downloaded of ", archive, " does not match the checksums")
        }
      }
      
      extractedArchives <- extractFromArchive(archive = archivePath, needed = targetFile)
      
      for (arch in extractedArchives)
      {
        unlink(arch)
      }
    }
  }
  
  fun <- getFromNamespace(loadFun, loadPackage)
  
  if (loadFun == "raster" && loadPackage == "raster")
  {
    x <- fun(targetFilePath)
  }
  else
  {
    x <- Cache(fun(targetFilePath), userTags = cacheTags)
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
    
    smallFN <- smallNamify(targetFilePath)
    
    if (!is.null(studyArea))
    {
      if (!identical(targetCRS, raster::crs(studyArea)))
        studyArea <- Cache(sp::spTransform, x = studyArea, CRSobj = targetCRS, userTags = tags)
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
            y = Cache(sp::spTransform, x = studyArea, CRSobj = raster::crs(x), userTags = tags),
            userTags = tags
          )
        }
      }
      
      if (!identical(raster::crs(x), targetCRS))
      {
        x <- Cache(raster::projectRaster, from = x, to = rasterToMatch, method = rasterInterpMethod, userTags = tags)
      }
      
      message("  Masking")
      x <- Cache(amc::fastMask, x = x, mask = studyArea, userTags = tags)
      
      if (writeCropped)
      {
        Cache(
          raster::writeRaster,
          x = x,
          overwrite = TRUE, 
          format = "GTiff",
          datatype = rasterDatatype, 
          filename = smallFN, 
          userTags = tags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    } 
    else if ("spatialObjects" %in% objClass)
    {
      if (!suppressWarnings(rgeos::gIsValid(x)))
      {
        xValid <- Cache(raster::buffer, x, dissolve = FALSE, width = 0, userTags = tags)
        x <- Cache(sp::SpatialPolygonsDataFrame, Sr = xValid, data = as.data.frame(x), userTags = tags)
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sp::spTransform, x = x, CRSobj = targetCRS, userTags = tags)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = tags)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = tags)
      }
      
      if (writeCropped)
      {
        Cache(
          raster::shapefile,
          x = x,
          overwrite = TRUE,
          filename = smallFN, 
          userTags = tags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    } 
    else if ("sf" %in% objClass)
    {
      if (!suppressWarnings(sf::st_is_valid(x)))
      {
        x <- Cache(sf::st_buffer, x, dist = 0, userTags = tags)
      }
      
      if (!identical(targetCRS, raster::crs(x)))
        x <- Cache(sf::st_transform, x = x, crs = targetCRS, userTags = tags)
      
      if (!is.null(studyArea))
      {
        x <- Cache(raster::crop, x, studyArea, userTags = tags)
      }
      
      if (!is.null(rasterToMatch)) 
      {
        x <- Cache(raster::crop, x, rasterToMatch, userTags = tags)
      }
      # x <- Cache(sf::st_collection_extract, x = x, type = "POLYGON")
      
      if (writeCropped)
      {
        Cache(
          sf::st_write,
          obj = x, 
          delete_dsn = TRUE,
          dsn = smallFN, 
          userTags = tags,
          notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
        )
      }
    }
  }

  x
}

