library(reproducible)

function(mapping, modulePath, moduleName, loadFun, loadPackage, mask = NULL)
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
  
  # Check if the checkSums match, otherwise download the files
  checkSums <- checksum[expectedFile == c(mapping, mask),]
  
  needDownload <- !all(OK <- compareNA(checkSums[["result"]], "OK"))

  if (needDownload)
  {
    filesNeeded <- checkSums[OK,][["expectedFile"]]
    
    filesNeeded <- unlist(
      lapply(
        filesNeeded,
        function(file)
        {
          maybeInArchive <- TRUE
          for (i in 1:nrow(webDatabases::urls))
          {
            if (any(file == webDatabases::urls$files[[i]]))
            {
              download.file(
                url = paste0(webDatabases::urls$url[[i]], file),
                destfile = file.path(dataPath, file),
                method = "auto",
                mode = "wb"
              )
              maybeInArchive <- FALSE
              break
            }
          }
          
          if (maybeInArchive)
          {
            return(file)
          }
        }
      )
    )
    
    if (length(filesNeeded) > 0) # Missing files are possibly in archives ?
    {
      lapply(
        checkSums[grepl(tolower(tools::file_ext(expectedFile)), pattern = "(zip|tar)"), "expectedFile"],
        function(archive)
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
          if (any(filesNeeded %in% filesInArchive))
          {
            fun(archivePath, exdir = dataPath, files = filesNeeded[filesNeeded %in% filesInArchive])
          }
        }
      )
    }
    
    # Update checksums
    Cache(checksums, 
          module = moduleName, 
          path = modulePath,
          digestPathContent = TRUE,
          notOlderThan = Sys.time(),
          write = TRUE
    )
  }
    
  # Load objects
  env <- new.env()
  
  for (file in c(mapping, mask))
  {
    assign(x = names(file), value = getFromNamespace(loadFun, loadPackage)(file), envir = env)
  }
  
  if (!is.null(mask))
  {
    for (obj in ls(env, all.names = TRUE))
    {
      x <- env[[obj]]
      objClass <- is(env[[obj]])
      
      if ("RasterLayer" %in% objClass || "RasterStack" %in% objClass)
      {
        x <- Cache(raster::crop, x = x, y = mask)
        env[[obj]] <- Cache(raster::mask, x = x, mask = mask)
      } 
      else if ("spatialObjects" %in% objClass)
      {
        env[[obj]] <- Cache(rgeos::gIntersection, spgeom1 = x, spgeom2 = mask, byid = TRUE, drop_lower_td = TRUE)
      } 
      else if ("sf" %in% objClass)
      {
        x <- Cache(sf::st_intersection, x = x, y = mask)
        env[[obj]] <- Cache(sf::st_collection_extract, x = x, type = "POLYGON")
      }
    }
  }
    
  # Copy to globalenv()
  list2env(as.list(env), envir = globalenv())
  
}

