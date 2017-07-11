
ggvisFireReturnInterval <- function(shpStudyRegion, shpStudyRegionFull) {
  shpStudyAreaFort <- broom::tidy(shpStudyRegion, region = 'Name_1') 
  shpStudyAreaFort <- dplyr::left_join(shpStudyAreaFort, shpStudyRegion@data[, c("Name_1", "fireReturnInterval")], by = c("id" = "Name_1"))
  shpStudyAreaOrigFort <- broom::tidy(shpStudyRegionFull, region = 'Name_1') 
  shpStudyAreaOrigFort <- dplyr::left_join(shpStudyAreaOrigFort, shpStudyRegionFull@data[, c("Name_1", "fireReturnInterval")], by = c("id" = "Name_1"))
  #shpStudyAreaOrigFort<-shpStudyAreaOrigFort[order(shpStudyAreaOrigFort$order), ]
  
  #map <- ggplot2::fortify(maine, region="name")
  
  wdth <- 650
  ht <- wdth * (ymax(shpStudyRegionFull) - ymin(shpStudyRegionFull)) /
    (xmax(shpStudyRegionFull) - xmin(shpStudyRegionFull))
  df2 <- data.frame(fri = unique(shpStudyAreaOrigFort$fireReturnInterval), 
                    colrs = colorRampPalette(c("orange", "dark green"))(diff(range(shpStudyAreaOrigFort$fireReturnInterval)))[unique(shpStudyAreaOrigFort$fireReturnInterval) - min(shpStudyAreaOrigFort$fireReturnInterval) + 1]
  )
  shpStudyAreaOrigFort <- dplyr::left_join(shpStudyAreaOrigFort, df2, by = c("fireReturnInterval" = "fri"))
  # 
  a <- shpStudyAreaOrigFort %>%
    ggvis(~long, ~lat) %>%
    dplyr::group_by(group, id) %>%
    layer_paths(strokeOpacity := 0.5, stroke := "#7f7f7f", fill := ~fireReturnInterval) %>%
    add_tooltip(function(data) {
      paste0("Fire Return Interval: ", data$fireReturnInterval)
    }, "hover") %>% 
    layer_paths(data = shpStudyAreaFort %>% dplyr::group_by(group, id),
                stroke := "red") %>%
    #hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    #set_options(width = 400, height = 800)#, keep_aspect = TRUE)
    set_options(width = wdth, height = ht, keep_aspect = TRUE)
  
}


#' Calculate proportion of landscape occupied by each vegetation class
#' @return A data.table with proportion of the pixels in each vegetation class, for
#'         each given age class within each polygon
leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles,
                           polygonToSummarizeBy, 
                           ageCutoffs = ageClassCutOffs,  ageClasses, cl) {
  if (missing(cl)) {
    lapplyFn <- "lapply" 
  } else {
    lapplyFn <- "parLapplyLB"
    #clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy"),
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list(ls()),
                    envir = environment())
      clusterEvalQ(cl = cl, {
        library(raster)
      })
    }
  }
  out <- lapply(ageCutoffs, function(ages) {
    y <- match(ages, ageCutoffs)
    if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
      startList <- list(cl = cl)
    } else {
      startList <- list()
    }
    startList <- append(startList, list(y = y))
    
    out1 <- #Cache(cacheRepo = paths$cachePath, 
      do.call(lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
        x <- match(x, timeSinceFireFiles)
        timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
        leadingRast <- raster(vegTypeMapFiles[x])
        leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- 0
        if ((y + 1) < length(ageCutoffs))
          leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- 0
        leadingRast
      })))
    names(out1) <- gsub(paste(basename(dirname(timeSinceFireFiles)), 
                              basename(timeSinceFireFiles), sep = "_"), 
                        replacement = "_", pattern = "\\.")
    out1
  })
  names(out) <- ageClasses
  allStack <- stack(unlist(lapply(out, function(ageClasses) {
    out <- lapply(ageClasses, function(rep) {
      #lapply(rep, function(vegType) {
      rep
      #}) 
    })
    #names(out) <- 
    out
  })))
  IDs <- levels(out[[1]][[1]])[[1]]$ID
  Factors <- levels(out[[1]][[1]])[[1]]$Factor
  ii <- 3
  aa <- raster::extract(allStack, polygonToSummarizeBy)
  
  aa1 <- lapply(aa, function(x,  ...) {
    if(!is.null(x)) {
      apply(x, 2, function(y) {
        nonNACells <- na.omit(y)
        vals <- tabulate(nonNACells, max(IDs))
        names(vals)[IDs] <- Factors
        vals <- vals[!is.na(names(vals))]
        if(sum(vals)) {
          vals / sum(vals)
        } else {
          vals
        }
      })
    } else {
      NULL
    }
  })
  
  
  nonNulls <- unlist(lapply(aa1, function(x) !is.null(x)))
  vegType <- unlist(lapply(aa1[nonNulls], rownames))
  aa1 <- lapply(aa1, function(a) {
    rownames(a) <- NULL
    a
  })
  
  aadf <- data.frame(zone=rep(ecodistricts$ECODISTRIC[nonNulls], each = length(Factors)), 
                     polygonNum = rep(seq_along(ecodistricts$ECODISTRIC)[nonNulls], each = length(Factors)),
                     vegType = vegType, do.call(rbind, aa1[nonNulls]),
                     stringsAsFactors = FALSE)
  
  temp <- list()
  for(ages in ageClasses) {
    temp[[ages]] <- aadf %>%
      dplyr::select(starts_with(ages) , zone:vegType) %>%
      tidyr::gather(key = "label", value = "proportion", -(zone:vegType)) %>%
      mutate(ageClass = unlist(lapply(strsplit(label, split = "\\."), function(x) x[[1]])))
  }
  
  aa <- rbindlist(temp)
  aa
}



# A function that creates a raster with contiguous patches labelled as such            
countNumPatches <- function(ras, cellIDByPolygon, ...) {
  clumpedRas <- clump(ras, gaps=FALSE, ...)
  cellIDByPolygon[,newRas:=clumpedRas[][cell]]
  cellIDByPolygon[,list(sizeInHa=.N * prod(res(clumpedRas))/1e4),by=c("polygonID","newRas")] %>% na.omit()
}


cellNumbersForPolygon <- function(dummyRaster, Polygon) {
  aa <- raster::extract(dummyRaster, 
                        y = Polygon, cellnumbers = TRUE)
  notNull <- !unlist(lapply(aa, is.null))
  rbindlist(lapply(seq_along(aa)[notNull], function(x) data.frame(cell=aa[[x]][,"cell"], polygonID=x)))
}

#' Calculate number of "large" patches in a landscape
#' @return A matrix with counts of number of large patches
largePatchesFn <- function(timeSinceFireFiles, vegTypeMapFiles,
                           polygonToSummarizeBy, cl, 
                           ageCutoffs = ageClassCutOffs, countNumPatches = countNumPatches,
                           ageClasses, notOlderThan = Sys.time() - 1e7) {

#  withProgress(message = 'Calculation in progress',
#               detail = 'This may take a while...', value = 0, {
                   
    withoutPath <- unlist(lapply(strsplit(timeSinceFireFiles, split = paths$outputPath), function(x) x[-1])) %>%
      gsub(pattern = "\\/", replacement = "_")
    yearNames <- unlist(lapply(strsplit(withoutPath, split = "_rstTimeSinceFire_|\\."), function(x) 
      paste0(gsub(x[-length(x)], pattern="\\/",replacement="_"), collapse="")))
    
    rasWithNAs <- raster(raster(timeSinceFireFiles[1]))
    rasWithNAs[] <- NA
    #polygonToSummarizeByRast <- Cache(rasterize, polygonToSummarizeBy, y = rasWithNAs, cacheRepo = paths$cachePath)
    cellIDByPolygon <- Cache(cacheRepo = paths$cachePath,
                cellNumbersForPolygon, rasWithNAs, polygonToSummarizeBy)
    
    if (missing(cl)) {
      lapplyFn <- "lapply"
    } else {
      lapplyFn <- "parLapplyLB"
      if (Sys.info()[["sysname"]] == "Windows") {
        
        clusterExport(cl = cl,
                      varlist = list(c(ls(), "countNumPatches")),
                      envir = environment())
        clusterEvalQ(cl = cl, {
          library(raster)
          library(magrittr)
          library(SpaDES.core)
          library(data.table)
        })
      }
    }
    
    out <- lapply(ageCutoffs, function(ages) {
      y <- match(ages, ageCutoffs)
      if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
        startList <- list(cl = cl)
      } else {
        startList <- list()
      }
      startList <- append(startList, list(y = y))
      out1 <- Cache(cacheRepo = paths$cachePath, #notOlderThan = Sys.time(),
        do.call, lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
          x <- match(x, timeSinceFireFiles)
          timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
          leadingRast <- raster(vegTypeMapFiles[x])
          leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- NA
          if ((y + 1) < length(ageCutoffs))
            leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- NA
          
          clumpedRasts <- lapply(levels(leadingRast)[[1]]$ID, function(ID) {
            spRas <- leadingRast
            spRas[spRas != ID] <- NA
            countNumPatches(spRas, cellIDByPolygon, directions = 8)
          })
          names(clumpedRasts) <- levels(leadingRast)[[1]]$Factor
          clumpedRasts <- append(clumpedRasts,
                                 list("All species" =
                                        countNumPatches(leadingRast,  
                                                        cellIDByPolygon,
                                                        directions = 8)
                                 ))
          clumpedRasts
        })))
      names(out1) <- yearNames
      outDT <- rbindlist(lapply(seq_along(out1), function(y) {
          a <- rbindlist(lapply(seq_along(out1[[y]]), function(x) out1[[y]][[x]][,vegCover:=names(out1[[y]])[x]]))
          a[,rep:=names(out1)[y]]
        }))
    }
    )
    out <- setNames(out, ageClasses)
    out <- rbindlist(lapply(seq_along(out), function(z) {
      out[[z]][, ageClass:=names(out)[z]]
    }))
    
    out[sizeInHa>=100] # never will need patches smaller than 100 ha
    #setProgress(1)
  #})
  
}


reprojectRasts <- function(tsf, lfltFN, crs, endTime = end(mySim), flammableFile) {
  message("Reprojecting rasters & loading into RAM")
  rstFlammableNum <- raster(flammableFile)
  rstFlammableNum <- projectRaster(rstFlammableNum, crs = crs, 
                                   method = "ngb")
  globalRasters <- lapply(seq_along(tsf), function(FN) {
    r <- raster(tsf[[FN]])
    r <- projectRaster(r, crs = crs, method = "ngb",
                       datatype = "INT2U")
    r[is.na(r) & (rstFlammableNum == 0)] <- endTime
    r <- writeRaster(r, filename = lfltFN[FN], overwrite = TRUE,
                     datatype="INT2U")
    r
  })
  message("  Finished reprojecting rasters & loading into RAM")
  globalRasters
}


gdal2TilesFn <- function(r, filename, zoomRange=6:11, color_text_file = asPath(colorTableFile)) {
  filename1 <- filename(r)
  prefix <- file.path("www",studyArea)
  checkPath(prefix, create = TRUE)
  filename2 <- file.path(prefix, paste0("out",basename(filename1)))
  filename3 <- file.path(prefix,paste0("out2",basename(filename1)))
  filename4 <- file.path(prefix,paste0("out3",basename(filename1)))
  filename5 <- file.path(prefix,paste0("out4",basename(filename1)))
  filename5 <- gsub(pattern="tif", x=filename5, replacement = "vrt")
  foldername <- gsub(pattern=".tif", filename2, replacement = "")
  
  if(anyNA(r[])){
    r[is.na(r[])] <- -1
  } 
  if(minValue(r)<0 ) {
    r <- r-minValue(r)
  }
  r <- writeRaster(x=r, filename = filename2,
                   overwrite=TRUE,datatype="INT2S")
  gdalUtils::gdaldem(mode="color-relief", filename2, #alpha = TRUE,
                     color_text_file = as.character(color_text_file), 
                     filename3)
  system(paste0("python ",
                file.path(getOption("gdalUtils_gdalPath")[[1]]$path,"rgb2pct.py "),
                filename3,
                " ",
                filename4))
  gdalUtils::gdal_translate(of="VRT", expand="rgb", filename4, filename5)
  system(paste0("python ", 
                file.path(getOption("gdalUtils_gdalPath")[[1]]$path,"gdal2tiles.py "), 
                "--s_srs=EPSG:4326 ",
                " --zoom=",min(zoomRange),"-",max(zoomRange)," ",
                "--srcnodata=0 ",
                filename5," ",
                foldername),
         wait=TRUE)
  unlink(filename5)
  unlink(filename4)
  unlink(filename3)
  unlink(filename2)
  
  return(invisible(NULL))
}

