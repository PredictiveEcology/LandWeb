intersectListShps <- function(listShps, intersectShp) {
  message("Intersecting reporting polygons with shpStudyRegion")
  if (!is(intersectShp, "sf")) {
    intersectSF <- sf::st_as_sf(intersectShp)
  } else {
    intersectSF <- intersectShp
  }

  outerOut <- mapply(shp = listShps, shpNames = names(listShps),
                     function(shp, shpNames, useSF = FALSE) {
    message("  ", shpNames)
    tryCatch({
      if (!is(shp, "sf")) {
        wasShp <- TRUE
        shpSF <- sf::st_as_sf(shp)
      } else {
        wasShp <- FALSE
        shpSF <- shp
      }
      if (!identical(sf::st_crs(intersectSF), sf::st_crs(shpSF)))
        intersectSF <- Cache(sf::st_transform, intersectSF, crs = sf::st_crs(shpSF) )
      out <- sf::st_intersection(shpSF, intersectSF)
      if(wasShp) {
        out <- as(out, "Spatial")
      }
    }, error = function(x) {
      message("  intersectListShps -- sf package failed, using sp")
      if (!identical(crs(intersectShp), crs(shp)))
        intersectShp <- Cache(spTransform, intersectShp, crs(shp) )
      out <- raster::intersect(shp, intersectShp)
    })



  })
}

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
  allStack <- raster::stack(unlist(lapply(out, function(ageClasses) {
    out <- lapply(ageClasses, function(rep) {
      #lapply(rep, function(vegType) {
      rep
      #})
    })
    #names(out) <-
    out
  })))
  IDs <- raster::levels(out[[1]][[1]])[[1]]$ID
  Factors <- raster::levels(out[[1]][[1]])[[1]]$Factor
  ii <- 3
  aa <- raster::extract(allStack, spTransform(polygonToSummarizeBy, CRSobj = crs(allStack)))

  aa1 <- lapply(aa, function(x,  ...) {
    if (!is.null(x)) {
      apply(x, 2, function(y) {
        nonNACells <- na.omit(y)
        if (length(nonNACells)) {
          vals <- tabulate(nonNACells, max(IDs))
          names(vals)[IDs] <- Factors
          vals <- vals[!is.na(names(vals))]
          if (sum(vals)) {
            vals / sum(vals)
          } else {
            vals
          }
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

  aadf <- data.frame(zone = rep(polygonToSummarizeBy$shinyLabel[nonNulls], 
                                each = length(Factors)),
                     polygonNum = rep(seq_along(polygonToSummarizeBy$shinyLabel)[nonNulls], each = length(Factors)),
                     vegType = vegType, do.call(rbind, aa1[nonNulls]),
                     stringsAsFactors = FALSE)

  temp <- list()
  for (ages in ageClasses) {
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
  clumpedRas <- clump(ras, gaps = FALSE, ...)
  cellIDByPolygon[, newRas := clumpedRas[][cell]]
  cellIDByPolygon[, list(sizeInHa = .N * prod(res(clumpedRas))/1e4),
                  by = c("polygonID","newRas")] %>% na.omit()
}


cellNumbersForPolygon <- function(dummyRaster, Polygon) {
  aa <- raster::extract(dummyRaster, y = Polygon, cellnumbers = TRUE)
  notNull <- !unlist(lapply(aa, is.null))
  rbindlist(lapply(seq_along(aa)[notNull], function(x) data.frame(cell = aa[[x]][,"cell"], polygonID = x)))
}

reprojectRasts <- function(tsf, lfltFN, crs, flammableFile) {
  message("Reprojecting rasters & loading into RAM")
  rstFlammableNum <- raster(flammableFile)
  rstFlammableNum <- projectRaster(rstFlammableNum, crs = crs, method = "ngb")
  globalRasters <- lapply(seq_along(tsf), function(FN) {
    r <- raster(tsf[[FN]])
    r <- projectRaster(r, crs = crs, method = "ngb", datatype = "INT2U")
    minAge <- as.numeric(strsplit(strsplit(tsf[[1]], split = "year")[[1]][2], split = "\\.tif")[[1]])
    r[is.na(r) & (rstFlammableNum == 0)] <- minAge
    r <- writeRaster(r, filename = lfltFN[FN], overwrite = TRUE, datatype = "INT2U")
    r
  })
  message("  Finished reprojecting rasters & loading into RAM")
  globalRasters
}

# Set up gdal stuff -- first, find the installation
gdalSet <- function() {
  gdal_setInstallation()
  getOption("gdalUtils_gdalPath")
}

gdal2TilesFn <- function(r, filename, zoomRange = 6:11, color_text_file = asPath(colorTableFile)) {
  filename1 <- filename(r)
  prefix <- file.path("www", studyArea)
  checkPath(prefix, create = TRUE)
  filename2 <- file.path(prefix, paste0("out", basename(filename1)))
  filename3 <- file.path(prefix, paste0("out2", basename(filename1)))
  filename4 <- file.path(prefix, paste0("out3", basename(filename1)))
  filename5 <- file.path(prefix, paste0("out4", basename(filename1)))
  filename5 <- gsub(pattern = "tif", x = filename5, replacement = "vrt")
  foldername <- gsub(pattern = ".tif", filename2, replacement = "")

  if (anyNA(r[])) r[is.na(r[])] <- -1
  if (minValue(r) < 0) r <- r - minValue(r)

  r <- writeRaster(x = r, filename = filename2, overwrite = TRUE, datatype = "INT2S")
  gdalUtils::gdaldem(mode = "color-relief", filename2, #alpha = TRUE,
                     color_text_file = as.character(color_text_file), filename3)
  system(paste0("python ",
                file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "rgb2pct.py "),
                filename3,
                " ",
                filename4))
  gdalUtils::gdal_translate(of = "VRT", expand = "rgb", filename4, filename5)
  system(paste0("python ",
                file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "gdal2tiles.py "),
                "--s_srs=EPSG:4326 ",
                " --zoom=", min(zoomRange), "-", max(zoomRange), " ",
                "--srcnodata=0 ",
                filename5, " ",
                foldername),
         wait = TRUE)
  unlink(filename5)
  unlink(filename4)
  unlink(filename3)
  unlink(filename2)

  return(invisible(NULL))
}


PredictiveEcologyPackages <- c("reproducible", "SpaDES.core", "SpaDES.tools")

workingShas <- function(date) {
  shas <- lapply(PredictiveEcologyPackages, devtools:::local_sha)
  names(shas) <- PredictiveEcologyPackages
  shas$LandWeb <- system("git rev-parse HEAD", intern = TRUE)
  shas
}

showWorkingShas <- function(cachePath) {
  sc <- showCache(cachePath, "workingShas")
  setorderv(sc, "createdDate", order = -1L)
  sc
}

reloadWorkingShas <- function(md5hash, cachePath, shaOnly = FALSE) {
  shas <- archivist::loadFromLocalRepo(repoDir = cachePath, md5hash, value = TRUE)
  whPackages <- names(shas) %in% PredictiveEcologyPackages
  lapply(seq_along(shas[whPackages]), function(n) {
    if ((devtools:::local_sha(names(shas)[n])) != shas[[n]]) {
      install_github(paste0("PredictiveEcology/",names(shas)[n], "@", shas[n]),
                     dependencies = FALSE)
    } else {
      message(names(shas)[n], " is already correct version")
    }
  })
  isError <- tryCatch(
    checkoutCondition <<- reproducible:::checkoutVersion(
      paste0("eliotmcintire/LandWeb@", shas$LandWeb), cred = "GITHUB_PAT"),
    error = function(x) TRUE)
  #checkoutCondition <- reproducible:::checkoutVersion(shas$LandWeb, cred = "GITHUB_PAT")
  if (isTRUE(isError)) message("no previous branch on github with that sha")
  return(invisible(shas))
}
noLongerWaiting <- function() {
  hide("loading_page")
  #show("main_content")
}

mycss <- "
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
"

reloadPreviousWorkingFn <- function(reloadPreviousWorking) {
  reloadPreviousWorkingLogical <- any(reloadPreviousWorking != FALSE)
  if (Sys.info()["nodename"] %in% c("W-VIC-A105388", "W-VIC-A128863")) {
    if (!exists(".reloadPreviousWorking")) {
      if (!reloadPreviousWorkingLogical) {
        .reloadPreviousWorking <- 0
      } else {
        .reloadPreviousWorking <- 1
      }
    } else if (.reloadPreviousWorking != 2) {
      .reloadPreviousWorking <- reloadPreviousWorkingLogical + 0
    } else if (reloadPreviousWorkingLogical) {
      .reloadPreviousWorking <- reloadPreviousWorkingLogical + 0
    }
  } else {
    .reloadPreviousWorking <- 0
  }

  if (.reloadPreviousWorking == 1) {
    #library(git2r) # has git repo internally
    md5s <- tryCatch(showWorkingShas(reproducibleCache), error = function(x) TRUE)
    if (NROW(md5s)) {
      system("git stash")
      if (is.character(reloadPreviousWorking))  {
        searchTerm <- reloadPreviousWorking
      } else {
        searchTerm <- unique(md5s$artifact)[as.numeric(reloadPreviousWorking)]
      }
      searchTerm <- unique(showCache(searchTerm, x = reproducibleCache)$artifact)
      shas <- reloadWorkingShas(md5hash = searchTerm[1],
                                cachePath = reproducibleCache) # 1 is most recent
      .reloadPreviousWorking <- 2
      stop("Run app again")
    } else {
      message("No previous working version. Proceeding.")
    }
  }

  return(.reloadPreviousWorking)
}


loadCCSpecies <- function(mapNames, url, dPath, userTags) {
  if (!any(grepl("1$", mapNames) )) {
    filenames <- paste0(mapNames, "1")
  } else {
    filenames <- mapNames
    mapNames <- gsub(mapNames, "1$", "")
  }
  names(filenames) <- mapNames
  
  mapply(filename = filenames, mapName = mapNames, function(filename, mapName) {
    #fn <- paste0("CC", mapName, "Filename")
    #mn <- paste0("CC", mapName, "Files")
    tifName <-  asPath(file.path(dPath, paste0(filename, ".tif")))
    filenames <- asPath(paste0(filenames, ".", c("tfw", "tif.aux.xml", "tif.ovr", "tif.vat.cpg", "tif.vat.dbf")))
    Cache(prepInputs, userTags = "stable",  
          url =  url,
          archive = "CurrentCondition.zip",
          targetFile = tifName,
          alsoExtract = filenames,
          destinationPath = dPath)
  })
  
  
}


createReportingPolygons <- function(layerNames) {
  layerNames <- c("Alberta Ecozones", "National Ecozones", 
                  "National Ecodistricts", "Forest Management Areas", 
                  "Alberta FMUs", "Caribou Herds")
  names(layerNames) <- layerNames
  
  polys <- as.list(layerNames)
  # Alberta Ecozone
  layerNamesIndex <- 1
  dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
  albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf", 
                                  "Natural_Regions_Subregions_of_Alberta.lyr", "Natural_Regions_Subregions_of_Alberta.prj", 
                                  "Natural_Regions_Subregions_of_Alberta.shp.xml", 
                                  "Natural_Regions_Subregions_of_Alberta.shx", "natural_regions_subregions_of_alberta.zip", 
                                  "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
  albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
  albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
  polys[[layerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable", 
                                                url = albertaEcozoneURL, targetFile = albertaEcozoneFilename,
                                                fun = "shapefile", destinationPath = dPath, alsoExtract = albertaEcozoneFiles)
  polys[[layerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[layerNames[layerNamesIndex]]] $NSRNAME
  
  # National Ecozone
  layerNamesIndex <- 2
  dPath <- file.path(paths$inputPath, "ecozones", "National")
  ecozoneFilename <-   file.path(dPath, "ecozones.shp")
  ecozoneFiles <- c("ecozones.dbf", "ecozones.prj", 
                    "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
  polys[[layerNames[layerNamesIndex]]]  <- Cache(prepInputs, userTags = "stable", 
                                                 url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                                                 targetFile = asPath(ecozoneFilename),
                                                 alsoExtract = ecozoneFiles,
                                                 fun = "shapefile", destinationPath = dPath)
  polys[[layerNames[layerNamesIndex]]] @data[[labelColumn]] <- polys[[layerNames[layerNamesIndex]]] $ZONE_NAME
  
  # National Ecodistrict
  layerNamesIndex <- 3
  dPath <- file.path(paths$inputPath, "ecodistricts", "National")
  ecodistrictFilename <-   file.path(dPath, "ecodistricts.shp")
  ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj", 
                        "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
  polys[[layerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable", 
                                                url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                                targetFile = asPath(ecodistrictFilename),
                                                alsoExtract = ecodistrictFiles,
                                                fun = "shapefile", destinationPath = dPath)
  polys[[layerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[layerNames[layerNamesIndex]]]$ZONE_NAME
  
  
  ## Polygons for report/shiny app
  ## All FMAs - 
  layerNamesIndex <- 4
  dPath <- file.path(paths$inputPath, "allFMAs")
  allFMAsFilename <- asPath(file.path(dPath, "FMA_Boudary.shp"))
  allFMAsFiles <- c("FMA_Boudary.CPG", "FMA_Boudary.dbf", "FMA_Boudary.prj", 
                    "FMA_Boudary.sbn", "FMA_Boudary.sbx", "FMA_Boudary.shp", "FMA_Boudary.shp.xml", 
                    "FMA_Boudary.shx")
  polys[[layerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable", 
                                                url = "https://drive.google.com/open?id=1oCMiHRRT1bCWe0Uv69nRSrE1nsh-4Tic",
                                                #targetFile = albertaFMUFilename,
                                                #alsoExtract = albertaFMUFiles,
                                                fun = "shapefile", 
                                                destinationPath = dPath)
  polys[[layerNames[layerNamesIndex]]]@data[[labelColumn]] <- 
    polys[[layerNames[layerNamesIndex]]]$Name
  
  
  ## Alberta FMU - 
  layerNamesIndex <- 5
  dPath <- file.path(paths$inputPath, "FMU_Alberta_2015-11")
  albertaFMUFilename <- asPath(file.path(dPath, "FMU_Alberta_2015-11.shp"))
  albertaFMUFiles <- c("FMU_Alberta_2015-11.cpg", "FMU_Alberta_2015-11.dbf", 
                       "FMU_Alberta_2015-11.prj", "FMU_Alberta_2015-11.sbn", 
                       "FMU_Alberta_2015-11.sbx", "FMU_Alberta_2015-11.shp", 
                       "FMU_Alberta_2015-11.shp.xml", "FMU_Alberta_2015-11.shx")
  polys[[layerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable", 
                                                url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
                                                targetFile = albertaFMUFilename,
                                                alsoExtract = albertaFMUFiles,
                                                fun = "shapefile", 
                                                destinationPath = dPath)
  polys[[layerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[layerNames[layerNamesIndex]]]$FMU_NAME
  
  
  # Caribou Zones
  layerNamesIndex <- 6
  dPath <- file.path(paths$inputPath, "Caribou")
  caribouFilename <-   file.path(dPath, "LP_MASTERFILE_June62012.shp")
  caribouFiles <- c("LP_MASTERFILE_June62012.dbf", "LP_MASTERFILE_June62012.prj", 
                    "LP_MASTERFILE_June62012.sbn", "LP_MASTERFILE_June62012.sbx", 
                    "LP_MASTERFILE_June62012.shp", "LP_MASTERFILE_June62012.shp.xml", 
                    "LP_MASTERFILE_June62012.shx")
  CaribouZonesColumn <- "HERD"
  polys[[layerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable", 
                                                url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
                                                targetFile = asPath(caribouFilename),
                                                alsoExtract = caribouFiles,
                                                fun = "shapefile", 
                                                destinationPath = dPath)
  polys[[layerNames[layerNamesIndex]]]@data[[labelColumn]] <- 
    polys[[layerNames[layerNamesIndex]]]$HERD
  
  
  
  
  ########################################################
  ########################################################
  ########################################################
  # Make them all crsStudyArea
  polys <- Cache(lapply, polys, function(shp) {
    spTransform(shp, CRSobj = crsStudyArea)
  }, userTags = "stable")
  
  
  # Make SubRegion
  polysSubRegion <- Cache(intersectListShps, polys, shpStudyRegion, userTags = "stable")
  #polys <- append(polys, polysSubRegion)
  
  #### Thin polygons
  if (FALSE) {
    message("Thinning polygons for faster plotting in leaflet")
    polygons <- Cache(mapply, p = polygons, nam = names(polygons), userTags = "stable", 
                      function(p, nam) {
                        print(nam)
                        out <- Cache(rgeos::gSimplify, p, userTags = "stable", 
                                     tol = (xmax(p) - xmin(p))/10000, topologyPreserve = TRUE)
                        #out <- suppressWarnings(thin(p))
                        isSimp <- tryCatch(if(isTRUE(!all(rgeos::gIsSimple(out, byid = TRUE)))) FALSE else TRUE, 
                                           error = function(xx) FALSE)
                        browser(expr = "shpNationalEcodistrictDemo" %in% nam)
                        #if (rgeos::gIsSimple(out)) out <- raster::buffer(out, width = 0, dissolve = FALSE)
                        if (!isSimp) {
                          out <- raster::buffer(out, width = 0, dissolve = FALSE)
                        }
                        out <- SpatialPolygonsDataFrame(out, data = p@data, match.ID = TRUE)
                        
                        return(out)
                      }) 
  }
  
  
  # Make Leaflet versions of all
  message("Making leaflet versions of all reporting polygons")
  polysLflt <- Cache(mapply, p = polys, nam = names(polys), userTags = "stable", 
                     function(p, nam) {
                       message("  ", nam)
                       out <- tryCatch(spTransform(p, CRSobj = CRS(lflt)), error = function(x) {
                         p <- spChFIDs(p, as.character(seq(NROW(p))))
                         spTransform(p, CRSobj = CRS(lflt))
                       })
                     })
  polysLfltSubStudyRegion <- Cache(mapply, p = polysSubRegion, nam = names(polysSubRegion), userTags = "stable", 
                                   function(p, nam) {
                                     message("  ", nam)
                                     out <- tryCatch(spTransform(p, CRSobj = CRS(lflt)), error = function(x) {
                                       p <- spChFIDs(p, as.character(seq(NROW(p))))
                                       spTransform(p, CRSobj = CRS(lflt))
                                     })
                                   })
  
  # Put them all together in the structure:
  #   LayerName $ Projection (crsSR or crsLFLT) $ Scale (studyRegion or subStudyRegion)
  polysAll <- list("crsSR" = list("studyRegion" = polys, 
                                  "subStudyRegion" = polysSubRegion), 
                   "crsLFLT" = list("studyRegion" = polysLflt, 
                                    "subStudyRegion" = polysLfltSubStudyRegion))
  reportingPolygonsTmp <- lapply(polysAll, purrr::transpose)
  purrr::transpose(reportingPolygonsTmp)
}
