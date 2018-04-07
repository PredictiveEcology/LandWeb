source("simInitAndExperiment.R")
source("largePatchesFn.R")

intersectListShps <- function(listShps, intersectShp) {
  message("Intersecting reporting polygons with shpStudyRegion")
  if (!is(intersectShp, "sf")) {
    intersectSF <- sf::st_as_sf(intersectShp)
  } else {
    intersectSF <- intersectShp
  }

  intersectShp <- raster::aggregate(intersectShp)
  problem1 <- !rgeos::gIsSimple(intersectShp)
  problem2 <- !rgeos::gIsValid(intersectShp)
  if (isTRUE(problem1 || problem2 )) {
    browser()
  }

  outerOut <- mapply(shp = listShps, shpNames = names(listShps),
                     function(shp, shpNames, useSF = FALSE) {
                       message("  ", shpNames)
                       # tryCatch({
                       #   if (!is(shp, "sf")) {
                       #     wasShp <- TRUE
                       #     shpSF <- sf::st_as_sf(shp)
                       #   } else {
                       #     wasShp <- FALSE
                       #     shpSF <- shp
                       #   }
                       #   if (!identical(sf::st_crs(intersectSF), sf::st_crs(shpSF)))
                       #     intersectSF <- Cache(sf::st_transform, intersectSF, crs = sf::st_crs(shpSF) )
                       #   intersectSF <- sf::st_combine(intersectSF)
                       #   out <- sf::st_intersection(sf::st_geometry(intersectSF), sf::st_geometry(shpSF))
                       #   if (wasShp) {
                       #     out <- as(out, "Spatial")
                       #   }
                       # }, error = function(x) {
                       #   message("  intersectListShps -- sf package failed, using sp")
                         if (!identical(crs(intersectShp), crs(shp)))
                           intersectShp <- Cache(spTransform, intersectShp, crs(shp) )
                         out <- raster::intersect(shp, intersectShp)
                       #})
                     })
}

#' Calculate proportion of landscape occupied by each vegetation class
#'
#' @return A data.table with proportion of the pixels in each vegetation class, for
#'         each given age class within each polygon
leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles, polygonToSummarizeBy,
                           ageClassCutOffs,  ageClasses, cl) {

  lapplyFn <- "lapply"
  if (!missing(cl)) { # not missing
    if (!identical(cl, "FALSE")) { # is NOT FALSE
      lapplyFn <- "parLapplyLB"
      if (isTRUE(cl)) { # Is TRUE
        if (detectCores() > 12) {
          ncores <- min(length(timeSinceFireFiles), detectCores() / 4)
          message("making ", ncores, " node cluster")
          if (Sys.info()[["sysname"]] == "Windows") {
            cl <- makeCluster(ncores)
          } else {
            cl <- makeForkCluster(ncores)
          }
          on.exit({
            message("Closing cores")
            stopCluster(cl)
            }
          )
        } else { # not enough clusters
          lapplyFn <- "lapply"
          cl <- FALSE
        }
      }
    }
  }
  if (is(cl, "cluster")) {
    ## By here, it must be a cluster
    #clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy"),
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list(ls()), envir = environment())
      clusterEvalQ(cl = cl, {
        library(raster)
      })
    }

  }

  out <- lapply(ageClassCutOffs, function(ages) {
    y <- match(ages, ageClassCutOffs)
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
        leadingRast[timeSinceFireFilesRast[] < ageClassCutOffs[y]] <- 0
        if ((y + 1) < length(ageClassCutOffs))
          leadingRast[timeSinceFireFilesRast[] >= ageClassCutOffs[y + 1]] <- 0
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

  aadf <- data.frame(
    zone = rep(polygonToSummarizeBy$shinyLabel[nonNulls], each = length(Factors)),
    polygonID = as.character(rep(seq_along(polygonToSummarizeBy$shinyLabel)[nonNulls], each = length(Factors))), ## TODO:
    vegCover = vegType, do.call(rbind, aa1[nonNulls]),
    stringsAsFactors = FALSE
  )

  temp <- list()
  for (ages in ageClasses) {
    temp[[ages]] <- aadf %>%
      dplyr::select(starts_with(ages) , zone:vegCover) %>%
      tidyr::gather(key = "label", value = "proportion", -(zone:vegCover)) %>%
      mutate(ageClass = unlist(lapply(strsplit(label, split = "\\."), function(x) x[[1]])))
  }

  aa <- rbindlist(temp)
  aa
}

# A function that creates a raster with contiguous patches labelled as such
countNumPatches <- function(ras, cellIDByPolygon, ...) {
  clumpedRas <- clump(ras, gaps = FALSE, ...)
  #data.table::set(cellIDByPolygon, , "newRas", clumpedRas[][cellIDByPolygon$cell])
  cellIDByPolygon[, newRas := clumpedRas[][cell]]
  cellIDByPolygon[, list(sizeInHa = .N * prod(res(clumpedRas)) / 1e4),
                  by = c("polygonID", "newRas")] %>% na.omit()
}

cellNumbersForPolygon <- function(dummyRaster, Polygons) {
  dtList <- Map(Polygon = Polygons, PolygonName = names(Polygons),
                function(Polygon, PolygonName) {
                  message("  Assigning PolygonIDs for each pixel from ", PolygonName)
                  aa <- raster::extract(dummyRaster, y = Polygon, cellnumbers = TRUE)
                  notNull <- !unlist(lapply(aa, is.null))
                  dt <- rbindlist(lapply(seq_along(aa)[notNull], function(x) {
                    data.table(cell = aa[[x]][, "cell"], polygonID = as.character(x))
                  }))
                  data.table::copy(dt)
  })

  # There is a weird bug that makes the data.table from previous line. copy() is a work around
  # Error in data.table::set(cellIDByPolygon, , "newRas", clumpedRas[][cellIDByPolygon$cell]) :
  #   Internal logical error. DT passed to assign has not been allocated enough column slots. l=2, tl=2, adding 1
  return(dtList)
}

reprojectRasts <- function(tsf, lfltFN, crs, flammableFile) {
  message("Reprojecting rasters, filling in minimum age, saving to disk")
  rstFlammableNum <- raster(flammableFile)
  rstFlammableNum <- projectRaster(rstFlammableNum, crs = crs, method = "ngb")
  rastsLFLT <- Cache(lapply, userTags = "reprojectRasts", 
                     seq_along(tsf), function(FN) {
    message("  ", tsf[[FN]])
    r <- raster(tsf[[FN]])
    # gdalwarp(srcfile = filename(r), dstfile = lfltFN[FN], s_srs = crs(r),
    #          t_srs = crs, r = "near",
    #          te = c(xmin(rstFlammableNum), ymin(rstFlammableNum),
    #                 xmax(rstFlammableNum), ymax(rstFlammableNum)),
    #          tr = res(rstFlammableNum),
    #          overwrite = TRUE
    # )
    # r2 <- raster(lfltFN[FN])
    # r2 <- setMinMax(r2)
    # r2[] <- r2[]
    r <- projectRaster(r, crs = crs, method = "ngb", datatype = "INT2U")
    minAge <- as.numeric(strsplit(strsplit(tsf[[1]], split = "year")[[1]][2], split = "\\.tif")[[1]])
    r[is.na(r) & (rstFlammableNum == 0)] <- minAge
    r <- writeRaster(r, filename = lfltFN[FN], overwrite = TRUE, datatype = "INT2U")
    r
  })
  rastsCRSSR2 <- lapply(tsf, raster)

  globalRasts <- list("crsSR" = rastsCRSSR2, "crsLFLT" = rastsLFLT)

  message("  Finished reprojecting rasters")
  globalRasts
}

# Set up gdal stuff -- first, find the installation
gdalSet <- function() {
  gdal_setInstallation()
  getOption("gdalUtils_gdalPath")
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

# Used in global_file.R to load the Current Condition Rasters from SilvaCom
loadCCSpecies <- function(mapNames, userTags = "", ...) {
  if (!any(grepl("1$", mapNames) )) {
    filenames <- paste0(mapNames, "1")
  } else {
    filenames <- mapNames
    mapNames <- gsub(mapNames, "1$", "")
  }
  names(filenames) <- mapNames

  Map(filename = filenames, mapName = mapNames, MoreArgs = list(userTags = userTags),
      function(filename, mapName, userTags) {
        tifName <-  asPath(file.path(dPath, paste0(filename, ".tif")))
        filenames <- asPath(paste0(filenames, ".", c("tfw", "tif.aux.xml", "tif.ovr", "tif.vat.cpg", "tif.vat.dbf")))
        Cache(prepInputs, userTags = c(userTags, "stable"),
              archive = "CurrentCondition.zip",
              targetFile = tifName,
              alsoExtract = filenames, ...)
      })
}


createReportingPolygons <- function(layerNames, shpStudyRegion, shpSubStudyRegion,
                                    intersectListShpsFn) {

  cannonicalLayerNames <- c("Alberta Ecozones", "National Ecozones",
                   "National Ecodistricts", "Forest Management Areas",
                   "Alberta FMUs", "Caribou Herds")
  if (!(all(layerNames %in% cannonicalLayerNames)) ) {
    stop("This function can only handle ", paste(cannonicalLayerNames, collapse = ", "))
  }

  names(layerNames) <- layerNames

  polys <- as.list(layerNames)
  # Alberta Ecozone
  layerNamesIndex <- 1
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
    albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf",
                                    "Natural_Regions_Subregions_of_Alberta.lyr", "Natural_Regions_Subregions_of_Alberta.prj",
                                    "Natural_Regions_Subregions_of_Alberta.shp.xml",
                                    "Natural_Regions_Subregions_of_Alberta.shx", "natural_regions_subregions_of_alberta.zip",
                                    "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
    albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
    albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
    polys[[cannonicalLayerNames[layerNamesIndex]]] <- Cache(
      prepInputs, userTags = "stable",
      url = albertaEcozoneURL, targetFile = albertaEcozoneFilename,
      fun = "shapefile", destinationPath = dPath, alsoExtract = albertaEcozoneFiles
    )
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[cannonicalLayerNames[layerNamesIndex]]]$NSRNAME
  }

  # National Ecozone
  layerNamesIndex <- 2
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- file.path(paths$inputPath, "ecozones", "National")
    ecozoneFilename <-   file.path(dPath, "ecozones.shp")
    ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
                      "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
    polys[[cannonicalLayerNames[layerNamesIndex]]]  <- Cache(
      prepInputs, userTags = "stable",
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      targetFile = asPath(ecozoneFilename),
      alsoExtract = ecozoneFiles,
      fun = "shapefile", destinationPath = dPath
    )
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[cannonicalLayerNames[layerNamesIndex]]]$ZONE_NAME
  }

  # National Ecodistrict
  layerNamesIndex <- 3
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- file.path(paths$inputPath, "ecodistricts", "National")
    ecodistrictFilename <-   file.path(dPath, "ecodistricts.shp")
    ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj",
                          "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
    polys[[cannonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                                  targetFile = asPath(ecodistrictFilename),
                                                  alsoExtract = ecodistrictFiles,
                                                  fun = "shapefile", destinationPath = dPath)
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[cannonicalLayerNames[layerNamesIndex]]]$ECODISTRIC
  }

  ## Polygons for report/shiny app
  ## All FMAs -
  layerNamesIndex <- 4
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- file.path(paths$inputPath, "allFMAs")
    allFMAsFilename <- asPath(file.path(dPath, "FMA_Boudary.shp"))
    allFMAsFiles <- c("FMA_Boudary.CPG", "FMA_Boudary.dbf", "FMA_Boudary.prj",
                      "FMA_Boudary.sbn", "FMA_Boudary.sbx", "FMA_Boudary.shp", "FMA_Boudary.shp.xml",
                      "FMA_Boudary.shx")
    polys[[cannonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/open?id=1oCMiHRRT1bCWe0Uv69nRSrE1nsh-4Tic",
                                                  #targetFile = albertaFMUFilename,
                                                  #alsoExtract = albertaFMUFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <-
      polys[[cannonicalLayerNames[layerNamesIndex]]]$Name
  }

  ## Alberta FMU -
  layerNamesIndex <- 5
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- file.path(paths$inputPath, "FMU_Alberta_2015-11")
    albertaFMUFilename <- asPath(file.path(dPath, "FMU_Alberta_2015-11.shp"))
    albertaFMUFiles <- c("FMU_Alberta_2015-11.cpg", "FMU_Alberta_2015-11.dbf",
                         "FMU_Alberta_2015-11.prj", "FMU_Alberta_2015-11.sbn",
                         "FMU_Alberta_2015-11.sbx", "FMU_Alberta_2015-11.shp",
                         "FMU_Alberta_2015-11.shp.xml", "FMU_Alberta_2015-11.shx")
    polys[[cannonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
                                                  targetFile = albertaFMUFilename,
                                                  alsoExtract = albertaFMUFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[cannonicalLayerNames[layerNamesIndex]]]$FMU_NAME
  }

  # Caribou Zones
  layerNamesIndex <- 6
  if (cannonicalLayerNames[layerNamesIndex] %in% layerNames) {
    dPath <- file.path(paths$inputPath, "Caribou")
    caribouFilename <-   file.path(dPath, "LP_MASTERFILE_June62012.shp")
    caribouFiles <- c("LP_MASTERFILE_June62012.dbf", "LP_MASTERFILE_June62012.prj",
                      "LP_MASTERFILE_June62012.sbn", "LP_MASTERFILE_June62012.sbx",
                      "LP_MASTERFILE_June62012.shp", "LP_MASTERFILE_June62012.shp.xml",
                      "LP_MASTERFILE_June62012.shx")
    CaribouZonesColumn <- "HERD"
    polys[[cannonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
                                                  targetFile = asPath(caribouFilename),
                                                  alsoExtract = caribouFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[cannonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <-
      polys[[cannonicalLayerNames[layerNamesIndex]]]$HERD
  }


  ########################################################
  ########################################################
  ########################################################
  # Make them all crsStudyRegion
  polys <- Cache(lapply, polys, function(shp) {
    spTransform(shp, CRSobj = crsStudyRegion)
  }, userTags = "stable")

  # Make SubRegion
  polysSubRegion <- Cache(intersectListShpsFn, polys, shpSubStudyRegion, userTags = "stable")

  # Add shpStudyRegion and shpSubStudyRegion to lists
  polys$`LandWeb Study Area` <- shpStudyRegion
  polysSubRegion$`LandWeb Study Area` <- shpSubStudyRegion

  #### Thin polygons
  if (FALSE) {
    message("Thinning polygons for faster plotting in leaflet")
    polygons <- Cache(mapply, p = polygons, nam = names(polygons), userTags = "stable",
                      function(p, nam) {
                        print(nam)
                        out <- Cache(rgeos::gSimplify, p, userTags = "stable",
                                     tol = (xmax(p) - xmin(p))/10000, topologyPreserve = TRUE)
                        #out <- suppressWarnings(thin(p))
                        isSimp <- tryCatch(if (isTRUE(!all(rgeos::gIsSimple(out, byid = TRUE)))) FALSE else TRUE,
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
                       out <- tryCatch(spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT)), error = function(x) {
                         p <- spChFIDs(p, as.character(seq(NROW(p))))
                         spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT))
                       })
                     })
  polysLfltSubStudyRegion <- Cache(mapply, p = polysSubRegion, nam = names(polysSubRegion), userTags = "stable",
                                   function(p, nam) {
                                     message("  ", nam)
                                     out <- tryCatch(
                                       spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT)), error = function(x) {
                                       p <- spChFIDs(p, as.character(seq(NROW(p))))
                                       spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT))
                                     })
                                   })

  # Put them all together in the structure:
  #   LayerName $ Projection (crsSR or crsLFLT) $ Scale (studyRegion or subStudyRegion)
  polysAll <- list("crsSR" = list("shpStudyRegion" = polys,
                                  "shpSubStudyRegion" = polysSubRegion),
                   "crsLFLT" = list("shpStudyRegion" = polysLflt,
                                    "shpSubStudyRegion" = polysLfltSubStudyRegion))
  reportingPolygonsTmp <- lapply(polysAll, purrr::transpose)
  purrr::transpose(reportingPolygonsTmp)
}

createReportingPolygonsAll <- function(shpStudyRegion, shpSubStudyRegion, authenticationType,
                                       createReportingPolygonsFn, intersectListShpsFn) {
  message("Loading Reporting Polygons")
  reportingPolygons <- list()
  reportingPolygons$Free <- Cache(createReportingPolygonsFn, intersectListShpsFn = intersectListShps,
                                  c("Alberta Ecozones", "National Ecozones", "National Ecodistricts"),
                                  shpStudyRegion = shpStudyRegion,
                                  shpSubStudyRegion = shpSubStudyRegion)

  if ("Proprietary" %in% authenticationType) {
    tmpProprietary <- Cache(createReportingPolygonsFn, intersectListShpsFn = intersectListShps,
                            c("Forest Management Areas", "Alberta FMUs", "Caribou Herds"),
                            shpStudyRegion = shpStudyRegion,
                            shpSubStudyRegion = shpSubStudyRegion)
    reportingPolygons$Proprietary <- reportingPolygons$Free
    reportingPolygons$Proprietary[names(tmpProprietary)] <- tmpProprietary
    rm(tmpProprietary)
  }
  reportingPolygons
}


reportingAndLeadingFn <- function(createReportingPolygonsAllFn, createReportingPolygonsFn,
                                  intersectListShpsFn, leadingByStageFn,
                                  shpStudyRegion, shpSubStudyRegion, authenticationType,
                                  tsfs, vtms, cl, ageClasses, ageClassCutOffs) {
  reportingPolygon <- createReportingPolygonsAllFn(shpStudyRegion, shpSubStudyRegion, authenticationType,
                                                 createReportingPolygonsFn = createReportingPolygonsFn)
  reportingPolysWOStudyArea <- lapply(reportingPolygon, function(rp) rp[-which(names(rp) == "LandWeb Study Area")])
  leadingOut <- Map(reportingPolys = reportingPolysWOStudyArea, tsf = tsfs, vtm = vtms,
                    MoreArgs = list(cl = cl, ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs),
                    function(reportingPolys, tsf, vtm, cl, ageClasses, ageClassCutOffs) {
                      polys = lapply(reportingPolys, function(p) p$crsSR)
                      polyNames = names(reportingPolys)
                      message("  Determine leading species by age class, by polygon (loading, ", length(tsf),
                              " rasters, summarize by ", polyNames, ")")
                      Map(poly = polys, polyName = polyNames, function(poly, polyName) {
                        message("    Doing ", polyName)
                        Cache(leadingByStageFn, timeSinceFireFiles = asPath(tsf, 2),
                              vegTypeMapFiles = asPath(vtm, 2),
                              polygonToSummarizeBy = poly$shpSubStudyRegion,
                              cl = TRUE, omitArgs = "cl", ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
                        })
                    })
  return(list(leading = leadingOut, reportingPolygons = reportingPolygon))
}

createCCfromVtmTsf <- function(CCspeciesNames, vtmRasters, dPath, loadCCSpeciesFn,
                            shpSubStudyRegion, tsfRasters) {
  if (!is.null(CCspeciesNames)) {
    ageName <- CCspeciesNames[agrep("age", CCspeciesNames)]
    onlySpeciesNames <- CCspeciesNames[CCspeciesNames %in% c("Pine", "BlackSpruce", "Deciduous", "WhiteSpruce")]
    simulatedMapVegTypes <- lapply(vtmRasters, function(r) {
      as.character(levels(r$crsSR[[1]])[[1]][,2])
    })
    #lapply(simulatedMapVegTypes, function(vtm) {
    matchSpNames <- lapply(CCspeciesNames, function(sn) {
      agrep(sn, simulatedMapVegTypes$Proprietary)
    }
    )
    CCspeciesNames <- Map(msn = seq(matchSpNames),
                          MoreArgs = list(matchSpNames = matchSpNames, CCspeciesNames = CCspeciesNames,
                                          simulatedSpNames = simulatedMapVegTypes$Proprietary),
                          function(msn, matchSpNames, CCspeciesNames, simulatedSpNames) {
                            if (length(matchSpNames[[msn]]) > 0)
                              names(CCspeciesNames)[msn] <- simulatedSpNames[matchSpNames[[msn]]]
                            CCspeciesNames[msn]
                          })
    CCspeciesNames <- unlist(CCspeciesNames, use.names = TRUE)
    CCspeciesNames <- CCspeciesNames[nzchar(names(CCspeciesNames))]

    rstCurrentConditionList <- Cache(loadCCSpecies, CCspeciesNames,
                                     url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                                     destinationPath = dPath,
                                     studyArea = shpSubStudyRegion,
                                     rasterToMatch = tsfRasters[[1]]$crsSR[[1]]
    )
    stkCurrentCondition <- stack(rstCurrentConditionList[CCspeciesNames])
    sumVegPct <- sum(stkCurrentCondition)
    stkCurrentCondition$Mixed <- all(stkCurrentCondition/sumVegPct < vegLeadingPercent)*10
    CCvtm <- which.max(stkCurrentCondition)
    CCspeciesNames <- c(CCspeciesNames, "Mixed" = "Mixed")
    levels(CCvtm) <- data.frame(ID = seq(CCspeciesNames), Factor = names(CCspeciesNames))
    CCvtm <- writeRaster(CCvtm, filename = file.path(dPath, "currentConditionVTM"), overwrite = TRUE)

    # tsf
    CCtsf <- Cache(loadCCSpecies, ageName,
                   url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                   destinationPath = dPath,
                   studyArea = shpSubStudyRegion,
                   writeCropped = "CurrentCondition.tif",
                   rasterToMatch = tsfRasters$Proprietary$crsSR[[1]]
    )

    list(CCvtm = CCvtm, CCtsf = CCtsf$Age)
  }
}

convertPath <- function(paths, old, new) {
  hasOldPathStyle <- grepl(pattern = old, paths)
  if (any(hasOldPathStyle)) {
    paths <- gsub(pattern = old, replacement = new, x = paths)
  }
  paths
  
}