
#' Check and intersect a list of shapefiles with one single shapefile.
#' Caches and internal \code{spTransform} to
#'
#' @param listShps List of shapefiles
#' @param intersectShp The single shapefile to oversect with each of \codeP{listShps}
#'
intersectListShps <- function(listShps, intersectShp) {
  message("Intersecting reporting polygons with shpStudyRegion")

  intersectShp <- raster::aggregate(intersectShp)
  problem1 <- !rgeos::gIsSimple(intersectShp)
  problem2 <- !rgeos::gIsValid(intersectShp)
  if (isTRUE(problem1 || problem2)) {
    stop("intersectShp inside the function 'intersectListShps' is either not valid or not simple")
  }

  outerOut <- mapply(shp = listShps, shpNames = names(listShps),
                     function(shp, shpNames, useSF = FALSE) {
                       message("  ", shpNames)
                       if (!identical(crs(intersectShp), crs(shp)))
                         intersectShp <- Cache(spTransform, intersectShp, crs(shp) )
                       out <- raster::intersect(shp, intersectShp)
                       #})
                     })
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
                  message("        ", PolygonName)
                  aa <- tryCatch(Cache(raster::extract, dummyRaster, y = Polygon, cellnumbers = TRUE), error = function(x) NULL)
                  if (!is.null(aa)) {
                    notNull <- !unlist(lapply(aa, is.null))
                    dt <- rbindlist(lapply(seq_along(aa)[notNull], function(x) {
                      data.table(cell = aa[[x]][, "cell"], polygonID = as.character(x))
                    }))
                    data.table::copy(dt)
                  } else {
                    data.table(cell = numeric(), polygonID = character())
                  }
  })

  # There is a weird bug that makes the data.table from previous line. copy() is a work around
  # Error in data.table::set(cellIDByPolygon, , "newRas", clumpedRas[][cellIDByPolygon$cell]) :
  #   Internal logical error. DT passed to assign has not been allocated enough column slots. l=2, tl=2, adding 1
  return(dtList)
}

reprojectRasts <- function(tsf, lfltFN, crs, flammableFile) {
  rastsLFLT <- if (!(isTRUE(all(unlist(lapply(lfltFN, file.exists)))))) {

    message("Reprojecting rasters, filling in minimum age, saving to disk")
    rstFlammableNum <- raster(flammableFile)
    rstFlammableNum <- Cache(projectRaster, rstFlammableNum, crs = crs, method = "ngb")
    rastsLFLT <- lapply(seq_along(tsf), function(FN) {
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
  } else {
    rastsLFLT <- lapply(lfltFN, raster)
  }

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
loadCCSpecies <- function(mapNames, userTags = "", destinationPath, ...) {
  if (!any(grepl("1$", mapNames) )) {
    filenames <- paste0(mapNames, "1")
  } else {
    filenames <- mapNames
    mapNames <- gsub(mapNames, "1$", "")
  }
  names(filenames) <- mapNames

  Map(filename = filenames, mapName = mapNames, MoreArgs = list(userTags = userTags,
                                                                destinationPath = destinationPath),
      function(filename, mapName, userTags, destinationPath) {
        tifName <-  asPath(file.path(destinationPath, paste0(filename, ".tif")))

        filenames <- asPath(paste0(filename, ".", c("tfw", "tif.aux.xml", "tif.ovr", "tif.vat.cpg", "tif.vat.dbf")))
        prepInputs(userTags = c(userTags, "stable"),
              archive = "CurrentCondition.zip",
              targetFile = tifName,
              destinationPath = destinationPath,
              alsoExtract = filenames, ...)
      })
}


createReportingPolygons <- function(polygonNames, shpStudyRegion, shpSubStudyRegion,
                                    intersectListShpsFn, ...) {
  canonicalLayerNames <- c("Alberta Ecozones", "National Ecozones",
                   "National Ecodistricts", "Forest Management Areas",
                   "Alberta FMUs", "Caribou Ranges")
  if (!(all(polygonNames %in% canonicalLayerNames)) ) {
    stop("This function can only handle ", paste(canonicalLayerNames, collapse = ", "))
  }

  names(polygonNames) <- polygonNames

  polys <- as.list(polygonNames)
  # Alberta Ecozone
  layerNamesIndex <- 1
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
    albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf",
                                    "Natural_Regions_Subregions_of_Alberta.lyr", "Natural_Regions_Subregions_of_Alberta.prj",
                                    "Natural_Regions_Subregions_of_Alberta.shp.xml",
                                    "Natural_Regions_Subregions_of_Alberta.shx", "natural_regions_subregions_of_alberta.zip",
                                    "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
    albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
    albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
    polys[[canonicalLayerNames[layerNamesIndex]]] <- Cache(
      prepInputs, userTags = "stable",
      url = albertaEcozoneURL, targetFile = albertaEcozoneFilename,
      fun = "shapefile", destinationPath = dPath, alsoExtract = albertaEcozoneFiles
    )
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[canonicalLayerNames[layerNamesIndex]]]$NSRNAME
  }

  # National Ecozone
  layerNamesIndex <- 2
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- file.path(paths$inputPath, "ecozones", "National")
    ecozoneFilename <-   file.path(dPath, "ecozones.shp")
    ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
                      "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
    polys[[canonicalLayerNames[layerNamesIndex]]]  <- Cache(
      prepInputs, userTags = "stable",
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      targetFile = asPath(ecozoneFilename),
      alsoExtract = ecozoneFiles,
      fun = "shapefile", destinationPath = dPath
    )
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[canonicalLayerNames[layerNamesIndex]]]$ZONE_NAME
  }

  # National Ecodistrict
  layerNamesIndex <- 3
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- file.path(paths$inputPath, "ecodistricts", "National")
    ecodistrictFilename <-   file.path(dPath, "ecodistricts.shp")
    ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj",
                          "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
    polys[[canonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                                  targetFile = asPath(ecodistrictFilename),
                                                  alsoExtract = ecodistrictFiles,
                                                  fun = "shapefile", destinationPath = dPath)
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[canonicalLayerNames[layerNamesIndex]]]$ECODISTRIC
  }

  ## Polygons for report/shiny app
  ## All FMAs -
  layerNamesIndex <- 4
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- file.path(paths$inputPath, "allFMAs")
    allFMAsFilename <- asPath(file.path(dPath, "FMA_Boudary.shp"))
    allFMAsFiles <- c("FMA_Boudary.CPG", "FMA_Boudary.dbf", "FMA_Boudary.prj",
                      "FMA_Boudary.sbn", "FMA_Boudary.sbx", "FMA_Boudary.shp", "FMA_Boudary.shp.xml",
                      "FMA_Boudary.shx")
    polys[[canonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/open?id=1oCMiHRRT1bCWe0Uv69nRSrE1nsh-4Tic",
                                                  #targetFile = albertaFMUFilename,
                                                  #alsoExtract = albertaFMUFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <-
      polys[[canonicalLayerNames[layerNamesIndex]]]$Name
  }

  ## Alberta FMU -
  layerNamesIndex <- 5
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- file.path(paths$inputPath, "FMU_Alberta_2015-11")
    albertaFMUFilename <- asPath(file.path(dPath, "FMU_Alberta_2015-11.shp"))
    albertaFMUFiles <- c("FMU_Alberta_2015-11.cpg", "FMU_Alberta_2015-11.dbf",
                         "FMU_Alberta_2015-11.prj", "FMU_Alberta_2015-11.sbn",
                         "FMU_Alberta_2015-11.sbx", "FMU_Alberta_2015-11.shp",
                         "FMU_Alberta_2015-11.shp.xml", "FMU_Alberta_2015-11.shx")
    polys[[canonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
                                                  targetFile = albertaFMUFilename,
                                                  alsoExtract = albertaFMUFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <- polys[[canonicalLayerNames[layerNamesIndex]]]$FMU_NAME
  }

  # Caribou Zones
  layerNamesIndex <- 6
  if (canonicalLayerNames[layerNamesIndex] %in% polygonNames) {
    dPath <- file.path(paths$inputPath, "Caribou")
    caribouFilename <-   file.path(dPath, "LP_MASTERFILE_June62012.shp")
    caribouFiles <- c("LP_MASTERFILE_June62012.dbf", "LP_MASTERFILE_June62012.prj",
                      "LP_MASTERFILE_June62012.sbn", "LP_MASTERFILE_June62012.sbx",
                      "LP_MASTERFILE_June62012.shp", "LP_MASTERFILE_June62012.shp.xml",
                      "LP_MASTERFILE_June62012.shx")
    polys[[canonicalLayerNames[layerNamesIndex]]] <- Cache(prepInputs, userTags = "stable",
                                                  url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
                                                  targetFile = asPath(caribouFilename),
                                                  alsoExtract = caribouFiles,
                                                  fun = "shapefile",
                                                  destinationPath = dPath)
    polys[[canonicalLayerNames[layerNamesIndex]]]@data[[labelColumn]] <-
      polys[[canonicalLayerNames[layerNamesIndex]]]$LABEL2
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
  shpStudyRegion$shinyLabel <- as.character(seq(NROW(shpStudyRegion)))
  polys$`LandWeb Study Area` <- shpStudyRegion
  shpSubStudyRegion$shinyLabel <- as.character(seq(NROW(shpSubStudyRegion)))
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
  message("Making leaflet crs versions of reportingPolygons")
  polysLflt <- Cache(mapply, p = polys, nam = names(polys), userTags = "stable",
                     function(p, nam) {
                       message("  ", nam)
                       out <- tryCatch(spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT)), error = function(x) {
                         p <- spChFIDs(p, as.character(seq(NROW(p))))
                         spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT))
                       }, error = function(x) NULL)
                     })
  polysLfltSubStudyRegion <- Cache(mapply, p = polysSubRegion, nam = names(polysSubRegion), userTags = "stable",
                                   function(p, nam) {
                                     message("  ", nam)
                                     out <- tryCatch(
                                       spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT)), error = function(x) {
                                       p <- spChFIDs(p, as.character(seq(NROW(p))))
                                       spTransform(p, CRSobj = CRS(SpaDES.shiny:::proj4stringLFLT))
                                     }, error = function(x) NULL)
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


#' @param freeReportingPolygonNames Character vector which will be the names given to the polygons that are in the Free
#' @param proprietaryReportingPolygonNames Character vector which will be the names given to the polygons that are in the Proprietary
#' @param authenticationType Character vector, currently expected to be Free, Proprietary or All
#' @param ... Passed to \code{createReportingPolygonsFn}, so \code{polygonNames}, \code{shpStudyRegion},
#'            \code{shpSubStudyRegion}, \code{intersectListShpsFn}
createReportingPolygonsAll <- function(authenticationType,
                                       freeReportingPolygonNames,
                                       proprietaryReportingPolygonNames,
                                       createReportingPolygonsFn,
                                       ...
                                       ) {
  message("Loading Reporting Polygons")
  reportingPolygons <- list()
  reportingPolygons$Free <- createReportingPolygonsFn(polygonNames = freeReportingPolygonNames, ...)

  if ("Proprietary" %in% authenticationType) {
    tmpProprietary <- createReportingPolygonsFn(polygonNames = proprietaryReportingPolygonNames, ...)
    reportingPolygons$Proprietary <- reportingPolygons$Free
    reportingPolygons$Proprietary[names(tmpProprietary)] <- tmpProprietary
    rm(tmpProprietary)
  }
  reportingPolygons
}


#' Run a collection of functions
#' @param createReportingPolygonsAllFn The function called createReportingPolygonsAll
#' @param calculateLeadingVegTypeFn The function called calculateLeadingVegType
#' @param ... Passed to \code{createReportingPolygonsAll} (e.g., ) and \code{calculateLeadingVegType}
reportingAndLeading <- function(createReportingPolygonsAllFn, calculateLeadingVegTypeFn, ...) {
  reportingPolygon <- createReportingPolygonsAllFn(...)

  # remove study area
  reportingPolysWOStudyArea <- lapply(reportingPolygon, function(rp) rp[-which(names(rp) == "LandWeb Study Area")])
  leadingOut <- calculateLeadingVegTypeFn(reportingPolys = reportingPolysWOStudyArea, ...)
  return(list(leading = leadingOut, reportingPolygons = reportingPolygon))
}

calculateLeadingVegType <- function(reportingPolys, leadingByStageFn, tsfs, vtms, ...) {
  Map(reportingPoly = reportingPolys, tsf = tsfs, vtm = vtms,
      MoreArgs = list(...),
      f = function(reportingPoly, tsf, vtm, ...){
        polys = lapply(reportingPoly, function(p) p$crsSR)
        polyNames = names(reportingPoly)
        message("  ",paste(polyNames, collapse = ", ")," -- Determine leading species by age class, for each")
        Map(poly = polys, polyName = polyNames, MoreArgs = append(list(tsf = tsf, vtm = vtm), list(...)),
            function(poly, polyName, tsf, vtm, ...) {
          message("    ", polyName)
          if (!is.null(poly$shpSubStudyRegion)) {
            a <- Cache(leadingByStageFn, tsf = tsf,
                  vtm = vtm,
                  polygonToSummarizeBy = poly$shpSubStudyRegion,
                  omitArgs = "cl",
                  ...
                  )
          } else {
            NULL
          }
        })
      })
}

#' Calculate proportion of landscape occupied by each vegetation class
#'
#' @return A data.table with proportion of the pixels in each vegetation class, for
#'         each given age class within each polygon
leadingByStage <- function(tsf, vtm, polygonToSummarizeBy,
                           ageClassCutOffs,  ageClasses, cl = NULL, lapplyFn = "lapply", ...) {
  if (!is.null(tsf)) {
    out <- lapply(ageClassCutOffs, function(ages) {
      y <- match(ages, ageClassCutOffs)
      if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
        startList <- list(cl = cl)
      } else {
        startList <- list()
      }
      startList <- append(startList, list(y = y))

      message("      ", ageClasses[y], " for\n        ", paste0(basename(tsf), collapse = "\n        "))
      out1 <- #Cache(cacheRepo = paths$cachePath,
        do.call(lapplyFn, append(startList, list(X = tsf, function(x, ...) {
          x <- match(x, tsf)
          timeSinceFireFilesRast <- raster(tsf[x])
          leadingRast <- raster(vtm[x])
          leadingRast[timeSinceFireFilesRast[] < ageClassCutOffs[y]] <- 0
          if ((y + 1) < length(ageClassCutOffs))
            leadingRast[timeSinceFireFilesRast[] >= ageClassCutOffs[y + 1]] <- 0
          leadingRast
        })))
      names(out1) <- gsub(paste(basename(dirname(tsf)),
                                basename(tsf), sep = "_"),
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
    aa <- tryCatch(
      raster::extract(allStack, spTransform(polygonToSummarizeBy, CRSobj = crs(allStack))),
      error = function(x) NULL)

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
    if (NROW(aadf) > 0) { # if polygon doesn't overlap, the tryCatch on raster::extract returns NULL
      for (ages in ageClasses) {
        temp[[ages]] <- aadf %>%
          dplyr::select(starts_with(ages) , zone:vegCover) %>%
          tidyr::gather(key = "label", value = "proportion", -(zone:vegCover)) %>%
          mutate(ageClass = unlist(lapply(strsplit(label, split = "\\."), function(x) x[[1]])))
      }
    }

    aa <- rbindlist(temp)
    aa
  }
}



createCCfromVtmTsf <- function(CCspeciesNames, vtmRasters, dPath, loadCCSpeciesFn,
                            shpSubStudyRegion, tsfRasters, ...) {
  if (!is.null(CCspeciesNames)) {
    ageName <- CCspeciesNames[agrep("age", CCspeciesNames)]
    simulatedMapVegTypes <- lapply(vtmRasters, function(r) {
      as.character(levels(r$crsSR[[1]])[[1]][,2])
    })

    matchSpNames <- lapply(CCspeciesNames, function(sn) {
      agrep(sn, getAllIfExists(simulatedMapVegTypes, ifNot = "Proprietary"))
    }
    )

    CCspeciesNames <- Map(msn = seq(matchSpNames),
                          MoreArgs = list(matchSpNames = matchSpNames, CCspeciesNames = CCspeciesNames,
                                          simulatedSpNames = getAllIfExists(simulatedMapVegTypes, ifNot = "Proprietary")),
                          function(msn, matchSpNames, CCspeciesNames, simulatedSpNames) {
                            if (length(matchSpNames[[msn]]) > 0)
                              names(CCspeciesNames)[msn] <- simulatedSpNames[matchSpNames[[msn]]]
                            CCspeciesNames[msn]
                          })
    CCspeciesNames <- mapply(function(y) y, unlist(CCspeciesNames), USE.NAMES = TRUE)
    CCspeciesNames <- CCspeciesNames[nzchar(names(CCspeciesNames))]

    rstCurrentConditionList <- Cache(loadCCSpecies, CCspeciesNames, #notOlderThan = Sys.time(),
                                     url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                                     destinationPath = dPath,
                                     studyArea = shpSubStudyRegion, ...,
                                     rasterToMatch = tsfRasters[[1]]$crsSR[[1]]
    )
    stkCurrentCondition <- stack(rstCurrentConditionList[CCspeciesNames])
    sumVegPct <- sum(stkCurrentCondition)
    stkCurrentCondition$Mixed <- all(stkCurrentCondition/sumVegPct < vegLeadingPercent)*10
    CCvtm <- raster::which.max(stkCurrentCondition)
    CCspeciesNames <- c(CCspeciesNames, "Mixed" = "Mixed")
    levels(CCvtm) <- data.frame(ID = seq(CCspeciesNames), Factor = names(CCspeciesNames))
    CCvtm <- writeRaster(CCvtm, filename = file.path(dPath, "currentConditionVTM"), overwrite = TRUE)

    # tsf
    CCtsf <- Cache(loadCCSpecies, ageName, #notOlderThan = Sys.time(),
                    url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                    destinationPath = dPath,
                    studyArea = shpSubStudyRegion, omitArgs = "purge",
                    writeCropped = "CurrentCondition.tif", ...,
                    rasterToMatch = tsfRasters$Proprietary$crsSR[[1]]
      )

    list(CCvtm = CCvtm, CCtsf = CCtsf$Age)
  }
}


convertPaths <- function(paths, ...) {
  dots <- list(...)
  for (i in seq_along(dots$pattern)) {
    paths <- gsub(x = paths, pattern = dots$pattern[i], replacement = dots$pattern[i])
  }
  paths
}

convertRasterFileBackendPath <- function(rasterObj, ...) {
  if (is.list(rasterObj)) {
    rasterObj <- lapply(rasterObj, convertRasterFileBackendPath, ...)
  } else if (!is.null(rasterObj)) {
    fps <- rasterObj@file@name
    fps <- convertPaths(fps, ...)
    rasterObj@file@name <- fps
  }
  rasterObj # handles null case

}

setupParallelCluster <- function(cl, numClusters) {
  lapplyFn <- "lapply"
  if (!missing(cl)) { # not missing
    if (!identical(cl, "FALSE")) { # is NOT FALSE
      lapplyFn <- "parLapplyLB"
      if (isTRUE(cl)) { # Is TRUE
        if (detectCores() > 12) {
          ncores <- min(numClusters, detectCores() / 4)
          message("   making ", ncores, " node cluster")
          if (Sys.info()[["sysname"]] == "Windows") {
            cl <- makeCluster(ncores)
          } else {
            cl <- makeForkCluster(ncores)
          }

        } else { # not enough clusters
          lapplyFn <- "lapply"
          cl <- FALSE
        }
      }
    }
  }
  if (is(cl, "cluster")) {
    ## By here, it must be a cluster
    #clusterExport(cl = cl, varlist = list("tsf", "vtm", "polygonToSummarizeBy"),
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list(ls()), envir = environment())
      clusterEvalQ(cl = cl, {
        library(raster)
      })
    }

  }
  return(list(cl = cl, lapplyFn = lapplyFn))
}

getAllIfExists <- function(List, ifNot, returnList = FALSE) {
  if ("All" %in% names(List)) {
    if (returnList) {
      List["All"]
    } else {
      List[["All"]]
    }

  } else {
    if (missing(ifNot)) {
      if (length(List) > 1) {
        List
      } else {
        if (returnList) {
          List[1]
        } else {
          List[[1]]
        }

      }
    } else{
      if (returnList) {
        List[ifNot]
      } else {
        List[[ifNot]]
      }
    }
  }
}


MapWithVariableInputs <- function(f, ..., possibleList, MoreArgs) {
  f <- match.call()$f
  #mapLists <- list(reportingPolygons = reportingPolygons, authenticationType = authenticationType)
  dots <- list(...)
  if (length(unique(unlist(lapply(dots, function(x) length(x))))) == 1) {
    if (length(unique(unlist(lapply(possibleList, function(x) length(x))))) == 1) {
      if (length(dots[[1]]) == length(possibleList[[1]])) {
        putInDots <- TRUE
      } else {
        putInDots <- FALSE
      }
    } else {
      putInDots <- FALSE
    }
  } else {
    stop("Map requires that all elements of ... be the same length")
  }

  if (putInDots) {
    dots <- append(dots, possibleList)
  } else {
    MoreArgs <- append(MoreArgs, possibleList)
  }
  do.call(Map, append(list(f = f, MoreArgs = MoreArgs), dots))
}

