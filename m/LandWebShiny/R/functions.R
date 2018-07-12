#' Check and intersect a list of shapefiles with one single shapefile.
#' Caches and internal \code{spTransform} to
#'
#' @param listShps List of shapefiles
#' @param intersectShp The single shapefile to oversect with each of \codeP{listShps}
#'
intersectListShps <- function(listShps, intersectShp) {
  message("Intersecting reporting polygons with intersectShp")

  outerOut <- lapply(listShps, function(shp) {
    message("  ", names(shp))
    out <- reproducible::maskInputs(shp, intersectShp)
  })
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
                                                                destinationPath = destinationPath, ...),
      function(filename, mapName, userTags, destinationPath, ...) {
        tifName <-  asPath(file.path(destinationPath, paste0(filename, ".tif")))

        filenames <- asPath(paste0(filename, ".", c("tfw", "tif.aux.xml", "tif.ovr", "tif.vat.cpg", "tif.vat.dbf")))
        prepInputs(userTags = c(userTags, "stable"),
                   archive = "CurrentCondition.zip",
                   targetFile = tifName,
                   destinationPath = destinationPath,
                   alsoExtract = filenames, ...) # dots include things like method = "ngb" for projectRaster
      })
}


createReportingPolygons <- function(polygonNames, shpLandWebSA, #shpStudyRegion,
                                    shpStudyArea,
                                    prepInputsFromSilvacomFn,
                                    namedUrlsLabelColumnNames = namedUrlsLabelColumnNames,
                                    destinationPath, labelColumn, ...) {
  names(polygonNames) <- polygonNames

  polys <- list()

  layerNamesIndex <- "AB Natural Sub Regions"
  if (layerNamesIndex %in% polygonNames) {
    #dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
    albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf",
                                    "Natural_Regions_Subregions_of_Alberta.lyr",
                                    "Natural_Regions_Subregions_of_Alberta.prj",
                                    "Natural_Regions_Subregions_of_Alberta.shp.xml",
                                    "Natural_Regions_Subregions_of_Alberta.shx",
                                    "natural_regions_subregions_of_alberta.zip",
                                    "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
    albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
    albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
    polys[[layerNamesIndex]] <- Cache(
      prepInputs, userTags = "stable",
      url = albertaEcozoneURL, targetFile = albertaEcozoneFilename, studyArea = shpStudyArea,
      fun = "shapefile", alsoExtract = albertaEcozoneFiles, destinationPath = destinationPath
    )
    polys[[layerNamesIndex]]@data[[labelColumn]] <- polys[[layerNamesIndex]]$NSRNAME
  }

  # National Ecozone
  layerNamesIndex <- "National Ecozones"
  if (layerNamesIndex %in% polygonNames) {
    #dPath <- file.path(paths$inputPath, "ecozones", "National")
    ecozoneFilename <-   file.path(destinationPath, "ecozones.shp")
    ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
                      "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
    polys[[layerNamesIndex]]  <- Cache(
      prepInputs, userTags = "stable",
      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      targetFile = asPath(ecozoneFilename),
      alsoExtract = ecozoneFiles, studyArea = shpStudyArea,
      fun = "shapefile", destinationPath = destinationPath
    )
    polys[[layerNamesIndex]]@data[[labelColumn]] <- polys[[layerNamesIndex]]$ZONE_NAME
  }

  # National Ecodistrict
  layerNamesIndex <- "National Ecodistricts"
  if (layerNamesIndex %in% polygonNames) {
    #dPath <- file.path(paths$inputPath, "ecodistricts", "National")
    ecodistrictFilename <-   file.path(destinationPath, "ecodistricts.shp")
    ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj",
                          "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
    polys[[layerNamesIndex]] <- Cache(prepInputs, userTags = "stable",
                                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                      targetFile = asPath(ecodistrictFilename),
                                      alsoExtract = ecodistrictFiles, studyArea = shpStudyArea,
                                      fun = "shapefile", destinationPath = destinationPath)
    polys[[layerNamesIndex]]@data[[labelColumn]] <- polys[[layerNamesIndex]]$ECODISTRIC
  }

  polys$provinces <- Cache(getData, 'GADM', country = 'CAN', level = 1)
  polys$provinces[[labelColumn]] <- polys$provinces$NAME_1

  # Get all SilvaCom-generated datasets - they have a common structure
  polys2 <- prepInputsFromSilvacomFn(polygonNames = polygonNames, studyArea = shpStudyArea,
                                     shinyLabel = labelColumn, destinationPath = destinationPath,
                                     namedUrlsLabelColumnNames = namedUrlsLabelColumnNames)

  polys[names(polys2)] <- polys2
  if ("National Ecozones" %in% names(polys)) {
    # remove the french name column because it has accents that aren't correctly dealt with
    polys$`National Ecozones`@data <-
      polys$`National Ecozones`@data[-which(colnames(polys$`National Ecozones`@data) == "ZONE_NOM")]
  }

  polys3 <- studyAreaPolygonsFn(shpLandWebSA, shpStudyArea, labelColumn)
  polys[names(polys3)] <- polys3

  ########################################################
  ########################################################
  ########################################################
  # Make them all crsStudyRegion
  polys <- Cache(lapply, polys, function(shp) {
    spTransform(shp, CRSobj = crsStudyRegion)
  }, userTags = "stable")


  # Make Leaflet versions of all
  message("Making leaflet crs versions of reportingPolygons")
  polysLflt <- Cache(Map, p = polys, nam = names(polys), userTags = "stable",
                     function(p, nam) {
                       message("  ", nam)
                       out <- tryCatch(Cache(spTransform, p, CRSobj = CRS(SpaDES.shiny::proj4stringLFLT)), error = function(x) {
                         p <- spChFIDs(p, as.character(seq(NROW(p))))
                         spTransform(p, CRSobj = CRS(SpaDES.shiny::proj4stringLFLT))
                       }, error = function(x) NULL)
                     })
  # polysLfltSubStudyRegion <- Cache(mapply, p = polysSubRegion, nam = names(polysSubRegion), userTags = "stable",
  #                                  function(p, nam) {
  #                                    message("  ", nam)
  #                                    out <- tryCatch(
  #                                      spTransform(p, CRSobj = CRS(SpaDES.shiny::proj4stringLFLT)), error = function(x) {
  #                                      p <- spChFIDs(p, as.character(seq(NROW(p))))
  #                                      spTransform(p, CRSobj = CRS(SpaDES.shiny::proj4stringLFLT))
  #                                    }, error = function(x) NULL)
  #                                  })

  # Put them all together in the structure:
  #   LayerName $ Projection (crsSR or crsLFLT)
  polysAll <- list("crsSR" = polys, "crsLFLT" = polysLflt)
  purrr::transpose(polysAll)
}


#' @param freeReportingPolygonNames Character vector which will be the names given to the polygons that are in the Free
#' @param proprietaryReportingPolygonNames Character vector which will be the names given to the polygons that are in the Proprietary
#' @param authenticationType Character vector, currently expected to be Free, Proprietary or All
#' @param ... Passed to \code{createReportingPolygonsFn}, so \code{polygonNames}
createReportingPolygonsAll <- function(authenticationType,
                                       freeReportingPolygonNames,
                                       proprietaryReportingPolygonNames,
                                       createReportingPolygonsFn,
                                       ...) {
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
#' @param ... Passed to \code{createReportingPolygonsAll} (e.g., )
reportingAndLeading <- function(createReportingPolygonsAllFn, leadingByStageFn, ...) {
  reportingPolygon <- createReportingPolygonsAllFn(...)

  # remove study area
  reportingPolysWOStudyArea <-
    lapply(reportingPolygon, function(rp) {
      rp <- rp[-which(names(rp) == "LandWeb Study Area")]
    })
  polysSR <- lapply(reportingPolysWOStudyArea, function(rp) {
    lapply(rp, function(rpInner) {
      rpInner$crsSR
    })
  })
  leadingOut <- leadingByStageFn(polygonToSummarizeBy = polysSR,
                                 #omitArgs = c("cl", "showSimilar",
                                 #              formalsNotInCurrentDots(leadingByStageFn, ...)),
                                 ...
  )
  #leadingOut1 <- calculateLeadingVegTypeFn(reportingPolys = reportingPolysWOStudyArea, ...)
  return(list(leading = leadingOut, reportingPolygons = reportingPolygon))
}

#' Calculate proportion of landscape occupied by each vegetation class
#'
#' This function is recursive. If \code{polygonToSummarizeBy} is a SpatialPolygon,
#' then the function will enter once, and convert this to a fasterized version, and
#' pass that in to the function replacing \code{polygonToSummarizeBy}. It is
#' also recursive of passed a vector of filenames for \code{tsf} and \code{vtm}.
#' @return A data.table with proportion of the pixels in each vegetation class, for
#'         each given age class within each polygon
leadingByStage <- function(tsf, vtm, polygonToSummarizeBy,
                           ageClassCutOffs,  ageClasses, objName = NULL, ...) {
  if (!is.null(tsf)) {
    if (is.list(polygonToSummarizeBy)) {
      if (!is.null(objName)) {
        message("    ", objName, ":  Calculating leading species by stage")
      }
      out <- Map(
        polygonToSummarizeBy = polygonToSummarizeBy,
        objName = names(polygonToSummarizeBy),
        leadingByStage,
        MoreArgs = list(
          tsf = tsf,
          vtm = vtm,
          ageClassCutOffs = ageClassCutOffs,
          ageClasses = ageClasses,
          ...
        )
      )
      return(out)
    } else {
      if (!is(polygonToSummarizeBy, "Raster")) {
        # if a polygon, fasterize it then rerun
        if (is.list(tsf)) {
          singleTsf <- unlist(tsf)[1]
        } else {
          singleTsf <- tsf
        }
        if (!is.null(singleTsf)) {
          if (!is.null(objName)) message("      ", objName, ":  Fasterizing")
          aa2 <- Cache(fasterize2, raster(raster(singleTsf)), polygonToSummarizeBy,
                       field = "polygonNum")
          out <- leadingByStage(
            tsf = tsf,
            vtm = vtm,
            polygonToSummarizeBy = aa2,
            ageClassCutOffs = ageClassCutOffs,
            ageClasses = ageClasses,
            ...
          )
          return(out)
        } else {
          return(NULL)
        }
      } else {
        if (is.list(tsf)) {
          # if tsf is a list
          out <- Map(
            tsf = tsf,
            vtm = vtm,
            leadingByStage,
            MoreArgs = list(
              polygonToSummarizeBy = polygonToSummarizeBy,
              ageClassCutOffs = ageClassCutOffs,
              ageClasses = ageClasses,
              ...
            )
          )
          if ((is.list(out) && length(out) == 1)) {
            out <- out[[1]]
          }
          return(out)
        } else {
          if (length(tsf) > 1) {
            # recursive call to this same function, but one tsf and vtm at a time
            numClust <- optimalClusterNum(16000, maxNumClusters = length(tsf))
            out <-
              if (numClust > 1) {
                cl <- makeForkCluster(numClust, type = "FORK")
                on.exit(stopCluster(cl))
                clusterMap(cl = cl,
                           leadingByStage,
                           tsf = tsf,
                           vtm = vtm,
                           objName = basename(tsf),
                           MoreArgs = list(
                             polygonToSummarizeBy = polygonToSummarizeBy,
                             ageClassCutOffs = ageClassCutOffs,
                             ageClasses = ageClasses,
                             ...
                           ),
                           .scheduling = "dynamic")
              } else {
                Map(
                  leadingByStage,
                  tsf = tsf,
                  vtm = vtm,
                  objName = basename(tsf),
                  MoreArgs = list(
                    polygonToSummarizeBy = polygonToSummarizeBy,
                    ageClassCutOffs = ageClassCutOffs,
                    ageClasses = ageClasses,
                    ...
                  )
                )
              }

            names(out) <- basename(names(out))
            out <- rbindlist(out)
            amc::.gc()
            return(out)
          } else {
            startTime <- Sys.time()
            if (is.null(objName)) {
              objName = basename(tsf)
            }
            message("        ", objName, ":  Calculating leading species by stage")


            # main function code
            if (tail(ageClassCutOffs, 1) != Inf)
              ageClassCutOffs <- c(ageClassCutOffs, Inf)

            timeSinceFireFilesRast <- raster(tsf[1])
            timeSinceFireFilesRast[] <- timeSinceFireFilesRast[]

            # Use this when NOT in parallel
            #timeSinceFireFilesRast <- Cache(rasterToMemory, tsf[1])

            rasTsf <- reclassify(
              timeSinceFireFilesRast,
              cbind(
                from = ageClassCutOffs[-length(ageClassCutOffs)] -
                  0.1,
                to = ageClassCutOffs[-1],
                seq_along(ageClasses)
              )
            )

            levels(rasTsf) <-
              data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

            rasVeg <- raster(vtm[1])
            rasVeg[] <- rasVeg[] # 3 seconds

            splitVal <-
              paste0("_", 75757575, "_") # unlikely to occur for any other reason

            # Individual species
            nas3 <- is.na(rasVeg[]) | rasVeg[] == 0
            nas1 <- is.na(rasTsf[]) | rasTsf[] == 0
            nas <- nas3 | nas1
            name1 <-
              as.character(factorValues(rasTsf, rasTsf[][!nas])[, 1])
            #as.character(raster::levels(rasTsf)[[1]]$Factor)[rasTsf[][!nas]]
            name3 <-
              as.character(factorValues(rasVeg, rasVeg[][!nas])[, 1])
            #as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]
            ff <- paste(name1, name3, sep = splitVal) # 4 seconds


            ras <- raster(rasVeg)
            ffFactor <- factor(ff)
            ras[!nas] <- ffFactor # 2 seconds

            eTable <-
              data.frame(ID = seq_along(levels(ffFactor)),
                         VALUE = levels(ffFactor))
            types <-
              strsplit(as.character(eTable$VALUE), split = splitVal)
            types <- do.call(rbind, types)

            levels(ras) <-
              data.frame(eTable, ageClass = types[, 1], vegCover = types[, 2])

            levs <- raster::levels(polygonToSummarizeBy)[[1]]
            levs <-
              factorValues(polygonToSummarizeBy, levs$ID) # this is same, if all values present, e.g., 1, 2, 3, 4, 5 ... but not if missing, 1, 2, 3, 5
            facVals <-
              factorValues(
                polygonToSummarizeBy,
                polygonToSummarizeBy[],
                att = c("shinyLabel", "polygonNum")
              )

            bb <-
              data.table(
                zone = facVals$shinyLabel,
                polygonID = facVals$polygonNum,
                cell = seq_len(ncell(ras))
              )
            #bb <- na.omit(bb)

            # add age and vegCover by reference
            bb[, c("ageClass", "vegCover") := factorValues(ras, ras[][bb$cell], att = c("ageClass", "vegCover"))]
            bb <- na.omit(bb)

            #set(bb, , "zone", polygonToSummarizeBy$shinyLabel[as.numeric(bb$polygonNum)])
            tabulated <-
              bb[, list(N1 = .N), by = c("zone", "polygonID", "ageClass", "vegCover")]
            tabulated[, proportion := round(N1 / sum(N1), 4), by = c("zone", "ageClass")]

            allCombos <-
              expand.grid(
                ageClass = ageClasses,
                stringsAsFactors = FALSE,
                vegCover = raster::levels(rasVeg)[[1]]$Factor,
                zone = levs$shinyLabel# factorValues(polygonToSummarizeBy, 1:5)$shinyLabel
              )
            allCombos$polygonID <-
              match(allCombos$zone, levs$shinyLabel)
            #allCombos$proportion <- 0
            data.table::setDT(allCombos)

            #allCombos[tabulated, on = c("zone", "vegCover", "ageClass")]

            tabulated <-
              merge(
                tabulated,
                allCombos,
                by = c("zone", "vegCover", "ageClass", "polygonID"),
                all.y = TRUE
              )
            tabulated[is.na(proportion), proportion := 0]
            set(tabulated, , "N1", NULL)
            set(tabulated,
                ,
                "label",
                paste(
                  tabulated$ageClass,
                  paste(basename(dirname(tsf)), basename(tsf), sep = "_"),
                  sep = "."
                ))

            endTime <- Sys.time()
            message("    Leading cover calculation took ",
                    format(endTime - startTime, digits = 2))

            return(tabulated)
          }
        }
      }
    }
  }
}

createCCfromVtmTsf <- function(CCspeciesNames, vtmRasters, dPath, loadCCSpeciesFn,
                               shpStudyArea, tsfRasters, vegLeadingPercent, ...) {
  if (!is.null(CCspeciesNames)) {
    ageName <- CCspeciesNames[agrep("age", CCspeciesNames)]
    simulatedMapVegTypes <- lapply(vtmRasters, function(r) {
      as.character(raster::levels(r$crsSR[[1]])[[1]][,2])
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

    rstCurrentConditionList <- Cache(loadCCSpeciesFn, CCspeciesNames, #notOlderThan = Sys.time(),
                                     url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                                     destinationPath = dPath, #method = "ngb",
                                     studyArea = shpStudyArea, ...,
                                     rasterToMatch = getAllIfExists(tsfRasters, "Proprietary")$crsSR[[1]]
    )
    stkCurrentCondition <- stack(rstCurrentConditionList[CCspeciesNames])
    sumVegPct <- sum(stkCurrentCondition, na.rm = TRUE)
    stkCurrentCondition$Mixed <- all(stkCurrentCondition/sumVegPct < vegLeadingPercent)*10
    CCvtm <- raster::which.max(stkCurrentCondition)
    CCspeciesNames <- c(CCspeciesNames, "Mixed" = "Mixed")
    levels(CCvtm) <- data.frame(ID = seq(CCspeciesNames), Factor = names(CCspeciesNames))
    CCvtm <- writeRaster(CCvtm, filename = file.path(dPath, "currentConditionVTM"), overwrite = TRUE)

    # tsf
    CCtsf <- Cache(loadCCSpecies, ageName, #notOlderThan = Sys.time(),
                   url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                   destinationPath = dPath, method = "ngb",
                   studyArea = shpStudyArea, omitArgs = "purge",
                   postProcessedFilename = "CurrentCondition.tif", ...,
                   rasterToMatch = getAllIfExists(tsfRasters, "Proprietary")$crsSR[[1]]
    )

    list(CCvtm = CCvtm, CCtsf = CCtsf$Age)
  }
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
    } else {
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

prepInputsFromSilvacom <- function(namedUrlsLabelColumnNames, destinationPath, polygonNames,
                                   shinyLabel, studyArea) {
  out <- Map(url = namedUrlsLabelColumnNames, layerName = names(namedUrlsLabelColumnNames),
             MoreArgs = list(destinationPath = destinationPath,
                             polygonNames = polygonNames,
                             shinyLabel = shinyLabel),
             function(url, layerName, destinationPath, polygonNames, shinyLabel) {
               if (layerName %in% polygonNames) {
                 #dPath <- file.path(paths$inputPath, "Caribou")
                 target <- gsub(pattern = " ", replacement = "_", layerName)
                 targetZip <- paste0(target, ".zip")
                 targetFilename <-   file.path(destinationPath, paste0(target, ".shp"))
                 targetFilenames <- paste0(target, c(".dbf", ".prj", ".sbn", ".sbx",
                                                     ".shp", ".shp.xml", ".shx"))
                 message(layerName, ": Running prepInputs")
                 polygonOut <- Cache(prepInputs, userTags = "stable",
                                     archive = asPath(targetZip),
                                     url = url$url,
                                     targetFile = asPath(targetFilename),
                                     alsoExtract = asPath(targetFilenames),
                                     fun = "shapefile", destinationPath = destinationPath,
                                     studyArea = studyArea)

                 if (!is.null(polygonOut))
                   polygonOut@data[[shinyLabel]] <- polygonOut[[url$labelColumnName]]
                 polygonOut
               }
             }
  )
  out[!unlist(lapply(out, is.null))]
}

formalsNotInCurrentDots <- function(.f, ...) {
  names(list(...))[!(names(list(...)) %in% names(formals(.f)))]
}


studyAreaPolygonsFn <- function(shpLandWebSA = NULL, shpStudyArea = NULL, labelColumn) {
  wholeStudyAreaTxt <- "Whole Study Area"
  #shpLandWebSA$shinyLabel <- as.character(seq(NROW(shpLandWebSA)))
  polys <- list()
  if (!is.null(shpLandWebSA)) {
    polys[["LandWeb Study Area"]] <- raster::aggregate(shpLandWebSA, dissolve = TRUE)
    polys[["LandWeb Study Area"]] <- sp::SpatialPolygonsDataFrame(
      polys[["LandWeb Study Area"]], data = data.frame(NAME = wholeStudyAreaTxt)
    )
    polys[["LandWeb Study Area"]][[labelColumn]] <- wholeStudyAreaTxt
  }

  if (!is.null(shpStudyArea)) {
    polys[["shpStudyArea"]] <- raster::aggregate(shpStudyArea, dissolve = TRUE)
    polys[["shpStudyArea"]] <- sp::SpatialPolygonsDataFrame(
      polys[["shpStudyArea"]], data = data.frame(NAME = wholeStudyAreaTxt)
    )
    polys[["shpStudyArea"]][[labelColumn]] <- wholeStudyAreaTxt
  }
  polys
}



optimalClusterNum <- function(memRequiredMB = 5000, maxNumClusters = 1) {
  if (Sys.info()["sysname"] == "Linux") {
    detectedNumCores <- parallel::detectCores()
    shouldUseCluster <- (maxNumClusters > 0)

    if (shouldUseCluster) {
      # try to scale to available RAM
      try(aa <- system("free -lm", intern = TRUE))
      if (!is(aa, "try-error")) {
        bb <- strsplit(aa[2], split = " ");
        availMem <- as.numeric(bb[[1]][nzchar(bb[[1]])][7]) # available column in "free -lm" call
        numClusters <- floor(min(detectedNumCores, availMem / memRequiredMB))
      } else {
        message("The OS function, 'free' is not available. Returning 1 cluster")
        numClusters <- 1
      }
      numClusters <- min(maxNumClusters, numClusters, detectedNumCores)
    } else {
      numClusters <- 1
    }
  } else {
    message("This function returns 1 cluster on Windows and Mac")
    numClusters <- 1
  }
  return(numClusters)
}


#' Create a parallel Fork cluster, if useful
#'
#' Given the size of a problem, it may not be useful to create a cluster.
#' This will make a Fork cluster (so Linux only)
#' @param useParallel Logical. If \code{FALSE}, returns NULL
#' @param MBper Numeric. Passed to \code{memRequiredMB} in
#'              \code{\link{optimalClusterNum}}
#' @param maxNumClusters Numeric or Integer. The theoretical upper limit
#'        for number of clusters to create (e.g., because there are only
#'        3 problems to solve, not \code{parallel::detectCores})
#' @param ... Passed to \code{makeForkClusterRandom}.
#'            Only relevant for \code{iseed}.
makeOptimalCluster <- function(useParallel = FALSE, MBper = 5e3,
                               maxNumClusters = parallel::detectCores(),
                               ...) {
  if (useParallel && tolower(Sys.info()[["sysname"]]) != "windows") {
    numClus <- optimalClusterNum(MBper, maxNumClusters = maxNumClusters)
    if (numClus <= 1) {
      NULL
    } else {
      makeForkClusterRandom(numClus, ...)
    }
  } else {
    NULL
  }
}
#' Fasterize with crop & spTransform first
#'
#' @param emptyRaster An empty raster with res, crs, extent all
#'        correct for to pass to \code{fasterize}
#' @param polygonToFasterize passed to \code{fasterize}, but it
#'        will be cropped first if
#'        \code{extent(emptyRaster) < extent(polygonToFasterize)}
#' @param field passed to fasterize
fasterize2 <- function(emptyRaster, polygonToFasterize, field) {
  ras <- raster(emptyRaster)
  if (extent(polygonToFasterize) > extent(ras)) {
    polygonToFasterize <- Cache(crop, polygonToFasterize, ras)
  }
  thePoly <-
    spTransform(polygonToFasterize, CRSobj = crs(ras))
  thePoly$polygonNum <- seq_along(thePoly)
  if (!is.factor(thePoly[[field]])) {
    thePoly[[field]] <- factor(thePoly[[field]])
  }
  aa2 <-
    fasterize::fasterize(sf::st_as_sf(thePoly), ras, field = field)
  levels(aa2) <-
    data.frame(ID = seq_len(nlevels(thePoly[[field]])),
               Factor = levels(thePoly[[field]]))
  # levels(aa2) <-
  #   data.frame(ID = seq_along(thePoly), as.data.frame(thePoly))
  aa2
}


#' \code{makeForkCluster} with random seed set
#'
#' This will set different randon seeds on the clusters (not the default)
#' with \code{makeForkCluster}. It also defaults to creating a logfile with
#' message of where it is.
#'
#' @param ... passed to \code{makeForkCluster}, e.g.,
#' @param iseed passed to \code{clusterSetRNGStream}
makeForkClusterRandom <- function(..., iseed = NULL) {
  require(parallel)
  dots <- list(...)
  if (!("outfile" %in% names(dots))) {
    dots$outfile <- "~/.log.txt"
    file.remove(dots$outfile)
  }
  cl <- do.call(makeForkCluster, args = dots)
  message("  Starting a cluster with ", length(cl)," threads")
  message("    Log file is ", dots$outfile, ". To prevent log file, pass outfile = ''")
  clusterSetRNGStream(cl, iseed = iseed)
  cl
}

#' Map and parallel::clusterMap together
#'
#' This will send to Map or clusterMap, depending on whether cl is provided.
#'
#' @param ... passed to Map or clusterMap
#' @param cl passed to clusterMap
Map2 <- function(..., cl = NULL) {
  formsMap <- formalsNotInCurrentDots(mapply, ...)
  formsClusterMap <- formalsNotInCurrentDots(clusterMap, ...)
  if (is.null(cl)) {
    argList <- list(...)
    wrongFun1 <- "fun" %in% names(argList)
    if (wrongFun1) {
      fun <- argList$fun
    }
    wrongFun2 <- "FUN" %in% names(argList)
    if (wrongFun2) {
      fun <- argList$FUN
    }
    argList[setdiff(formsMap, formsClusterMap)] <- NULL
    if (wrongFun1 || wrongFun2) {
      argList$f <- fun
    }
    do.call(Map, args = argList)

  } else {
    argList <- list(...)
    wrongFun1 <- "f" %in% names(argList)
    if (wrongFun1) {
      fun <- argList$f
    }
    wrongFun2 <- "FUN" %in% names(argList)
    if (wrongFun2) {
      fun <- argList$FUN
    }
    argList[setdiff(formsClusterMap, formsMap)] <- NULL
    if (wrongFun1 || wrongFun2) {
      argList$fun <- fun
    }
    do.call(clusterMap, append(list(cl = cl), argList))
  }
}


simplifyColumns <- function(df) {
  if (is(df, "list")) {
    df <- lapply(df, simplifyColumns)
  } else {
    for (i in colnames(df)) {
      if (!is.integer(df[[i]])) {
        if (!is.factor(df[[i]])) {
          if (is.numeric(df[[i]])) {
            if (all(df[[i]] %% 1 == 0)) {
              df[[i]] <- as.integer(df[[i]])
            }
          } else if (is.character(df[[i]])) {
            df[[i]] <- factor(df[[i]])
          }
        }
      }
    }
  }
  df
}
