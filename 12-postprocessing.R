################################################################################
## Simulation post-processing (largePatches & leading)
################################################################################

analysesOutputsTimes <- seq(summaryPeriod[1], summaryPeriod[2], by = summaryInterval)

if (FALSE) { ## futures don't work properly in Rstudio
  library(future)
  options("future.availableCores.custom" = function() { min(getOption("Ncpus"), 4) })
  future::plan("multiprocess")
}

stopifnot(packageVersion("map") >= "0.0.2")
stopifnot(packageVersion("LandWebUtils") >= "0.0.2")

padL <- ifelse(grepl("prov", runName), 3, 4) ## TODO: confirm this is always true now

if (grepl("Manning", runName)) {
  timeSeriesTimes <- 450:500
}

#allouts <- unlist(lapply(mySimOuts, function(sim) outputs(sim)$file))
allouts <- dir(Paths$outputPath, full.names = TRUE, recursive = TRUE)
allouts <- grep("vegType|TimeSince", allouts, value = TRUE)
allouts <- grep("gri|png|txt|xml", allouts, value = TRUE, invert = TRUE)
allouts2 <- grep(paste(paste0("year", paddedFloatToChar(timeSeriesTimes, padL = padL)), collapse = "|"),
                 allouts, value = TRUE, invert = TRUE)
stopifnot(length(allouts2) == 120) ## i.e., 60 reps worth of tsf and vtm maps

layerName <- gsub(allouts2, pattern = paste0(".*", Paths$outputPath), replacement = "")
layerName <- gsub(layerName, pattern = "[/\\]", replacement = "_")
layerName <- gsub(layerName, pattern = "^_", replacement = "")
ag1 <- gsub(layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2") %>%
  grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)
destinationPath <- dirname(allouts2)
tsf <- gsub(".*vegTypeMap.*", NA, allouts2) %>%
  grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)
vtm <- gsub(".*TimeSinceFire.*", NA, allouts2) %>%
  grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)

if (FALSE) {
  ### manually identify any corrupted tsf/vtm files
  res1 <- lapply(tsf, function(x) {
    message("Loading: ", x)
    tmp <- tryCatch(raster(x), error = function(e) FALSE)
    extent(tmp)
  }) %>%
    unlist() %>%
    unique()

  res2 <- lapply(vtm, function(x) {
    message("Loading: ", x)
    tmp <- tryCatch(raster(x), error = function(e) FALSE)
    extent(tmp)
  }) %>%
    unlist() %>%
    unique()
}

ml <- simOutPreamble$ml

rm(simOutPreamble)

if (!is(ml@metadata[["leaflet"]], "Path"))
  ml@metadata[["leaflet"]] <- asPath(as.character(ml@metadata[["leaflet"]]))

if (!is(ml@metadata[["targetFile"]], "Path"))
  ml@metadata[["targetFile"]] <- asPath(as.character(ml@metadata[["targetFile"]]))

if (!is(ml@metadata[["tsf"]], "Path"))
  ml@metadata[["tsf"]] <- asPath(as.character(ml@metadata[["tsf"]]))

################################################################################
## create vtm and tsf stacks for animation
################################################################################

if (FALSE) {
  tsfTimeSeries <- gsub(".*vegTypeMap.*", NA, allouts) %>%
    grep(paste(timeSeriesTimes, collapse = "|"), ., value = TRUE)
  vtmTimeSeries <- gsub(".*TimeSinceFire.*", NA, allouts) %>%
    grep(paste(timeSeriesTimes, collapse = "|"), ., value = TRUE)

  if (length(tsfTimeSeries)) {
    tsfStack <- raster::stack(tsfTimeSeries)# %>% writeRaster(file.path(Paths$outputPath, "stack_tsf.tif"))
    gifName <- file.path(normPath(Paths$outputPath), "animation_tsf.gif")
    future({
      animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                         movie.name = gifName, expr = {
                           brks <- c(0, 1, 40, 80, 120, 1000)
                           cols <- RColorBrewer::brewer.pal(5, "RdYlGn")
                           for (i in seq(numLayers(tsfStack))) {
                             plot(mask(tsfStack[[i]], studyArea(ml, 2)), breaks = brks, col = cols)
                           }
      })
    })
    rm(tsfStack)
  }

  #if (length(vtmTimeSeries)) {
  #  vtmStack <- raster::stack(vtmTimeSeries)# %>% writeRaster(file.path(Paths$outputPath, "stack_vtm.tif"))
  #  gifName <- file.path(normPath(Paths$outputPath), "animation_vtm.gif")
  #  animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
  #                     movie.name = gifName, expr = {
  #                       for (i in seq(numLayers(vtmStack)))
  #                         plot(mask(vtmStack[[i]], studyArea(ml, 2))) # TODO: this animation isn't great!
  #  })
  #  rm(vtmStack)
  #}
}

################################################################################
## begin post-processing
################################################################################

paths4 <- list(
    cachePath = file.path("cache", "postprocessing"),
    modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
    inputPath = "inputs",
    outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths4)

tilePath <- asPath(file.path(Paths$outputPath, "tiles"))

if (isTRUE(grepl("Ubuntu 20.04", osVersion)))
  tiler::tiler_options(python = Sys.which("python3"))

vtmCC <- vegTypeMapGenerator(simOutSpeciesLayers$speciesLayers, vegLeadingProportion, mixedType = 2,
                             sppEquiv = sppEquivalencies_CA, sppEquivCol = "LandWeb", colors = sppColorVect,
                             doAssertion = FALSE)
fname <- file.path(Paths$outputPath, "CurrentConditionVTM.tif")
writeRaster(vtmCC, fname, overwrite = TRUE)

fname2 <- file.path(Paths$outputPath, "CurrentConditionTSF.tif")
writeRaster(ml[["CC TSF"]], fname2, overwrite = TRUE)

rm(simOutSpeciesLayers)

ml <- mapAdd(map = ml, layerName = "CC VTM", analysisGroup1 = "CC",
             targetFile = asPath(fname),
             destinationPath = asPath(Paths$outputPath),
             filename2 = NULL,
             tsf = asPath(fname2),
             vtm = asPath(fname),
             CC = TRUE,
             overwrite = TRUE,
             #useCache = "overwrite",
             leaflet = tilePath)

## TODO: WORKAROUND for some funny business with col names.
if (any(grepl("ANSR", names(ml)))) {
  id <- which(grepl("ANSR", names(ml)))
  if (is.null(ml[[names(ml)[id]]][["Name"]])) {
    ml[[names(ml)[id]]][["Name"]] <- ml[[names(ml)[id]]][["Name.1"]]
    ml[[names(ml)[id]]][["Name.1"]] <- ml[[names(ml)[id]]][["Name.2"]] <- NULL
  }

  if (is.null(ml[[names(ml)[id]]][["shinyLabel"]])) {
    ml[[names(ml)[id]]][["shinyLabel"]] <- ml[[names(ml)[id]]][["shinyLabel.1"]]
    ml[[names(ml)[id]]][["shinyLabel.1"]] <- ml[[names(ml)[id]]][["shinyLabel.2"]] <- NULL
  }
}

if (any(grepl("Caribou$|Caribou Joined", names(ml)))) { ## be sure not to include "LandWeb Caribou Ranges" polygon
  ids <- which(grepl("Caribou$|Caribou Joined", names(ml)))
  lapply(ids, function(id) {
    if (is.null(ml[[names(ml)[id]]][["Name"]])) {
      ml[[names(ml)[id]]][["Name"]] <<- ml[[names(ml)[id]]][["Name.1"]]
      ml[[names(ml)[id]]][["Name.1"]] <<- ml[[names(ml)[id]]][["Name.2"]] <- NULL
    }

    if (is.null(ml[[names(ml)[id]]][["shinyLabel"]])) {
      ml[[names(ml)[id]]][["shinyLabel"]] <<- ml[[names(ml)[id]]][["shinyLabel.1"]]
      ml[[names(ml)[id]]][["shinyLabel.1"]] <<- ml[[names(ml)[id]]][["shinyLabel.2"]] <- NULL
    }
  })
}

options(map.useParallel = FALSE)
ml <- mapAdd(map = ml, layerName = layerName, analysisGroup1 = ag1,
             targetFile = asPath(allouts2),
             destinationPath = asPath(destinationPath),
             filename2 = NULL, tsf = asPath(tsf), vtm = asPath(vtm),
             overwrite = TRUE,
             #useCache = "overwrite",
             leaflet = FALSE) # tilePath
#options(map.useParallel = mapParallel)

saveRDS(ml, simFile("ml", Paths$outputPath))
#ml <- readRDS(simFile("ml", Paths$outputPath)) ## TODO: use loadSimList throughout

options(map.useParallel = FALSE)
ml <- mapAddAnalysis(ml, functionName = "LeadingVegTypeByAgeClass",
                     #purgeAnalyses = "LeadingVegTypeByAgeClass",
                     ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                     sppEquivCol = "EN_generic_short", sppEquiv = sppEquivalencies_CA)
#options(map.useParallel = mapParallel)

# add an analysis -- this will trigger analyses because there are already objects in the map
#    This will trigger 2 more analyses ... largePatches on each raster x polygon combo
#    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
options(map.useParallel = FALSE)
ml <- mapAddAnalysis(ml, functionName = "LargePatches",
                     id = "1", labelColumn = "shinyLabel",
                     #purgeAnalyses = "LargePatches",
                     ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                     sppEquivCol = "EN_generic_short", sppEquiv = sppEquivalencies_CA)
#options(map.useParallel = mapParallel)

saveRDS(ml, simFile("ml_partial", Paths$outputPath))
#ml <- readRDS(simFile("ml_partial", Paths$outputPath))

histDirOld <- file.path(Paths$outputPath, "hists") %>% normPath(.)
histDirNew <- file.path(Paths$outputPath, "histograms") %>% normPath(.)
if (dir.exists(histDirOld))
  file.rename(from = histDirOld, to = histDirNew)

options(map.useParallel = FALSE)
## this analysisGroupReportingPolygon MUST be the same as one of ones already analysed
ml <- mapAddPostHocAnalysis(map = ml, functionName = "rbindlistAG",
                            postHocAnalysisGroups = "analysisGroupReportingPolygon",
                            #purgeAnalyses = "rbindlistAG",
                            postHocAnalyses = "all")
ml <- mapAddPostHocAnalysis(map = ml, functionName = "runBoxPlotsVegCover",
                            postHocAnalysisGroups = "analysisGroupReportingPolygon",
                            postHocAnalyses = "rbindlistAG",
                            #purgeAnalyses = "runBoxPlotsVegCover",
                            dPath = file.path(Paths$outputPath, "boxplots"))
ml <- mapAddPostHocAnalysis(map = ml, functionName = "runHistsVegCover",
                            postHocAnalysisGroups = "analysisGroupReportingPolygon",
                            postHocAnalyses = "rbindlistAG",
                            #purgeAnalyses = "runHistsVegCover",
                            dPath = file.path(Paths$outputPath, "histograms"))
ml <- mapAddPostHocAnalysis(map = ml, functionName = "runHistsLargePatches",
                            postHocAnalysisGroups = "analysisGroupReportingPolygon",
                            postHocAnalyses = "rbindlistAG",
                            #purgeAnalyses = "runHistsLargePatches",
                            dPath = file.path(Paths$outputPath, "histograms"))

saveRDS(ml, simFile("ml_done", Paths$outputPath))
message(crayon::red(runName))

if (requireNamespace("slackr") & file.exists("~/.slackr")) {
  slackr::slackr_setup()
  slackr::text_slackr(
    paste0("Post-processing for `", runName, "` completed on host `", Sys.info()[["nodename"]], "`."),
    channel = config::get("slackchannel"), preformatted = FALSE
  )
}

#unlink(tempdir(), recursive = TRUE)
