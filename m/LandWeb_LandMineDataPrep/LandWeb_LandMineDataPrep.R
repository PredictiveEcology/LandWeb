
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LandWeb_LandMineDataPrep",
  description = "Creates the Flammable map, derived from LCC2005 layer",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3", LandWeb_LandMineDataPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWeb_LandMineDataPrep.Rmd"),
  reqdPkgs = list("PredictiveEcology/pemisc"),
  parameters = rbind(),
  inputObjects = bind_rows(
    expectsInput("LCC2005", "RasterLayer",
                 desc = "2005 land classification map in study area, default is Canada national land classification in 2005",
                 sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
  ),
  outputObjects = bind_rows(
    createsOutput("rstFlammable", "RasterLayer",
                  desc = "rstFlammable is 1 for flammable, 0 for non flammable. This is simply for masking out fire pixels that can't burn")
  )
))

doEvent.LandWeb_LandMineDataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim$rstFlammable <- defineFlammable(sim$LCC2005, nonFlammClasses = c(36, 37, 38, 39))
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    stop("please supply a rasterToMatch layer")
  }

  if (!suppliedElsewhere("LCC2005", sim)) {
    sim$LCC2005 <- prepInputsLCC(destinationPath = dataPath(sim),
                                 rasterToMatch = sim$rasterToMatch)
  }
}
