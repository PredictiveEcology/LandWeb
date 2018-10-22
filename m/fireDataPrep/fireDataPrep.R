# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(
  sim,
  list(
    name = "fireDataPrep",
    description = "basic data preparation for LCC05 based fire models",
    keywords = c("LCC05"),
    authors = c(
      person(c("Steve", "G"), "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut", "cre"))
    ),
    childModules = character(),
    version = numeric_version("1.2.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.txt", "fireDataPrep.Rmd"),
    reqdPkgs = list("raster", "sp", "SpaDES.tools"),
    parameters = rbind(
      defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
        "simulation time at which the first plot event should occur"),
      defineParameter(".plotInterval", "numeric", NA, NA, NA,
        "simulation time at which the first plot event should occur"),
      defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
        "simulation time at which the first save event should occur"),
      defineParameter(".saveInterval", "numeric", NA, NA, NA,
        "simulation time at which the first save event should occur"),
      defineParameter(".useCache", "logical", FALSE, NA, NA,
        paste("Whether the module should be cached for future calls.",
              "This is generally intended for data-type modules, where stochasticity and time are not relevant.")
      )
    ),
    inputObjects = bind_rows(
      expectsInput("LCC05", "RasterLayer",
                   desc = "", sourceURL = NA), ## TODO: needs proper description
      expectsInput("shpStudyRegion", "SpatialPolygonsDataFrame",
                   desc = "", sourceURL = NA), ## TODO: needs proper description
      expectsInput("rstStudyRegion", "RasterLayer",
                   desc = "", sourceURL = NA) ## TODO: needs proper description
    ),
    outputObjects = bind_rows(
      createsOutput("rstFlammable", "RasterLayer",
                    desc = "rstFlammable is 0 for flammable, 1 for non flammable") ## TODO: improve description
    )
  )
)

## event types
#   - type `init` is required for initialiazation

doEvent.fireDataPrep <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    # do stuff for this event
    sim <- Init(sim)
  } else {
    warning(paste(
      "Undefined event type: '",
      current(sim)[1, "eventType", with = FALSE],
      "' in module '",
      current(sim)[1, "moduleName", with = FALSE],
      "'",
      sep = ""
    ))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization

Init <- function(sim) {
  nonFlammClasses <- c(36, 37, 38, 39)
  oldClass <- 0:39
  newClass <-
    ifelse(oldClass %in% nonFlammClasses, 1, 0)   #1 codes for non flammable
  #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  #according to Yong, Canada Landcover 2005 is loaded as LCC05
  sim$rstFlammable <- ratify(reclassify(sim$LCC05, flammableTable, count = TRUE))
  sim$rstFlammable <- writeRaster(sim$rstFlammable,
                                  filename = file.path(outputPath(sim), "rstFlammable"),
                                  overwrite = TRUE)

  setColors(sim$rstFlammable, n = 2) <- colorRampPalette(c("blue", "red"))(2)
  sim$rstFlammable[is.na(sim$rstStudyRegion[])] <- NA

  # Much faster than call rasterize again
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  # if (!suppliedElsewhere(sim$shpStudyRegion)) {

  #   sim$shpStudyRegion <- randomPolygon(matrix(c(-90, 60), ncol = 2), 5)
  # }
  # if (!suppliedElsewhere(sim$LCC05)) {
  #   sim$LCC05 <- raster(extent(sim$shpStudyRegion))
  # }
  # if (!suppliedElsewhere(sim$rstStudyRegion)) {
  #   sim$rstStudyRegion <- rasterize(sim$shpStudyRegion, sim$LCC05)
  # }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
