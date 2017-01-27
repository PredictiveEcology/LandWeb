
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LandWebOutput",
  description = "Summary the output for the LandWeb natural range of variation",
  keywords = c("land NRV"),
  authors = person("Yong", "Luo", email = "yong.luo@canada.ca", 
                   role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("1.3.1.9027"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWebOutput.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("summaryInterval", "numeric", 50, NA, NA, "This describes summary interval for this module"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "summaryPeriod", objectClass = "numeric", 
                 desc = "a numeric vector contains the start year and end year of summary", 
                 sourceURL = NA),
    expectsInput(objectName = "summaryUnit", objectClass = "sptialPolygons", 
                 desc = "a forest management unit map or something like this, default is ecodistrict", 
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "seralStage", objectClass = "data.frame", 
                 desc = "a data frame contains how to define seral stage by stand age,
                 how each stage is named", 
                 sourceURL = NA),
    expectsInput(objectName = "vegLeadingPercent", objectClass = "numeric", 
                 desc = "a number that define whether a species is lead for a given pixel", 
                 sourceURL = NA),
    expectsInput(objectName = "patchSize", objectClass = "numeric", 
                 desc = "A number to define patche, ie., bigger than or equal to this size", 
                 sourceURL = NA),
    expectsInput(objectName = "timeSinceFire", objectClass = "RasterLayer", 
                 desc = "a time since fire map", 
                 sourceURL = NA),
    expectsInput(objectName = "cohortData", objectClass = "data.table", 
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step, this is imported from forest succession module",
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                 desc = "updated community map at each succession time step, this is imported from
                 forest succession module",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
    ))

doEvent.LandWebOutput = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- sim$LandWebOutputInit(sim)
    sim <- scheduleEvent(sim, sim$summaryPeriod[1], "LandWebOutput", "allEvents", 
                         eventPriority = 7.5)
    sim <- scheduleEvent(sim, sim$summaryPeriod[1], "LandWebOutput", "save",
                         eventPriority = 8)
  }   else if (eventType == "allEvents" & time(sim) <= sim$summaryPeriod[2]) {
    sim <- scheduleEvent(sim,  time(sim) + P(sim)$summaryInterval,
                         "LandWebOutput", "allEvents", eventPriority = 7.5)
  } else if (eventType == "save") {
    sim <- scheduleEvent(sim, time(sim) + P(sim)$summaryInterval, 
                         "LandWebOutput", "save", eventPriority = 8)
    
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
LandWebOutputInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

### template for save events
LandWebOutputSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
LandWebOutputPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
LandWebOutputAllEvents <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  
  # seral stage summary 
  SAdata <- cohortdata[,.(SA=max(age, na.rm = TRUE)), by = pixelGroup] 
  SAdata[SA < 40, SAStage:=1] # stage 1 young forests 
  SAdata[SA >= 40 & SA < 80, SAStage:=2] # stage 2 mature forests 
  SAdata[SA >= 80 & SA < 100, SAStage:=3] # stage 3 old forests 
  SAdata[SA >= 100, SAStage:=4] # stage 4 overold forests 
  names(pixelGroupMap) <- "pixelGroup" 
  seralStageMap <- rasterizeReduced(SAdata, pixelGroupMap, "SAStage") 
  
  # vegetation type summary 
  species[species == "PINU.BAN" | species == "PINU.CON", speciesGroup := "PINU"] 
  species[species == "BETU.PAP" | species == "POPU.BAL" | 
            species == "POPU.TRE" | species == "LARI.LAR", speciesGroup := "DECI"] 
  species[species == "PICE.MAR" | species == "PICE.GLA", speciesGroup := "PICE"] 
  shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[,.(speciesCode, speciesGroup)], 
                                                            speciesCode), nomatch = 0] 
  shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- shortcohortdata[,.(speciesGroupB = sum(B, na.rm = TRUE), 
                                        totalB = mean(totalB, na.rm = TRUE)), 
                                     by = c("pixelGroup", "speciesGroup")] 
  shortcohortdata[,speciesPercentage:=speciesGroupB/totalB] 
  shortcohortdata[speciesGroup == "PINU" & speciesPercentage > 0.5, speciesLeading := 1]# pine leading 
  shortcohortdata[speciesGroup == "DECI" & speciesPercentage > 0.5, speciesLeading := 2]# deciduous leading 
  shortcohortdata[speciesGroup == "PICE" & speciesPercentage > 0.5, speciesLeading := 3]# spruce leading 
  shortcohortdata[is.na(speciesLeading), speciesLeading := 0] 
  shortcohortdata[,speciesLeading:=max(speciesLeading, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- unique(shortcohortdata[,.(pixelGroup, speciesLeading)], by = "pixelGroup") 
  shortcohortdata[speciesLeading == 0, speciesLeading := 4] # 4 is mixed forests 
  vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading") 
  writeRaster(seralStageMap, file.path(outputpath, 
                                       paste("seralStageMap_Year", year, ".tif", sep = "")), 
              overwrite = TRUE) 
  writeRaster(vegTypeMap, file.path(outputpath, 
                                    paste("vegTypeMap_Year", year, ".tif", sep = "")), 
              overwrite = TRUE) 

# ! ----- STOP EDITING ----- ! #
return(invisible(sim))
}


.inputObjects = function(sim) {
  sim$summaryPeriod <- c(1000, 1500)
  sim$seralStage <- data.frame(SA = c(0, 40, 80, 120), 
                               seralName = c("Young", "Immature", "Mature", "Old"))
  sim$patchSize <- 5000
  sim$vegLeadingPercent <- 0.80
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
