
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
  reqdPkgs = list(""),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("summaryInterval", "numeric", 50, NA, NA, "This describes summary interval for this module"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "summaryPeriod", objectClass = "numeric", 
                 desc = "a numeric vector contains the start year and end year of summary", 
                 sourceURL = NA),
    expectsInput(objectName = "vegLeadingPercent", objectClass = "numeric", 
                 desc = "a number that define whether a species is lead for a given pixel", 
                 sourceURL = NA),
    expectsInput(objectName = "cohortData", objectClass = "data.table", 
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step, this is imported from forest succession module",
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                 desc = "updated community map at each succession time step, this is imported from
                 forest succession module",
                 sourceURL = NA),
    expectsInput("species", "data.table", "Columns: species, speciesCode, Indicating several features about species",
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput("vegTypeMapGenerator", "function", "converts a species table and cohortdata and pixelGroupMap to vegTypeMap raster")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "vegTypeMap", objectClass = "Raster", desc = NA)
  )
    ))

doEvent.LandWebOutput = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- Init(sim)
    sim <- scheduleEvent(sim, sim$summaryPeriod[1], "LandWebOutput", "allEvents", 
                         eventPriority = 7.5)
  }   else if (time(sim) >= sim$summaryPeriod[1] & eventType == "allEvents" & 
               time(sim) <= sim$summaryPeriod[2]) {
    sim <- AllEvents(sim)
    sim <- scheduleEvent(sim,  time(sim) + P(sim)$summaryInterval,
                         "LandWebOutput", "allEvents", eventPriority = 7.5)
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
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

### template for your event1
AllEvents <- function(sim) {
  sim$vegTypeMap <- sim$vegTypeMapGenerator(sim$species, sim$cohortData, sim$pixelGroupMap, 
                                                     sim$vegLeadingPercent)

    # vegetation type summary 
  # if(is.null(sim$LandMine$vegTypeMapGenerator)) { # This may be produced in a specific fire module
  #   species <- sim$species
  #   species[species == "Pinu_sp" | species == "Pinu_sp", speciesGroup := "PINU"] 
  #   species[species == "Betu_pap" | species == "Popu_bal"| 
  #             species == "Popu_tre" | species == "Lari_lar", speciesGroup := "DECI"] 
  #   species[species == "Pice_mar" | species == "Pice_gla", speciesGroup := "PICE"] 
  #   cohortdata <- sim$cohortData
  #   shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[,.(speciesCode, speciesGroup)], 
  #                                                             speciesCode), nomatch = 0] 
  #   shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup] 
  #   shortcohortdata <- shortcohortdata[,.(speciesGroupB = sum(B, na.rm = TRUE), 
  #                                         totalB = mean(totalB, na.rm = TRUE)), 
  #                                      by = c("pixelGroup", "speciesGroup")] 
  #   shortcohortdata[,speciesPercentage:=speciesGroupB/totalB] 
  #   shortcohortdata[speciesGroup == "PINU" & speciesPercentage > vegLeadingPercent,
  #                   speciesLeading := 1]# pine leading 
  #   shortcohortdata[speciesGroup == "DECI" & speciesPercentage > vegLeadingPercent,
  #                   speciesLeading := 2]# deciduous leading 
  #   shortcohortdata[speciesGroup == "PICE" & speciesPercentage > vegLeadingPercent,
  #                   speciesLeading := 3]# spruce leading 
  #   shortcohortdata[is.na(speciesLeading), speciesLeading := 0] 
  #   shortcohortdata[,speciesLeading:=max(speciesLeading, na.rm = TRUE), by = pixelGroup] 
  #   shortcohortdata <- unique(shortcohortdata[,.(pixelGroup, speciesLeading)], by = "pixelGroup") 
  #   shortcohortdata[speciesLeading == 0, speciesLeading := 4] # 4 is mixed forests 
  #   attritable <- data.table(ID = unique(shortcohortdata$speciesLeading))
  #   attritable[ID == 1, Factor := "Pine leading"]
  #   attritable[ID == 2, Factor := "Deciduous leading"]
  #   attritable[ID == 3, Factor := "Spruce leading"]
  #   attritable[ID == 4, Factor := "Mixed"]
  #   pixelGroupMap <- sim$pixelGroupMap
  #   vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading") 
  #   vegTypeMap <- setValues(vegTypeMap, as.integer(getValues(vegTypeMap)))
  #   levels(vegTypeMap) <- as.data.frame(attritable)
  #   projection(vegTypeMap) <- projection(sim$pixelGroupMap)
  #   sim$vegTypeMap <- vegTypeMap
  # } else {
  #   sim$vegTypeMap <- sim$LandMine$vegTypeMapGenerator(sim$species, sim$cohortData, sim$pixelGroupMap, 
  #                                             sim$vegLeadingPercent)
  # }
  return(invisible(sim))
}


.inputObjects = function(sim) {
  sim$summaryPeriod <- c(1000, 1500)
  if (is.null(sim$vegTypeMapGenerator)) { # otherwise created in LandMine
    sim$vegTypeMapGenerator <- function(species, cohortdata, pixelGroupMap, vegLeadingPercent) {
      species[species == "Pinu_ban" | species == "Pinu_con" | species == "Pinu_sp", speciesGroup := "PINU"]
      species[species == "Betu_pap" | species == "Popu_bal" | species == "Popu_tre" |
                species == "Lari_lar", speciesGroup := "DECI"]
      species[species == "Pice_mar" , speciesGroup := "PICE_MAR"]
      species[species == "Pice_gla", speciesGroup := "PICE_GLA"]
      species[species == "Abie_sp" , speciesGroup := "ABIE"]
      #cohortdata <- sim$cohortData
      shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[, .(speciesCode, speciesGroup)],
                                                                speciesCode), nomatch = 0]
      shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup]
      shortcohortdata <- shortcohortdata[, .(speciesGroupB = sum(B, na.rm = TRUE),
                                             totalB = mean(totalB, na.rm = TRUE)),
                                         by = c("pixelGroup", "speciesGroup")]
      shortcohortdata[,speciesPercentage := speciesGroupB/totalB]
      
      speciesLeading <- NULL
      Factor <- NULL
      ID <- NULL
      pixelGroup <- NULL
      speciesPercentage <- NULL
      speciesGroup <- NULL
      speciesCode <- NULL
      totalB <- NULL
      B <- NULL
      speciesGroupB <- NULL
      
      shortcohortdata[speciesGroup == "PINU" & speciesPercentage > vegLeadingPercent,
                      speciesLeading := 1]# pine leading
      shortcohortdata[speciesGroup == "DECI" & speciesPercentage > vegLeadingPercent,
                      speciesLeading := 2]# deciduous leading
      shortcohortdata[speciesGroup == "PICE_MAR" & speciesPercentage > vegLeadingPercent,
                      speciesLeading := 3]# spruce leading
      shortcohortdata[speciesGroup == "PICE_GLA" & speciesPercentage > vegLeadingPercent,
                      speciesLeading := 4]# spruce leading
      shortcohortdata[is.na(speciesLeading), speciesLeading := 0]
      shortcohortdata[,speciesLeading := max(speciesLeading, na.rm = TRUE), by = pixelGroup]
      shortcohortdata <- unique(shortcohortdata[, .(pixelGroup, speciesLeading)], by = "pixelGroup")
      shortcohortdata[speciesLeading == 0, speciesLeading := 5] # 5 is mixed forests
      attritable <- data.table(ID = sort(unique(shortcohortdata$speciesLeading)))
      attritable[ID == 1, Factor := "Pine leading"]
      attritable[ID == 2, Factor := "Deciduous leading"]
      attritable[ID == 3, Factor := "Black spruce leading"]
      attritable[ID == 4, Factor := "White spruce leading"]
      attritable[ID == 5, Factor := "Mixed"]
      vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading", "pixelGroup")
      vegTypeMap <- setValues(vegTypeMap, as.integer(getValues(vegTypeMap)))
      levels(vegTypeMap) <- as.data.frame(attritable)
      projection(vegTypeMap) <- projection(pixelGroupMap)
      vegTypeMap
    }
    
  }
  sim$vegLeadingPercent <- 0.80
  if (is.null(sim$cohortData))
    sim$cohortData <- data.table()
  if (is.null(sim$pixelGroupMap))
    sim$pixelGroupMap <- raster()
  if (is.null(sim$species)) {
    localSpeciesFilename <- file.path(dataPath(sim), "speciesTraits.csv")
    if (!file.exists(localSpeciesFilename)) {
      mm <- moduleMetadata(currentModule(sim), getPaths()$modulePath)$inputObjects
      download.file(subset(mm, objectName=="species")$sourceURL, 
                    destfile = localSpeciesFilename)
    }
    sim$species <- read.csv(localSpeciesFilename, header = TRUE,
                                 stringsAsFactors = FALSE) %>%
      data.table()
    
  }
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
