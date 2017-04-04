
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
  reqdPkgs = list("SpaDES"),
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
    expectsInput(objectName = "seralStageTable", objectClass = "data.frame", 
                 desc = "a data frame contains how to define seral stage by stand age,
                 how each stage is named", 
                 sourceURL = NA),
    expectsInput(objectName = "vegLeadingPercent", objectClass = "numeric", 
                 desc = "a number that define whether a species is lead for a given pixel", 
                 sourceURL = NA),
    expectsInput(objectName = "patchSize", objectClass = "numeric", 
                 desc = "A number, in ha, to define patch size, ie., bigger than or equal to this size", 
                 sourceURL = NA),
    expectsInput(objectName = "rstTimeSinceFire", objectClass = "raster", 
                 desc = "a time since fire raster layer", 
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
    # sim <- scheduleEvent(sim, sim$summaryPeriod[1], "LandWebOutput", "save",
    #                      eventPriority = 8) #
  }   else if (time(sim) >= sim$summaryPeriod[1] & eventType == "allEvents" & 
               time(sim) <= sim$summaryPeriod[2]) {
    sim <- sim$LandWebOutputAllEvents(sim)
    sim <- scheduleEvent(sim,  time(sim) + P(sim)$summaryInterval,
                         "LandWebOutput", "allEvents", eventPriority = 7.5)
  } else if (eventType == "save") {
    sim <- sim$LandWebOutputSave(sim)
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

### template for your event1
LandWebOutputAllEvents <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # seral stage summary 
  nonActivePixels <- which(sim$pixelGroupMap[] == -1)
  
  if(FALSE) {
    seralStageMap <- sim$rstTimeSinceFire
    seralStageMap[nonActivePixels] <- NA
    SAPixelTable <- data.table(pixelIndex = 1:ncell(seralStageMap),
                              SA = getValues(seralStageMap))[!is.na(SA),]
    SAPixelTable[, seralStage:=cut(SA, breaks = c(seralStageTable$SA, max(SAPixelTable$SA)), 
                                   labels = seralStageTable$seralName,
                                   include.lowest = TRUE)]
    SAPixelTable[, Classification:=as.numeric(seralStage)]
    seralStageMap[SAPixelTable$pixelIndex] <- SAPixelTable$Classification
    levels(seralStageMap) <- as.data.frame(unique(SAPixelTable[,.(ID=Classification, factor = seralStage)], by = "ID"))
    sim$seralStageMap <- seralStageMap
  }
  # vegetation type summary 
  species <- sim$species
  species[species == "Pinu_Ban" | species == "Pinu_Con", speciesGroup := "Pinu"] 
  species[species == "Betu_Pap" | species == "Popu_Bal"| 
            species == "Popu_Tre" | species == "Lari_Lar", speciesGroup := "DECI"] 
  species[species == "Pice_Mar" | species == "Pice_Gla", speciesGroup := "PICE"] 
  cohortdata <- sim$cohortData
  shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[,.(speciesCode, speciesGroup)], 
                                                            speciesCode), nomatch = 0] 
  shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- shortcohortdata[,.(speciesGroupB = sum(B, na.rm = TRUE), 
                                        totalB = mean(totalB, na.rm = TRUE)), 
                                     by = c("pixelGroup", "speciesGroup")] 
  shortcohortdata[,speciesPercentage:=speciesGroupB/totalB] 
  shortcohortdata[speciesGroup == "PINU" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 1]# pine leading 
  shortcohortdata[speciesGroup == "DECI" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 2]# deciduous leading 
  shortcohortdata[speciesGroup == "PICE" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 3]# spruce leading 
  shortcohortdata[is.na(speciesLeading), speciesLeading := 0] 
  shortcohortdata[,speciesLeading:=max(speciesLeading, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- unique(shortcohortdata[,.(pixelGroup, speciesLeading)], by = "pixelGroup") 
  shortcohortdata[speciesLeading == 0, speciesLeading := 4] # 4 is mixed forests 
  attritable <- data.table(ID = unique(shortcohortdata$speciesLeading))
  attritable[ID == 1, Factor := "Pine leading"]
  attritable[ID == 2, Factor := "Deciduous leading"]
  attritable[ID == 3, Factor := "Spruce leading"]
  attritable[ID == 4, Factor := "Mixed"]
  pixelGroupMap <- sim$pixelGroupMap
  vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading") 
  levels(vegTypeMap) <- as.data.frame(attritable)
  projection(vegTypeMap) <- projection(sim$pixelGroupMap)
  sim$vegTypeMap <- vegTypeMap
  # oldSeral <- raster(seralStageMap)
  # seralStageTable <- data.table(seralStageTable)
  # oldSeralClass <- unique(SAPixelTable[,.(seralStage, Classification)], by = "seralStage")[seralStage == seralStageTable[SA == max(seralStageTable$SA),]$seralName,]$Classification
  # if(length(oldSeralClass) == 0){
  #   sim$oldBigPatch <- oldSeral
  # } else {
  #   oldSeral[Which(seralStageMap==oldSeralClass, cell = TRUE)] <- 1
  #   oldSeral[Which(seralStageMap==3, cell = TRUE)] <- 1
  #   oldPatchs <- clump(oldSeral, directions = 8)
  #   freqTable <- data.table(freq(oldPatchs))[!is.na(value),][, area:=count*(res(oldPatchs)[1]^2)/10000]
  #   targetPatchs <- freqTable[area >= patchSize,][,newValue:=as.numeric(as.factor(value))]
  #   oldBigPatch <- raster(seralStageMap)
  #   if(nrow(targetPatchs)==0){
  #     sim$oldBigPatch <- oldBigPatch
  #   } else {
  #     for(i in 1:nrow(targetPatchs)){
  #     oldBigPatch[Which(oldPatchs==targetPatchs$value[i], cell = TRUE)] <- targetPatchs$newValue[i]
  #     }
  #     sim$oldBigPatch <- oldBigPatch
  #   }
  # }
# ! ----- STOP EDITING ----- ! #
return(invisible(sim))
}

### template for save events
LandWebOutputSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  if(!dir.exists(file.path(outputPath(sim), "seralStageMaps"))){
    dir.create(file.path(outputPath(sim), "seralStageMaps"))
  }
  if(!dir.exists(file.path(outputPath(sim), "vegLeadingMaps"))){
    dir.create(file.path(outputPath(sim), "vegLeadingMaps"))
  }
  if(!dir.exists(file.path(outputPath(sim), "patchMaps"))){
    dir.create(file.path(outputPath(sim), "patchMaps"))
  }
  writeRaster(sim$seralStageMap, file.path(outputPath(sim), "seralStageMaps",
                                       paste("seralStageMap_Year", time(sim), ".grd", sep = "")), 
              overwrite = TRUE) 
  writeRaster(sim$vegTypeMap, file.path(outputPath(sim), "vegLeadingMaps",
                                    paste("vegTypeMap_Year", time(sim), ".grd", sep = "")), 
              overwrite = TRUE) 
  writeRaster(sim$oldBigPatch, file.path(outputPath(sim), "patchMaps",
                                        paste("patchMap_Year", time(sim), ".grd", sep = "")), 
              overwrite = TRUE)
  return(invisible(sim))
}

.inputObjects = function(sim) {
  sim$summaryPeriod <- c(1000, 1500)
  sim$seralStageTable <- data.frame(SA = c(0, 40, 80, 120), 
                               seralName = c("Young", "Immature", "Mature", "Old"))
  sim$patchSize <- 5000
  sim$vegLeadingPercent <- 0.80
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
