defineModule(sim, list(
  name="biomassSuccessionLANDIS",
  description="simulate biomass succession modified from LANDIS II",
  keywords=c("forest succession"),
  authors=c(person(c("Yong"), "Luo", email="Yong.Luo@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Jean"), "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("1.1.0.9000"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list("citation.bib"),
  documentation=list("README.txt", "biomassSuccessionLandis.Rmd"),
  reqdPkgs=list("raster","sp","data.table", "dplyr","ggplot2", "fpCompare","grid","archivist","tidyr"),
  parameters=rbind(
    defineParameter("growthInitialTime", "numeric", 0, NA_real_, NA_real_, "Initial time for the growth event to occur"),
    defineParameter("fireDisturbanceInitialTime", "numeric", 1, NA_real_, NA_real_, "Initial time for the post fire reproduction event to occur"),
    #    defineParameter("seedingReproductionInitialTime", "numeric", 0.00003, 0.0, 1.0, "Initial time for the seeding reproduction event to occur"),
    defineParameter(".saveInitialTime", "numeric", 0.0, 0.0, 100.0, "Initial time for the save event to occur")),
  inputObjects=data.frame(objectName = c("growthInterval"),
                          objectClass = c("numeric"),
                          other = rep(NA_character_, 1L), 
                          stringsAsFactors = FALSE,
                          sourceURL = ""),
  outputObjects=data.frame(objectName = c("cohortData"),
                           objectClass = c("data.table"),
                           other = rep(NA_character_, 1L), 
                           stringsAsFactors = FALSE)
))

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.biomassSuccessionLANDIS = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    depends <- NULL
    sim <- biomassSuccessionLANDISInit(sim) # to generate initial cohort data from initialCommunities
    if(sim$successionTimestep != 1){
      sim <- scheduleEvent(sim, start(sim) + 2*sim$successionTimestep - 1, "biomassSuccessionLANDIS",
                           "cohortAgeReclassification")
    }
    sim <- scheduleEvent(sim, start(sim) + params(sim)$biomassSuccessionLANDIS$growthInitialTime,
                         "biomassSuccessionLANDIS", "mortalityAndGrowth")
    sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "summaryBGM")
    if(!is.null(sim$fireMap)){ # anything related to fire disturbance
      sim <- scheduleEvent(sim, start(sim) + params(sim)$biomassSuccessionLANDIS$fireDisturbanceInitialTime,
                           "biomassSuccessionLANDIS", "fireDisturbance")
    }
    if(sim$seedingAlgorithm == "noDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep - 0.003,
                           "biomassSuccessionLANDIS", "noDispersalSeeding")
    } else if(sim$seedingAlgorithm == "universalDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep - 0.003,
                           "biomassSuccessionLANDIS", "universalDispersalSeeding")
    } else if(sim$seedingAlgorithm == "wardDispersal"){
      sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep - 0.003,
                           "biomassSuccessionLANDIS", "wardDispersalSeeding")
    } else {
      stop("Undefined seed dispersal type!")
    }
    sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep - 0.002,
                         "biomassSuccessionLANDIS", "summaryRegen")
    sim <- scheduleEvent(sim, start(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "plot")
    sim <- scheduleEvent(sim, params(sim)$biomassSuccessionLANDIS$.saveInitialTime,
                         "biomassSuccessionLANDIS", "save")
  } else if (eventType == "mortalityAndGrowth") {
    sim <- biomassSuccessionLANDISMortalityAndGrowth(sim)
    sim <- scheduleEvent(sim, time(sim) + 1, "biomassSuccessionLANDIS", "mortalityAndGrowth")
  } else if (eventType == "summaryBGM"){
    sim <- biomassSuccessionLANDISSummaryBGM(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "summaryBGM")
  } else if (eventType == "fireDisturbance" & !is.null(sim$fireMap)) {
    sim <- biomassSuccessionLANDISFireDisturbance(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$fireTimeStep,
                         "biomassSuccessionLANDIS", "fireDisturbance")
  } else if (eventType == "noDispersalSeeding" | eventType=="universalDispersalSeeding" | eventType=="wardDispersalSeeding") {
    if(sim$seedingAlgorithm=="noDispersal"){
      sim <- biomassSuccessionLANDISNoDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep, 
                           "biomassSuccessionLANDIS", "noDispersalSeeding")
    }
    if(sim$seedingAlgorithm == "universalDispersal"){
      sim <- biomassSuccessionLANDISUniversalDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep, 
                           "biomassSuccessionLANDIS", "universalDispersalSeeding")
    }
    if(sim$seedingAlgorithm == "wardDispersal"){
      sim <- biomassSuccessionLANDISWardDispersalSeeding(sim)
      sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                           "biomassSuccessionLANDIS", "wardDispersalSeeding")
    }
  } else if (eventType == "summaryRegen"){
    sim <- biomassSuccessionLANDISSummaryRegen(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "summaryRegen")
  } else if (eventType == "plot") {
    sim <- biomassSuccessionLANDISPlot(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "plot")
  } else if (eventType == "save") {
    sim <- biomassSuccessionLANDISSave(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "save")
  } else if (eventType == "cohortAgeReclassification" & sim$successionTimestep != 1) {
    sim <- biomassSuccessionLANDISCohortAgeReclassification(sim)
    sim <- scheduleEvent(sim, time(sim) + sim$successionTimestep,
                         "biomassSuccessionLANDIS", "cohortAgeReclassification")
  }
  else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                  "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

biomassSuccessionLANDISInit = function(sim) {
  communities <- sim$initialCommunities %>%
    gather(key=cohort, value=age, -mapcode,-description,-species,na.rm=TRUE) %>%
    data.table %>%
    .[,`:=`(age = as.integer(ceiling(as.numeric(age)/sim$successionTimestep) * sim$successionTimestep),
            communityGroup = as.integer(mapcode),
            mapcode = NULL)] %>%
    unique(., by = c("communityGroup", "species", "age"))
  browser()
  species <- data.table(sim$species)[,speciesCode:=as.integer(as.factor(species))]
  tempspecies <- setkey(species[,.(species,speciesCode)], species)
  communities <- setkey(communities, species)[tempspecies, nomatch=0]
  speciesEcoregion <- setkey(data.table(sim$speciesEcoregion), species)[tempspecies, nomatch=0]
  sim$species <- setkey(species, speciesCode)
  ecoregion <- data.table(sim$ecoregion)[, ecoregionGroup := as.integer(mapcode)]
  ecoregion_temp <- setkey(ecoregion[, .(ecoregion,ecoregionGroup)], ecoregion)
  sim$minRelativeB <- data.table(sim$minRelativeB, key = "ecoregion")[ecoregion_temp, nomatch = 0]
  speciesEcoregion <- setkey(speciesEcoregion, ecoregion)[ecoregion_temp, nomatch=0]
  sim$speciesEcoregion <- setkey(speciesEcoregion, ecoregionGroup, speciesCode)
  nrowCommunities <- nrow(communities)
  initialCommunitiesMap <- sim$initialCommunitiesMap
  napixels <- which(is.na(getValues(initialCommunitiesMap)))
  initialCommunitiesMap[napixels] <- max(getValues(initialCommunitiesMap), na.rm=TRUE) + 1
  pixelGroupFactor <- 10^ceiling(log10((max(getValues(initialCommunitiesMap), na.rm=TRUE) + 1)))
  ecoregionMap <- sim$ecoregionMap
  pixelGroupMap <- initialCommunitiesMap + ecoregionMap*pixelGroupFactor
  active_ecoregion <- setkey(ecoregion[active=="yes" ,.(k=1,ecoregionGroup)], k)
  cohortData <- setkey(communities[, k:=1], k)[active_ecoregion, allow.cartesian=TRUE][, k:=NULL]
  set(cohortData, ,"pixelGroup", cohortData$communityGroup + cohortData$ecoregionGroup*pixelGroupFactor)
  set(cohortData, , "B", as.integer(0L))
  cohortData <- cohortData[, .(pixelGroup, ecoregionGroup, speciesCode, age, B)] # removed the collumn cummunityGroup
  # the cohortData here is a full joint table of community Group and ecoregion Group
  # some redundant pixelGroups are removed, because they are not present on the pixelGroupMap
  # we are dealing with the case that all the ecoregion is active, how about some ecoregion is not active
  activePixelIndex <- which(getValues(ecoregionMap) %in% active_ecoregion$ecoregionGroup)
  inactivePixelIndex <- seq(from=1, to=ncell(ecoregionMap))[(seq(from=1, to=ncell(ecoregionMap)) %in% activePixelIndex) == FALSE]
  sim$activePixelIndex <- activePixelIndex # store this for future use
  sim$inactivePixelIndex <- inactivePixelIndex # store this for future use
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)[activePixelIndex]),]
  rm(nrowCommunities, pixelGroupFactor)
  # pixels with -1 in the pixelGroupMap are inactive
  if(length(inactivePixelIndex) > 0){
    pixelGroupMap[inactivePixelIndex] <- -1
  }
  cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                 time = round(time(sim)), cohortData = cohortData)
  cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = cohortData)
  
  if(is.null(sim$calibrate)){
    sim$calibrate <- FALSE
  }
  sim <- cacheSpinUpFunction(sim, cachePath = outputPath(sim))
  spinupstage <- sim$spinUpCache(cohortData = cohortData, calibrate = sim$calibrate,
                                 successionTimestep = sim$successionTimestep,
                                 spinupMortalityfraction = sim$spinupMortalityfraction,
                                 species = sim$species)
  browser()
  cohortData <- spinupstage$cohortData
  if(sim$calibrate){
    sim$spinupOutput <- spinupstage$spinupOutput
  }
  if(sim$calibrate){
    sim$simulationTreeOutput <- data.table(Year = numeric(), siteBiomass = numeric(), Species = character(),
                                           Age = numeric(), iniBiomass = numeric(), ANPP = numeric(),
                                           Mortality = numeric(), deltaB = numeric(), finBiomass = numeric())
    sim$regenerationOutput <- data.table(seedingAlgorithm = character(), species = character(),
                                         Year = numeric(), numberOfReg=  numeric())
  }
  simulationOutput <- data.table(Ecoregion = numeric(), NofCell = numeric(), Year = numeric(), Biomass = numeric(),
                                 ANPP = numeric(), Mortality = numeric(), Regeneration = numeric())
  simulationOutput_byspecies <- data.table(Ecoregion = integer(), ncell = integer(), Species = character(), Year = integer(), 
                                           Biomass = numeric(), ANPP = numeric(), Mortality = numeric())
  names(pixelGroupMap) <- "pixelGroup"
  pixelAll <- cohortData[,.(uniqueSumB = as.numeric(sum(B, na.rm=TRUE))), by=pixelGroup]
  biomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
  ANPPMap <- setValues(biomassMap, 0)
  mortalityMap <- setValues(biomassMap, 0)
  reproductionMap <- setValues(biomassMap, 0)
  sim$pixelGroupMap <- pixelGroupMap
  sim$cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age,
                                  B, mortality = 0, aNPPAct = 0)]
  updateoutputs <- updateOutputs(sim, biomassMap, ANPPMap, mortalityMap, reproductionMap)
  sim$simulationOutput <- updateoutputs$simulationOutput
  sim$simulationOutput_byspecies <- updateoutputs$simulationOutput_byspecies
  set(sim$cohortData, ,"aNPPAct", NULL)
  sim$lastReg <- 0
  speciesEcoregion[, identifier:=year>sim$successionTimestep]
  speciesEcoregion_True <- speciesEcoregion[identifier == "TRUE",]
  speciesEcoregion_False <- speciesEcoregion[identifier == "FALSE",]
  speciesEcoregion_True_addon <- speciesEcoregion_False[year == max(speciesEcoregion_False$year),]
  sim$speciesEcoregion <- rbindlist(list(speciesEcoregion_True_addon, speciesEcoregion_True))[
    ,':='(year = year - min(year), identifier = NULL)]
  sim$lastFireYear <- "noFire"
  sim$timeRecorder <- data.table(simuTime = numeric(), systemTime = numeric())
  return(invisible(sim))
}

cacheSpinUpFunction <- function(sim, cachePath) {
  # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
  if(sim$useCache) {
    # Step 1 - create a location for the cached data
    sim$cacheLoc <- file.path(cachePath, "spinUp")
    # Step 1a - check whether that location already exists
    if(!dir.exists(sim$cacheLoc) ){
      # Step 1b - if not, create it
      archivist::createEmptyRepo(file.path(cachePath, "spinUp"))
    }
    # Step 2 - create a version of every function that is slow that includes the caching implicitly
    sim$spinUpCache <- function(...) {
      archivist::cache(cacheRepo = sim$cacheLoc, FUN = spinUp, ...)
    }
  } else {
    # Step 3 - create a non-caching version in case caching is not desired
    #  sim$spinUp <- sim$spinUpRaw
    sim$spinUpCache <- spinUp
  }
  return(invisible(sim))
}

spinUp <- function(cohortData, calibrate, successionTimestep, spinupMortalityfraction, species){
  maxAge <- max(cohortData$age) # determine the pre-simulation length
  set(cohortData, ,"origAge", cohortData$age)
  set(cohortData, ,c("age","sumB"), as.integer(0L))
  set(cohortData, ,c("mortality","aNPPAct"), as.numeric(0))
  if(calibrate){
    spinupOutput <- data.table(pixelGroup = integer(), species = character(), age = integer(), 
                               iniBiomass = integer(), ANPP = numeric(), Mortality=numeric(),
                               finBiomass = integer())
  }
  k <- 0
  if(successionTimestep == 1 & maxAge!=1){
    presimuT_end <- 2
  } else {
    presimuT_end <- 1
  }
  
  for(presimuT in (maxAge):presimuT_end) {
    k <- k+1
    cohortData[origAge == presimuT,     age:=1L]
    cohortData[origAge >= presimuT,     age:=age+1L]
    
    if(successionTimestep !=1 &
         as.integer(k/successionTimestep) == k/successionTimestep){
      cohortData <- ageReclassification(cohortData = cohortData, successionTimestep = successionTimestep,
                                        stage = "spinup")
    }
    # 1. assign the biomass for the first cohort
    if(nrow(cohortData[age == 2,])>0){
      lastReg <- k-1
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k, 
                                  successionTimestep = successionTimestep)
      cohortData[age == 2, B:=as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[age == 2, B:=as.integer(pmin(maxANPP, B))]
    }
    if(maxAge!=1){
      # 2. calculate age-related mortality
      cohortData <- calculateAgeMortality(cohortData, stage="spinup",
                                          spinupMortalityfraction = spinupMortalityfraction)
      # 3. calculate the actual ANPP
      # calculate biomass Potential, for each cohort
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k-1,
                                  successionTimestep = successionTimestep)
      cohortData <- calculateCompetition(cohortData, stage = "spinup")
      # calculate ANPP
      cohortData <- calculateANPP(cohortData, stage = "spinup")
      cohortData[age > 0, aNPPAct:=pmax(1, aNPPAct - mAge)]
      # calculate growth related mortality
      cohortData <- calculateGrowthMortality(cohortData, stage = "spinup")
      cohortData[age > 0, mBio:=pmax(0,mBio - mAge)]
      cohortData[age > 0, mBio:=pmin(mBio, aNPPAct)]
      cohortData[age > 0, mortality:=mBio + mAge]
      cohortData[age > 0, B:=as.integer(B + as.integer(aNPPAct - mortality))]
      set(cohortData, ,c("bPM", "mBio"), NULL)
    }
    if(calibrate){
      if(maxAge != 1){
        spoutput <- cohortData[origAge >= presimuT, .(pixelGroup, speciesCode, age,
                                                      iniBiomass = B + as.integer(mortality - aNPPAct),
                                                      ANPP = round(aNPPAct, 1),
                                                      Mortality = round(mortality, 1),finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[,.(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      ,species := NULL]
        
        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
        cohortData[,':='(bAP = NULL)]
      } else {
        spoutput <- cohortData[origAge >= presimuT,.(pixelGroup, speciesCode, age,
                                                     iniBiomass = 0, ANPP = 0,
                                                     Mortality = 0, finBiomass = B)]
        spoutput <- setkey(spoutput, speciesCode)[setkey(species[,.(species, speciesCode)], speciesCode),
                                                  nomatch = 0][
                                                    , speciesCode := species][
                                                      ,species := NULL]
        
        setnames(spoutput, "speciesCode", "species")
        spinupOutput <- rbind(spinupOutput, spoutput)
        rm(spoutput)
      }
    }
    lastnewcohorts <- which(cohortData$origAge == 1)
    if(presimuT == presimuT_end & length(lastnewcohorts) > 0 & maxAge != 1){
      cohortData <- calculateSumB(cohortData, lastReg = lastReg, simuTime = k,
                                  successionTimestep = successionTimestep)
      cohortData[origAge == 1,B:=as.integer(pmax(1, maxANPP*exp(-1.6*sumB/maxB_eco)))]
      cohortData[origAge == 1,B:=as.integer(pmin(maxANPP, B))]
    }
  }
  cohortData[,':='(age = origAge, origAge = NULL)]
  if(calibrate){
    all <- list(cohortData = cohortData, spinupOutput = spinupOutput)
  } else {
    all <- list(cohortData = cohortData)
  }
  return(all)
}

### template for your event1
biomassSuccessionLANDISMortalityAndGrowth = function(sim) {
  #   cohortData <- sim$cohortData
  set(sim$cohortData, ,"age", sim$cohortData$age + 1)
  sim$cohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                     time = round(time(sim)), cohortData = sim$cohortData)
  sim$cohortData <- updateSpeciesAttributes(species = sim$species, cohortData = sim$cohortData)
  
  #   if(as.integer(time(sim)/sim$successionTimestep) == time(sim)/sim$successionTimestep){
  #     cohortData <- 
  #     cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, species, speciesCode, age,
  #                                 B, maxANPP, maxB,  establishprob, maxB_eco,longevity, mortalityshape,
  #                                 growthcurve, sexualmature, shadetolerance,
  #                                 mortality, prevMortality = 0, sumB = as.integer(0L), aNPPAct = 0)]
  #   }
  sim$cohortData <- calculateSumB(cohortData = sim$cohortData, 
                                  lastReg = sim$lastReg, 
                                  simuTime = time(sim),
                                  successionTimestep = sim$successionTimestep)
  sim$cohortData <- sim$cohortData[age <= longevity,]
  sim$cohortData <- calculateAgeMortality(cohortData = sim$cohortData,
                                          stage = "mainsimulation", 
                                          spinupMortalityfraction = 0)
  set(sim$cohortData, , c("longevity", "mortalityshape"), NULL)
  sim$cohortData <- calculateCompetition(cohortData = sim$cohortData,
                                         stage = "mainsimulation")
  if(!sim$calibrate){
    set(sim$cohortData, , "sumB", NULL)
  }
  #### the below two lines of codes are to calculate actual ANPP
  sim$cohortData <- calculateANPP(cohortData = sim$cohortData, 
                                  stage = "mainsimulation")
  set(sim$cohortData, , "growthcurve", NULL)
  set(sim$cohortData, ,"aNPPAct",
      pmax(1, sim$cohortData$aNPPAct - sim$cohortData$mAge))
  sim$cohortData <- calculateGrowthMortality(cohortData = sim$cohortData,
                                             stage = "mainsimulation")
  set(sim$cohortData, ,"mBio",
      pmax(0, sim$cohortData$mBio - sim$cohortData$mAge))
  set(sim$cohortData, ,"mBio",
      pmin(sim$cohortData$mBio, sim$cohortData$aNPPAct))
  set(sim$cohortData, ,"mortality",
      sim$cohortData$mBio + sim$cohortData$mAge)
  set(sim$cohortData, ,c("mBio", "mAge", "maxANPP",
                         "maxB", "maxB_eco", "bAP", "bPM"),
      NULL)
  if(sim$calibrate){
    set(sim$cohortData, ,"deltaB",
        as.integer(sim$cohortData$aNPPAct - sim$cohortData$mortality))
    set(sim$cohortData, ,"B",
        sim$cohortData$B + sim$cohortData$deltaB)
    tempcohortdata <- sim$cohortData[,.(pixelGroup, Year = time(sim), siteBiomass = sumB, speciesCode,
                                        Age = age, iniBiomass = B - deltaB, ANPP = round(aNPPAct, 1),
                                        Mortality = round(mortality,1), deltaB, finBiomass = B)]
    
    tempcohortdata <- setkey(tempcohortdata, speciesCode)[setkey(sim$species[,.(species, speciesCode)],
                                                                             speciesCode),
                                                          nomatch = 0][, ':='(speciesCode = species,
                                                                              species = NULL,
                                                                              pixelGroup = NULL)]
    setnames(tempcohortdata, "speciesCode", "Species")
    sim$simulationTreeOutput <- rbind(sim$simulationTreeOutput, tempcohortdata)
    set(sim$cohortData, ,c("deltaB", "sumB"), NULL)
  } else {
    set(sim$cohortData, ,"B",
        sim$cohortData$B + as.integer(sim$cohortData$aNPPAct - sim$cohortData$mortality))
  }
  return(invisible(sim))
}

biomassSuccessionLANDISSummaryBGM = function(sim) {
  summaryBGMtable <- sim$cohortData[,.(uniqueSumB = as.numeric(sum(B, na.rm=TRUE)),
                                       uniqueSumANPP = as.numeric(sum(aNPPAct, na.rm=TRUE)),
                                       uniqueSumMortality = as.numeric(sum(mortality,na.rm=TRUE))),
                                    by = pixelGroup]
  # the unit for sumB, sumANPP, sumMortality are g/m2, g/m2/year, g/m2/year, respectively. 
  names(sim$pixelGroupMap) <- "pixelGroup"
  sim$biomassMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap,
                                     "uniqueSumB")
  setColors(sim$biomassMap) <- c("light green", "dark green")

  sim$ANPPMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumANPP")
  setColors(sim$ANPPMap) <- c("light green", "dark green")
  
  sim$mortalityMap <- rasterizeReduced(summaryBGMtable, sim$pixelGroupMap, "uniqueSumMortality")
  setColors(sim$mortalityMap) <- c("light green", "dark green")
  
  # the following codes for preparing the data table for saving
  updateoutputs <- updateOutputs(sim, sim$biomassMap, sim$ANPPMap,
                                 sim$mortalityMap, sim$reproductionMap)
  sim$simulationOutput <- updateoutputs$simulationOutput
  sim$simulationOutput_byspecies <- updateoutputs$simulationOutput_byspecies
  return(invisible(sim))
}

biomassSuccessionLANDISFireDisturbance = function(sim) {
  # the presence of valid fire can cause three processes:
  # 1. remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration
  # 3. change of cohortdata and pixelgroup map
  # may be a supplemenatary function is needed to convert non-logical map
  # to a logical map
  postFireReproData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(), 
                                  speciesCode = numeric(), pixelIndex = numeric())
  if(sim$calibrate){
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(), 
                                           species = character(), 
                                           numberOfRegen = numeric())
  }
  if(length(sim$inactivePixelIndex) > 0){
    sim$fireMap[sim$inactivePixelIndex] <- 0 # this is to prevent avaluating the pixels that are inactive
  }
  firePixelTable <- data.table(cbind(pixelIndex = Which(sim$fireMap == 1, cell = TRUE),
                                     pixelGroup = sim$pixelGroupMap[Which(sim$fireMap == 1, cell = TRUE)]))
  sim$pixelGroupMap[firePixelTable$pixelIndex] <- 0 # 0 is the fire burnt pixels without regenerations
  burnedcohortData <- sim$cohortData[pixelGroup %in% unique(firePixelTable$pixelGroup)]
  set(burnedcohortData, ,c("B", "mortality", "aNPPAct"), NULL)
#   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)
  tempspecies <- sim$species[postfireregen == "serotiny",
                             .(speciesCode, postfireregen)]
  serotinyAssessCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]
  
  rm(tempspecies)
  if(NROW(serotinyAssessCohortData) > 0){
    # assess potential serotiny reg
    serotinyAssessCohortData <- setkey(serotinyAssessCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
                                                                              nomatch = 0]
    newCohortData <- serotinyAssessCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode")) 
    set(newCohortData, ,"sexualmature", NULL)
    # this is amazing!!!!
    # select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- firePixelTable[pixelGroup %in% unique(newCohortData$pixelGroup)]
    
    # from now on the regeneration process is assessed for each potential pixel
    setkey(serotinyPixelTable, pixelGroup)
    setkey(newCohortData, pixelGroup)
    newCohortData <- serotinyPixelTable[newCohortData, nomatch = 0, allow.cartesian = TRUE]
    
    # light check
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                                       nomatch = 0][,siteShade := 0]
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    set(newCohortData, ,c("shadetolerance", "siteShade", "lightProb"), NULL)
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]
    newCohortData <- newCohortData[(runif(nrow(newCohortData), 0, 1)) %<<% establishprob][, establishprob := NULL]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    if(NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)] #
      if(sim$calibrate){
        serotinyRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode] 
        serotinyRegenSummary <- serotinyRegenSummary[,.(year = time(sim), regenMode = "Serotiny",
                                                        speciesCode, numberOfRegen)]
        serotinyRegenSummary <- setkey(serotinyRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                                      nomatch = 0]
        serotinyRegenSummary[, ':='(speciesCode = species, species = NULL)]
        setnames(serotinyRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbind(sim$postFireRegenSummary, serotinyRegenSummary)
      }
      serotinyPixel <- unique(newCohortData$pixelIndex) # save the pixel index for resprouting assessment use,
      # i.e., removing these pixel from assessing resprouting
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
    } else {
      serotinyPixel <- NULL
    }
    rm(newCohortData)
  } else {
    serotinyPixel <- NULL
  }
  
  #############################################################
  #############################################################
  # from now on, starting assessing resprouting reproduction:
  # basically same thing as serotiny
  # remove the pixels that had successful serotiny regeneration
  if(is.null(serotinyPixel)){
    resproutingPixelTable <- setkey(firePixelTable, pixelGroup)
  } else {
    resproutingPixelTable <- setkey(data.table(dplyr::anti_join(firePixelTable, 
                                                                data.table(cbind(pixelIndex = serotinyPixel)),
                                                                by = "pixelIndex")),
                                    pixelGroup)
  }
  setkey(burnedcohortData, speciesCode)
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]
  resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingAssessCohortData, ,c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)
  if(NROW(resproutingAssessCohortData) > 0){
    resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    setkey(resproutingAssessCohortData, pixelGroup)
    newCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][, siteShade := 0]
    
    # Light check
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    newCohortData <- newCohortData[runif(nrow(newCohortData), 0, 1) %<<% newCohortData$resproutprob]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    set(newCohortData, ,c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)
    # remove all columns that were used temporarily here
    if(NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)]#
      if(sim$calibrate){
        resproutRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode] 
        resproutRegenSummary <- resproutRegenSummary[,.(year = time(sim), regenMode = "Resprout",
                                                        speciesCode, numberOfRegen)]
        resproutRegenSummary <- setkey(resproutRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        resproutRegenSummary[,':='(speciesCode = species, species = NULL)]
        setnames(resproutRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbind(sim$postFireRegenSummary, resproutRegenSummary)
      }
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
      postFirePixel <- c(serotinyPixel, unique(newCohortData$pixelIndex))
      sim$postFirePixel <- postFirePixel # send it to a sim object
      rm(newCohortData)
    } else{
      sim$postFirePixel <- serotinyPixel
      postFireReproData <- postFireReproData
    }
  } else {
    postFireReproData <- postFireReproData
    sim$postFirePixel <- serotinyPixel
  }
  if(NROW(postFireReproData) > 0) {
    maxPixelGroup <- max(getValues(sim$pixelGroupMap))
    if(!is.null(sim$postFirePixel)){
      sim$pixelGroupMap[sim$postFirePixel] <- maxPixelGroup +
        as.numeric(as.factor(sim$ecoregionMap[sim$postFirePixel]))
      postFireReproData[, pixelGroup := maxPixelGroup + 
                          as.numeric(as.factor(postFireReproData$ecoregionGroup))]
    }
    sim$cohortData[,sumB := sum(B, na.rm = TRUE), by = pixelGroup]
    addnewcohort <- addNewCohorts(postFireReproData, sim$cohortData, sim$pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastFireYear <- time(sim)
  sim$firePixelTable <- firePixelTable
  return(invisible(sim))
}

biomassSuccessionLANDISNoDispersalSeeding = function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  if(sim$lastFireYear == round(time(sim))){ # if current year is both fire year and succession year
    # find new active pixel that remove successful postfire regeneration
    # since this is on site regeneration, all the burnt pixels can not seeding
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$firePixelTable$pixelIndex)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = time(sim),
                              successionTimestep = sim$successionTimestep) 
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[,.(speciesCode, sexualmature)],
                                                               speciesCode),
                                                        nomatch = 0]
  
  newCohortData <- sim$cohortData[age >= sexualmature]
  set(sim$cohortData, ,"sexualmature", NULL)
  set(newCohortData, ,c("sexualmature", "age", "B", "mortality", "aNPPAct"), NULL)
  siteShade <- setkey(data.table(calcSiteShade(time = round(time(sim)), sim$cohortData,
                                               sim$speciesEcoregion, sim$minRelativeB)), pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[siteShade, nomatch=0]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[,.(speciesCode, shadetolerance)],
                                                               speciesCode),
                                                        nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, ,c("shadetolerance", "lightProb", "siteShade", "sumB"), NULL)
  newCohortData <- unique(newCohortData, by = c("pixelGroup", "speciesCode"))
  
  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(newCohortData$pixelGroup)], pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)
  specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, establishprob, ecoregionGroup)],
                               ecoregionGroup, speciesCode)
  newCohortData <- newCohortData[specieseco_current, nomatch = 0]
  newCohortData <- newCohortData[establishprob %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, ,c("establishprob"), NULL)
  if(sim$calibrate == TRUE & NROW(newCohortData) > 0){
    newCohortData_summ <- newCohortData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                           numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][,.(species, seedingAlgorithm,
                                                                                  Year, numberOfReg)]
    sim$regenerationOutput <- rbind(sim$regenerationOutput, newCohortData_summ)
  }
  if(NROW(newCohortData) > 0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

biomassSuccessionLANDISUniversalDispersalSeeding = function(sim) {
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- Which(pixelGroupMap == 0, cell = TRUE)
  if(length(fire_nonRegPixels) > 0){
    pixelGroupMap[fire_nonRegPixels] <- as.numeric(as.factor(getValues(sim$ecoregionMap)[fire_nonRegPixels])) + max(getValues(pixelGroupMap))
  }
  if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(sim$cohortData, lastReg = sim$lastReg, simuTime = round(time(sim)),
                              successionTimestep = sim$successionTimestep) 
  species <- sim$species
  # all species can provide seed source, i.e. age>=sexualmature
  speciessource <- setkey(sim$species[,.(speciesCode, k = 1)], k)
  siteShade <- data.table(calcSiteShade(time = round(time(sim)), sim$cohortData, 
                                        sim$speciesEcoregion, sim$minRelativeB))
  activePixelGroup <- unique(data.table(pixelGroup = pixelGroupMap[tempActivePixel],
                                        ecoregionGroup = sim$ecoregionMap[tempActivePixel]),
                             by = "pixelGroup")
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table
  siteShade[is.na(siteShade), siteShade := 0]
  setkey(siteShade[, k := 1], k)
  # i believe this is the latest version how the landis guys calculate sufficient light
  # http://landis-extensions.googlecode.com/svn/trunk/succession-library/trunk/src/ReproductionDefaults.cs
  newCohortData <- siteShade[speciessource, allow.cartesian = TRUE][, k := NULL]
  newCohortData <- setkey(newCohortData, speciesCode)[setkey(sim$species[,.(speciesCode, shadetolerance)],
                                                             speciesCode),
                                                      nomatch = 0]
  newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight, newCohortData)
  newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0 , 1),]
  set(newCohortData, , c("siteShade", "lightProb", "shadetolerance"), NULL)
#   pixelGroupEcoregion <- unique(sim$cohortData, by = c("pixelGroup"))[,'.'(pixelGroup, sumB)]
  
  pixelsInfor <- setkey(data.table(pixelIndex = tempActivePixel,
                                   pixelGroup = getValues(pixelGroupMap)[tempActivePixel]), pixelGroup)
  pixelsInfor <- setkey(pixelsInfor[pixelGroup %in% unique(newCohortData$pixelGroup)], pixelGroup)
  newCohortData <- setkey(newCohortData, pixelGroup)[pixelsInfor, allow.cartesian = TRUE]
  newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)
  specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, establishprob, ecoregionGroup)],
                               ecoregionGroup, speciesCode)
  newCohortData <- newCohortData[specieseco_current, nomatch = 0]


  newCohortData <- newCohortData[establishprob %>>% runif(nrow(newCohortData), 0, 1),]
  set(newCohortData, ,"establishprob", NULL)
  if(sim$calibrate == TRUE){
    newCohortData_summ <- newCohortData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                       numberOfReg = length(pixelIndex)),
                                    by = speciesCode]
    newCohortData_summ <- setkey(newCohortData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                              nomatch = 0][,.(species, seedingAlgorithm,
                                                                              Year, numberOfReg)]
    sim$regenerationOutput <- rbind(sim$regenerationOutput, newCohortData_summ)
  }
  if(NROW(newCohortData)>0) {
    addnewcohort <- addNewCohorts(newCohortData, sim$cohortData, pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- addnewcohort$pixelGroupMap
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}

biomassSuccessionLANDISWardDispersalSeeding = function(sim) {
  #   cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  fire_nonRegPixels <- Which(pixelGroupMap == 0, cell = TRUE)
  if(length(fire_nonRegPixels) > 0){
    pixelGroupMap[fire_nonRegPixels] <- as.numeric(as.factor(getValues(sim$ecoregionMap)[fire_nonRegPixels])) + max(sim$cohortData$pixelGroup)
  }
  if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
    tempActivePixel <- sim$activePixelIndex[!(sim$activePixelIndex %in% sim$postFirePixel)]
  } else {
    tempActivePixel <- sim$activePixelIndex
  }
  sim$cohortData <- calculateSumB(cohortData = sim$cohortData,
                                  lastReg = sim$lastReg, simuTime = round(time(sim)),
                                  successionTimestep = sim$successionTimestep) 
  siteShade <- calcSiteShade(time = round(time(sim)), cohortData = sim$cohortData, 
                             sim$speciesEcoregion, sim$minRelativeB)
  activePixelGroup <- data.table(pixelGroup = unique(pixelGroupMap[tempActivePixel]))
  siteShade <- dplyr::left_join(activePixelGroup, siteShade, by = "pixelGroup") %>% data.table
  siteShade[is.na(siteShade),siteShade := 0]
  # Seed source cells:
  # 1. Select only sexually mature cohorts, then
  # 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  sim$cohortData <- setkey(sim$cohortData, speciesCode)[setkey(sim$species[,.(speciesCode, sexualmature)],
                                                               speciesCode),
                                                        nomatch = 0]
  matureCohorts <- setkey(sim$cohortData[age >= sexualmature] %>% unique(by = c("pixelGroup", "speciesCode")),
                          speciesCode)
  matureCohorts <- matureCohorts[,.(pixelGroup, speciesCode)]
  set(sim$cohortData, ,"sexualmature", NULL)
  
  if(NROW(matureCohorts)>0){
    seedSource <- setkey(sim$species[,list(speciesCode, seeddistance_eff, seeddistance_max)], speciesCode) %>%
      .[matureCohorts]
    setkey(seedSource, speciesCode)
    #  Seed Receiving cells:
    #  1. Must be sufficient light 
    # seed receive just for the species that are seed source
    tempspeices <- sim$species[speciesCode %in% unique(matureCohorts$speciesCode),][
      ,.(speciesCode, shadetolerance, seeddistance_eff, seeddistance_max)]
    seedReceive = setkey(tempspeices[,c(k = 1, .SD)], k)[setkey(siteShade[,c(k = 1, .SD)], k), allow.cartesian=TRUE][
      ,k:=NULL]
    seedReceive <- assignLightProb(sufficientLight = sim$sufficientLight, seedReceive)
    set(seedReceive, ,"siteShade", NULL)
    seedReceive <- seedReceive[lightProb %>>% runif(nrow(seedReceive), 0, 1), ][
      ,.(pixelGroup, speciesCode, seeddistance_eff, seeddistance_max)]
    setkey(seedReceive, speciesCode)
    
    
    # 3. Remove any species from the seedSource that couldn't regeneration anywhere on the map due to insufficient light
    #    (info contained within seedReceive)
    # this is should be a inner join, needs to specify the nomatch=0, nomatch = NA is default that sugest the full joint.
    seedSource <- seedSource[speciesCode %in% unique(seedReceive$speciesCode),]
    
    # Add inSituReceived data.table from the inSitu seeding function or event
    inSituReceived <- data.table(fromInit = numeric(), species = character())
    
    # it could be more effecient if pixelGroupMap is reduced map by removing the pixels that have successful postdisturbance regeneration
    # and the inactive pixels
    # how to subset the reducedmap
    if(sim$lastFireYear == round(time(sim))){ # the current year is both fire year and succession year
      inactivePixelIndex <- c(sim$inactivePixelIndex, sim$postFirePixel)
    } else {
      inactivePixelIndex <- sim$inactivePixelIndex
    }
    if(length(inactivePixelIndex) > 0){
      reducedPixelGroupMap <- pixelGroupMap
      reducedPixelGroupMap[inactivePixelIndex] <- NA
    } else {
      reducedPixelGroupMap <- pixelGroupMap
    }
    source('~/GitHub/nrv-succession/code blitz succession/modeltesting-data/seedDispersalLANDIS.R')
    seedingData <- LANDISDisp(sim, dtRcv=seedReceive, plot.it = FALSE,
                              dtSrc = seedSource, inSituReceived = inSituReceived,
                              species = sim$species,
                              reducedPixelGroupMap,
                              maxPotentialsLength = 3e5,
                              verbose = globals(sim)$verbose)
    rm(seedReceive, seedSource)
    if(NROW(seedingData) > 0) {
      seedingData$ecoregionGroup <- getValues(sim$ecoregionMap)[seedingData$pixelIndex]
      seedingData <- setkey(seedingData, ecoregionGroup, speciesCode)
      specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
      specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                      .(speciesCode, establishprob, ecoregionGroup)],
                                   ecoregionGroup, speciesCode)
      seedingData <- seedingData[specieseco_current, nomatch = 0]
      
      seedingData <- seedingData[establishprob >= runif(nrow(seedingData), 0, 1), ]
      set(seedingData, ,"establishprob", NULL)
      if(sim$calibrate == TRUE){
        seedingData_summ <- seedingData[,.(seedingAlgorithm = sim$seedingAlgorithm, Year = round(time(sim)),
                                           numberOfReg = length(pixelIndex)),
                                        by = speciesCode]
        seedingData_summ <- setkey(seedingData_summ, speciesCode)[setkey(sim$species[,.(species,speciesCode)], speciesCode),
                                                                  nomatch = 0][,.(species, seedingAlgorithm,
                                                                                  Year, numberOfReg)]
        sim$regenerationOutput <- rbind(sim$regenerationOutput, seedingData_summ)
      }
      addnewcohort <- addNewCohorts(seedingData, cohortData = sim$cohortData, pixelGroupMap,
                                    time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
      sim$cohortData <- addnewcohort$cohortData
      sim$pixelGroupMap <- addnewcohort$pixelGroupMap
    }
  }
  sim$lastReg <- round(time(sim))
  return(invisible(sim))
}


biomassSuccessionLANDISSummaryRegen = function(sim){
  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  names(pixelGroupMap) <- "pixelGroup"
  # please note that the calculation of reproduction is based on successioinTime step interval,
  pixelAll <- cohortData[age == 1,
                         .(uniqueSumReproduction = sum(B, na.rm=TRUE)),
                         by = pixelGroup]
  if(NROW(pixelAll)>0){
    reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumReproduction")
    setColors(reproductionMap) <- c("light green", "dark green")
  } else {
    reproductionMap <- setValues(pixelGroupMap, 0)
  }
  rm(pixelAll)
  sim$reproductionMap <- reproductionMap
  return(invisible(sim))
}

biomassSuccessionLANDISPlot = function(sim) {
  biomassMap <- sim$biomassMap
  ANPPMap <- sim$ANPPMap
  mortalityMap <- sim$mortalityMap
  reproductionMap <- sim$reproductionMap
  dev(4)
  Plot(biomassMap, ANPPMap, mortalityMap, reproductionMap,
       new = ifelse(round(time(sim)) == sim$successionTimestep, TRUE, FALSE),
       speedup = 1)
  seekViewport("ANPPMap")
  grid.rect(1.02,1.07,width = 0.4,height = 0.1,gp = gpar(fill = "white", col = "white"))
  grid.text(label = paste0("Year = ",round(time(sim))),x = 1.02, y = 1.07)
  
  # landWebOutput(cohortdata = sim$cohortData, pixelgroupmap = sim$pixelGroupMap,
  #               species = sim$species, year = time(sim), outputpath = paths(sim)$outputPath)
  sim$timeRecorder <- rbind(sim$timeRecorder,
                            data.table(simuTime = as.numeric(time(sim)),
                                       systemTime = as.numeric(Sys.time())))
  if(is.null(sim$produceMap)){sim$produceMap <- FALSE}
  if(sim$produceMap == TRUE){
    writeRaster(biomassMap, paste("biomassMap", round(time(sim)), ".tif",sep=""), datatype='INT4S',
                overwrite = TRUE)
    writeRaster(ANPPMap, paste("ANPPMap", round(time(sim)), ".tif",sep=""), datatype = 'INT4S',
                overwrite = TRUE)
    writeRaster(mortalityMap, paste("mortalityMap", round(time(sim)), ".tif", sep=""),datatype = 'INT4S',
                overwrite = TRUE)
    writeRaster(reproductionMap, paste("reproductionMap", round(time(sim)), ".tif",sep=""), datatype = 'INT4S',
                overwrite = TRUE)
  }
  return(invisible(sim))
}

biomassSuccessionLANDISSave = function(sim) {
  saveFiles(sim)
  return(invisible(sim))
}

biomassSuccessionLANDISCohortAgeReclassification = function(sim) {
  if(time(sim) != 0){
    cohortData <- sim$cohortData
    cohortData <- ageReclassification(cohortData = cohortData, successionTimestep = successionTimestep,
                                      stage = "mainSimulation")
    sim$cohortData <- cohortData
    return(invisible(sim))
  } else {
    return(invisible(sim))
  }
  
}

updateSpeciesEcoregionAttributes <- function(speciesEcoregion, time, cohortData){
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco:=max(maxB), by = ecoregionGroup]
  
  cohortData <- setkey(cohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch=0]
  return(cohortData)
}

updateSpeciesAttributes <- function(species, cohortData){
  # to assign longevity, mortalityshape, growthcurve to cohortData
  species_temp <- setkey(species[,.(speciesCode, longevity, mortalityshape,
                                    growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch=0]
  return(cohortData)
}

ageReclassification <- function(cohortData, successionTimestep, stage){
  if(stage == "spinup"){
    # for spin up stage
    cohortData[age == successionTimestep+1, age:=successionTimestep]
  } else {
    # non- spinup stage
    targetData <- cohortData[age <= successionTimestep, ]
    targetData <- targetData[,.(ecoregionGroup = mean(ecoregionGroup),
                                age = successionTimestep - 1,
                                B = sum(B, na.rm = TRUE), 
                                mortality = sum(mortality, na.rm = TRUE), 
                                aNPPAct = sum(aNPPAct, na.rm = TRUE)),
                             by = .(pixelGroup, speciesCode)]
    targetData <- targetData[,.(pixelGroup, ecoregionGroup, speciesCode, age,
                                B, mortality, aNPPAct)]
    cohortData <- cohortData[age >= successionTimestep + 1]
    cohortData <- rbindlist(list(cohortData, targetData))
  }
  return(cohortData)
}

calculateAgeMortality <- function(cohortData, stage, spinupMortalityfraction){
  # for age-related mortality calculation
  if(stage == "spinup"){
    cohortData[age > 0, mAge:=B*(exp((age)/longevity*mortalityshape)/exp(mortalityshape))]
    cohortData[age > 0, mAge:=mAge+B*spinupMortalityfraction]
    cohortData[age > 0, mAge:=pmin(B,mAge)]
  } else {
    set(cohortData, ,"mAge",
        cohortData$B*(exp((cohortData$age)/cohortData$longevity*cohortData$mortalityshape)/exp(cohortData$mortalityshape)))
    set(cohortData, ,"mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

calculateANPP <- function(cohortData,stage){
  if(stage=="spinup"){
    cohortData[age>0,aNPPAct:=maxANPP*exp(1)*(bAP^growthcurve)*exp(-(bAP^growthcurve))*bPM]
    cohortData[age>0,aNPPAct:=pmin(maxANPP*bPM,aNPPAct)]
  } else {
    set(cohortData, ,"aNPPAct",
        cohortData$maxANPP*exp(1)*(cohortData$bAP^cohortData$growthcurve)*exp(-(cohortData$bAP^cohortData$growthcurve))*cohortData$bPM)
    set(cohortData, ,"aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM,cohortData$aNPPAct))
  }
  return(cohortData)
}

calculateGrowthMortality <- function(cohortData, stage){
  if(stage == "spinup"){
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1+bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, , "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, , "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
}

calculateSumB <- function(cohortData, lastReg, simuTime, successionTimestep){
  # this function is used to calculate total stand biomass that does not include the new cohorts
  # the new cohorts are defined as the age younger than simulation time step
  # reset sumB
  set(cohortData, ,"sumB", 0L)
  if(simuTime == lastReg + successionTimestep - 2){
    sumBtable <- cohortData[age > successionTimestep,
                            .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
  } else {
    sumBtable <- cohortData[age >= successionTimestep,
                            .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
  }
  cohortData <- dplyr::left_join(cohortData, sumBtable, by = "pixelGroup") %>% data.table
  cohortData[is.na(tempsumB), tempsumB:=as.integer(0L)][,':='(sumB = tempsumB, tempsumB = NULL)]
  rm(sumBtable)
  return(cohortData)
}


calculateCompetition <- function(cohortData,stage){
  # two competition indics are calculated bAP and bPM
  if(stage=="spinup"){
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, , "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier) ,by=pixelGroup]
    cohortData[age > 0, bPM := cMultiplier/cMultTotal]
    set(cohortData, , c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, , "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))
    set(cohortData, , "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, , "bPot", NULL)
    set(cohortData, , "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))
    cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
    set(cohortData, , "bPM", cohortData$cMultiplier/cohortData$cMultTotal)
    set(cohortData, , c("cMultiplier", "cMultTotal"), NULL)
  }
  return(cohortData)
}

calcSiteShade <- function(time, cohortData, speciesEcoregion, minRelativeB) {
  # the siteshade was calculated based on the codes:
  # https://github.com/LANDIS-II-Foundation/Extensions-Succession/blob/master/biomass-succession/trunk/src/PlugIn.cs
  if(nrow(cohortData[age > 5,]) > 0){
    bAMterm1 <- cohortData [age > 5, ':='(prevMortality = sum(mortality, na.rm = TRUE), 
                                          sumB = sum(B, na.rm = TRUE)),
                            by = .(pixelGroup, ecoregionGroup)]
    bAMterm1[is.na(sumB), sumB := 0]
    bAMterm1[is.na(prevMortality), prevMortality := 0]
    bAMterm1 <- unique(bAMterm1, by = c("pixelGroup", "ecoregionGroup"))
    set(cohortData, ,"prevMortality", NULL)
  } else {
    bAMterm1 <- unique(cohortData, by=c("pixelGroup", "ecoregionGroup"))[
      ,.(pixelGroup, ecoregionGroup)][
        ,':='(prevMortality = 0, sumB = 0)]
  }
  #bAM <- data.table(speciesEcoregion)[year <= time(sim) & (year > (time(sim)-sim$successionTimestep))]
  bAM <- speciesEcoregion[year <= time]
  bAM <- bAM[year == max(bAM$year)]
  bAM <- bAM[, .(maxMaxB = max(maxB)), by = ecoregionGroup]
  setkey(bAM, ecoregionGroup)
  setkey(bAMterm1, ecoregionGroup)
  bAMterm1 <- bAMterm1[bAM, nomatch = 0]
  bAMterm1[, sumB := pmin((maxMaxB - prevMortality), sumB)]
  bAMterm1[, bAM := sumB/maxMaxB]
  minRelativeB <- data.table(minRelativeB)
  setkey(minRelativeB, ecoregionGroup)
  bAMterm1 <- bAMterm1[minRelativeB, nomatch = 0]
  bAMterm1$bAM <- round(bAMterm1$bAM, 3)
  bAMterm1[,siteShade := cut(bAM,c(0, X1, X2, X3, X4, X5, 1),
                             labels = FALSE, right = FALSE, include.lowest = TRUE) - 1,by = pixelGroup]
  bAMterm1 <- bAMterm1[,.(pixelGroup, siteShade)]
  return(bAMterm1)
}

assignLightProb <- function(sufficientLight, newCohortData){
  newCohortData[ , lightProb := sufficientLight[cbind(shadetolerance, siteShade + 2)]]
}

addNewCohorts <- function(newCohortData, cohortData, pixelGroupMap, time, speciesEcoregion){
  # this function is for 1) adding new cohort data into cohortdata
  # 2) assign initial biomass and age for new cohort
  # 3) assign the new pixelgroup to the pixels that have new cohort
  # 4) update the pixelgroup map
  # newCohortData must have the original pixelgroup, regenerated species and pixelindex
  # it also would be better if it has the collums of cohortData plus pixelindex
  newCohortData$pixelGroup <- getValues(pixelGroupMap)[newCohortData$pixelIndex]
  set(newCohortData, , "temppixelGroup", as.integer(as.factor(newCohortData$pixelGroup)))
  set(newCohortData, , "speciesposition", 2^(newCohortData$speciesCode))
  # newCohortDataExtra is used to connect the original pixelGroup to the newPixelGroup
  newCohortDataExtra <- newCohortData[, .(community = sum(speciesposition),
                                          pixelGroup = mean(pixelGroup),
                                          temppixelGroup = mean(temppixelGroup)), by = pixelIndex]
  set(newCohortData, , c("temppixelGroup", "speciesposition"), NULL)
  set(newCohortDataExtra, , "community",
      as.numeric(as.factor(newCohortDataExtra$community)))
  if(max(newCohortDataExtra$community) > max(newCohortDataExtra$temppixelGroup)){
    set(newCohortDataExtra, ,  "community",
        newCohortDataExtra$community + max(newCohortDataExtra$community)*newCohortDataExtra$temppixelGroup)
  } else {
    set(newCohortDataExtra, , "community",
        newCohortDataExtra$temppixelGroup + max(newCohortDataExtra$temppixelGroup)*newCohortDataExtra$community)
  }
  maxPixelGroup <- max(max(cohortData$pixelGroup), max(getValues(pixelGroupMap)))
  set(newCohortDataExtra, ,  "newpixelGroup",
      as.numeric(as.factor(newCohortDataExtra$community)) + maxPixelGroup)
  set(newCohortDataExtra, , c("community", "temppixelGroup"), NULL)
  setkey(newCohortData, pixelIndex)
  setkey(newCohortDataExtra, pixelIndex)
  newCohortData <- newCohortData[,pixelGroup:=NULL][newCohortDataExtra][,pixelIndex := NULL]
  newCohortData <- unique(newCohortData, by = c("newpixelGroup", "speciesCode"))
  sumTable <- cohortData[, .(pixelGroup,sumB)] %>%
    unique(, by = c("pixelGroup"))
  newCohortData <- dplyr::left_join(newCohortData, sumTable, by = "pixelGroup") %>% data.table
  newCohortData[is.na(sumB),sumB:=0]
  set(cohortData, ,"sumB", NULL)
  set(newCohortData, , "pixelGroup", newCohortData$newpixelGroup)
  set(newCohortData, , c("newpixelGroup"), NULL)
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco:=max(maxB), by = ecoregionGroup]
  newCohortData <- setkey(newCohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch=0]
  set(newCohortData, , "age", 1)
  set(newCohortData, ,"B",
      as.integer(pmax(1, newCohortData$maxANPP*exp(-1.6*newCohortData$sumB/newCohortData$maxB_eco))))
  set(newCohortData, ,"B", as.integer(pmin(newCohortData$maxANPP, newCohortData$B)))
  
  newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age, B,
                                    mortality = 0, aNPPAct = 0)]
  newCohortDataExtra2 <- unique(newCohortDataExtra, by = c("pixelGroup", "newpixelGroup"))
  # newCohortDataExtra2 is further simplified form
  # identify which pixelGroups in cohortData have new regeneration
  existingData <- cohortData[pixelGroup %in% unique(newCohortDataExtra2$pixelGroup)]
  setkey(newCohortDataExtra2, pixelGroup)
  setkey(existingData, pixelGroup)
  existingData <- existingData[newCohortDataExtra2, allow.cartesian = TRUE]
  existingData <- existingData[!is.na(ecoregionGroup)]
  set(existingData, ,"pixelGroup", existingData$newpixelGroup)
  set(existingData, ,c("pixelIndex", "newpixelGroup"), NULL)
  existingData <- unique(existingData, by = c("pixelGroup", "speciesCode", "age"))
  rm(newCohortDataExtra2)
  cohortData <- setkey(rbindlist(list(cohortData, newCohortData, existingData)),
                       pixelGroup, speciesCode, age)
  pixelGroupMap[as.integer(newCohortDataExtra$pixelIndex)] <- newCohortDataExtra$newpixelGroup
  
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)),]
  pixelGroupMap_new <- pixelGroupMap
  
  temppixelIndex11 <- which(!(getValues(pixelGroupMap) %in% c(0, -1)))
  pixelGroupMap_new[temppixelIndex11] <- as.numeric(as.factor(pixelGroupMap[temppixelIndex11]))
  pixelGroupConnection <- data.table(pixelGroup = pixelGroupMap[temppixelIndex11],
                                     newPixelGroup = pixelGroupMap_new[temppixelIndex11]) %>%
    unique(by = "pixelGroup")
  setkey(pixelGroupConnection, pixelGroup)
  setkey(cohortData, pixelGroup)
  cohortData <- cohortData[pixelGroupConnection, nomatch = 0]
  set(cohortData, , "pixelGroup", cohortData$newPixelGroup)
  set(cohortData, , "newPixelGroup", NULL)
  pixelGroupMap <- pixelGroupMap_new
  return(list(cohortData = cohortData,pixelGroupMap = pixelGroupMap))
}


updateOutputs <- function(sim, biomassMap, ANPPMap, mortalityMap, reproductionMap){
  simulationSummaryTable <- data.table(pixelGroup = sim$pixelGroupMap[sim$activePixelIndex],
                                       Ecoregion = sim$ecoregionMap[sim$activePixelIndex],
                                       Biomass = biomassMap[sim$activePixelIndex],
                                       ANPP = ANPPMap[sim$activePixelIndex],
                                       Mortality = mortalityMap[sim$activePixelIndex],
                                       Regeneration = reproductionMap[sim$activePixelIndex])
  simulationSummaryTable[is.na(simulationSummaryTable)] <- 0
  overalloutput <- simulationSummaryTable[,.(NofCell = length(Biomass),
                                             Year = round(time(sim)),
                                             Biomass = round(as.numeric(mean(Biomass))),
                                             ANPP = round(as.numeric(mean(ANPP))),
                                             Mortality = round(as.numeric(mean(Mortality))),
                                             Regeneration = round(as.numeric(mean(Regeneration)))),
                                          by = "Ecoregion"]
  simulationOutput <- rbind(sim$simulationOutput, overalloutput)
  outputbyspecies <- setkey(simulationSummaryTable[,.(pixelGroup, Ecoregion)][
    ,nCell:=length(pixelGroup), by = Ecoregion], pixelGroup)
  cohortData <- sim$cohortData
  setkey(cohortData, pixelGroup)
  outputbyspecies <- outputbyspecies[cohortData, nomatch = NA, allow.cartesian = TRUE]
  outputbyspecies <- setkey(outputbyspecies[,.(nCell = mean(nCell), Year = round(time(sim)), 
                                               Biomass = sum(B, na.rm = TRUE),
                                               ANPP = sum(aNPPAct, na.rm=TRUE),
                                               Mortality = sum(mortality, na.rm=TRUE)),
                                            by = c("Ecoregion", "speciesCode")],
                            speciesCode)
  
  outputbyspecies <- outputbyspecies[setkey(sim$species[,.(species, speciesCode)], speciesCode),
                                     nomatch = 0]
  setnames(outputbyspecies, "species", "Species")
  outputbyspecies <- outputbyspecies[,.(Ecoregion, nCell, Species, Year, 
                                        Biomass, ANPP, Mortality)]
  simulationOutput_byspecies <- rbind(sim$simulationOutput_byspecies, outputbyspecies)
  return(list(simulationOutput = simulationOutput, simulationOutput_byspecies = simulationOutput_byspecies))
}

landWebOutput <- function(cohortdata, pixelgroupmap, species, year, outputpath){
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
}

