
origDir <- getwd()
setwd("C:/Eliot/GitHub/nrv-succession/code blitz succession/modeltesting-data/")
#download.file(url="https://raw.githubusercontent.com/PredictiveEcology/SpaDES/eliotSpread_landis/R/seedDispersalLANDIS.R",
#              destfile="seedDispersalLANDIS.R")
#source("seedDispersalLANDIS.R")
setwd(origDir)

### Specify module (and dependencies) definitions:
###
### name:         biomassSuccessionLANDIS
###
### description:  <simulate biomass succession modified from LANDIS II>
###
### keywords:     <simulation, biomass, succession>
###
### authors:      <author name(s) and email address(es)>
###
### version:      0.0.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     NA
###
### citation:     NA
###
### reqdPkgs:     SpaDES, raster,sp,data.table, dplyr,ggplot
###
### inputObjects: objectName: NA
###               objectClass: NA
###               other: NA
###
### outputObjects: objectName: NA
###                objectClass: NA
###                other: NA
###
### biomassSuccessionLANDIS module metadata
defineModule(sim, list(
  name="biomassSuccessionLANDIS",
  description="simulate biomass succession modified from LANDIS II",
  keywords=c("forest succession"),
  authors=c(person(c("Yong"), "Luo", email="Yong.Luo@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Jean"), "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  version=numeric_version("0.0.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=60*60*24*365.25,
  citation=list(),
  reqdPkgs=list("raster","sp","data.table", "dplyr","ggplot2"),
  parameters=rbind(
    defineParameter("growthInterval", "numeric", 1.0),
    defineParameter("mortalityInterval", "numeric", 1.0),
    defineParameter("postFireReproductionInterval", "numeric", 5.0),
    defineParameter("seedingReproductionInterval", "numeric", 10.0),
    defineParameter("biomassUpdatingInterval", "numeric", 1.0),
    defineParameter("simulationSummaryInterval", "numeric", 10.0),
    defineParameter("growthInitialTime", "numeric", 0.01),
    defineParameter("mortalityInitialTime", "numeric", 0.02),
    defineParameter("biomassUpdatingInitialTime", "numeric", 0.03),
    defineParameter("postFireReproductionInitialTime", "numeric", 0.04),
    defineParameter("seedingReproductionInitialTime", "numeric", 0.05),
    defineParameter("simulationSummaryInitialTime", "numeric", 0.06),
    defineParameter(".plotInitialTime", "numeric", 0.07),
    defineParameter(".plotInterval", "numeric", 10),
    defineParameter(".saveInitialTime", "numeric", 0.08),
    defineParameter(".saveInterval", "numeric", 10)),
  inputObjects=data.frame(objectName=c("growthInterval"),
                          objectClass=c("numeric"),
                          other=rep(NA_character_, 1L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("cohortData"),
                           objectClass=c("data.table"),
                           other=rep(NA_character_, 1L), stringsAsFactors=FALSE)
))

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.biomassSuccessionLANDIS = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    depends <- NULL
    ### (use `checkObject` or similar)
    
    if(reloadModuleLater(sim,depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "firesuccession", "init")
    }else{
      # do stuff for this event
      sim <- biomassSuccessionLANDISInit(sim) # to generate initial cohort data from initialCommunities
      # and to presimulate biomass during the spinup
      # schedule future event(s)
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$growthInitialTime, "biomassSuccessionLANDIS", "growth")
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$mortalityInitialTime, "biomassSuccessionLANDIS", "mortality")
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$biomassUpdatingInitialTime, "biomassSuccessionLANDIS", "biomassUpdating")
      
      if(!is.null(sim$fireMap)){ # anything related to fire disturbance
        sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$postFireReproductionInitialTime, "biomassSuccessionLANDIS", "postFireReproduction")
      }
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$seedingReproductionInitialTime, "biomassSuccessionLANDIS", "seedingReproduction")
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$simulationSummaryInitialTime, "biomassSuccessionLANDIS", "simulationSummary")
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$.plotInitialTime, "biomassSuccessionLANDIS", "plot")
      sim <- scheduleEvent(sim, simParams(sim)$biomassSuccessionLANDIS$.saveInitialTime, "biomassSuccessionLANDIS", "save")
      
    }
  } else if (eventType=="growth") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- biomassSuccessionLANDISGrowth(sim)
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$growthInterval,
                         "biomassSuccessionLANDIS", "growth")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="mortality") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- biomassSuccessionLANDISMortality(sim)
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$mortalityInterval,
                         "biomassSuccessionLANDIS", "mortality")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="biomassUpdating") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- biomassSuccessionLANDISBiomassUpdating(sim)
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$biomassUpdatingInterval,
                         "biomassSuccessionLANDIS", "biomassUpdating")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="postFireReproduction" & !is.null(sim$fireMap)) {
    # We assume that fireMap is a map of the most recent fires that should be used by
    #  biomassSuccessionLANDIS post fire regeneration.
    
    sim <- biomassSuccessionLANDISPostFireReproduction(sim)
    
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$postFireReproductionInterval,
                         "biomassSuccessionLANDIS", "postFireReproduction")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="seedingReproduction") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    #sim <- biomassSuccessionLANDISRegeneration(sim)
    sim <- biomassSuccessionLANDISSeedingReproduction(sim)
    
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$seedingReproductionInterval,
                         "biomassSuccessionLANDIS", "seedingReproduction")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="simulationSummary"){
    sim <- biomassSuccessionLANDISSimulationSummary(sim)
    
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$simulationSummaryInterval,
                         "biomassSuccessionLANDIS", "simulationSummary")
  } else if (eventType=="plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- biomassSuccessionLANDISPlot(sim)
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$.plotInterval,
                         "biomassSuccessionLANDIS", "plot")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType=="save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- biomassSuccessionLANDISSave(sim)
    
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$biomassSuccessionLANDIS$.saveInterval,
                         "biomassSuccessionLANDIS", "save")
    
    # ! ----- STOP EDITING ----- ! #
  }
  else {
    warning(paste("Undefined event type: '", simEvents(sim)[1, "eventType", with=FALSE],
                  "' in module '", simEvents(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  
  return(invisible(sim))
}

### template initilization
biomassSuccessionLANDISInit = function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  
  #   browser()
  #   mb <- microbenchmark(times=30L,
  #   communities = {getGlobal("initialCommunities") %>%
  #     gather(key=cohort, value=age, -mapcode,-description,-species,na.rm=TRUE) %>%
  #     mutate(age=ceiling(age/getGlobal("successionTimestep")*getGlobal("successionTimestep") ))%>%
  #     arrange(mapcode,species) %>%
  #     group_by(mapcode,species) %>%
  #     filter(!duplicated(age)) %>%
  #     dplyr::select(-cohort) %>%
  #     dplyr::rename(communityGroup=mapcode) %>%
  #     data.table}, 
  communities <- sim$initialCommunities %>%
    gather(key=cohort, value=age, -mapcode,-description,-species,na.rm=TRUE) %>%
    data.table %>%
    .[,`:=`(age=ceiling(age/sim$successionTimestep*sim$successionTimestep ),
            communityGroup=mapcode,
            mapcode=NULL)] %>%
    unique(., by = c("communityGroup", "species", "age"))
  
  
  
  nrowCommunities <- nrow(communities)
  
  communityMap <- sim$initialCommunitiesMap
  pixelGroupFactor <- 10^ceiling(log10((max(getValues(initialCommunitiesMap+1)))))
  pixelGroupMap <- sim$initialCommunitiesMap + sim$ecoregionMap*pixelGroupFactor
  uniquePixelGroup <- sort(unique(getValues(pixelGroupMap)))
  cohortData <- rbindlist(list(communities,communities))
  set(cohortData, ,"ecoregionGroup", rep(1:2,each=nrowCommunities))
  set(cohortData, ,"pixelGroup", cohortData$communityGroup+cohortData$ecoregionGroup*pixelGroupFactor)
  set(cohortData, , "B", 0)
  cohortData <- cohortData[, .(pixelGroup,ecoregionGroup,species,age,B)] # removed the collumn cummunityGroup
  # the cohortData here is a full joint table of community Group and ecoregion Group
  # some redundant pixelGroups are removed, because they are not present on the pixelGroupMap
  cohortData <- cohortData[pixelGroup==unique(getValues(pixelGroupMap)),] 
  
  speciesEcoregion <- sim$speciesEcoregion
  specieseco_temp <- setkey(data.table(speciesEcoregion)[year <= simCurrentTime(sim) & (year > (simCurrentTime(sim)-successionTimestep))],
                            ecoregionGroup,species)
  specieseco_temp[,maxB_eco:=max(maxB),by=ecoregionGroup]
  specieseco_temp[,":="(year=NULL,establishprob=NULL,ecoregion=NULL)]
  setkey(cohortData, ecoregionGroup,species)
  cohortData <- cohortData[specieseco_temp,nomatch=0]
  species <- sim$species
  species_temp <- setkey(data.table(species)[,.(species,longevity,mortalityshape,growthcurve)],
                         key="species")
  setkey(cohortData,species)
  cohortData <- cohortData[species_temp, nomatch=0]
  rm(specieseco_temp,species_temp,nrowCommunities,pixelGroupFactor,uniquePixelGroup)
  
  
  
  maxAge <- max(cohortData$age) # determine the pre-simulation length
  
  set(cohortData, ,"mortality", 0) # assign initial mortality
  set(cohortData, ,"prevMortality", 0)
  
  
  
  for(presimuT in (maxAge):0) {
    # sum of biomass for all existing cohorts in each site
    cohortData[age>=presimuT,     sumB:=sum(B,na.rm=TRUE),by=pixelGroup]
    # add new cohort Biomass, the initial biomass should be between 1 and maxANPP
    cohortData[age==presimuT,     B:=pmax(1,0.025*maxB*exp(-1.6*sumB/maxB_eco))]
    #
    cohortData[age==presimuT,     B:=pmin(maxANPP,B)]
    # calculate the maximum mortality and sum of biomass, in each site
    cohortData[age>=presimuT,`:=`(prevMortality=max(prevMortality, na.rm=TRUE),
                                  sumB=sum(B,na.rm=TRUE)),by=pixelGroup]
    # calculate biomass Potential, for each cohort
    cohortData[age>=presimuT,     bPot:=pmax(prevMortality,pmax(0,maxB-sumB))]
    
    # cohort Annual NPP
    # //  Actual ANPP: equation (4) from Scheller & Mladenoff, 2004.
    # double actualANPP =
    #  maxANPP * Math.E * Math.Pow(B_AP, growthShape) * Math.Exp(-1 * Math.Pow(B_AP, growthShape)) * B_PM;
    cohortData[age>=presimuT,`:=`(bAP=B/bPot)]
    # based on the C# codes in LANDIS II, the calculation for competition index bPM is different from documentation.
    
    cohortData[age>=presimuT,cMultiplier:=pmax(B^0.95,1)]
    cohortData[age>=presimuT,cMultTotal:=sum(cMultiplier),by=pixelGroup]
    cohortData[age>=presimuT,bPM:=cMultiplier/cMultTotal]
    cohortData[,':='(bPot=NULL,cMultiplier=NULL,cMultTotal=NULL)]
    # for growth, aNPPAct can not exceed the limit set by maximum ANPP times the ratio of potential to maximum biomass
    
    # Calculated actual ANPP can not exceed the limit set by the
    # maximum ANPP times the ratio of potential to maximum biomass.
    # This down regulates actual ANPP by the available growing space.
    cohortData[age>=presimuT,aNPPAct:=maxANPP*exp(1)*(bAP^growthcurve)*exp(-(bAP^growthcurve))*bPM]
    cohortData[age>=presimuT,aNPPAct:=pmin(maxANPP*bPM,aNPPAct)]
    
    cohortData[age>=presimuT & bAP>1.0, mBio:=maxANPP*bPM]
    cohortData[age>=presimuT & bAP<=1.0,mBio:=maxANPP*(2.0*bAP)/(1.0+bAP)*bPM]
    # Mortality should not exceed the amount of living biomass
    cohortData[age>=presimuT,mBio:=pmin(B,mBio)]
    # Calculated actual ANPP can not exceed the limit set by the
    # maximum ANPP times the ratio of potential to maximum biomass.
    # This down regulates actual ANPP by the available growing space.
    cohortData[age>=presimuT,mBio:=pmin(maxANPP*bPM,mBio)]
    # For age-related mortality
    cohortData[age>=presimuT,mAge:=B*(exp((age-presimuT+1)/longevity*mortalityshape)/exp(mortalityshape))
               +B*sim$spinupMortalityfraction]
    cohortData[age>=presimuT,mAge:=pmin(B,mAge)]
    
    cohortData[age>=presimuT,     mortality:=mBio+mAge]
    cohortData[,':='(mBio=NULL,mAge=NULL)]
    #Biomass Updating
    cohortData[age>=presimuT,     B:=B+aNPPAct-mortality]
    # site mortality, sum of cohort mortality
    cohortData[age>=presimuT,     prevMortality:=sum(mortality,na.rm=TRUE),by=pixelGroup]
    cohortData[,':='(aNPPAct=NULL,bAP=NULL,bPM=NULL,sumB=NULL)]
  }
  rm(presimuT,maxAge)
  
  overallChange <- data.table(Year=numeric(), Biomass=numeric(), ANPP=numeric(), Mortality=numeric(), Reproduction=numeric())
  overallChangeByEcoregion <- data.table(Ecoregion=numeric(), Year=numeric(), Biomass=numeric(), 
                                         ANPP=numeric(), Mortality=numeric(), Reproduction=numeric())
  sim$overallChange <- overallChange
  sim$overallChangeByEcoregion <- overallChangeByEcoregion
  sim$cohortData <- cohortData
  sim$pixelGroupMap <- pixelGroupMap
  return(invisible(sim))
}
biomassSuccessionLANDISInitMem <- memoise(biomassSuccessionLANDISInit)

### template for your event1
biomassSuccessionLANDISGrowth = function(sim) {
  cohortData <- sim$cohortData
  cohortData[,':='(sumB=sum(B,na.rm=TRUE),prevMortality=sum(mortality,na.rm=TRUE)),by=pixelGroup]
  set(cohortData, , "bPot", pmax(cohortData$prevMortality,pmax(0,cohortData$maxB-cohortData$sumB)) )
  set(cohortData, , "bAP", cohortData$B/cohortData$bPot)
  set(cohortData, , "cMultiplier", pmax(cohortData$B^0.95,1))
  cohortData[,cMultTotal:=sum(cMultiplier),by=pixelGroup]
  set(cohortData, ,"bPM", cohortData$cMultiplier/cohortData$cMultTotal)
  cohortData[,":="(prevMortality=NULL,bPot=NULL,cMultiplier=NULL,cMultTotal=NULL)]
  #### the below two lines of codes are to calculate actual ANPP
  set(cohortData, ,"aNPPAct",
      cohortData$maxANPP*exp(1)*(cohortData$bAP^cohortData$growthcurve)*exp(-(cohortData$bAP^cohortData$growthcurve))*cohortData$bPM)
  set(cohortData, ,"aNPPAct", pmin(cohortData$maxANPP*cohortData$bPM,cohortData$aNPPAct))
  sim$cohortData <- cohortData
  return(invisible(sim))
}

biomassSuccessionLANDISMortality = function(sim) {
  cohortData <- sim$cohortData
  ## for bio-related mortality
  cohortData[bAP>1.0, mBio:=maxANPP*bPM]
  cohortData[bAP<=1.0,mBio:=maxANPP*(2.0*bAP)/(1.0+bAP)*bPM]
  ## for age-related mortality
  set(cohortData, ,"mAge", cohortData$B*(exp((cohortData$age)/
                                               cohortData$longevity*cohortData$mortalityshape)/exp(cohortData$mortalityshape)))
  set(cohortData, ,"mAge", pmin(cohortData$B,cohortData$mAge))
  set(cohortData, ,"mortality", cohortData$mBio+cohortData$mAge)
  cohortData[,':='(mBio=NULL,mAge=NULL,bAP=NULL,bPM=NULL)]
  sim$cohortData <- cohortData
  return(invisible(sim))
}

biomassSuccessionLANDISBiomassUpdating = function(sim) {
  cohortData <- sim$cohortData
  set(cohortData, ,"B", cohortData$B+cohortData$aNPPAct-cohortData$mortality)
  set(cohortData, ,"age", cohortData$age+1)
  cohortData<-cohortData[age<longevity,]
  sim$cohortData <- cohortData
  return(invisible(sim))
}

biomassSuccessionLANDISPostFireReproduction = function(sim) {
  cohortData <- sim$cohortData
  fireMap <- sim$fireMap
  #fireMap <- randomFire()
  pixelGroupMap <- sim$pixelGroupMap
  species<-sim$species
  postFireReproData<-data.table(pixelIndex=numeric(), pixelGroup=integer(), ecoregionGroup=numeric(), species=character(),
                                age=integer(), B=numeric(), maxANPP=numeric(), maxB=numeric(), maxB_eco=numeric(), 
                                longevity=numeric(), mortalityshape=numeric(),
                                growthcurve=numeric(), mortality=numeric(), sumB=numeric(), aNPPAct=numeric())
  postFireGroupFactor <- 10^ceiling(log10((max(getValues(pixelGroupMap)+1))))
  browser()
  postFirePixelGroupMap <- pixelGroupMap + fireMap*postFireGroupFactor
  postFireCohortData <- setkey(data.table(cohortData[pixelGroup==(unique(getValues(postFirePixelGroupMap))[
    unique(getValues(postFirePixelGroupMap))>postFireGroupFactor]) %% postFireGroupFactor]),pixelGroup)
  
  burnedcohortData <- postFireCohortData[setkey(
    calcSiteShade(sim, postFireCohortData, speciesEcoregion, minRelativeB)[,.(
      pixelGroup,siteShade)],pixelGroup),nomatch=0]
  setkey(burnedcohortData,species)
  species_temp<-setkey(data.table(species)[postfireregen=="serotiny",.(species,sexualmature,shadetolerance,postfireregen)],species)
  serotinyAssessCohortData <- burnedcohortData[species_temp,nomatch=0]
  rm(species_temp)
  
  newCohortData<-serotinyAssessCohortData[age>=sexualmature,newSiteShade:=0] %>% # NOTE should be in mortalityFromDisturbance module or event
    unique(., by=c("pixelGroup", "species")) # this is amazing!!!!
  
  #   newCohortData1<-serotinyAssessCohortData[age>=sexualmature,newSiteShade:=0] %>% # NOTE should be in mortalityFromDisturbance module or event
  #     group_by(pixelGroup) %>%
  #     distinct(species) %>% # remove duplicated species in the same pixel group
  #     ungroup %>%
  #     data.table
  rm(serotinyAssessCohortData)
  # the below map is the map that had been assessed for serotiny
  serotinyAssessCohortMap <- postFirePixelGroupMap %in% unique(newCohortData$pixelGroup+postFireGroupFactor)
  # from now on the regeneration process is assessed for each potential pixel
  pixelTable<-data.table(cbind(pixelIndex=Which(serotinyAssessCohortMap==1, cell=TRUE),
                               pixelGroup=pixelGroupMap[Which(serotinyAssessCohortMap==1, cell=TRUE)]))
  setkey(pixelTable,pixelGroup)
  setkey(newCohortData,pixelGroup)
  newCohortData <- pixelTable[newCohortData, nomatch=0, allow.cartesian=TRUE] # need attention here
  serotinyPixel <- pixelTable$pixelIndex # save the pixel index for resprouting assessment use, i.e., removing these pixel from assessing resprouting
  rm(pixelTable)
  
  # light check
  newCohortData[,suffLight:=sufficientLight[cbind(newCohortData$shadetolerance, newCohortData$newsiteShade+2)]]
  newCohortData[,establishLight:=(runif(nrow(newCohortData),0,1)< suffLight)]
  
  # establishment probability check
  specieseco_temp<-setkey(data.table(speciesEcoregion)[year <= simCurrentTime(sim) & (year > (simCurrentTime(sim)-successionTimestep)),.(species,establishprob,ecoregionGroup)],species,ecoregionGroup)
  setkey(newCohortData,species,ecoregionGroup)
  newCohortData<-newCohortData[specieseco_temp,nomatch=0]
  rm(specieseco_temp)
  # determine establishment
  newCohortData[,establishOther:=(runif(nrow(newCohortData),0,1) < establishprob)]
  newCohortData[,est:=establishOther*establishLight]
  # serotiny regeneration pixel and species
  newCohortData<-newCohortData[est==1,]
  
  # remove all columns that were used temporarily here
  
  if(NROW(newCohortData)>0) {
    newCohortData <- newCohortData[,.(pixelIndex,pixelGroup, ecoregionGroup, species, age=0,
                                      B=0, maxANPP, maxB, maxB_eco, longevity, mortalityshape, growthcurve,
                                      mortality=NA,sumB,aNPPAct=NA)] #
    postFireReproData <- rbindlist(list(postFireReproData,newCohortData))
  }
  rm(newCohortData)
  
  
  #############################################################
  #############################################################
  # from now on, starting assessing resprouting reproduction:
  # basically same thing as serotiny
  
  burnedcohortData <- postFireCohortData[setkey(calcSiteShade(sim, postFireCohortData, speciesEcoregion, minRelativeB)
                                                [,.(pixelGroup,siteShade)],
                                                pixelGroup),nomatch=0]
  
  setkey(burnedcohortData,species)
  species_temp<-setkey(data.table(species)[postfireregen=="resprout" | postfireregen=="serotiny",
                                           .(species, sexualmature, shadetolerance, postfireregen,
                                             resproutage_min, resproutage_max, resproutprob)],species)
  resproutingAssessCohortData<-burnedcohortData[species_temp,nomatch=0]
  rm(species_temp)
  resproutingAssessCohortData[,drop:=any(postfireregen=="serotiny"),by=pixelGroup]
  newCohortData<-resproutingAssessCohortData[drop!="TRUE" & age>=resproutage_min & age<resproutage_max,]
  newCohortData[,':='(drop=NULL,newSiteShade=0)]
  newCohortData <- unique(newCohortData, by=c("pixelGroup", "species"))
  
  
  # given the below map is resprouting assessing map
  resproutAssessMap <- postFirePixelGroupMap %in% unique(newCohortData$pixelGroup+postFireGroupFactor)
  # we need to remove the pixel that had been assessed in the serotiny reproduction
  resproutAssessMap[serotinyPixel] <- 0
  
  pixelTable<-data.table(cbind(pixelIndex=Which(resproutAssessMap==1,cell=TRUE),
                               pixelGroup=pixelGroupMap[Which(resproutAssessMap==1,cell=TRUE)]))
  setkey(pixelTable,pixelGroup)
  setkey(newCohortData,pixelGroup)
  newCohortData <- pixelTable[newCohortData, nomatch=0, allow.cartesian=TRUE]
  postFirePixel <- c(serotinyPixel,pixelTable$pixelIndex)
  rm(pixelTable)
  
  # Light check
  newCohortData[,suffLight:=sufficientLight[cbind(newCohortData$shadetolerance, newCohortData$newsiteShade+2)]]
  newCohortData[,establishLight:=(runif(nrow(newCohortData),0,1)< suffLight)]
  
  # Resprout check
  newCohortData[,establishResprout:=(runif(nrow(newCohortData),0,1) < newCohortData$resproutprob)]
  newCohortData[,est:=establishLight*establishResprout]
  newCohortData <- newCohortData[est==1,]
  # remove all columns that were used temporarily here
  if(NROW(newCohortData)>0) {
    newCohortData <- newCohortData[,.(pixelIndex,pixelGroup, ecoregionGroup, species, age=0,
                                      B=0, maxANPP, maxB, maxB_eco, longevity, mortalityshape, growthcurve,
                                      mortality=NA,sumB,aNPPAct=NA)] #
    postFireReproData <- rbindlist(list(postFireReproData,newCohortData))
    rm(newCohortData)
  }
  # the below function does three things:
  # 1. update the newcohort lookup table including assigning Biomass for new regenerations,
  #  
  #                                                assigning new pixel group number;
  # 
  # 2. update the pixel group map
  # 3. assign cohortData, and pixelGroupMap to global environment.
  if(NROW(postFireReproData)>0) {
    pixelGroupUpdating(sim,postFireReproData,cohortData,pixelGroupMap)
  }
  return(invisible(sim))
}


biomassSuccessionLANDISSeedingReproduction = function(sim) {
  cohortData <- sim$cohortData
  speciesEcoregion <- sim$speciesEcoregion
  minRelativeB <- sim$minRelativeB
  species<-setkey(data.table(sim$species), species)
  pixelGroupMap <- pixelGroupMap
  
  siteShade <- calcSiteShade(mySim, cohortData, speciesEcoregion, minRelativeB)
  
  
  #  Seed Receiving cells:
  #  1. Must be sufficient light
  seedReceive = setkey(species[,c(k=1,.SD)],k)[siteShade[,c(k=1,.SD)], allow.cartesian=TRUE][
    ,k:=NULL][,suffLight:=sufficientLight[cbind(shadetolerance,siteShade+2)]][suffLight!=0][
      ,.(pixelGroup,species,sexualmature,shadetolerance,seeddistance_eff,seeddistance_max,suffLight, siteShade)]
  setkey(seedReceive, species)
  
  # Seed source cells:
  # 1. Select only sexually mature cohorts, then 
  # 2. collapse to pixelGroup by species, i.e,. doesn't matter that there is >1 cohort of same species
  setkey(cohortData, species)
  cohortData <- species[,list(species,sexualmature)] %>%
    .[cohortData]
  
  matureCohorts <- cohortData[age>sexualmature] %>% unique(by=c("pixelGroup","species")) 
  seedSource <- species[,list(species,seeddistance_eff,seeddistance_max)] %>%
    .[matureCohorts]
  setkey(seedSource, species)
  # 3. Remove any species from the seedSource that couldn't regeneration anywhere on the map due to insufficient light
  #    (info contained within seedReceive)
  seedSource <- seedSource[unique(seedReceive)][
    ,.(species, seeddistance_eff, seeddistance_max, pixelGroup)]
  #seedSource <- setkey(species[,list(species,seeddistance_eff,seeddistance_max)] %>%
  #                      matureCohorts[.], species)[unique(seedReceive)]
  
  #seedingData <- data.table(pixelIndex=numeric(), species=character())
  
  #browser()
  
  #lp <- lineprof(seedingData <- seedDispRcvFast(sim, dtRcv=seedReceive, dtSrc=seedSource, species=species, pixelGroupMap,
  #                               maxPotentialsLength=3e5, verbose=TRUE), interval=0.25)
  #st <- system.time(seedingData <- seedDispRcvFast(sim, dtRcv=seedReceive, dtSrc=seedSource, species=species, pixelGroupMap,
  #                                              maxPotentialsLength=3e4, verbose=FALSE))
  #browser()
  #source('C:/Eliot/GitHub/nrv-succession/code blitz succession/modeltesting-data/seedDispersalLANDIS.R')
  seedingData <- seedDispRcvFast(sim, dtRcv=seedReceive, dtSrc=seedSource, species=species, pixelGroupMap,
                                 maxPotentialsLength=3e5, verbose=FALSE)
  
  verbose=FALSE
  if(verbose) {
    seedsReceived <- raster(seedSourceMap); seedsReceived[] <- NA; 
    seedsReceived[seedingData[species==unique(seedingData$species)[i],pixelIndex]] <- 1; 
    Plot(seedsReceived, cols="orange", na.color = "white");
    #i=i+1; 
    #print(unique(seedingData$species)[i-1])
    
    potSeedsReceived <- pixelGroupMap %in% seedReceive[unique(seedReceive$species)[i],pixelGroup]
    #potSeedsReceived[seedReceive[species==unique(seedReceive$species)[i],pixelIndex]] <- 1; 
    Plot(potSeedsReceived, cols="orange", na.color = "white", zero.color="white");
    i=i+1; 
    print(unique(seedingData$species)[i-1])
    
  }
  #   seedReceiveMap = pixelGroupMap %in% seedReceive[species==speciesCode]$pixelGroup
  #   assignGlobal("seedReceiveMap")
  #   Plot(seedReceiveMap)
  #   seedSourceMap = pixelGroupMap %in% seedSource[species==speciesCode]$pixelGroup
  #   assignGlobal("seedSourceMap")
  #   Plot(seedSourceMap)
  #   
  # 
  # 
  #   for(speciesCode in unique(allSpecies$species)) { # it would be better using allSpecies, if allSpecies does not include all the species
  #     #effDis <- as.numeric(species$seeddistance_eff[which(species$species==speciesCode)])
  #     #maxDis <- as.numeric(species$seeddistance_max[which(species$species==speciesCode)])
  #     allSpeciesSource <- cohortData[age>sexualmature,] %>%
  #       unique(., by=c("pixelGroup"))
  # #     singleSpeciesSource <- cohortData[species==speciesCode & age>sexualmature,] %>%
  # #       unique(., by=c("pixelGroup", "species"))
  # #     
  #     #set(singleSpeciesSource, ,"speciesNum",1)
  #     seedSourceMap <- pixelGroupMap %in% allSpeciesSource$pixelGroup
  # #     singleSpeciesReceive <- allSpecies[species==speciesCode,]
  # #     set(singleSpeciesReceive, , "speciesNum", 1)
  #     seedReceiveMap = pixelGroupMap %in% allSpecies$pixelGroup
  #     browser()
  #     pixelIndex <- seedDispRcvFast(seedSrc=seedSourceMap, seedRcv=seedReceiveMap,
  #                               effDist=effDis, maxDist=maxDis)
  #     
  #     rm(effDis,maxDis,singleSpeciesReceive,singleSpeciesSource,seedSourceMap,seedReceiveMap)
  #     # now, we have pixel index for a given species that have successful seed arrival.
  #     if(length(pixelIndex)>0) {
  #       singleSpeciesData <- data.table(pixelIndex=pixelIndex,species=speciesCode)
  #       seedingData <- rbindlist(list(seedingData,singleSpeciesData))
  #       rm(singleSpeciesData)
  #     }
  #     
  #   }
  #   
  # 
  # # OLD WAY
  #      allSpecies <- setkey(data.table(expand.grid(pixelGroup=siteShade$pixelGroup,species=species$species)),
  #                           species,pixelGroup)
  #    species_temp <- setkey(data.table(species)
  #                             [,.(species,sexualmature,shadetolerance,seeddistance_eff,seeddistance_max)],
  #                             species)
  #      allSpecies <- allSpecies[species_temp,nomatch=0]
  #      rm(species_temp)
  #      setkey(allSpecies, pixelGroup)
  #      allSpecies <- allSpecies[siteShade,nomatch=0]
  #      
  #      allSpecies[,suffLight:=sufficientLight[cbind(allSpecies$shadetolerance,
  #                                                   allSpecies$siteShade+2)]]
  #      # assess the species within a community which have sufficient light to establish, ie, suffLight!=0
  #      allSpecies <- allSpecies[suffLight!=0][,.(pixelGroup,
  #                                                species,sexualmature,shadetolerance,seeddistance_eff,
  #                                                seeddistance_max,suffLight, siteShade)]
  #      
  #      setkey(cohortData,species)
  #   species_temp<-setkey(data.table(species)[,.(species)],
  #                        species)
  #   cohortData <- cohortData[species_temp,nomatch=0]
  #   seedingData <- data.table(pixelIndex=numeric(), species=character())
  #   
  # #  allSingleSpeciesSource <- singleSpeciesSource %>% .[0]
  # allSeedReceiveMap  <- raster(seedSourceMap)
  # allSeedReceiveMap[] <- 0
  # 
  #   for(speciesCode in unique(allSpecies$species)) { # it would be better using allSpecies, if allSpecies does not include all the species
  #    # effDis <- as.numeric(species$seeddistance_eff[which(species$species==speciesCode)])
  #   #  maxDis <- as.numeric(species$seeddistance_max[which(species$species==speciesCode)])
  #     print(speciesCode)
  #     singleSpeciesSource <- cohortData[species==speciesCode & age>sexualmature,] %>%
  #       unique(., by=c("pixelGroup", "species"))
  # #     allSingleSpeciesSource=rbindlist(list(allSingleSpeciesSource, singleSpeciesSource))
  # #   }
  # 
  #     set(singleSpeciesSource, ,"speciesNum",1)
  #   #  seedReceiveMap <- pixelGroupMap %in% singleSpeciesSource$pixelGroup
  #     seedReceiveMap = pixelGroupMap %in% singleSpeciesReceive$pixelGroup
  #     allSeedReceiveMap[seedReceiveMap==1] <- 1
  # }
  #     singleSpeciesReceive <- allSpecies[species==speciesCode,]
  #     set(singleSpeciesReceive, , "speciesNum", 1)
  #     seedReceiveMap = pixelGroupMap %in% singleSpeciesReceive$pixelGroup
  #     browser()
  #     pixelIndex <- seedDispRcv(seedSrc=seedSourceMap, seedRcv=seedReceiveMap,
  #                   effDist=effDis, maxDist=maxDis)
  # 
  #     rm(effDis,maxDis,singleSpeciesReceive,singleSpeciesSource,seedSourceMap,seedReceiveMap)
  #     # now, we have pixel index for a given species that have successful seed arrival.
  #     if(length(pixelIndex)>0) {
  #       singleSpeciesData <- data.table(pixelIndex=pixelIndex,species=speciesCode)
  #       seedingData <- rbindlist(list(seedingData,singleSpeciesData))
  #       rm(singleSpeciesData)
  #     }
  # 
  #   }
  
  #browser()
  if(NROW(seedingData)>0) {
    seedingData <- setkey(data.table(seedingData),pixelIndex)
    # we need conditional codes that indicates there are fires that happen at current time
    # then the pixel that has serotiny and resprouting production should be removed for the below processes.
    #if(currentTime(sim)==??){
    
    #}
    
    
    seedingData$pixelGroup <- getValues(pixelGroupMap)[seedingData$pixelIndex]
    seedingData$ecoregionGroup <- getValues(ecoregionMap)[seedingData$pixelIndex]
    
    setkey(seedingData,species,ecoregionGroup)
    specieseco_temp<-setkey(data.table(speciesEcoregion)[year <= simCurrentTime(sim) & (year > (simCurrentTime(sim)-successionTimestep)),
                                                         .(species,establishprob,ecoregionGroup,maxB,maxANPP)],species,ecoregionGroup)
    
    seedingData<-seedingData[specieseco_temp,nomatch=0] #
    seedingData <- seedingData[establishprob >= runif(nrow(seedingData),0,1),]
    
    setkey(seedingData,pixelGroup)
    tempcohortdata <- cohortData[,.(pixelGroup,maxB_eco,mortality=NA,sumB,aNPPAct=NA)]
    tempcohortdata <- distinct(tempcohortdata,pixelGroup)
    setkey(tempcohortdata,pixelGroup)
    seedingData <- seedingData[tempcohortdata,nomatch=0]
    rm(tempcohortdata)
    species_temp<-setkey(data.table(species)[,.(species,longevity, mortalityshape, growthcurve)],
                         species)
    setkey(seedingData,species)
    seedingData <- seedingData[species_temp,nomatch=0]
    rm(species_temp)
    cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, species, age,
                                B, maxANPP, maxB, maxB_eco, longevity, mortalityshape, growthcurve,
                                mortality,sumB,aNPPAct)]
    seedingData <- seedingData[,.(pixelIndex,pixelGroup, ecoregionGroup, species, age=0,
                                  B=0, maxANPP, maxB, maxB_eco, longevity, mortalityshape, growthcurve,
                                  mortality=NA,sumB,aNPPAct=NA)] #
    
    
    
    pixelGroupUpdating(sim,seedingData,cohortData,pixelGroupMap)
  }
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}




### template for your event4
biomassSuccessionLANDISSimulationSummary = function(sim) {
  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap
  names(pixelGroupMap) <- "pixelGroup" 
  
  
  pixelAll <- cohortData[,.(uniqueSumB=round(sum(B,na.rm=TRUE))/(cellSize^2/10000)),by=pixelGroup]
  biomassMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumB")
  setColors(biomassMap) <- c("light green", "dark green")
  rm(pixelAll)
  
  pixelAll <- cohortData[,.(uniqueSumANPP=round(sum(aNPPAct,na.rm=TRUE)/(cellSize^2/10000))),by=pixelGroup]
  ANPPMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumANPP")
  setColors(ANPPMap) <- c("light green", "dark green")
  rm(pixelAll)
  
  pixelAll <- cohortData[,.(uniqueSumMortality=round(sum(mortality,na.rm=TRUE)/(cellSize^2/10000))),by=pixelGroup]
  mortalityMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumMortality")
  setColors(mortalityMap) <- c("light green", "dark green")
  rm(pixelAll)
  
  pixelAll <- cohortData[age<successionTimestep,.(uniqueSumReproduction=round(sum(B,na.rm=TRUE)/(cellSize^2/10000))),by=pixelGroup]
  reproductionMap <- rasterizeReduced(pixelAll, pixelGroupMap, "uniqueSumReproduction")
  setColors(reproductionMap) <- c("light green", "dark green")
  rm(pixelAll)
  
  
  
  # the following codes for preparing the data table for saving
  overallChange <- sim$overallChange
  overallChangeByEcoregion <- sim$overallChangeByEcoregion
  ecoregionMap <- sim$ecoregionMap
  cellSize <- sim$cellSize
  
  simulationSummaryTable <- data.table(Ecoregion=getValues(ecoregionMap),
                                       Biomass=getValues(biomassMap),
                                       ANPP=getValues(ANPPMap),
                                       Mortality=getValues(mortalityMap),
                                       Reproduction=getValues(reproductionMap))
  temtable <- round(simulationSummaryTable[,.(Year=round(simCurrentTime(sim)),
                                              Biomass = mean(Biomass,),
                                              ANPP = mean(ANPP),
                                              Mortality = mean(Mortality),
                                              Reproduction = mean(Reproduction))]/(cellSize^2/10000))
  overallChange <- rbindlist(list(overallChange,temtable))
  rm(temtable)
  
  
  temtable <- simulationSummaryTable[,.(Year=round(simCurrentTime(sim)),
                                        Biomass = round(mean(Biomass,)/(cellSize^2/10000)),
                                        ANPP = round(mean(ANPP)/(cellSize^2/10000)),
                                        Mortality = round(mean(Mortality)/(cellSize^2/10000)),
                                        Reproduction = round(mean(Reproduction)/(cellSize^2/10000))),
                                     by="Ecoregion"]
  
  overallChangeByEcoregion <- rbindlist(list(overallChangeByEcoregion, temtable))
  rm(temtable)
  
  sim$overallChange <- overallChange
  sim$overallChangeByEcoregion <- overallChangeByEcoregion
  #  if(!is.na())
  sim$biomassMap <- biomassMap
  sim$ANPPMap <- ANPPMap
  sim$mortalityMap <- mortalityMap
  sim$reproductionMap <- reproductionMap
  return(invisible(sim))
  
}

### template for save events
### template for plot events
biomassSuccessionLANDISPlot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  # prepare a data table that contains, for each pixel group and current time, the sum of Biomass, sum of anpp, sum of mortality and reproduction.
  #
  #
  #sim$biomassMap
  #sim$ANPPMap
  #sim$mortalityMap
  #sim$reproductionMap
  dev(4)
  Plot(sim$biomassMap, sim$ANPPMap, sim$mortalityMap, sim$reproductionMap, legendRange=list(NULL, NULL, NULL, NULL))#,
  #new=ifelse(simCurrentTime(sim)==simParams(sim)$biomassSuccessionLANDIS$.plotInitialTime,TRUE,FALSE),
  #speedup=1)
  
  seekViewport("sim$ANPPMap")
  grid.rect(1.02,1.07,width = 0.4,height=0.1,gp=gpar(fill="white", col="white"))
  grid.text(label = paste0("Year = ",round(simCurrentTime(sim))),x=1.02, y=1.07)
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}



biomassSuccessionLANDISSave = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  # how many files should we save?
  # 1. gis file?
  # 2. cohort data?
  # 3. PixGroup data?
  # 4. total biomass, anpp, mortality, and regeneration by year table?
  
  
  overallChange <- sim$overallChange
  overallChangeByEcoregion <- sim$overallChangeByEcoregion
  
  # should look at saveFiles function in SpaDES
  saveFiles(sim)
  
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


calcSiteShade <- function(sim,cohortData,speciesEcoregion,minRelativeB) {
  bAMterm1 <- cohortData [,.(sumB=sum(B)),by=.(pixelGroup, ecoregionGroup)]
  bAM <- data.table(speciesEcoregion)[year <= simCurrentTime(sim) & (year > (simCurrentTime(sim)-successionTimestep))]
  bAM<-bAM[,.(maxMaxB=max(maxB)),by=ecoregionGroup]
  setkey(bAM,ecoregionGroup)
  setkey(bAMterm1,ecoregionGroup)
  bAMterm1<-bAMterm1[bAM, nomatch=0]
  bAMterm1[,bAM:=sumB/maxMaxB]
  minRelativeB<-data.table(minRelativeB)
  setkey(minRelativeB,ecoregionGroup)
  bAMterm1<-bAMterm1[minRelativeB, nomatch=0]
  bAMterm1[,siteShade:=cut(bAM,c(0,X1,X2,X3,X4,X5,1),labels=FALSE, include.lowest=TRUE)-1,by=pixelGroup]
  bAMterm1<-bAMterm1[,.(pixelGroup, siteShade)]
  return(bAMterm1)
}

# this function is to generate the new community map and community lookup table
# the below function does three things:
# 1. update the newcohort lookup table including assigning Biomass for new regenerations,
#                                                assigning new community group number,
#                                                assigning new pixel group number;
# 2. update the community map;
# 3. update the pixel group map
pixelGroupUpdating<-function(sim,cohortdatanew,cohortdata,pixelgroupmap){
  
  cohortdatanew[,':='(B=pmax(1,0.025*maxB*exp(-1.6*sumB/maxB_eco)),age=1)]
  # the cohortdatanew must be sorted for species within pixelIndex
  setkey(cohortdatanew,pixelIndex,species)
  newPixelIndex <- cohortdatanew %>%
    reshape2::dcast(pixelIndex + pixelGroup ~ species, value.var="species")
  newPixelIndex <- tidyr::unite(newPixelIndex,Allspecies, 2:ncol(newPixelIndex), sep="") %>%
    data.table
  temptable <- data.table(Allspecies=unique(newPixelIndex$Allspecies),
                          newPixelGroup=seq(from=max(cohortdata$pixelGroup)+1,
                                            to=max(cohortdata$pixelGroup)+length(unique(newPixelIndex$Allspecies,by=1))))
  setkey(temptable,Allspecies)
  setkey(newPixelIndex,Allspecies)
  newPixelIndex <- newPixelIndex[temptable,nomatch=0]
  rm(temptable)
  setkey(newPixelIndex,pixelIndex)
  setkey(cohortdatanew,pixelIndex)
  cohortdatanew <- cohortdatanew[newPixelIndex,nomatch=0]
  
  cohortdatanew <- unique(cohortdatanew,by=c("newPixelGroup","species"))
  
  
  # the new cohort data has two components
  # 1. existing cohort data
  # 2. new cohort data
  # the below codes to generate the existing cohort data, and assign new community Group number
  existCohortData <- data.table(pixelGroup=integer(), ecoregionGroup=numeric(), species=character(),
                                age=integer(), B=numeric(), maxANPP=numeric(), maxB=numeric(), maxB_eco=numeric(),
                                longevity=numeric(), mortalityshape=numeric(),
                                growthcurve=numeric(), mortality=numeric(), sumB=numeric(), aNPPAct=numeric())
  for(newpixelgroup in unique(cohortdatanew$newPixelGroup)) {
    singleNewPixelCohortData <- cohortdata[pixelGroup==cohortdatanew$pixelGroup[which(cohortdatanew$newPixelGroup==newpixelgroup)][1]]
    singleNewPixelCohortData[,pixelGroup:=newpixelgroup]
    existCohortData <- rbindlist(list(existCohortData,singleNewPixelCohortData))
  }
  # need to reorganize the date structure to match the cohortdata structure
  # put new cohort data and existing data together
  # put newCummunity data into cohort data
  cohortdatanew$pixelGroup <- cohortdatanew$newPixelGroup
  
  
  cohortdatanew[,':='(pixelIndex=NULL,Allspecies=NULL,newPixelGroup=NULL)]
  
  
  cohortdata <- rbindlist(list(cohortdata,existCohortData,cohortdatanew))
  
  
  
  # update the communitymap based on pixelIndex and newcommunity group code
  pixelgroupmap[as.numeric(newPixelIndex$pixelIndex)] <- newPixelIndex$newPixelGroup
  tempixelgroup <- unique(getValues(pixelgroupmap))
  cohortdata <- cohortdata[pixelGroup==tempixelgroup,]
  
  # to prevent unlimited increase of pixelgroup number
  if(min(cohortdata$pixelGroup>ncell(pixelgroupmap))){
    k<-1
    for(pixelgroup in unique(cohortdata$pixelGroup)){
      cohortdata[pixelGroup==pixelgroup,pixelGroup:=k]
      pixelgroupmap[Which(pixelgroupmap==pixelgroup,cell=TRUE)] <- k
      k <- k+1
    }
    rm(k)
  }
  
  sim$cohortData <- cohortdata
  sim$pixelGroupMap <- pixelgroupmap
  return(invisible(list()))
}

randomFire <- function(intitialCommunitiesMap){
  #   fireMap <- SpaDES::spread(initialCommunitiesMap)
  #   fireMap <- as.logical(fireMap)
  #   assignGlobal("fireMap")
}

#sim$mortalityInterval <- 1
#sim$growthInterval <- 1
#sim$biomassUpdatingInterval <- 1
### add additional events as needed by copy/pasting from above
