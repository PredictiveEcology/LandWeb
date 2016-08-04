test_that("test summary aboveground biomass, growth, mortality. ",{
  library(raster)
  library(data.table)
  module <- list("biomassSuccessionLANDIS")
  path <- list(modulePath=file.path("~/GitHub/nrv-succession/code blitz succession/modules"),
               outputPath="~/output")
  parameters <- list(.progress=list(type="graphical", interval=1),
                     .globals=list(verbose=FALSE),
                     biomassSuccessionLANDIS=list( .saveInitialTime=NA))
  cohortData <- data.table(expand.grid(speciesCode = 1:3,
                                       pixelGroup = 1:5))
  cohortData[,':='(B = seq(700, by = 50, length = 15),
                   aNPPAct = seq(100, by = 20, length = 15),
                   mortality = seq(200, by = 20, length = 15))]
  cohortData[pixelGroup == 1 | pixelGroup == 2, ecoregionGroup := 2]
  cohortData[pixelGroup == 3 | pixelGroup == 4 | pixelGroup == 5, ecoregionGroup := 1]
  cohortData <- cohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age = 50,
                              B, mortality, aNPPAct)]
  pixelGroupMap <- raster(xmn=50,xmx=50+5*100,
                          ymn=50,ymx=50+5*100,
                          res=c(100,100), val=c(rep(5, 5), rep(4, 5), rep(3, 5),
                                                rep(2, 3), rep(-1, 2), rep(1, 3),
                                                rep(-1, 2)))
  species <- data.table(species = c("species1", "species2", "species3"),
                        speciesCode = 1:3)
  
  reproductionMap <- setValues(pixelGroupMap, 0)
  reproductionMap[c(1, 3, 5, 7, 9, 16, 22)] <- c(100, 300, 500, 700, 900, 150, 220)
  ecoregionMap <- setValues(pixelGroupMap, 0)
  ecoregionMap[c(1:15)] <- 1
  ecoregionMap[c(16:18, 21:23)] <- 2
  ecoregionMap[c(19:20, 24:25)] <- 3
  activePixelIndex <- c(1:18, 21:23)
  simulationOutput <- data.table(Ecoregion = numeric(), NofCell = numeric(), Year = numeric(), Biomass = numeric(),
                                 ANPP = numeric(), Mortality = numeric(), Regeneration = numeric())
  simulationOutput_byspecies <- data.table(Ecoregion = numeric(), Species = character(), Year = numeric(), 
                                           Biomass = numeric(), ANPP = numeric(), Mortality = numeric())
  cellSize <- 100
  objects <- list("cohortData" = cohortData,
                  "pixelGroupMap" = pixelGroupMap, 
                  "reproductionMap" = reproductionMap,
                  "ecoregionMap" = ecoregionMap,
                  "activePixelIndex" = activePixelIndex,
                  "simulationOutput" = simulationOutput,
                  "simulationOutput_byspecies" = simulationOutput_byspecies,
                  "cellSize" = cellSize,
                  "species" = species)
  mySim <- simInit(times=list(start=0, end=2),
                   params=parameters, 
                   modules=module,
                   objects=objects,
                   paths=path)
  if(exists("biomassSuccessionLANDISSummaryBGM")){
    simOutput <- biomassSuccessionLANDISSummaryBGM(mySim)
  } else {
    simOutput <- mySim$biomassSuccessionLANDISSummaryBGM(mySim)
  }
  # check the maps
  expect_is(simOutput$biomassMap, "RasterLayer")
  expect_equal(getValues(simOutput$biomassMap),
               c(rep(4050, 5), rep(3600, 5), rep(3150, 5),
                 rep(2700, 3), NA, NA, rep(2250, 3), NA, NA))
  
  expect_is(simOutput$ANPPMap, "RasterLayer")
  expect_equal(getValues(simOutput$ANPPMap),
               c(rep(1080, 5), rep(900, 5), rep(720, 5),
                 rep(540, 3), NA, NA, rep(360, 3), NA, NA))
  
  expect_is(simOutput$mortalityMap, "RasterLayer")
  expect_equal(getValues(simOutput$mortalityMap),
               c(rep(1380, 5), rep(1200, 5), rep(1020, 5),
                 rep(840, 3), NA, NA, rep(660, 3), NA, NA))
  
  # check the outputs
  expect_is(simOutput$simulationOutput, "data.table")
  expect_equal(simOutput$simulationOutput,
               data.table(Ecoregion = c(1, 2), 
                          NofCell = c(15, 6), 
                          Year = c(0, 0), 
                          Biomass = c(3600, 2475), 
                          ANPP = c(900, 450), 
                          Mortality = c(1200, 750),
                          Regeneration = c(167, 62)))
  
  expect_is(simOutput$simulationOutput_byspecies, "data.table")
  simOutput_simulationOutput_byspecies <- setkey(simOutput$simulationOutput_byspecies,
                                                 Ecoregion, Species)
  expect_equal(simOutput_simulationOutput_byspecies,
               data.table(Ecoregion = c(2, 2, 2, 1, 1, 1), 
                          Species = c("species1", "species2", "species3",
                                      "species1", "species2", "species3"), 
                          Year = c(0, 0, 0, 0, 0, 0),
                          Biomass = c(4650, 4950, 5250, 17250, 18000, 18750),
                          ANPP = c(780, 900, 1020, 4200, 4500, 4800),
                          Mortality = c(1380, 1500, 1620, 5700, 6000, 6300), 
               key = c("Ecoregion", "Species")))
})