rm(list=ls())
dataPath <- "~/GitHub/LandWeb/landWebDataPrep/outputs"
ecoregionmaps <- readRDS(file.path(dataPath, "ecoregionMap.rds"))
initialcommunitymaps <- readRDS(file.path(dataPath, "initialCommunityiesMap.rds"))
wholeEcoregionTable <- read.csv(file.path(dataPath, "ecoregiontable.csv"), 
                           header = TRUE, stringsAsFactors = FALSE) %>%
  data.table
wholeEcoregionTable[,ecoregion:=mapcode]
wholeSpeciesTable <- read.csv(file.path(dataPath, "speciesTable.csv"),
                         header = TRUE, stringsAsFactors = FALSE) %>%
  data.table

wholeInitialCommTable <- read.csv(file.path(dataPath, "initialcommunitytable.csv"),
                             header = TRUE, stringsAsFactors = FALSE) %>%
  data.table
wholeInitialCommTable <- wholeInitialCommTable[,.(mapcode, description = NA, species)]
set(wholeInitialCommTable, , c(paste("age", 1:15, sep = "")),  NA)
wholeInitialCommTable <- setkey(wholeInitialCommTable, species)[setkey(wholeSpeciesTable[,.(species, longevity)], species),
                                                      nomatch = 0]
wholeInitialCommTable[,randomnum:=sample(x = 1:15, size = nrow(wholeInitialCommTable), replace = TRUE)]
wholeInitialCommTable <- data.frame(wholeInitialCommTable)
for(i in 1:nrow(wholeInitialCommTable)){
  wholeInitialCommTable[i,4:(wholeInitialCommTable$randomnum[i]+3)] <- as.numeric(sample(x = 1:wholeInitialCommTable$longevity[i],
                                                   size = wholeInitialCommTable$randomnum[i],
                                                   replace = TRUE))
}
rm(i)
wholeInitialCommTable <- data.table(wholeInitialCommTable)[,':='(longevity = NULL, randomnum = NULL)]

speciesecoregiontable <- read.csv(file.path(dataPath, "speciesEcoregionTable.csv"),
                                  header = TRUE, stringsAsFactors = FALSE) %>%
  data.table
wholeminRelativeB <- wholeEcoregionTable[,.(ecoregion,
                                  X1 = 0.2, X2 = 0.4, X3 = 0.5, X4 = 0.7, X5 = 0.9)]
speciesecoregiontable <- speciesecoregiontable[,.(year = 0, ecoregion, species,
                                                  establishprob = SEP,
                                                  maxANPP = as.integer(maxANPP),
                                                  maxB = as.integer(maxBiomass))]
sufficientLight <- data.frame(speciesshadetolerance = c(1, 2, 3, 4, 5),
                              X0 = c(1, 1, 1, 1, 1), 
                              X1 = c(0.5, 1, 1, 1, 1), 
                              X2 = c(0, 0.5, 1, 1, 1), 
                              X3 = c(0, 0, 0.5, 1, 1), 
                              X4 = c(0, 0, 0, 0.5, 1), 
                              X5 = c(0, 0, 0, 0, 1))
names(initialcommunitymaps) <- "initialCommunityMap"
intialcommSplited <- splitRaster(initialcommunitymaps, nx = 5, ny = 5,
                                 buffer = 200,
                                 path = file.path(dataPath, "initialCommunityMap"))
names(ecoregionmaps) <- "ecoregionMap"
ecoregionSplited <- splitRaster(ecoregionmaps, nx = 5, ny = 5,
                                 buffer = 200,
                                path = file.path(dataPath, "ecoregionMap"))
spinupMortalityfraction <- 0.002
cellSize <- 250
seedingAlgorithm <- "wardDispersal"
calibrate  <- FALSE
useCache <- TRUE
successionTimestep <- 10
for(i in 1:25){
  initialCommunitiesMap <- readAll(intialcommSplited[[i]])
  ecoregionMap <- readAll(ecoregionSplited[[i]])
  ecoregionTable <- wholeEcoregionTable[ecoregion %in% unique(getValues(ecoregionMap)),]
  initialCommunities <- wholeInitialCommTable[mapcode %in% unique(getValues(initialCommunitiesMap))]
  speciesTable <- wholeSpeciesTable[species %in% unique(initialCommunities$species),]
  speciesEcoregion <- speciesecoregiontable[species %in% unique(speciesTable$species),]
  speciesEcoregion <- speciesecoregiontable[ecoregion %in% unique(ecoregionTable$ecoregion),]
  minRelativeB <- wholeminRelativeB[ecoregion %in% unique(ecoregionTable$ecoregion),]
  
  save(initialCommunitiesMap, ecoregionMap, ecoregionTable, initialCommunities,
       speciesTable, speciesEcoregion, minRelativeB, cellSize, spinupMortalityfraction,
       seedingAlgorithm, calibrate, useCache,
       sufficientLight, successionTimestep,
       file = file.path(dataPath, paste("simulationInputs_Tile", i, ".RData", sep = "")))
}



