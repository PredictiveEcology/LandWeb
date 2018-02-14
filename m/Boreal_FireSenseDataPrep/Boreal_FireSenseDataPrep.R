
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Boreal_FireSenseDataPrep",
  description = "A data preparation module for running the fireSense module in the LandWeb project",
  keywords = c("LandWeb", "fireSense"),
  authors = c(person(c("Jean", "Marchal"), email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1", Boreal_FireSenseDataPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Boreal_FireSenseDataPrep.Rmd"),
  reqdPkgs = list("dplyr", "raster", "rgeos", "sp", "sf"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "NFDB_point", objectClass = NA,
                 desc = "Canadian National Fire Database (CNFDB) Fire Point locations and fire attributes",
                 sourceURL = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"),
    expectsInput(objectName = "speciesLayers", objectClass = "RasterStack",
                 desc = "Species composition for each species",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-Species.tar"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "This shapefile describes the outline of the study area.",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Boreal_FireSenseDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Boreal_FireSenseDataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Boreal_FireSenseDataPrep", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #Plot(objectFromModule) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "Boreal_FireSenseDataPrep", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "Boreal_FireSenseDataPrep", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  sim <- prepFireSenseInputs(sim)
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}


prepFireSenseInputs <- function(sim)
{
  prepGrid <- function(sim, studyArea, cellsize)
  {
    gd <- Cache(
      as,
      SpatialPixels(
        SpatialPoints( makegrid(studyArea, cellsize = cellsize) ),
        proj4string = studyArea@proj4string
      ),
      "SpatialPolygons",
      userTags = c("stable", currentModule(sim)), 
      quick = .quickCheck
    )
    
    isContained <- Cache(rgeos::gContains,
                         studyArea, 
                         gd, 
                         byid = TRUE, 
                         userTags = c("stable", currentModule(sim)), 
                         quick = .quickCheck
    )
    
    gd <- gd[isContained,]
    gd <- st_as_sf(gd)
    gd$CELL_ID <- 1:nrow(gd)
    gd
  }
  
  # Make 10k grid (frequency and size analyses)
  template_10k_grid <- Cache(
    prepGrid, 
    sim,
    studyArea = sim$studyAreaNAD83,
    cellsize = 10000,
    userTags = c("stable", currentModule(sim)),
    quick = .quickCheck
  )
  
  message("  10k grid created")
  
  # Make 250m grid (escape and spread analyses)
  template_250m_grid <- Cache(
    prepGrid, 
    sim, 
    studyArea = sim$studyAreaNAD83,
    cellsize = 250,
    userTags = c("stable", currentModule(sim)),
    quick = .quickCheck
  )
  
  message("  250m grid created")
  
  NFDB_10k_grid <- Cache(
    st_join, template_10k_grid, sim$NFDB_point, left = TRUE,
    userTags = c("stable", currentModule(sim)), quick = .quickCheck
  )

  message("  NFDB_10k created")
  
  rm(template_10k_grid)
  
  NFDB_250m_grid <- Cache(
    st_join, template_250m_grid, sim$NFDB_point, left = FALSE,
    userTags = c("stable", currentModule(sim)), quick = .quickCheck
  )
  
  message("  NFDB_250m created")
  
  rm(template_250m_grid)
  
  extract_fun <- function(x, ...) sum(x, na.rm = TRUE) / length(x)
  
  data_10k <- Cache(
    raster::extract, 
    sim$broadLeafPc, 
    Cache(
      raster::extract,
      sim$needleLeafPc, 
      (
        NFDB_10k_grid %>%
          group_by(CELL_ID) %>%
          summarise(n_fires = n())
      ),
      fun = extract_fun, 
      sp = TRUE,
      userTags = c("stable", currentModule(sim)), 
      quick = .quickCheck
    ),
    fun = extract_fun, 
    sp = TRUE,
    userTags = c("stable", currentModule(sim)), 
    quick = .quickCheck
  ) %>%
    slot("data") %>%
    rename(bl = !!names(sim$broadLeafPc), nl = !!names(sim$needleLeafPc)) %>%
    mutate(ot = 100 - bl - nl) %>%
    right_join(NFDB_10k_grid, "CELL_ID")
  
  message("  Extraction of veg data finished (10k)")
  
  data_250m <- Cache(
    raster::extract, 
    sim$broadLeafPc, 
    Cache(
      raster::extract,
      sim$needleLeafPc, 
      (
        NFDB_250m_grid %>%
          group_by(CELL_ID) %>%
          summarise(n_fires = n())
      ),
      fun = extract_fun, 
      sp = TRUE,
      userTags = c("stable", currentModule(sim)), 
      quick = .quickCheck
    ),
    fun = extract_fun,
    sp = TRUE,
    userTags = c("stable", currentModule(sim)), 
    quick = .quickCheck
  ) %>%
    slot("data") %>%
    rename(bl = !!names(sim$broadLeafPc), nl = !!names(sim$needleLeafPc)) %>%
    mutate(ot = 100 - bl - nl) %>%
    right_join(NFDB_250m_grid, "CELL_ID")
  
  message("  Extraction of veg data finished (250m)")
  
  sim <- frequencyInputs(sim, data_10k) 
  sim <- sizeInputs(sim, data_10k)
  sim <- escapeInputs(sim, data_250m)
  sim <- spreadInputs(sim, data_250m)
  
  rm(NFDB_point, studyArea, studyAreaNAD83, envir = envir(sim))
  
  sim
}

frequencyInputs <- function(sim, .data)
{
  sim$dataFireSense_FrequencyFit <- .data %>%
    mutate(n_fires = ifelse(is.na(n_fires), 0, n_fires))
  sim
}


sizeInputs <- function(sim, .data)
{
  sim$dataFireSense_SizeFit <- .data %>%
    filter(!is.na(SIZE_HA)) %>%
    select(CELL_ID, SIZE_HA, bl, nl, ot) %>%
    rename(size = SIZE_HA) %>%
    filter(size >= 1)
  sim
}


escapeInputs <- function(sim, .data)
{
  sim$dataFireSense_EscapeFit <- .data %>%
    select(CELL_ID, SIZE_HA, bl, nl, ot) %>%
    mutate(escaped = as.integer(SIZE_HA >= 1))
  sim
}


spreadInputs <- function(sim, .data)
{
  sim$fireLoc_FireSense_SpreadFit <- .data %>%
    select(CELL_ID, SIZE_HA) %>%
    rename(size = SIZE_HA) %>%
    group_by(CELL_ID) %>%
    slice(which.max(size)) # When more than one fire started in the same cell
                           # keep the largest fire
  sim
}


.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  
  crs <- "+init=epsg:3978" # LCC NAD83 Canada wide
  sim$studyAreaNAD83 <- spTransform(sim$studyArea, CRS(crs))

  # Filenames
  nfdbPointFilename <- "NFDB_point_20171106.shp"
  broadLeafFilename <- "NFI_MODIS250m_kNN_Species_Generic_BroadLeaf_Spp_v0.tif"
  needleLeafFilename <- "NFI_MODIS250m_kNN_Species_Generic_NeedleLeaf_Spp_v0.tif"
  
  # Also extract
  nfdbPointAlsoExtract <- basename(paste0(tools::file_path_sans_ext(nfdbPointFilename), ".", c("dbf", "prj", "sbn", "sbx", "shx")))
  
  # Fire data
  sim$NFDB_point <- Cache(
    prepInputs,
    targetFile = nfdbPointFilename,
    archive = "NFDB_point.zip", 
    alsoExtract = nfdbPointAlsoExtract,
    dataset = "NFDB_PT",
    modulePath = modulePath(sim),
    moduleName = currentModule(sim),
    studyArea = sim$studyAreaNAD83,
    fun = "st_read",
    pkg = "sf",
    writeCropped = TRUE, 
    cacheTags = c("stable", currentModule(sim)),
    quick = .quickCheck
  )
  
  # KNN species %
  sim$broadLeafPc <- Cache(
    prepInputs,
    targetFile = broadLeafFilename,
    archive = "kNN-Species.tar",
    dataset = "EOSD2000", # TODO: change to "KNN" when webDatabases::urls will be updated
    modulePath = modulePath(sim),
    moduleName = currentModule(sim),
    studyArea = sim$studyAreaNAD83,
    writeCropped = TRUE, 
    cacheTags = c("stable", currentModule(sim)),
    quick = .quickCheck
  )
  
  sim$needleLeafPc <- Cache(
    prepInputs,
    targetFile = needleLeafFilename,
    archive = "kNN-Species.tar",
    dataset = "EOSD2000", # TODO: change to "KNN" when webDatabases::urls will be updated
    modulePath = modulePath(sim),
    moduleName = currentModule(sim),
    studyArea = sim$studyAreaNAD83,
    writeCropped = TRUE, 
    cacheTags = c("stable", currentModule(sim)),
    quick = .quickCheck
  )
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
