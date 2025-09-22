box::use(R6[R6Class])
box::use(SpaDES.config[...])

box::use(DBI[dbConnect,dbDisconnect])
box::use(pemisc[availableMemory])

#' @keywords internal
.landwebRunName <- function(context, withRep = TRUE) {
  .runName <- paste0(
    context$studyAreaName,
    if (context$ROStype == "default") "" else paste0("_", context[["ROStype"]], "ROS"),
    if (context$pixelSize == 240) "" else paste0("_res", context[["pixelSize"]]),
    if (isTRUE(withRep)) {
      if (context$mode == "postprocess") "" else sprintf("_rep%02d", context$rep)
    } else {
      ""
    }
  )
  attr(.runName, "auto") <- TRUE

  return(.runName)
}

#' LandWeb project context class
#'
#' This extends the `projContext` class by setting various LandWeb defaults and
#' employing custom fied validation.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname landwebContext-class
landwebContext <- R6::R6Class(
  "landwebContext",
  inherit = projContext,

  public = list(
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param mode Character string. One of 'production', 'development', 'postprocess',
    #'             or 'profile'.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param res Numeric indicating the map resolution (pixel size) to use.
    #'            Must be one of 50, 125, 250 (default).
    #'
    #' @param ROStype Character string describing the scaling of the LandMine fire model's
    #'                'rate of spread' parameters.
    #'                One of 'default', 'burny', 'equal' (i.e., all 1), 'log'.
    #'
    #' @param studyAreaName Character string identifying a study area (see `LandWeb_preamble`
    #'                      module for up-to-date descriptions of each study area label).
    #'
    initialize = function(
      projectPath,
      mode = "development",
      rep = 1L,
      res = 240,
      ROStype = NA,
      studyAreaName = "random"
    ) {
      if (is.na(ROStype)) {
        ROStype <- "default"
      } else {
        ROStype <- tolower(ROStype)
      }

      stopifnot(
        res %in% c(120, 240), ## (res %% 30 == 0)
        ROStype %in% c("default", "burny")
      )

      private[[".pixelSize"]] <- res
      private[[".projectPath"]] <- normPath(projectPath)
      private[[".ROStype"]] <- ROStype
      private[[".version"]] <- 3L

      self$machine <- Sys.info()[["nodename"]]
      self$user <- Sys.info()[["user"]]

      self$mode <- mode
      self$rep <- rep
      self$studyAreaName <- studyAreaName

      self$runName <- .landwebRunName(self)

      return(invisible(self))
    },

    #' @description print the context object in markdown table format,
    #'              and invisibly return this formatted table for use
    #'              e.g., when writing the context info to a file for humans.
    print = function() {
      cntxt <- list(
        mode = self$mode,
        machine = self$machine,
        user = self$user,
        studyAreaName = self$studyAreaName,
        rep = self$rep,
        pixelSize = self$pixelSize, ## additional for landweb
        ROStype = self$ROStype, ## additional for landweb
        runName = self$runName
      )

      info <- context2md(cntxt)

      message(info)

      return(invisible(info))
    }
  ),

  active = list(
    #' @field mode  Character string giving the project run mode.
    #'              One of 'production', 'development', 'postprocess', or 'profile'.
    mode = function(value) {
      if (missing(value)) {
        return(private[[".mode"]])
      } else {
        stopifnot(tolower(value) %in% c("production", "development", "postprocess", "profile"))
        private[[".mode"]] <- tolower(value)

        if (private[[".mode"]] == "postprocess") {
          self$rep <- NA_integer_
        }
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        ## TODO: issues getting relative paths when studyAreaName == projDir
        ## workaround is to append some suffix to the studyAreaName (e.g., LandWeb_full)
        newValue <- if (identical(value, basename(private[[".projectPath"]]))) {
          paste0(value, "_full")
        } else {
          value
        }

        newValue <- paste0(newValue, "_v", private[[".version"]])

        private[[".studyAreaName"]] <- newValue
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field rep  replicate id (integer)
    rep = function(value) {
      if (missing(value)) {
        return(private[[".rep"]])
      } else {
        if (private[[".mode"]] == "postprocess" && !is.na(value)) {
          warning("unable to set context$rep because context$mode == 'postprocess'")
        } else {
          private[[".rep"]] <- as.integer(value)
          self$runName <- .landwebRunName(self)
        }
      }
    },

    #' @field pixelSize raster pixel resolution (in metres) to use for simulations
    pixelSize = function(value) {
      if (missing(value)) {
        return(private[[".pixelSize"]])
      } else {
        stopifnot(value %in% c(240, 120)) ## (value %% 30 == 0)
        private[[".pixelSize"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field ROStype  Character string describing the scaling of the LandMine fire model's
    #'                 'rate of spread' parameters.
    #'                 One of 'default' or 'burny'.
    ROStype = function(value) {
      if (missing(value)) {
        return(private[[".ROStype"]])
      } else {
        stopifnot(value %in% c("default", "burny"))
        private[[".ROStype"]] <- value
        self$runName <- .landwebRunName(self)
      }
    }
  ),

  private = list(
    .pixelSize = 240,
    .ROStype = NA_character_,
    .version = NA_integer_
  )
)

#' LandWeb project configuration class
#'
#' This extends the `projConfig` class by setting various LandWeb config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @note See note in `?projConfig` describing the list-update mechanism of assignment to
#' certain fields.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname landwebConfig-class
landwebConfig <- R6::R6Class(
  "landwebConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `landwebConfig` object
    #'
    #' @param projectName character string of length 1 giving the name of the project.
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments passed to creation of new context
    #'
    initialize = function(projectName, projectPath, ...) {
      dots <- list(...)

      self$context <- landwebContext$new(projectPath = projectPath, ...)

      .version <- 3

      ## do paths first as these may be used below
      # paths ---------------------------------------------------------------------------------------
      private[[".paths"]] <- list(
        cachePath = file.path(projectPaths("cache"), self$context[["studyAreaName"]]),
        inputPath = projectPaths("input"),
        logPath = projectPaths("log"),
        modulePath = "modules",
        outputPath = projectPaths("output"),
        projectPath = normPath(projectPath),
        scratchPath = file.path(dirname(tempdir()), "scratch", basename(projectPath)),
        tilePath = file.path(projectPaths("output"), "tiles")
      )

      # arguments -----------------------------------------------------------------------------------
      private[[".args"]] <- list(
        cloud = list(
          cacheDir = "LandWeb_cloudCache",
          googleUser = "",
          useCloud = FALSE ## TODO: cloudCache spams Google Drive; doesn't respect drive path
        ),
        delayStart = 0,
        fsimext = "rds", ## TODO: use "qs" once SpaDES.core is fixed
        endTime = 1000, ## TODO: use `simYears = list(start = 0, end = 1000)` in order to use
        ##       `self$args$simYears$start` instead of hardgoding `start(sim)`
        notifications = list(),
        useCache = FALSE ## TODO: caching simulations broken in SpaDES.core
      )

      # modules ------------------------------------------------------------------------------------
      private[[".modules"]] <- list(
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        Biomass_speciesParameters = "Biomass_speciesParameters",
        # burnSummaries = "burnSummaries", ## used for postprocess, not devel nor production
        # HSI_Caribou_MB = "HSI_Caribou_MB", ## used for postprocess in MB, not devel nor production
        LandMine = "LandMine",
        LandWeb_output = "LandWeb_output",
        LandWeb_preamble = "LandWeb_preamble",
        # LandWeb_summary = "LandWeb_summary", ## used for postprocess, not devel nor production
        timeSinceFire = "timeSinceFire"
      )

      # options ------------------------------------------------------------------------------------
      private[[".options"]] <- list(
        future.globals.maxSize = 1000 * 1024^2,
        LandR.assertions = TRUE,
        LandR.verbose = 1,
        reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
        reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
        reproducible.destinationPath = normPath(self$paths[["inputPath"]]),
        reproducible.inputPaths = NULL,
        reproducible.nThreads = 2,
        reproducible.overwrite = TRUE,
        reproducible.showSimilar = TRUE,
        reproducible.useCache = self$args[["useCache"]],
        reproducible.useCloud = self$args[["cloud"]][["useCloud"]],
        reproducible.useGDAL = FALSE, ## TODO: reassess
        reproducible.useTerra = TRUE,
        Require.install = FALSE, ## don't use Require; assume all pkgs installed
        spades.futurePlan = "callr",
        spades.memoryUseInterval = 10, ## track memory use every 10 seconds
        spades.messagingNumCharsModule = 36,
        spades.moduleCodeChecks = TRUE,
        spades.qsThreads = 4,
        spades.recoveryMode = FALSE,
        spades.scratchPath = normPath(self$paths[["scratchPath"]]),
        spades.useRequire = FALSE ## don't use Require; assume all pkgs installed
      )

      # parameters ---------------------------------------------------------------------------------
      private[[".params_full"]] <- list(
        .globals = list(
          dataYear = 2020,
          fireTimestep = 1L,
          initialB = 10,
          # reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
          # simOutputPath = self$paths[["outputPath"]],
          sppEquivCol = "LandWeb",
          successionTimestep = 10,
          summaryInterval = 100,
          summaryPeriod = c(700, 1000),
          vegLeadingProportion = 0.8,
          .plotInitialTime = 0,
          .plots = c("png"), # c("object", "png", "raw", "screen"),
          .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
          .studyAreaName = self$context[["studyAreaName"]],
          .useParallel = 2 ## doesn't benefit from more DT threads
        ),
        Biomass_borealDataPrep = list(
          biomassModel = quote(lme4::lmer(
            B ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)
          )),
          ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
          forestedLCCClasses = c(81, 210, 220, 230, 240), ## should match preamble's treeClassesLCC
          LCCClassesToReplaceNN = 240,
          # next two are used when assigning pixelGroup membership; what resolution for
          #   age and biomass
          pixelGroupAgeClass = 2 * 10, ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
          pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
          subsetDataAgeModel = 100,
          subsetDataBiomassModel = 100,
          speciesTableAreas = c("BSW", "BP", "MC"),
          speciesUpdateFunction = list(
            quote(LandR::speciesTableUpdate(
              sim$species,
              sim$speciesTable,
              sim$sppEquiv,
              P(sim)$sppEquivCol
            )),
            quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
          ),
          useCloudCacheForStats = self$args[["cloud"]][["useCloud"]],
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]]
        ),
        Biomass_core = list(
          growthInitialTime = 0, ## start(sim)
          initialBiomassSource = "cohortData",
          seedingAlgorithm = "wardDispersal",
          .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]]
        ),
        Biomass_regeneration = list(
          fireInitialTime = 1, ## start(sim, "year") + 1
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]]
        ),
        Biomass_speciesData = list(
          types = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
          .plots = c("png"),
          .useCache = self$args[["useCache"]]
        ),
        Biomass_speciesParameters = list(
          PSPdataTypes = "NFI"
        ),
        burnSummaries = list(
          reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
          simOutputPath = self$paths[["outputPath"]]
        ),
        HSI_Caribou_MB = list(
          ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
          ageClassCutOffs = c(0, 40, 80, 120), ## LandWebUtils:::.ageClassCutOffs
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
          simOutputPath = self$paths[["outputPath"]],
          summaryInterval = 100, ## also in .globals
          summaryPeriod = c(700, 1000), ## also in .globals
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          version = .version,
          .makeTiles = FALSE, ## no tiles until parallel tile creation resolved (ropensci/tiler#18)
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]],
          .useParallel = self$options[["map.maxNumCores"]]
        ),
        LandMine = list(
          biggestPossibleFireSizeHa = 5e5, ## 5e5 ha = 8e4 pixels @250m
          burnInitialTime = 1L, ## start(sim, "year") + 1; same as fireInitialTime
          maxReburns = c(1L, 20L),
          maxRetriesPerID = 9L,
          minPropBurn = 0.90,
          mode = if ("postprocess" %in% self$context[["mode"]]) "multi" else "single",
          ROSother = switch(self$context[["ROStype"]], equal = 1L, log = log(30L), 30L),
          ROStype = self$context[["ROStype"]],
          useSeed = NULL, ## NULL to avoid setting a seed
          .plotInitialTime = 1, ## sim(start) + 1
          .plotInterval = 1,
          .studyAreaName = self$context[["studyAreaName"]],
          .unitTest = TRUE,
          .useCache = self$args[["useCache"]]
        ),
        LandWeb_output = list(
          summaryInterval = 100, ## also set in .globals
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]]
        ),
        LandWeb_preamble = list(
          bufferDist = 20000, ## 20 km buffer
          bufferDistLarge = 50000, ## 50 km buffer
          dispersalType = "default",
          friMultiple = 1L,
          pixelSize = self$context[["pixelSize"]],
          minFRI = 25L,
          ROStype = self$context[["ROStype"]],
          treeClassesLCC = c(81, 210, 220, 230, 240), ## should match B_bDP's forestedLCCClasses
          .plotInitialTime = 0, ## sim(start)
          .useCache = self$args[["useCache"]]
        ),
        LandWeb_summary = list(
          ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
          ageClassCutOffs = c(0, 40, 80, 120), ## LandWebUtils:::.ageClassCutOffs
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
          simOutputPath = self$paths[["outputPath"]],
          summaryInterval = 100, ## also in .globals
          summaryPeriod = c(700, 1000), ## also in .globals
          standAgeMapFromCohorts = FALSE, ## use FALSE for re-postprocessing old sims (using TSF)
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          version = .version,
          # .clInit = NULL, ## NOTE: defined in user-config.R
          .makeTiles = FALSE, ## no tiles until parallel tile creation resolved (ropensci/tiler#18)
          .plotInitialTime = 0, ## sim(start)
          .studyAreaName = self$context[["studyAreaName"]],
          .useCache = self$args[["useCache"]],
          .useParallel = self$options[["map.maxNumCores"]]
        ),
        timeSinceFire = list(
          startTime = 1L,
          .useCache = self$args[["useCache"]]
        )
      )

      self$params <- private[[".params_full"]]

      invisible(self)
    },

    #' @description Update a `landwebConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      ## mode ---------------------------------------
      if (self$context[["mode"]] %in% c("development", "production")) {
        self$args <- list(
          cloud = list(
            useCloud = FALSE ## TODO: cloudCache spams Google Drive folder; doesn't respect drive path
          ),
          delayStart = if (self$context[["mode"]] == "production") delay_rnd(5L:15L) else 0L, # 5-15 minutes
          endTime = 1000,
          successionTimestep = 10,
          summaryPeriod = c(700, 1000),
          summaryInterval = 100,
          timeSeriesTimes = 601:650,
          useCache = if (self$context[["mode"]] == "production") TRUE else FALSE
        )

        self$params <- list(
          .globals = list(
            .plots = c("png", "raw") ## don't plot to screen; don't save objects
          )
        )
      } else if (self$context[["mode"]] == "profile") {
        self$args <- list(
          endTime = 20,
          successionTimestep = 10,
          summaryPeriod = c(10, 20),
          summaryInterval = 10,
          timeSeriesTimes = 10
        )

        self$params <- list(
          .globals = list(
            .plotInitialTime = 0,
            .studyAreaName = self$context[["studyAreaName"]]
          )
        )
      } else if (self$context[["mode"]] == "postprocess") {
        if (grepl("provMB", self$context[["studyAreaName"]])) {
          self$modules <- list(
            "LandWeb_preamble",
            "Biomass_speciesData",
            "HSI_Caribou_MB",
            "LandWeb_summary"
          )
        } else {
          self$modules <- list(
            "LandWeb_preamble",
            "Biomass_speciesData",
            "burnSummaries",
            "LandMine",
            "LandWeb_summary"
          )
        }
      }

      ## options -- update based on context ----------
      self$options <- list(
        LandR.assertions = if (self$context[["mode"]] == "production") FALSE else TRUE,
        spades.moduleCodeChecks = if (self$context[["mode"]] == "production") FALSE else TRUE
      )

      ## study area + run info -----------------------
      self$params <- list(
        .globals = list(
          .studyAreaName = self$context[["studyAreaName"]]
        ),
        Biomass_borealDataPrep = list(
          pixelGroupBiomassClass = 1000 / (250 / self$context[["pixelSize"]])^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
        ),
        LandMine = list(
          ROSother = switch(self$context[["ROStype"]], equal = 1L, log = log(30L), 30L),
          ROStype = self$context[["ROStype"]],
          .unitTest = if (self$context[["mode"]] == "production") FALSE else TRUE
        ),
        LandWeb_preamble = list(
          dispersalType = self$context[["dispersalType"]],
          forceResprout = self$context[["forceResprout"]],
          friMultiple = self$context[["friMultiple"]],
          pixelSize = self$context[["pixelSize"]],
          ROStype = self$context[["ROStype"]]
        )
      )

      if (grepl("FMU", self$context[["studyAreaName"]])) {
        self$params <- list(
          Biomass_borealDataPrep = list(
            biomassModel = quote(lme4::lmer(
              B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)
            ))
          )
        )
      } else if (grepl("provMB", self$context[["studyAreaName"]])) {
        self$params <- list(
          Biomass_speciesData = list(
            types = c("KNN", "CASFRI", "Pickell", "MBFRI")
          )
        )
      }

      if (isFALSE(self$context[["succession"]])) {
        self$modules <- list(
          "LandWeb_preamble",
          "Biomass_speciesData",
          "LandMine",
          "LandWeb_output",
          "timeSinceFire"
        )
      }

      ## paths --------------------------------------
      self$paths <- list(
        cachePath = file.path(projectPaths("cache"), self$context[["studyAreaName"]]),
        logPath = file.path(updateOutputPath(self, .landwebRunName), "log"),
        outputPath = updateOutputPath(self, .landwebRunName),
        tilePath = file.path(updateOutputPath(self, .landwebRunName), "tiles")
      )
      unlist(self$paths) |> fs::dir_create() ## ensure all paths exist

      return(invisible(self))
    }
  ),

  private = list(
    finalize = function() {
      if (!is.null(self$options[["reproducible.conn"]])) {
        if (requireNamespace("DBI", quietly = TRUE)) {
          DBI::dbDisconnect(self$options[["reproducible.conn"]])
        }
      }
    }
  )
)
