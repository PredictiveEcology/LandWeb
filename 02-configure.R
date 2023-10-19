source("01c-exptTbl.R") ## TODO

# configure project ---------------------------------------------------------------------------

config <- SpaDES.config::useConfig(projectName = "LandWeb", projectPath = prjDir,
                                   mode = .mode, rep = .rep, res = .res,
                                   studyAreaName = .studyAreaName, version = .version)

if (.version == 2) {
  config$context[["dispersalType"]] <- .dispersalType
  config$context[["ROStype"]] <- .ROStype

  config$update()
  config$validate()
}

## apply user and machine context settings here
source("02a-user-config.R")
config$args <- config.user$args
#config$modules <- config.user$modules ## no modules should differ among users/machines
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
config$modules

# project paths -------------------------------------------------------------------------------
config$paths
stopifnot(identical(checkPath(config$paths[["projectPath"]]), getwd()))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

paths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "landweb", tryEmail = config$args[["cloud"]][["googleUser"]])
