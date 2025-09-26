# configure project ---------------------------------------------------------------------------

box::use(box/landweb)
config <- landweb$landwebConfig$new(
  projectName = "LandWeb",
  projectPath = prjDir,
  mode = .mode,
  rep = .rep,
  res = .res,
  studyAreaName = .studyAreaName
)$update()$validate()

## apply user and machine context settings here
source("02a-user-config.R")
config$args <- config.user$args
# config$modules <- config.user$modules ## no modules should differ among users/machines
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
config$modules

# project paths -------------------------------------------------------------------------------
config$paths
stopifnot(identical(checkPath(config$paths[["projectPath"]]), getwd()))

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

sf::sf_proj_network(TRUE)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "landweb", tryEmail = config$args[["cloud"]][["googleUser"]])
