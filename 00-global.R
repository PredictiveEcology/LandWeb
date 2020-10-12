switch(peutils::user(),
       "achubaty" = Sys.setenv(R_CONFIG_ACTIVE = "alex"),
       "emcintir" = Sys.setenv(R_CONFIG_ACTIVE = "eliot"),
       "rstudio" = Sys.setenv(R_CONFIG_ACTIVE = "docker"), ## i.e., using LandWeb docker container
       Sys.setenv(R_CONFIG_ACTIVE = "test")
)
#Sys.getenv("R_CONFIG_ACTIVE") ## verify

if (isFALSE(config::get("batchmode"))) {
  runName <- paste0(
    config::get("studyarea"),
    "_", config::get("scenariodisp"),
    "_", config::get("scenariofire"),
    "_fri", config::get("frimultiple"),
    "_res", 250 / config::get("mapresfact"),
    if (isTRUE(config::is_active("test"))) "_test" else "",
    if (isTRUE(config::get("postprocess"))) "" else sprintf("_rep%02g", config::get("rep"))
  )
}
stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set: e.g., see batch_runs.R

message(crayon::red(runName))

source("01-init.R")
source("02-packages.R")
source("03-paths.R")
source("04-options.R")
source("05-sim-objects.R")

if (delayStart > 0) {
  message(crayon::green("\nStaggered job start: delaying by", as.integer(delayStart), "minutes."))
  Sys.sleep(delayStart*60)
}

source("06-preamble.R")
source("07-speciesLayers.R")

message(crayon::red(runName))

if (isFALSE(postProcessOnly)) {
  source("08-borealDataPrep.R")
  source("09-pre-sim.R")

  if (isFALSE(usePOM)) {
    source("10-main-sim.R")
    #source("11-post-sim.R")
  } else {
    source("10a-POM.R") ## TODO: may not work out-of-the-box; untested!!
  }
} else {
  #mySimOut <- readRDS(simFile("mySimOut", Paths$outputPath, 1000))
  source("12-postprocessing.R")
}

#source("11-post-sim.R")

if (FALSE) {
  source("13-old.R")
}
