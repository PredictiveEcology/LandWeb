################################################################################
## parameter estimation using POM (LandWeb#111)
################################################################################

#runName <- "tolko_SK_highDispersal_logROS__test_POM"

data.table::setDTthreads(useParallel)

testFn <- function(params, sim) {
  sim2 <- reproducible::Copy(sim)

  params(sim2)$speciesParams$seeddistance_eff$Abie_sp <- params[1]
  params(sim2)$speciesParams$seeddistance_eff$Pice_gla <- params[2]
  params(sim2)$speciesParams$seeddistance_eff$Pice_mar <- params[3]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_ban <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_con <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_sp <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Popu_sp <- params[5]

  params(sim2)$speciesParams$seeddistance_max$Abie_sp <- params[6]
  params(sim2)$speciesParams$seeddistance_max$Pice_gla <- params[7]
  params(sim2)$speciesParams$seeddistance_max$Pice_mar <- params[8]
  params(sim2)$speciesParams$seeddistance_max$Pinu_ban <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Pinu_con <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Pinu_sp <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Popu_sp <- params[10]

  out <- lapply(params, function(x) sum(sapply(x, "sum")))
  return(out$seeddistance_max - out$seeddistance_eff)
}

objectiveFunction <- function(params, sim) {
  sim2 <- Copy(sim)

  params(sim2)$speciesParams$seeddistance_eff$Abie_sp <- params[1]
  params(sim2)$speciesParams$seeddistance_eff$Pice_gla <- params[2]
  params(sim2)$speciesParams$seeddistance_eff$Pice_mar <- params[3]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_ban <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_con <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Pinu_sp <- params[4]
  params(sim2)$speciesParams$seeddistance_eff$Popu_sp <- params[5]

  params(sim2)$speciesParams$seeddistance_max$Abie_sp <- params[6]
  params(sim2)$speciesParams$seeddistance_max$Pice_gla <- params[7]
  params(sim2)$speciesParams$seeddistance_max$Pice_mar <- params[8]
  params(sim2)$speciesParams$seeddistance_max$Pinu_ban <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Pinu_con <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Pinu_sp <- params[9]
  params(sim2)$speciesParams$seeddistance_max$Popu_sp <- params[10]

  httr::set_config(httr::config(http_version = 0)) ## workaround 'HTTP2 framing layer' error

  mySimOut <- spades(sim2, .plotInitialTime = NA)

  summaryTable <- mySimOut$summaryBySpecies1[, totalPixels := sum(counts), by = year]
  summaryTable[, proportion := counts / totalPixels]

  initial <- summaryTable[year == 0, ]
  final <- summaryTable[year == end(sim), ]

  species <- unique(initial$leadingType)

  val <- 0
  for (x in species) {
    p_i <- initial[leadingType == x,]$proportion
    p_f <- final[leadingType == x,]$proportion

    # deal with missing species
    if (identical(p_i, numeric(0))) p_i <- 0
    if (identical(p_f, numeric(0))) p_f <- 0

    val <<- val + (p_f - p_i)^2
  }

  return(val)
}

parametersPOM <- parameters
lapply(names(parametersPOM), function(x) {
  parametersPOM[[x]]$.plotInitialTime <<- NA
  parametersPOM[[x]]$.useParallel <<- useParallel
})

opts2 <- options("LandR.assertions" = FALSE, "LandR.verbose" = 0)
mySim <- simInit(times = list(start = 0, end = 250),
                 params = parametersPOM,
                 modules = modules,
                 outputs = outputs,
                 objects = objects,
                 paths = paths3,
                 loadOrder = unlist(modules)
)

params4POM <- data.frame(
  name = c("seeddistance_eff_Abie_sp", "seeddistance_eff_Pice_gla", "seeddistance_eff_Pice_mar",
           "seeddistance_eff_Pinu_sp", "seeddistance_eff_Popu_sp",
           "seeddistance_max_Abie_sp", "seeddistance_max_Pice_gla", "seeddistance_max_Pice_mar",
           "seeddistance_max_Pinu_sp", "seeddistance_max_Popu_sp"),
  lower = c(200, 100,  80, 300, 200, 1000, 1250, 3000, 3000, 5000),
  upper = c(300, 300, 300, 500, 300, 1500, 3000, 5000, 5000, 5000),
  stringsAsFactors = FALSE
)

packages4POM <- unique(c("lme4", "LandR", "map", "quickPlot", "reproducible",
                         "SpaDES.core", "SpaDES.tools", moduleRqdPkgs, googleAuthPkgs))

if (isTRUE(useDEoptim)) {
  ## NOTE: bug in DEoptim prevents using our own cluster (ArdiaD/DEoptim#3)
  N <- 10 * nrow(params4POM) ## need 10 populations per parameter
  #cl <- parallel::makeCluster(N, type = "SOCK") ## forking doesn't work with data.table

  outPOM <- DEoptim::DEoptim(fn = objectiveFunction, #testFn,
                             sim = mySim,
                             control = DEoptim::DEoptim.control(
                               #cluster = cl, ## see ArdiaD/DEoptim#3
                               initialpop = matrix(c(
                                 runif(N, params4POM[1,]$lower, params4POM[1,]$upper),
                                 runif(N, params4POM[2,]$lower, params4POM[2,]$upper),
                                 runif(N, params4POM[3,]$lower, params4POM[3,]$upper),
                                 runif(N, params4POM[4,]$lower, params4POM[4,]$upper),
                                 runif(N, params4POM[5,]$lower, params4POM[5,]$upper),
                                 runif(N, params4POM[6,]$lower, params4POM[6,]$upper)
                               ), ncol = nrow(params4POM)),
                               itermax = 30,
                               packages = packages4POM,
                               parallelType = 1,
                               parVar = list("objectiveFunction", "mySim"),
                               VTR = 0
                             ),
                             lower = params4POM$lower,
                             upper = params4POM$upper
  )
  #parallel::stopCluster(cl) ## see ArdiaD/DEoptim#3
} else {
  n <- 2 ## number of values per parameter to use

  tableOfRuns <- expand.grid(
    seeddistance_eff_Abie_sp = seq(params4POM[1,]$lower, params4POM[1,]$upper, length.out = 1), ## TODO
    seeddistance_eff_Pice_gla = seq(params4POM[2,]$lower, params4POM[2,]$upper, length.out = n),
    seeddistance_eff_Pice_mar = seq(params4POM[3,]$lower, params4POM[3,]$upper, length.out = n),
    seeddistance_eff_Pinu_sp = seq(params4POM[4,]$lower, params4POM[4,]$upper, length.out = n),
    seeddistance_eff_Popu_sp = seq(params4POM[5,]$lower, params4POM[5,]$upper, length.out = n),

    seeddistance_max_Abie_sp = seq(params4POM[6,]$lower, params4POM[6,]$upper, length.out = 1), ## TODO
    seeddistance_max_Pice_gla = seq(params4POM[7,]$lower, params4POM[7,]$upper, length.out = n),
    seeddistance_max_Pice_mar = seq(params4POM[8,]$lower, params4POM[8,]$upper, length.out = n),
    seeddistance_max_Pinu_sp = seq(params4POM[9,]$lower, params4POM[9,]$upper, length.out = n),
    seeddistance_max_Popu_sp = seq(params4POM[10,]$lower, params4POM[10,]$upper, length.out = n)
  )
  tableOfRuns$objFnReturn <- rep(NA_real_, NROW(tableOfRuns))

  cl <- parallel::makePSOCKcluster(2 * nrow(params4POM)) ## forking doesn't work with data.table
  parallel::clusterExport(cl, list("objectiveFunction", "googleAuthPkgs", "moduleRqdPkgs"))
  parallel::clusterEvalQ(cl, {
    library(plyr)
    library(dplyr)
    library(reproducible)
    Require(c("SpaDES.core", "pemisc", "map", "LandR", "LandWebUtils", googleAuthPkgs, moduleRqdPkgs))
  })

  out <- parallel::parLapplyLB(cl = cl,
                               purrr::transpose(tableOfRuns),
                               function(x, sim) {
                                 #testFn(unlist(x[1:6]), sim)
                                 objectiveFunction(unlist(x[1:10]), sim)
                               }, sim = mySim)
  tableOfRuns$objFnReturn <- unlist(out)

  # FROM = 1; TO = 1;
  # FROM = 1; TO = nrow(tableOfRuns);
  # ids <- seq(FROM, TO, by = 1)
  # out <- lapply(purrr::transpose(tableOfRuns[ids,]),
  #               function(x, sim) {
  #                 #testFn(unlist(x[1:6]), sim)
  #                 objectiveFunction(unlist(x[1:6]), sim)
  #               }, sim = mySim)
  # tableOfRuns$objFnReturn[ids] <- unlist(out)

  parallel::stopCluster(cl)
}

options(opts2)
