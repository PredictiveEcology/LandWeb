################################################################################
## set run name
################################################################################

if (pemisc::user("emcintir")) {
  #runName <- "LandWeb_aspenDispersal_logROS"
  runName <- "LandWeb_highDispersal_logROS"
}

if (FALSE) {
  runName <- "tolko_SK_highDispersal_logROS_prof"
}

if (isTRUE(batchMode)) {
  stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set in batch_mode.R
} else {
  if (pemisc::user("achubaty") || pemisc::user("emcintir")) {
    #runName <- "tolko_SK_aspenDispersal_logROS_test01"
    #runName <- "tolko_SK_highDispersal_logROS_test01"
    #runName <- "tolko_SK_highDispersal_test01"
  }

  ## running locally
  #runName <- "ANC"
  #runName <- "ANC_aspenDispersal_logROS"
  #runName <- "ANC_doubleFRI"
  #runName <- "ANC_equalROS"
  #runName <- "ANC_logROS"
  #runName <- "ANC_noDispersal"

  ## running locally
  #runName <- "DMI"
  #runName <- "DMI_aspenDispersal_logROS"
  #runName <- "DMI_doubleFRI"
  #runName <- "DMI_equalROS"
  #runName <- "DMI_logROS"
  #runName <- "DMI_noDispersal"

  ## running locally
  #runName <- "LP_MB"
  #runName <- "LP_MB_aspenDispersal_logROS"
  #runName <- "LP_MB_doubleFRI"
  #runName <- "LP_MB_equalROS"
  #runName <- "LP_MB_logROS"
  #runName <- "LP_MB_noDispersal"

  ## running locally
  #runName <- "tolko_AB_N"
  #runName <- "tolko_AB_S"
  #runName <- "tolko_SK"

  ## running locally
  #runName <- "tolko_AB_N_doubleFRI"
  #runName <- "tolko_AB_S_doubleFRI"
  #runName <- "tolko_SK_doubleFRI"

  ## running locally
  #runName <- "tolko_AB_N_equalROS"
  #runName <- "tolko_AB_S_equalROS"
  #runName <- "tolko_SK_equalROS"

  ## running locally
  #runName <- "tolko_AB_N_logROS"
  #runName <- "tolko_AB_S_logROS"
  #if (pemisc::user("emcintir")) runName <- "tolko_SK_logROS"

  ## running locally
  #runName <- "tolko_AB_N_noDispersal"
  #runName <- "tolko_AB_S_noDispersal"
  #runName <- "tolko_SK_noDispersal"

  ## running locally
  #runName <- "tolko_AB_N_aspenDispersal_logROS"
  #runName <- "tolko_AB_S_aspenDispersal_logROS"
  #runName <- "tolko_SK_aspenDispersal_logROS"

  ## running locally
  #runName <- "tolko_AB_N_aspen80"
  #runName <- "tolko_AB_S_aspen80"
  #runName <- "tolko_SK_aspen80"

  ## running locally
  #runName <- "LandWeb_aspenDispersal_logROS"
}
message(crayon::red(runName))
