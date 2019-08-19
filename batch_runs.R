#####
1 -> rep; Sys.sleep(600*rep); runName <- paste0("DMI_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(20*3600 + 600*rep); runName <- paste0("tolko_AB_N_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(36*3600 + 600*rep); runName <- paste0("tolko_AB_S_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(50*3600 + 600*rep); runName <- paste0("tolko_SK_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(60*3600 + 600*rep); runName <- paste0("ANC_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(72*3600 + 600*rep); runName <- paste0("LP_MB_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

#####

1 -> rep; Sys.sleep(rep); runName <- paste0("tolko_SK_highDispersal_logROS_test_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R")

1 -> rep; Sys.sleep(rep); runName <- paste0("Manning_highDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R")

#####

2 -> rep; Sys.sleep(rep); runName <- paste0("LandWeb_highDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R")
