1 -> rep; Sys.sleep(600*rep); runName <- paste0("tolko_AB_N_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(36*3600 + 600*rep); runName <- paste0("tolko_AB_S_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(72*3600 + 600*rep); runName <- paste0("tolko_SK_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(24*3600 + 600*rep); runName <- paste0("ANC_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()

1 -> rep; Sys.sleep(12*3600 + 600*rep); runName <- paste0("LP_MB_aspenDispersal_logROS_rep", SpaDES.core::paddedFloatToChar(rep, padL = 2)); source("newStart.R"); Q()
