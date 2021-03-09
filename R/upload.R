Require::Require("jimhester/archive")
Require::Require("googledrive")

outputPath <- normalizePath(file.path("outputs", runName))

#f1 <- list.files(outputPath, "[.]png")

z1 <- file.path(outputPath, paste0(runName, "_boxplots.7z"))
archive::archive_write_dir(archive = z1, dir = file.path(outputPath, "boxplots"),
                           full.names = FALSE, recursive = TRUE)

z2 <- file.path(outputPath, paste0(runName, "_histograms.7z"))
archive::archive_write_dir(archive = z2, dir = file.path(outputPath, "histograms"),
                           full.names = FALSE, recursive = TRUE)

filesToUpload <- c(#f1,
                   z1, z2)

gdrive_ID <- switch(runName,
                    AlPac_highDispersal_logROS = "1Fmg9i070XHBRK6ScOixQZXNcZL6wtv9C",
                    ANC_aspenDiseprsal_logROS = "1aGzdCQJvE0L76ROEdxv09qzeEJQp2s4A",
                    BlueRidge_highDispersal_logROS = "1Lq6oNT0qG3P_HaTzjOi_VVR3IpdM9Say",
                    DMI_aspenDispersal_logROS = "1BMAKr_vuR3ZDnVKsRRDvckMVMhLoaSB0",
                    Edson_highDispersal_logROS = "14PKDcEZGorhLkaDQv5rF0-qD3bbWzixn",
                    FMANWT_highDispersal_logROS = "10BkqSRr5m5d1LQvT21ZmJUoi4EXKcyik",
                    FMANWT2_highDispersal_logROS = "1ZdBmICjA6pINzZ8KFIngU9QmjQDUDlxo",
                    LandWeb_highDispersal_logROS = "1k6MCpSiLEDSeys2_8mn9jc6DrKAlcgXL",
                    LP_BC_highDispersal_logROS = "1dnLS3XC89eg0-olNnHLZhtO-8R7qt_UO",
                    LP_MB_aspenDispersal_logROS = "18cd1rFWh-k-IvQjiwtiiAjRRBDKm1BPf",
                    Manning_highDispersal_logROS = "10NzbpAEDDTUHmId2DA_Z7hWwOaZbRtzo",
                    MillarWestern_highDispersal_logROS = "13J0aOUpIQPgnXl1w_PavBU7NLPC2wRXc",
                    Mistik_highDispersal_logROS = "1X6QgcBq8usDBLjtqoEQsP5W4yRi1of4Y",
                    provAB_highDispersal_logROS = "1qQW5osWLb1PaLjRIh64CnEqlxTKRl0Ar",
                    provNWT_highDispersal_logROS = "11kYmwV2jWi6S8XZ1i056nNSH9qOGfF7_",
                    provSK_highDispersal_logROS = "1PbmS3HH2WRN5NMg7htm-Xb0-DV4AgAWt",
                    Sundre_highDispersal_logROS = "1yMwGxHwKiY2umY7RcqRGpLDV14S8Z_sv",
                    Tolko_AB_N_aspenDispersal_logROS = "1ssKmOEhBY25mGWH5VH4WcxBwCIc1Sigt",
                    Tolko_AB_S_aspenDispersal_logROS = "1gVQUs9HhQvu-gg1YPklfRFIyQ5FCOfNb",
                    Tolko_SK_aspenDispersal_logROS = "1-u8MnaPfIrOTqZ0qqa4ZJJxX6iXOxy77",
                    Vanderwell_highDispersal_logROS = "1HCzpKDrskhwa8_jXabnlauwJBSqjr2-l",
                    WestFraser_N_highDispersal_logROS = "1Fqm6g1x45Db7j4oqG2BPp6ck3ARkjaYF",
                    WestFraser_S_highDispersal_logROS = "1lX2Gleal7mr-yDqOXo8om83Yna5liCyi",
                    WeyCo_GP_highDispersal_logROS = "1eBXczso7svpWY3fv152lf1vKapla-axn",
                    WeyCo_PT_highDispersal_logROS = "16A8TI1PZWyyGFKOEDY9qb-ydHnH6SeHm",
                    WeyCo_SK_highDispersal_logROS = "1XWe0eMOEvGXMOoapzcDzGWhu1ctsg8_T"
)

lapply(filesToUpload, function(f) {
  drive_upload(file.path("outputs", runName, f), as_id(gdrive_ID), overwrite = TRUE)
})
