Require::Require("jimhester/archive")
Require::Require("googledrive")

outputPath <- file.path("outputs", runName)

#f1 <- list.files(outputPath, "[.]png")

z1 <- file.path(outputPath, paste0(runName, "_boxplots.7z"))
archive::archive_write_dir(archive = z1, dir = file.path(outputPath, "boxplots"),
                           full.names = TRUE, recursive = TRUE)

z2 <- file.path(outputPath, paste0(runName, "_histograms.7z"))
archive::archive_write_dir(archive = z2, dir = file.path(outputPath, "histograms"),
                           full.names = TRUE, recursive = TRUE)

filesToUpload <- c(#f1,
                   z1, z2)

gdrive_ID <- switch(runName,
                    AlPac_highDispersal_logROS = "",
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    ,
                    LandWeb_highDispersal_logROS = "",
                    )

lapply(filesToUpload, function(f) {
  drive_upload(file.path("outputs", runName, f), as_id(gdrive_ID), overwrite = TRUE)
})
