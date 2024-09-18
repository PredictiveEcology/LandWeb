library("archive")
library("furrr")
library("googledrive")

SpaDES.config::authGoogle(tryToken = "landweb")

# plan(multisession, workers = 8)
plan(sequential)

#' Upload a folder to Google Drive
#'
#' Based on <https://github.com/tidyverse/googledrive/issues/200#issuecomment-1112766367>.
#'
#' Upload the contents of a folder (directory) to Google Drive recursively. The
#' implementation is depth-first. Only uploads objects that have type "file" or
#' "directory" according to `fs::dir_info()`; ignores types "symlink", "FIFO",
#' "socket", "character_device" or "block_device".
#'
#' @param folder The local folder that is to be uploaded, given as a path e.g. with `fs::path()`.
#'   The folder and its contents are uploaded.
#'
#' @param drive_path The destination folder on Google Drive, given as a URL, file id, or dribble
#'
#' @param batch_size the maximum number of files per upload batch.
#'   Uploads are done in batches to mitigate curl handle errors.
#'
#' @return A dribble of the uploaded files (not directories though.)
drive_upload_folder <- function(folder, drive_path, batch_size = 10) {
  ## avoid curl HTTP2 framing layer error:
  ## per https://github.com/tidyverse/googlesheets4/issues/233#issuecomment-889376499
  old <- httr::set_config(httr::config(http_version = 2)) ## corresponds to CURL_HTTP_VERSION_1_1
  on.exit(httr::set_config(old), add = TRUE)

  ## Only call fs::dir_info once in order to avoid weirdness if the contents of the folder is changing
  contents <- fs::dir_info(folder, type = c("file", "dir"))
  dirs_to_upload <- contents |>
    dplyr::filter(type == "directory") |>
    dplyr::pull(path)

  folderIDs <- googledrive::drive_ls(drive_path)
  fid <- folderIDs[folderIDs[["name"]] == basename(folder), "id"][[1]]
  if (length(fid) == 0) {
    fid <- googledrive::drive_mkdir(basename(folder), drive_path)[["id"]]
  }

  # Directly upload the files
  files_to_upload <- contents |>
    dplyr::filter(type == "file") |>
    dplyr::pull(path)
  g <- seq(files_to_upload) %/% batch_size
  uploaded_files <- files_to_upload |>
    split(g) |>
    lapply(function(x) {
      furrr::future_map_dfr(x, googledrive::drive_put, path = fid)
    }) |>
    dplyr::bind_rows()

  # Create the next level down of directories
  furrr::future_map2_dfr(dirs_to_upload, fid, drive_upload_folder) |>
    dplyr::bind_rows(uploaded_files) |>
    invisible() ## return a dribble of what's been uploaded
}

outputPath <- normalizePath(file.path("outputs", config$context[["runName"]]))

d1 <- file.path(outputPath, "boxplots")
d2 <- file.path(outputPath, "histograms")

dirsToUpload <- c(d1, d2)

f0 <- file.path(outputPath, "INFO.md")
f1 <- list.files(file.path(outputPath), "[.]csv", full.names = TRUE)
f2 <- list.files(file.path(outputPath, "figures"), "[.]png", full.names = TRUE)

z1 <- file.path(outputPath, paste0(config$context[["runName"]], "_boxplots.7z"))
archive::archive_write_dir(archive = z1, dir = file.path(outputPath, "boxplots"),
                           full.names = FALSE, recursive = TRUE)

z2 <- file.path(outputPath, paste0(config$context[["runName"]], "_histograms.7z"))
archive::archive_write_dir(archive = z2, dir = file.path(outputPath, "histograms"),
                           full.names = FALSE, recursive = TRUE)

filesToUpload <- c(f0, f1, f2, z1, z2)

LandWeb_Results <- googledrive::as_id("0AEyFltUAISU-Uk9PVA")

runName_Results <- googledrive::drive_ls(LandWeb_Results)

gdrive_ID <- runName_Results[runName_Results[["name"]] == config$context[["runName"]], ][["id"]]

if (length(gdrive_ID) == 0) {
  gdrive_ID <- googledrive::drive_mkdir(name = config$context[["runName"]], LandWeb_Results)[["id"]]
}

lapply(filesToUpload, function(f) {
  drive_put(f, as_id(gdrive_ID))
})

lapply(dirsToUpload, function(d) {
  SpaDES.config::authGoogle(tryToken = "landweb") ## TODO: why is re-auth needed?
  drive_upload_folder(d, gdrive_ID, batch_size = 20)
})
