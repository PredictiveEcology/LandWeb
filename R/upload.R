Require::Require("archive")
Require::Require("googledrive")

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
#' @return A dribble of the uploaded files (not directories though.)
drive_upload_folder <- function(folder, drive_path) {
  # Only call fs::dir_info once in order to avoid weirdness if the contents of the folder is changing
  contents <- fs::dir_info(folder, type = c("file", "dir"))
  dirs_to_upload <- contents %>%
    dplyr::filter(type == "directory") %>%
    pull(path)

  folderIDs <- drive_ls(drive_path)
  fid <- folderIDs[folderIDs[["name"]] == basename(folder), "id"][[1]]
  if (length(fid) == 0) {
    fid <- drive_mkdir(basename(folder), drive_path)[["id"]]
  }

  # Directly upload the files
  uploaded_files <- contents %>%
    dplyr::filter(type == "file") %>%
    pull(path) %>%
    purrr::map_dfr(googledrive::drive_put, path = fid)

  # Create the next level down of directories
  purrr::map2_dfr(dirs_to_upload, fid, drive_upload_folder) %>%
    dplyr::bind_rows(uploaded_files) %>%
    invisible() ## return a dribble of what's been uploaded
}

outputPath <- normalizePath(file.path("outputs", config$context[["runName"]]))

d1 <- file.path(outputPath, "boxplots")
d2 <- file.path(outputPath, "histograms")

dirsToUpload <- c(d1, d2)

f0 <- list.files(file.path(outputPath), "[.]csv", full.names = TRUE)
f1 <- list.files(file.path(outputPath, "figures"), "[.]png", full.names = TRUE)

z1 <- file.path(outputPath, paste0(config$context[["runName"]], "_boxplots.7z"))
archive::archive_write_dir(archive = z1, dir = file.path(outputPath, "boxplots"),
                           full.names = FALSE, recursive = TRUE)

z2 <- file.path(outputPath, paste0(config$context[["runName"]], "_histograms.7z"))
archive::archive_write_dir(archive = z2, dir = file.path(outputPath, "histograms"),
                           full.names = FALSE, recursive = TRUE)

filesToUpload <- c(f0, f1, z1, z2)

LandWeb_Results <- as_id("0AEyFltUAISU-Uk9PVA")
runName_Results <- drive_ls(LandWeb_Results)

gdrive_ID <- which(runName_Results[["name"]] == config$context[["runName"]])

if (length(gdrive_ID) == 0) {
  gdrive_ID <- drive_mkdir(name = config$context[["runName"]], LandWeb_Results)[["id"]]
}

lapply(filesToUpload, function(f) {
  drive_put(f, as_id(gdrive_ID))
})

lapply(dirsToUpload, function(d) {
  drive_upload_folder(d, gdrive_ID)
})
