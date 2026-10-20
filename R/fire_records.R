## The national fire records behind burnSummaries' historical burn map (mode = "multi"): NBAC
## perimeters, plus NFDB polygons for the years NBAC lacks. The module fetches them itself, into its
## inputPath, which every summaries_burn_<sa> shares; target fireRecords runs the same fetch once,
## before any of them, so that they find a verified extraction and only read it. A project helper
## rather than part of fireregimetools, whose every change re-runs mainSim (burnSummaries lists it).

## @param dest directory the records are fetched into: summaries_burn's inputPath.
## @return the shapefiles, each with its sidecar files, that burnSummaries' own fetch will read.
fire_records_fetch <- function(dest) {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  ## the fetchers burnSummaries calls, with their default archive URLs and the module's (absolute)
  ## directory, so the downloads, extractions and stamps are the ones its own fetch will find. They
  ## also load the records inside a study area; a 1 km square keeps that step short.
  probe <- terra::vect("POLYGON ((0 0, 1000 0, 1000 1000, 0 1000, 0 0))", crs = "EPSG:3978")
  fireregimetools::fetch_nbac_polys(probe, dest = normalizePath(dest))
  fireregimetools::fetch_nfdb_polys(probe, dest = normalizePath(dest))

  shp <- c(fire_records_shp(dest, "NBAC_.*\\.shp$"), fire_records_shp(dest, "NFDB_poly_.*\\.shp$"))
  files <- unlist(lapply(shp, function(f) {
    parts <- list.files(dirname(f), full.names = TRUE)
    parts[startsWith(basename(parts), sub("shp$", "", basename(f)))]
  }))
  ## a stamped extraction is trusted without a size check, and a truncated .shp still reads as the
  ## full feature count when its .shx is intact (the 2026 NBAC failure), so check the headers here
  idx <- files[tolower(tools::file_ext(files)) %in% c("shp", "shx")]
  short <- idx[!vapply(idx, fire_records_whole, logical(1))]
  if (length(short)) {
    stop("fire records shorter or longer than their headers say: ", paste(short, collapse = ", "))
  }
  sort(files)
}

## The shapefiles fireregimetools' fetch returns for `pattern`: those at the top level of `dest`, or
## if there are none, anywhere below it.
fire_records_shp <- function(dest, pattern) {
  found <- list.files(dest, pattern = pattern, full.names = TRUE)
  if (!length(found)) {
    found <- list.files(dest, pattern = pattern, full.names = TRUE, recursive = TRUE)
  }
  if (!length(found)) {
    stop("no fire records matching '", pattern, "' in ", dest)
  }
  sort(found)
}

## TRUE when a .shp or .shx file is as long as its header says (bytes 25-28: the length in 16-bit
## words, big-endian, after the file code 9994).
fire_records_whole <- function(f) {
  con <- file(f, "rb")
  on.exit(close(con))
  h <- as.integer(readBin(con, "raw", 28L))
  length(h) == 28L &&
    sum(h[1:4] * 256^(3:0)) == 9994 &&
    2 * sum(h[25:28] * 256^(3:0)) == file.size(f)
}
