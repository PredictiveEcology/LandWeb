## The national fire records behind burnSummaries' historical burn map (mode = "multi"): NBAC
## perimeters, plus NFDB polygons for the years NBAC lacks. The module fetches them itself, into its
## inputPath, which every summaries_burn_<sa> shares; target fireRecords runs the same fetch once,
## before any of them, so that they find a verified extraction and only read it. A project helper
## rather than part of fireregimetools, whose every change re-runs mainSim (burnSummaries lists it).
## The shapefile patterns and search order here, and the NFDB year columns, copy fireregimetools
## internals (.fire_archive_files(), the patterns in fetch_nbac_polys() and fetch_nfdb_polys(), and
## load_nfdb_polys(); R/fire_data.R): re-check them whenever it changes.

## @param dest directory the records are fetched into: summaries_burn's inputPath.
## @return the shapefiles, each with its sidecar files, that burnSummaries' own fetch will read.
fire_records_fetch <- function(dest) {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  ## the fetchers burnSummaries calls, with their default archive URLs and the module's (absolute)
  ## directory, so the downloads, extractions and stamps are the ones its own fetch will find. They
  ## also load the records inside a study area; a 1 km square keeps that step short. A missing .shx
  ## or .dbf stops that step, so on an error the damage checks run first, to say what is wrong.
  probe <- terra::vect("POLYGON ((0 0, 1000 0, 1000 1000, 0 1000, 0 0))", crs = "EPSG:3978")
  tryCatch(
    {
      fireregimetools::fetch_nbac_polys(probe, dest = normalizePath(dest))
      fireregimetools::fetch_nfdb_polys(probe, dest = normalizePath(dest))
    },
    error = function(e) {
      fire_records_check(dest, unlist(fire_records_sets(dest), use.names = FALSE))
      stop(e)
    }
  )

  sets <- fire_records_sets(dest)
  none <- names(sets)[lengths(sets) == 0L]
  if (length(none)) {
    stop("no ", paste(none, collapse = " or "), " fire records in ", dest)
  }
  shp <- unlist(sets, use.names = FALSE)
  fire_records_check(dest, shp)
  for (set in sets) {
    fire_records_one_release(set)
  }
  ## burnSummaries takes NFDB polygons only for the years NBAC (1972 on) lacks
  year <- unlist(lapply(fire_records_sidecar(sets$NFDB, "dbf"), function(f) {
    d <- foreign::read.dbf(f, as.is = TRUE)
    as.numeric(d[[intersect(c("YEAR", "FIRE_YEAR"), names(d))[1L]]])
  }))
  if (!any(year > 0 & year < 1972, na.rm = TRUE)) {
    stop(
      "no NFDB fire before 1972 in ",
      paste(sets$NFDB, collapse = ", "),
      ": burnSummaries uses NFDB only for the years NBAC (1972 on) lacks. The current ",
      "NFDB_poly.zip holds none; they are in NFDB_poly_pre1972.zip, which fireregimetools does ",
      "not fetch, so use the 20210707 release instead"
    )
  }

  files <- unlist(lapply(shp, function(f) {
    parts <- list.files(dirname(f), full.names = TRUE)
    parts[startsWith(basename(parts), sub("shp$", "", basename(f)))]
  }))
  sort(files, method = "radix")
}

## Stop, saying how to repair them, if any of the shapefiles `shp` in `dest` is damaged. A stamped
## extraction is trusted without a size check, and a .shp or .dbf cut short loses the records past
## the cut with no error (terra drops them with GDAL warnings only; sf kept the full feature count
## in the 2026 NBAC failure), so each file is checked against its header here.
fire_records_check <- function(dest, shp) {
  bad <- unlist(lapply(shp, fire_records_damage))
  if (length(bad)) {
    stamps <- list.files(dest, "^[.].*[.]complete$", all.files = TRUE)
    if (!length(stamps)) {
      stamps <- "none"
    }
    stop(
      "damaged fire records: ",
      paste(bad, collapse = "; "),
      ". To repair them, remove the archive's stamp from ",
      dest,
      " (found: ",
      paste(stamps, collapse = ", "),
      "), keeping its .zip, and run fireRecords again: the fetch then checks the files against ",
      "the archive and extracts it again if they fall short"
    )
  }
  invisible(shp)
}

## The shapefiles of each record set that fireregimetools' fetch reads: for each pattern, those at
## the top level of `dest`, or if there are none, anywhere below it.
fire_records_sets <- function(dest, full.names = TRUE) {
  lapply(c(NBAC = "NBAC_.*\\.shp$", NFDB = "NFDB_poly_.*\\.shp$"), function(pattern) {
    found <- list.files(dest, pattern = pattern, full.names = full.names)
    if (!length(found)) {
      found <- list.files(dest, pattern = pattern, full.names = full.names, recursive = TRUE)
    }
    sort(found)
  })
}

## Stop when the shapefiles `shp` of one record set would give burnSummaries a fire twice: its fetch
## reads every one that matches, so they must be one release (the 8-digit date in their names), in
## one directory, with no record (a row of the attribute table, on the columns they share) in two of
## them. A release in several partitions passes; a copy of one beside it does not.
fire_records_one_release <- function(shp) {
  release <- paste(dirname(shp), sub("^.*?([0-9]{8}).*$", "\\1", basename(shp), perl = TRUE))
  if (length(unique(release)) > 1L) {
    stop(
      "fire records from more than one release (or directory): ",
      paste(shp, collapse = ", "),
      ". burnSummaries reads them all, so a fire in two of them would count twice: keep one"
    )
  }
  if (length(shp) > 1L) {
    tables <- lapply(fire_records_sidecar(shp, "dbf"), foreign::read.dbf, as.is = TRUE)
    cols <- Reduce(intersect, lapply(tables, names))
    rows <- lapply(tables, function(d) unique(do.call(paste, c(unname(d[cols]), sep = "\r"))))
    key <- unlist(rows, use.names = FALSE)
    twice <- unique(rep(seq_along(rows), lengths(rows))[key %in% key[duplicated(key)]])
    if (length(twice)) {
      stop(
        "the same fire records in more than one shapefile: ",
        paste(shp[sort(twice)], collapse = ", "),
        ". burnSummaries reads them all, so it would count those fires twice: keep one"
      )
    }
  }
  invisible(shp)
}

## The `ext` file (shx or dbf, in either case) beside each shapefile in `shp`, or NA if none.
fire_records_sidecar <- function(shp, ext) {
  vapply(
    shp,
    function(f) {
      side <- paste0(sub("shp$", "", f), c(ext, toupper(ext)))
      c(side[file.exists(side)], NA_character_)[1L]
    },
    character(1),
    USE.NAMES = FALSE
  )
}

## What is wrong with shapefile `f`, if anything: no .shx or .dbf beside it, one of the three not as
## long as its header says, or a .dbf without one record for each entry in the .shx.
fire_records_damage <- function(f) {
  shx <- fire_records_sidecar(f, "shx")
  dbf <- fire_records_sidecar(f, "dbf")
  if (is.na(shx) || is.na(dbf)) {
    absent <- paste(c(".shx", ".dbf")[is.na(c(shx, dbf))], collapse = " or ")
    return(paste(f, "has no", absent, "beside it"))
  }
  n <- fire_records_dbf_n(dbf)
  short <- c(f, shx, dbf)[!c(fire_records_whole(f), fire_records_whole(shx), !is.na(n))]
  if (length(short)) {
    return(paste(short, "is shorter or longer than its header says"))
  }
  n_shx <- (file.size(shx) - 100) / 8
  if (n != n_shx) {
    return(sprintf("%s holds %.0f records but %s indexes %.0f shapes", dbf, n, shx, n_shx))
  }
  character(0)
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

## The number of records in a .dbf (bytes 5-8 of its header), or NA when the file is not as long as
## its header says: the header length (bytes 9-10) plus that many records of the record length
## (bytes 11-12), all little-endian, with or without an end-of-file byte.
fire_records_dbf_n <- function(f) {
  con <- file(f, "rb")
  on.exit(close(con))
  h <- as.integer(readBin(con, "raw", 12L))
  if (length(h) < 12L) {
    return(NA_real_)
  }
  n <- sum(h[5:8] * 256^(0:3))
  extra <- file.size(f) - sum(h[9:10] * 256^(0:1)) - n * sum(h[11:12] * 256^(0:1))
  if (extra %in% 0:1) n else NA_real_
}
