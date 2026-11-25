## Data for report 04, the differences between the version 2 and version 3 outputs
## (reports/04-output-differences.qmd).
##
## Report 04 compares, per v3 tenure with a v2 run (v2_v3_crosswalk(), R/v2_crosswalk.R), what the two
## versions simulated: the share of forest in each age class by vegetation type (the NRV box plots),
## the area of old forest in large patches (the histograms), the share of forest by vegetation type
## (the check on v2's succession problem), and the reporting boundary. Both versions' current
## conditions are kept, so each set of boxes has its own red circle.
##
## v2 side, from each v2 run's summary files (written by the v2 summary modules; see the column notes
## in read_v2_leading() and read_v2_patches()):
##   boxplots/leading_<layer>.csv, histograms/largePatches_<layer>_<size>.csv, INFO.md, and the
##   current-condition maps CurrentConditionVTM (.grd or .tif) and CurrentConditionTSF.tif.
## v3 side, from NRV_summary's `lw` parquet datasets, read as report 05 reads them
##   (.report05_lw(), R/report_data.R).
##
## build_report04_sa() gathers both for the tenures of one study-area group (target
## `report04Tenures_<sa>`, R/targets_report04.R); build_report04_data() combines the groups and
## computes the tables the report shows (target `report04Data`).

## ---- v2 summary files ------------------------------------------------------------------------

## v2 wrote a layer's CSVs under the layer name with spaces replaced ("Spray Lake" ->
## "leading_Spray_Lake.csv").
.v2_layer_token <- function(layer) gsub(" ", "_", layer, fixed = TRUE)

## v2 vegetation-type labels ("Bl Spruce", "Doug fir") -> the v3 reporting codes ("Bl_Spruce",
## "Doug_fir"; LandWebUtils::landweb_report_map())
.v2_veg_type <- function(x) {
  x <- as.character(x)
  out <- gsub(" ", "_", x, fixed = TRUE)
  out[x == "All species"] <- "All species"
  out
}

## v2 current-condition vegetation-type map codes -> v3 reporting codes. The map's categories are v2's
## species groups; v2's summaries label the same groups "Fir", "Mixed", "Wh Spruce", ... (checked in
## build_report04_sa() against the current-condition shares v2 saved).
.v2_cc_map_types <- c(
  Abie_sp = "Fir",
  Mixed = "Mixed",
  Pice_gla = "Wh_Spruce",
  Pice_mar = "Bl_Spruce",
  Pinu_sp = "Pine",
  Popu_sp = "Decid",
  Pseu_men = "Doug_fir"
)

## A v2 run's current-condition vegetation-type map. Runs re-summarised after 2021 wrote a `.grd`
## beside the original `.tif`, at the same time as their CurrentConditionTSF.tif: take that one.
.v2_cc_vtm_file <- function(run_dir) {
  f <- file.path(run_dir, c("CurrentConditionVTM.grd", "CurrentConditionVTM.tif"))
  f <- f[file.exists(f)]
  if (!length(f)) {
    stop("no current-condition vegetation-type map in ", run_dir, call. = FALSE)
  }
  f[[1L]]
}

## When a v2 run was simulated: the date of the first replicate's saved year-1000 state. Maps
## derived from it can be newer (WestFraser_N's were rewritten in 2023).
.v2_simulated <- function(run_dir) {
  f <- Sys.glob(file.path(run_dir, "rep*", "simulationOutput_year1000.rds"))
  if (length(f)) format(min(file.mtime(f)), "%Y-%m-%d") else ""
}

## "rep01_year0700._.Spray Lake" -> rep 1, year 700
.v2_snapshot <- function(group) {
  ok <- grepl("^rep[0-9]+_year[0-9]+", group)
  if (!all(ok)) {
    stop(
      "unexpected v2 snapshot labels: ",
      paste(utils::head(unique(group[!ok])), collapse = ", "),
      call. = FALSE
    )
  }
  list(
    rep = as.integer(sub("^rep([0-9]+)_year.*$", "\\1", group)),
    time = as.integer(sub("^rep[0-9]+_year([0-9]+).*$", "\\1", group))
  )
}

## the model settings an INFO.md records; it also names the machine and user, which never go in a report
.v2_info_settings <- c(
  "dispersalType",
  "forceResprout",
  "friMultiple",
  "pixelSize",
  "ROStype",
  "succession"
)

## The model settings in a v2 run's INFO.md table (`| dispersalType | high |`), as a named character
## vector; empty without one (most v2 runs predate it).
.v2_info_md <- function(run_dir) {
  f <- file.path(run_dir, "INFO.md")
  if (!file.exists(f)) {
    return(stats::setNames(character(0), character(0)))
  }
  x <- readLines(f, warn = FALSE)
  x <- x[grepl("^\\|.*\\|\\s*$", x)]
  cells <- strsplit(sub("^\\|(.*)\\|\\s*$", "\\1", x), "|", fixed = TRUE)
  cells <- cells[lengths(cells) == 2L]
  keys <- trimws(vapply(cells, `[[`, character(1), 1L))
  vals <- trimws(vapply(cells, `[[`, character(1), 2L))
  keep <- keys %in% .v2_info_settings
  stats::setNames(vals[keep], keys[keep])
}

#' A v2 run's settings
#'
#' The seed dispersal and rate-of-spread settings are in the run's directory name
#' (`<run>_<dispersal>Dispersal_<rate>ROS`); the pixel size is the resolution of its
#' current-condition map; when it was simulated is the modification date of the first replicate's
#' saved year-1000 state (`simulationOutput_year1000.rds`, blank if none); a run with an INFO.md adds
#' the settings recorded there (e.g. `forceResprout`, `succession`, `friMultiple`).
#'
#' @param run_dir A v2 run directory.
#'
#' @return A named character vector: `dispersal`, `rate_of_spread`, `pixel_m`, `simulated` (a date),
#'   then INFO.md's entries.
read_v2_run_settings <- function(run_dir) {
  b <- basename(run_dir)
  if (!grepl("^.+_[a-z]+Dispersal_[a-zA-Z]+ROS$", b)) {
    stop("not a v2 baseline run directory: ", b, call. = FALSE)
  }
  vtm <- terra::rast(.v2_cc_vtm_file(run_dir))
  c(
    dispersal = sub("^.+_([a-z]+)Dispersal_[a-zA-Z]+ROS$", "\\1", b),
    rate_of_spread = sub("^.+_([a-zA-Z]+)ROS$", "\\1", b),
    pixel_m = format(terra::res(vtm)[[1L]]),
    simulated = .v2_simulated(run_dir),
    .v2_info_md(run_dir)
  )
}

#' Read one polygon's age-class shares from a v2 run
#'
#' `boxplots/leading_<layer>.csv` has one row per polygon (`zone`), vegetation type (`vegCover`), age
#' class and simulated landscape (`group`, `"repNN_yearYYYY._.<layer>"`): `NPixels` of the type in the
#' age class, `proportion` = `NPixels` / `totalPixels` (all of the type's pixels in that landscape),
#' and `proportionCC`, the same share in the current condition (repeated on every row).
#'
#' @param run_dir A v2 run directory.
#' @param layer The v2 reporting layer (`layer` in [v2_v3_crosswalk()]).
#' @param zone The polygon's name in that layer (`zone`).
#'
#' @return A list: `nrv`, one row per vegetation type, age class and simulated landscape (`vegType`,
#'   `ageClass`, `rep`, `time`, `value` the share of the type's forest, `n_pixels`, `type_pixels`);
#'   and `cc`, one row per vegetation type and age class (`value`; `NA` where the current condition
#'   has none of the type). A type absent from a landscape has no age structure there, so its rows are
#'   left out of `nrv` (as report 05 leaves out v3's zero rows for `leadingProp`); `type_pixels` of the
#'   remaining rows gives the type's area in each landscape.
read_v2_leading <- function(run_dir, layer, zone) {
  f <- file.path(run_dir, "boxplots", paste0("leading_", .v2_layer_token(layer), ".csv"))
  if (!file.exists(f)) {
    stop("no v2 age-class file ", f, call. = FALSE)
  }
  cols <- c(
    "zone",
    "vegCover",
    "ageClass",
    "proportionCC",
    "group",
    "NPixels",
    "proportion",
    "totalPixels"
  )
  d <- data.table::fread(f, select = cols, na.strings = c("NA", "NaN", ""))
  ## compared outside `[`: inside it, `zone` is the column, not the argument
  in_zone <- d[["zone"]] == zone
  d <- d[in_zone]
  if (!nrow(d)) {
    stop("no rows for v2 polygon '", zone, "' in ", f, call. = FALSE)
  }
  snap <- .v2_snapshot(d[["group"]])
  nrv <- unique(data.table::data.table(
    vegType = .v2_veg_type(d[["vegCover"]]),
    ageClass = as.character(d[["ageClass"]]),
    rep = snap$rep,
    time = snap$time,
    value = as.numeric(d[["proportion"]]),
    n_pixels = as.numeric(d[["NPixels"]]),
    type_pixels = as.numeric(d[["totalPixels"]])
  ))
  if (anyDuplicated(nrv[, .(vegType, ageClass, rep, time)])) {
    stop("several values per type, age class and landscape for '", zone, "' in ", f, call. = FALSE)
  }
  cc <- unique(data.table::data.table(
    vegType = .v2_veg_type(d[["vegCover"]]),
    ageClass = as.character(d[["ageClass"]]),
    value = as.numeric(d[["proportionCC"]])
  ))
  if (anyDuplicated(cc[, .(vegType, ageClass)])) {
    stop(
      "several current-condition values per type and age class for '",
      zone,
      "' in ",
      f,
      call. = FALSE
    )
  }
  list(nrv = nrv[type_pixels > 0 & !is.na(value)], cc = cc)
}

#' The share of forest led by each vegetation type in each simulated v2 landscape
#'
#' @param nrv The `nrv` table from [read_v2_leading()].
#'
#' @return One row per vegetation type (not `"All species"`) and landscape (`vegType`, `rep`,
#'   `time`, `value`): the type's pixels over all forest pixels. A type absent from a landscape gets 0
#'   there, as v3's `leadingShare` does.
v2_type_share <- function(nrv) {
  nrv <- data.table::as.data.table(nrv)
  all <- unique(nrv[vegType == "All species", .(rep, time, all_pixels = type_pixels)])
  if (anyDuplicated(all[, .(rep, time)])) {
    stop("several all-forest totals per landscape", call. = FALSE)
  }
  types <- setdiff(unique(nrv$vegType), "All species")
  grid <- data.table::CJ(vegType = types, idx = seq_len(nrow(all)))
  grid[, `:=`(rep = all$rep[idx], time = all$time[idx])]
  grid[, idx := NULL]
  tp <- unique(nrv[vegType != "All species", .(vegType, rep, time, type_pixels)])
  out <- merge(grid, tp, by = c("vegType", "rep", "time"), all.x = TRUE)
  out[is.na(type_pixels), type_pixels := 0]
  out <- merge(out, all, by = c("rep", "time"))
  out[, .(vegType, rep, time, value = type_pixels / all_pixels)]
}

#' Read a tenure's all-forest age-class shares by sub-region from a v2 run
#'
#' v2 named a sub-region polygon `"<sub-region> <tenure>"` (the tenure as in its tenure layer), or
#' `"<sub-region> LandWeb Study Area"` in runs of a single tenure. A sub-region layer that names no
#' tenure (v2's BC run named BEC zones only, over two TSAs) cannot be split by tenure. Whatever the
#' names, the sub-regions are kept only if their forest adds up to the tenure's in every simulated
#' landscape (within `tol`), so a layer cut to another area is never read as the tenure's.
#'
#' @param run_dir A v2 run directory.
#' @param layer The run's sub-region layer ([v2_subregion_layers()]).
#' @param zone The tenure's name in the run (`zone` in [v2_v3_crosswalk()]).
#' @param tenure_nrv The tenure's own `nrv` table from [read_v2_leading()].
#' @param others The names of the other tenures in the run's tenure layer: a sub-region belongs to the
#'   tenure whose name is the longest that ends it ("<sub-region> Tolko ..., West Fraser Mills Ltd.
#'   (Slave Lake)" is the joint tenure's, though it also ends with "West Fraser Mills Ltd. (Slave
#'   Lake)").
#' @param single Whether the run's tenure layer holds this tenure only.
#' @param tol Largest relative difference allowed between the sum of the sub-regions' forest and the
#'   tenure's.
#'
#' @return A list: `nrv` (`subregion`, `ageClass`, `rep`, `time`, `value`), `cc` (`subregion`,
#'   `ageClass`, `value`) and `reason` (`NA`, or why the sub-regions were not used, with `nrv` and
#'   `cc` then `NULL`).
read_v2_subregions <- function(
  run_dir,
  layer,
  zone,
  tenure_nrv,
  others = character(0),
  single = FALSE,
  tol = 0.02
) {
  out <- function(nrv = NULL, cc = NULL, reason = NA_character_) {
    list(nrv = nrv, cc = cc, reason = reason)
  }
  f <- file.path(run_dir, "boxplots", paste0("leading_", .v2_layer_token(layer), ".csv"))
  if (!file.exists(f)) {
    return(out(reason = "the version 2 run has no summary by sub-region"))
  }
  cols <- c("zone", "vegCover", "ageClass", "proportionCC", "group", "proportion", "totalPixels")
  d <- data.table::fread(f, select = cols, na.strings = c("NA", "NaN", ""))
  d <- d[d[["vegCover"]] == "All species"]
  ## compared outside `[`: inside it, `zone` is the column
  suffix <- paste0(" ", c(zone, if (single) "LandWeb Study Area"))
  hit <- Reduce(`|`, lapply(suffix, function(s) endsWith(d[["zone"]], s)))
  ## not a sub-region of another tenure whose longer name also ends with this one's
  longer <- setdiff(others, zone)
  longer <- longer[endsWith(longer, paste0(" ", zone)) | endsWith(longer, zone)]
  for (o in longer) {
    hit <- hit & !endsWith(d[["zone"]], paste0(" ", o))
  }
  if (!any(hit)) {
    return(out(reason = "version 2 summarised these sub-regions over several tenures together"))
  }
  d <- d[hit]
  sub_of <- d[["zone"]]
  for (s in suffix) {
    sub_of <- ifelse(endsWith(sub_of, s), substr(sub_of, 1L, nchar(sub_of) - nchar(s)), sub_of)
  }
  snap <- .v2_snapshot(d[["group"]])
  nrv <- unique(data.table::data.table(
    subregion = sub_of,
    ageClass = as.character(d[["ageClass"]]),
    rep = snap$rep,
    time = snap$time,
    value = as.numeric(d[["proportion"]]),
    type_pixels = as.numeric(d[["totalPixels"]])
  ))
  ## the sub-regions' forest must add up to the tenure's, landscape by landscape
  sub_tot <- unique(nrv[, .(subregion, rep, time, type_pixels)])[,
    .(sub = sum(type_pixels)),
    by = .(rep, time)
  ]
  ten_tot <- unique(data.table::as.data.table(tenure_nrv)[
    vegType == "All species",
    .(rep, time, all = type_pixels)
  ])
  chk <- merge(ten_tot, sub_tot, by = c("rep", "time"), all.x = TRUE)
  chk[is.na(sub), sub := 0]
  off <- max(abs(chk$sub - chk$all) / chk$all)
  if (!is.finite(off) || off > tol) {
    return(out(
      reason = sprintf(
        "version 2's sub-regions do not add up to the tenure (off by up to %.0f%%)",
        100 * min(off, 9.99)
      )
    ))
  }
  cc <- unique(data.table::data.table(
    subregion = sub_of,
    ageClass = as.character(d[["ageClass"]]),
    value = as.numeric(d[["proportionCC"]])
  ))
  out(
    nrv = nrv[type_pixels > 0 & !is.na(value), .(subregion, ageClass, rep, time, value)],
    cc = cc[!is.na(value)]
  )
}

#' Read one polygon's old-forest patch areas from a v2 run
#'
#' `histograms/largePatches_<layer>_<size>.csv` has one row per polygon (`polygonName`), vegetation
#' type, age class and simulated landscape (`rep`, numbered 1 to the number of landscapes) where the
#' simulation or the current condition has patches of at least `<size>` ha: `totalArea` (ha) in such
#' patches and `totalAreaCC` in the current condition. A landscape with no such patch has no row.
#'
#' @inheritParams read_v2_leading
#' @param sizes Minimum patch sizes (ha), as in the file names.
#' @param n_snapshots Number of simulated landscapes, from [read_v2_leading()].
#'
#' @return A list: `nrv` (`min_patch_ha`, `snapshot`, `value` in ha, 0 for a landscape without
#'   such patches) and `cc` (`min_patch_ha`, `value`), for old forest of all types.
read_v2_patches <- function(
  run_dir,
  layer,
  zone,
  sizes = c(100L, 500L, 1000L, 5000L),
  n_snapshots
) {
  one <- function(s) {
    f <- file.path(
      run_dir,
      "histograms",
      paste0("largePatches_", .v2_layer_token(layer), "_", s, ".csv")
    )
    if (!file.exists(f)) {
      stop("no v2 patch file ", f, call. = FALSE)
    }
    d <- data.table::fread(f, na.strings = c("NA", "NaN", ""))
    keep <- d[["polygonName"]] == zone & d[["ageClass"]] == "Old" & d[["vegCover"]] == "All species"
    d <- d[keep]
    if (anyDuplicated(d[["rep"]])) {
      stop("several rows per landscape for '", zone, "' in ", f, call. = FALSE)
    }
    if (nrow(d) && max(d[["rep"]]) > n_snapshots) {
      stop("more landscapes in ", f, " than in the age-class file", call. = FALSE)
    }
    nrv <- data.table::data.table(min_patch_ha = as.integer(s), snapshot = seq_len(n_snapshots))
    nrv[, value := as.numeric(d[["totalArea"]])[match(snapshot, d[["rep"]])]]
    nrv[is.na(value), value := 0]
    cc <- unique(stats::na.omit(as.numeric(d[["totalAreaCC"]])))
    if (length(cc) > 1L) {
      stop("several current-condition values for '", zone, "' in ", f, call. = FALSE)
    }
    list(
      nrv = nrv,
      cc = data.table::data.table(min_patch_ha = as.integer(s), value = if (length(cc)) cc else 0)
    )
  }
  parts <- lapply(sizes, one)
  list(
    nrv = data.table::rbindlist(lapply(parts, `[[`, "nrv")),
    cc = data.table::rbindlist(lapply(parts, `[[`, "cc"))
  )
}

#' The current condition of a v2 polygon, from the run's current-condition maps
#'
#' v2's summary files keep the current share of each type's forest by age class but not the share of
#' forest each type leads, which the succession check needs; the run's current-condition maps have
#' both. Forest pixels are those with a vegetation type and an age (as in v2's summaries); age classes
#' are binned with intervals closed on the right, as nrvtools bins them.
#'
#' @param run_dir A v2 run directory.
#' @param geom The polygon (`SpatVector`).
#' @param age_classes,age_cutoffs Age classes and their lower bounds.
#'
#' @return A list: `share` (`vegType`, `n_pixels`, `value` the share of forest) and `by_age`
#'   (`vegType` including `"All species"`, `ageClass`, `value` the share of the type's forest).
v2_cc_composition <- function(run_dir, geom, age_classes, age_cutoffs) {
  vtm <- terra::rast(.v2_cc_vtm_file(run_dir))
  tsf <- terra::rast(file.path(run_dir, "CurrentConditionTSF.tif"))
  if (!terra::compareGeom(vtm, tsf, stopOnError = FALSE)) {
    stop("the v2 current-condition maps of ", run_dir, " are not on one grid", call. = FALSE)
  }
  geom <- terra::project(geom, vtm)
  cells <- terra::cells(vtm, geom)[, "cell"]
  lv <- terra::levels(vtm)[[1L]]
  ## raw values, never the active category (labels can be lost on a file round trip)
  id <- terra::values(vtm, mat = FALSE)[cells]
  code <- as.character(lv[[2L]])[match(id, lv[[1L]])]
  unknown <- setdiff(stats::na.omit(unique(code)), names(.v2_cc_map_types))
  if (length(unknown)) {
    stop("unknown v2 current-condition types: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  age <- terra::values(tsf, mat = FALSE)[cells]
  ac <- findInterval(age, age_cutoffs[-1L], left.open = TRUE) + 1L
  d <- data.table::data.table(vegType = unname(.v2_cc_map_types[code]), ageClass = age_classes[ac])
  d <- d[!is.na(vegType) & !is.na(age)]
  n_all <- nrow(d)
  share <- d[, .(n_pixels = .N), by = vegType][, value := n_pixels / n_all][]
  by_age <- data.table::rbindlist(
    list(
      d[, .(n = .N), by = .(vegType, ageClass)][, value := n / sum(n), by = vegType],
      d[, .(vegType = "All species", n = .N), by = ageClass][, value := n / sum(n)]
    ),
    use.names = TRUE
  )
  list(share = share[order(vegType)], by_age = by_age[, .(vegType, ageClass, value)])
}

## ---- one study-area group --------------------------------------------------------------------

## the old-forest patch metrics report 05 shows
.report04_patch_metrics <- paste0("totalArea_ge", c(100L, 500L, 1000L, 5000L), "ha")

#' Gather v2 and v3 outputs for the tenures of one study-area group
#'
#' @param study_area Study-area group name.
#' @param reporting_polygons Value of target `reportingPolygons_<sa>`.
#' @param postprocess_dir NRV_summary's `postprocess/` directory for the group.
#' @param v2_root Directory holding the v2 runs.
#' @param crosswalk [v2_v3_crosswalk()]; only its `primary` rows are compared.
#'
#' @return `NULL` if no tenure of the group has a v2 run, else a list: `tenures` (one row per
#'   compared tenure, with its v2 run and the area simulated as forest in each version); the
#'   `data.table`s `type_nrv`, `type_cc`, `share_nrv`, `share_cc`, `patch_nrv`, `patch_cc` and
#'   `design`, each with a `version` column (`"v2"`, `"v3"`); `boundary` (per tenure, from
#'   [fma_boundary_compare()], maps simplified to 100 m); `runs` (each v2 run's settings and the date
#'   of its summary files); `checks` (how closely the v2 current-condition maps reproduce the shares
#'   v2 saved); and `sources`. It is an error for a compared tenure's v2 run to be missing.
build_report04_sa <- function(
  study_area,
  reporting_polygons,
  postprocess_dir = file.path("outputs", study_area, "postprocess"),
  v2_root = "outputs_v2",
  crosswalk = v2_v3_crosswalk()
) {
  rp <- reporting_polygons
  cw <- data.table::as.data.table(crosswalk)[primary == TRUE]
  ten <- .report05_tenures(rp)[tenure %in% cw$tenure]
  if (!nrow(ten)) {
    return(NULL)
  }
  ten[, study_area := study_area]
  ten <- merge(ten, cw[, .(tenure, run, layer, zone, patch_layer, basis)], by = "tenure")
  ten[, run_dir := vapply(run, v2_run_dir, character(1), v2_root = v2_root)]
  if (anyNA(ten$run_dir)) {
    stop(
      "v2 runs not found under '",
      v2_root,
      "': ",
      paste(unique(ten$run[is.na(ten$run_dir)]), collapse = ", "),
      call. = FALSE
    )
  }
  age_classes <- LandWebUtils:::.ageClasses
  age_cutoffs <- LandWebUtils:::.ageClassCutOffs

  ## v3: NRV_summary's datasets, as report 05 reads them
  agg <- file.path(postprocess_dir, "_aggregates")
  lw <- .report05_lw(agg, "FMA", c("leadingProp", "leadingShare", .report04_patch_metrics))
  keep <- function(d) d[poly %in% ten$tenure][, tenure := poly][]
  v3_nrv <- keep(lw$nrv)
  v3_cc <- keep(lw$cc)
  ## every simulated landscape, before the absent types' rows are dropped
  v3_snap <- unique(v3_nrv[metric == "leadingProp", .(rep, time)])
  cols <- c("tenure", "vegType", "ageClass", "rep", "time", "value")
  v3_type_nrv <- .report05_drop_absent(v3_nrv[metric == "leadingProp"])[, cols, with = FALSE]
  v3_type_cc <- .report05_drop_absent(v3_cc[metric == "leadingProp"])[,
    .(tenure, vegType, ageClass, value)
  ]
  ## the share of forest each type leads: leadingShare summed over the age classes
  type_share <- function(d) {
    d[
      metric == "leadingShare" & vegType != "All species",
      .(value = sum(value)),
      by = .(tenure, vegType, rep, time)
    ]
  }
  v3_share_nrv <- type_share(v3_nrv)
  v3_share_cc <- type_share(v3_cc)[, .(tenure, vegType, value)]
  old_patches <- function(d) {
    d <- d[metric %in% .report04_patch_metrics & ageClass == "Old" & vegType == "All species"]
    d[, min_patch_ha := as.integer(sub("^totalArea_ge([0-9]+)ha$", "\\1", metric))]
    d[]
  }
  v3_patch_nrv <- old_patches(v3_nrv)[, .(tenure, min_patch_ha, rep, time, value)]
  v3_patch_cc <- old_patches(v3_cc)[, .(tenure, min_patch_ha, value)]

  ## v3 area simulated as forest, current condition
  vtm_file <- .report05_cc_vtm(postprocess_dir)
  vtm <- terra::rast(vtm_file)
  fma <- spatialutils::repair_geoms(terra::vect(rp[["FMA"]][rp[["FMA"]]$Name %in% ten$tenure, ]))
  forest_v3 <- .report05_forest_area(vtm, fma, "FMA")[vegType == "All species"]
  ten[, forest_v3_ha := forest_v3$area_ha[match(tenure, forest_v3$poly)]]

  ## v3 all forest by sub-region: report 05's layer for each tenure (Alberta natural subregions, BC
  ## BEC zones), read as report 05 reads it; the crossed polygons are named "<sub-region> <tenure>"
  sub_kinds <- c("ANSR", "BEC")
  v3_sub <- data.table::rbindlist(
    lapply(ten$tenure, function(t) {
      l <- .report05_subregion_layer(rp, t)
      if (is.na(l) || !sub("^.* ", "", l) %in% sub_kinds) {
        return(NULL)
      }
      lw_s <- lapply(.report05_lw(agg, l, "leadingProp", "All species"), .report05_drop_absent)
      data.table::rbindlist(list(
        if (!is.null(lw_s$nrv)) lw_s$nrv[, .(kind = "nrv", rep, time, poly, ageClass, value)],
        if (!is.null(lw_s$cc)) lw_s$cc[, .(kind = "cc", rep, time, poly, ageClass, value)]
      ))[, `:=`(tenure = t, subregion = sub(paste0(" ", t, "$"), "", poly))]
    }),
    use.names = TRUE
  )
  if (!nrow(v3_sub)) {
    v3_sub <- data.table::data.table(
      kind = character(0),
      rep = integer(0),
      time = integer(0),
      tenure = character(0),
      subregion = character(0),
      ageClass = character(0),
      value = numeric(0)
    )
  }
  v3_sub_nrv <- v3_sub[kind == "nrv", .(tenure, subregion, ageClass, rep, time, value)]
  v3_sub_cc <- v3_sub[kind == "cc", .(tenure, subregion, ageClass, value)]

  ## v2 and the boundary comparison, tenure by tenure
  simplify <- function(x) {
    if (is.null(x)) NULL else sf::st_simplify(x, preserveTopology = TRUE, dTolerance = 100)
  }
  per <- lapply(seq_len(nrow(ten)), function(i) {
    t <- ten$tenure[[i]]
    rd <- ten$run_dir[[i]]
    cmp <- fma_boundary_compare(t, fma, v2_root)
    out <- list(tenure = t, boundary = cmp)
    lead_csv <- file.path(
      rd,
      "boxplots",
      paste0("leading_", .v2_layer_token(ten$layer[[i]]), ".csv")
    )
    info <- c(
      read_v2_run_settings(rd),
      summaries_written = format(file.mtime(lead_csv), "%Y-%m-%d")
    )
    lead <- read_v2_leading(rd, ten$layer[[i]], ten$zone[[i]])
    snaps <- unique(lead$nrv[vegType == "All species", .(rep, time)])
    pat <- read_v2_patches(rd, ten$patch_layer[[i]], ten$zone[[i]], n_snapshots = nrow(snaps))
    pix_ha <- as.numeric(info[["pixel_m"]])^2 / 1e4
    out$v2 <- list(
      info = info,
      type_nrv = lead$nrv[, .(tenure = t, vegType, ageClass, rep, time, value)],
      type_cc = lead$cc[!is.na(value), .(tenure = t, vegType, ageClass, value)],
      share_nrv = v2_type_share(lead$nrv)[, .(tenure = t, vegType, rep, time, value)],
      patch_nrv = pat$nrv[, .(
        tenure = t,
        min_patch_ha,
        rep = NA_integer_,
        time = NA_integer_,
        value
      )],
      patch_cc = pat$cc[, .(tenure = t, min_patch_ha, value)],
      forest_ha = mean(
        lead$nrv[vegType == "All species", unique(type_pixels), by = .(rep, time)]$V1
      ) *
        pix_ha,
      snapshots = snaps
    )
    if (!is.null(cmp$geoms$v2)) {
      comp <- v2_cc_composition(rd, terra::vect(cmp$geoms$v2), age_classes, age_cutoffs)
      out$v2$share_cc <- comp$share[, .(tenure = t, vegType, value)]
      ## the area v2 simulated as forest today, the denominator of its patch shares
      out$v2$forest_cc_ha <- sum(comp$share$n_pixels) * pix_ha
      ## the maps must reproduce the current-condition shares v2 saved
      chk <- merge(
        comp$by_age,
        lead$cc,
        by = c("vegType", "ageClass"),
        suffixes = c("_map", "_saved")
      )
      out$v2$cc_check <- max(abs(chk$value_map - chk$value_saved), na.rm = TRUE)
    }
    ## all forest by sub-region, where the run summarised the tenure's sub-regions
    sl <- v2_subregion_layers()[t]
    if (!is.na(sl)) {
      sr <- read_v2_subregions(
        rd,
        sl,
        ten$zone[[i]],
        lead$nrv,
        others = unique(crosswalk$zone[
          crosswalk$run == ten$run[[i]] & crosswalk$layer == ten$layer[[i]]
        ]),
        single = sum(crosswalk$run == ten$run[[i]] & crosswalk$layer == ten$layer[[i]]) == 1L
      )
      out$v2$sub_reason <- sr$reason
      if (is.na(sr$reason)) {
        out$v2$sub_nrv <- sr$nrv[, .(tenure = t, subregion, ageClass, rep, time, value)]
        out$v2$sub_cc <- sr$cc[, .(tenure = t, subregion, ageClass, value)]
      }
    }
    out$boundary$geoms <- lapply(cmp$geoms, simplify)
    out
  })
  names(per) <- ten$tenure

  v2 <- lapply(per, `[[`, "v2")
  v2 <- v2[!vapply(v2, is.null, logical(1))]
  bind <- function(k, v3d) {
    ## a table no v2 tenure has (sub-regions) is an empty data.table, which `:=` cannot extend
    v2d <- data.table::rbindlist(lapply(v2, `[[`, k), use.names = TRUE)
    if (nrow(v2d)) {
      v2d[, version := "v2"]
    }
    data.table::rbindlist(
      list(v2d, data.table::copy(v3d)[, version := "v3"]),
      use.names = TRUE,
      fill = TRUE
    )
  }
  v2_num <- function(k) {
    vapply(
      ten$tenure,
      function(t) if (is.null(v2[[t]][[k]])) NA_real_ else v2[[t]][[k]],
      numeric(1)
    )
  }
  ten[, forest_v2_ha := v2_num("forest_ha")]
  ten[, forest_v2_cc_ha := v2_num("forest_cc_ha")]
  runs <- data.table::rbindlist(lapply(names(v2), function(t) {
    info <- v2[[t]]$info
    data.table::data.table(
      tenure = t,
      run_dir = basename(ten$run_dir[ten$tenure == t]),
      setting = names(info),
      value = unname(info)
    )
  }))
  design <- data.table::rbindlist(
    list(
      data.table::rbindlist(lapply(names(v2), function(t) {
        data.table::data.table(tenure = t, version = "v2", v2[[t]]$snapshots)
      })),
      data.table::CJ(tenure = ten$tenure, idx = seq_len(nrow(v3_snap)))[,
        .(tenure, version = "v3", rep = v3_snap$rep[idx], time = v3_snap$time[idx])
      ]
    ),
    use.names = TRUE
  )
  list(
    study_area = study_area,
    tenures = ten,
    type_nrv = bind("type_nrv", v3_type_nrv),
    type_cc = bind("type_cc", v3_type_cc),
    share_nrv = bind("share_nrv", v3_share_nrv),
    share_cc = bind("share_cc", v3_share_cc),
    sub_nrv = bind("sub_nrv", v3_sub_nrv),
    sub_cc = bind("sub_cc", v3_sub_cc),
    sub_reasons = data.table::data.table(
      tenure = names(v2),
      reason = vapply(
        v2,
        function(x) {
          if (is.null(x$sub_reason)) "no version 2 summary by sub-region" else x$sub_reason
        },
        character(1)
      )
    ),
    patch_nrv = bind("patch_nrv", v3_patch_nrv),
    patch_cc = bind("patch_cc", v3_patch_cc),
    design = design,
    boundary = lapply(per, `[[`, "boundary"),
    runs = runs,
    checks = data.table::data.table(
      tenure = names(v2),
      v2_cc_map_vs_saved = vapply(
        v2,
        function(x) {
          if (is.null(x$cc_check)) NA_real_ else x$cc_check
        },
        numeric(1)
      )
    ),
    sources = list(
      postprocess_dir = postprocess_dir,
      cc_vtm = vtm_file,
      pixel_m = terra::res(vtm)[[1L]]
    )
  )
}

## ---- all groups --------------------------------------------------------------------------------

#' Percentiles of each version's simulated values, side by side, with their differences
#'
#' @param nrv,cc Per-landscape and current-condition values with a `version` column.
#' @param by Grouping columns (without `version`).
#'
#' @return A list: `long`, [report05_percentiles()] for each version (one row per group and
#'   version); and `delta`, one row per group present in both versions, with the v2 and v3 median
#'   (`p50_v2`, `p50_v3`), interquartile bounds, current conditions and positions, and the
#'   differences v3 - v2 of the median (`d_p50`) and of the current condition (`d_cc`).
report04_compare <- function(nrv, cc, by) {
  nrv <- data.table::as.data.table(nrv)
  cc <- data.table::as.data.table(cc)
  long <- data.table::rbindlist(
    lapply(c("v2", "v3"), function(v) {
      n <- nrv[version == v]
      if (!nrow(n)) {
        return(NULL)
      }
      report05_percentiles(n, cc[version == v], by)[, version := v]
    }),
    use.names = TRUE
  )
  keep <- c(by, "p25", "p50", "p75", "cc", "position", "n")
  w <- merge(
    long[version == "v2", keep, with = FALSE],
    long[version == "v3", keep, with = FALSE],
    by = by,
    suffixes = c("_v2", "_v3")
  )
  w[, `:=`(d_p50 = p50_v3 - p50_v2, d_cc = cc_v3 - cc_v2)]
  list(long = long, delta = w[])
}

#' Old-forest patch areas as shares of each version's area simulated as forest
#'
#' The denominators are the area each version simulates as forest in its current condition: v2's from
#' its current-condition maps (`forest_v2_cc_ha`), v3's from its current-condition vegetation-type map
#' (`forest_v3_ha`), so that a change of boundary or of forest extent does not read as a change in
#' old forest.
#'
#' @param d Patch values (`tenure`, `version`, `value` in ha, ...).
#' @param tenures One row per tenure with `forest_v2_cc_ha` and `forest_v3_ha`.
#'
#' @return `d` with `value` as a proportion of the version's forest area.
report04_patch_share <- function(d, tenures) {
  d <- data.table::copy(data.table::as.data.table(d))
  tenures <- data.table::as.data.table(tenures)
  fa <- data.table::rbindlist(list(
    data.table::data.table(
      tenure = tenures$tenure,
      version = "v2",
      forest_ha = tenures$forest_v2_cc_ha
    ),
    data.table::data.table(
      tenure = tenures$tenure,
      version = "v3",
      forest_ha = tenures$forest_v3_ha
    )
  ))
  d <- merge(d, fa, by = c("tenure", "version"), all.x = TRUE, sort = FALSE)
  d[, value := value / forest_ha]
  d[, forest_ha := NULL]
  d[]
}

#' Combine the study-area groups' report 04 data
#'
#' @param parts A list of [build_report04_sa()] results (`NULL`s are dropped).
#' @param crosswalk [v2_v3_crosswalk()].
#'
#' @return A named list; see reports/04-output-differences.qmd for its use.
build_report04_data <- function(parts, crosswalk = v2_v3_crosswalk()) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    stop("no tenure of the study-area groups has a v2 run in the crosswalk", call. = FALSE)
  }
  rb <- function(k) data.table::rbindlist(lapply(parts, `[[`, k), use.names = TRUE, fill = TRUE)
  tenures <- rb("tenures")
  type_nrv <- rb("type_nrv")
  type_cc <- rb("type_cc")
  share_nrv <- rb("share_nrv")
  share_cc <- rb("share_cc")
  patch_nrv <- rb("patch_nrv")
  patch_cc <- rb("patch_cc")
  boundary <- do.call(c, lapply(parts, `[[`, "boundary"))
  bstats <- data.table::rbindlist(
    lapply(boundary, function(b) cbind(b$stats, reason = b$reason)),
    use.names = TRUE,
    fill = TRUE
  )

  type <- report04_compare(type_nrv, type_cc, c("tenure", "vegType", "ageClass"))
  patch <- report04_compare(patch_nrv, patch_cc, c("tenure", "min_patch_ha"))
  patch_share <- report04_compare(
    report04_patch_share(patch_nrv, tenures),
    report04_patch_share(patch_cc, tenures),
    c("tenure", "min_patch_ha")
  )
  share <- report04_compare(share_nrv, share_cc, c("tenure", "vegType"))
  ## all forest by sub-region, where both versions have the sub-region (matched by name)
  subr <- report04_compare(rb("sub_nrv"), rb("sub_cc"), c("tenure", "subregion", "ageClass"))

  ## the crosswalk's tenures with no group in this pipeline, or no v3 polygon
  cw <- data.table::as.data.table(crosswalk)
  cw[,
    status := data.table::fcase(
      !primary                    , "another v2 run is compared for this tenure"         ,
      !tenure %in% tenures$tenure , "the tenure's study-area group is not in this build" ,
      default = "compared"
    )
  ]

  list(
    built = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    crosswalk = cw,
    unmatched = data.table::as.data.table(v2_unmatched()),
    tenures = tenures,
    boundary = boundary,
    boundary_stats = bstats,
    type_nrv = type_nrv,
    type_cc = type_cc,
    share_nrv = share_nrv,
    share_cc = share_cc,
    patch_nrv = patch_nrv,
    patch_cc = patch_cc,
    pct_type = type$long,
    delta_type = type$delta,
    pct_patch = patch$long,
    delta_patch = patch$delta,
    pct_patch_share = patch_share$long,
    delta_patch_share = patch_share$delta,
    pct_share = share$long,
    delta_share = share$delta,
    pct_sub = subr$long,
    delta_sub = subr$delta,
    sub_reasons = rb("sub_reasons"),
    design = rb("design"),
    runs = rb("runs"),
    checks = rb("checks"),
    age_classes = LandWebUtils:::.ageClasses,
    age_cutoffs = LandWebUtils:::.ageClassCutOffs,
    sources = lapply(parts, `[[`, "sources")
  )
}

#' Write report 04's percentile tables as a CSV supplement
#'
#' The report shows a short table per tenure; this file holds every percentile of every metric, for
#' both versions: the share of each vegetation type's forest in each age class (percent), the share
#' of forest each type leads (percent), and the old forest in large patches (ha, and percent of the
#' version's area simulated as forest).
#'
#' @param data The list from [build_report04_data()].
#' @param out_csv Path to write (and the value returned, for a `format = "file"` target).
#'
#' @return `out_csv`.
write_report04_percentiles <- function(data, out_csv) {
  pcols <- c("min", "p12.5", "p25", "p50", "p75", "p87.5", "max")
  ten <- data.table::as.data.table(data$tenures)
  one <- function(d, metric, unit, scale) {
    if (is.null(d) || !nrow(d)) {
      return(NULL)
    }
    d <- data.table::copy(data.table::as.data.table(d))
    for (k in c("vegType", "ageClass", "subregion")) {
      if (!k %in% names(d)) d[, (k) := NA_character_]
    }
    if (!"min_patch_ha" %in% names(d)) {
      d[, min_patch_ha := NA_integer_]
    }
    places <- if (unit == "ha") 0L else 2L
    d[,
      (c("cc", pcols)) := lapply(.SD, function(x) round(scale * x, places)),
      .SDcols = c("cc", pcols)
    ]
    d[, .(
      tenure,
      tenure_name = ten$name[match(tenure, ten$tenure)],
      version,
      metric = metric,
      vegetation_type = vegType,
      subregion,
      age_class = ageClass,
      min_patch_ha,
      unit = unit,
      current = cc,
      landscapes = n,
      min,
      p12_5 = p12.5,
      p25,
      p50,
      p75,
      p87_5 = p87.5,
      max
    )]
  }
  out <- data.table::rbindlist(list(
    one(data$pct_type, "share of the type's forest in the age class", "%", 100),
    one(data$pct_share, "share of forest the type leads", "%", 100),
    one(data$pct_sub, "share of the sub-region's forest in the age class", "%", 100),
    one(data$pct_patch, "old forest in patches of at least min_patch_ha", "ha", 1),
    one(data$pct_patch_share, "old forest in patches of at least min_patch_ha", "% of forest", 100)
  ))
  data.table::setorderv(
    out,
    c(
      "tenure",
      "metric",
      "unit",
      "vegetation_type",
      "subregion",
      "age_class",
      "min_patch_ha",
      "version"
    ),
    na.last = TRUE
  )
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(out, out_csv)
  out_csv
}
