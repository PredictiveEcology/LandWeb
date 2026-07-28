## FMA reporting-boundary change, v2 -> v3, per study area.
##
## v3 adopts the v10 FMA shapefile (`inputs/FMA_Boundary_Updated.shp`), whose
## per-study-area reporting boundary differs from v2's (e.g. SprayLake's v3 reporting
## FMA is ~20% larger than v2's -- surfaced during the Leading/LargePatches port as
## 39,245 vs 32,560 forested px at 250 m). Because every NRV/fire summary keys on the
## reporting polygon, this boundary change shifts every v2-vs-v3 comparison, so it MUST
## be characterized in the v2-vs-v3 diff report (04) and every per-study-area summary
## report (05): v2-vs-v3 area, % overlap / added / dropped, and a difference map.
##
## Sources (both already in the LandWeb LCC, GRS80/NAD83, metres):
##  - v3: `outputs/<sa>/preamble/studyAreaReporting_year1.shp` -- the v10 FMA reporting
##        boundary the preamble clips + dissolves for study area <sa>.
##  - v2: `outputs_v2/<sa>_*logROS/ml_preamble.rds` -- v2's own reporting polygon, a
##        named layer inside the retired `map`-package object (the authoritative v2
##        baseline the Leading/LargePatches port used to confirm exact parity). The v2
##        run dirs do NOT save a plain reporting shapefile, so this is the only source.
##
## Modelled on R/lthfc_summary.R: two cache-aware producers for `format = "file"`
## targets (a stats CSV + a difference-map GeoPackage). Delete the output to force a
## fresh computation.

## Canonical LandWeb LCC (matches both the v3 preamble output and the v2 `map` CRS;
## the two differ only in datum spelling -- GRS80 vs NAD83 -- which are equivalent).
.fma_target_crs <- function() {
  paste(
    "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77",
    "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  )
}

## study-area name -> the reporting-polygon layer name inside the v2 `map` object.
## v2 named its layers by the informal FMA label ("Spray Lake"), not the study-area
## token ("SprayLake") nor the v3 FMA_NAME ("Spray Lake Sawmills (1980) Ltd.").
.fma_v2_layer_lookup <- function() {
  c(
    SprayLake = "Spray Lake",
    Edson = "Edson"
  )
}

## Locate the v2 baseline run dir for a study area: base-name match under `v2_root`,
## EXCLUDING the trial `*_v3*` dirs (those are v3 runs, not the v2 baseline), and
## preferring the canonical `*highDispersal_logROS` run when several match.
.fma_v2_run_dir <- function(study_area, v2_root = "outputs_v2") {
  hits <- list.dirs(v2_root, recursive = FALSE, full.names = TRUE)
  base <- basename(hits)
  keep <- startsWith(base, paste0(study_area, "_")) & !grepl("_v3", base, fixed = TRUE)
  hits <- hits[keep]
  if (!length(hits)) {
    return(NA_character_)
  }
  pref <- grepl("highDispersal_logROS$", basename(hits))
  if (any(pref)) hits <- hits[pref]
  hits[[1L]]
}

## Extract, validate, reproject and dissolve v2's own reporting boundary for a study
## area from its `ml_preamble.rds` `map` object. Returns an sf POLYGON (1 feature) in
## the target CRS, or NULL if the v2 run / layer is unavailable.
read_v2_fma_boundary <- function(study_area, v2_root = "outputs_v2") {
  run_dir <- .fma_v2_run_dir(study_area, v2_root)
  if (is.na(run_dir)) {
    return(NULL)
  }
  ml_rds <- file.path(run_dir, "ml_preamble.rds")
  if (!file.exists(ml_rds)) {
    return(NULL)
  }
  x <- readRDS(ml_rds)
  avail <- names(x)
  layer <- .fma_v2_layer_lookup()[[study_area]]
  if (is.null(layer) || !(layer %in% avail)) {
    ## fall back to a case/space-insensitive match against the study-area token
    norm <- function(s) tolower(gsub("[^a-z]", "", tolower(s)))
    layer <- avail[norm(avail) == norm(study_area)][1L]
  }
  if (is.na(layer) || !(layer %in% avail)) {
    stop(
      "No v2 reporting layer for study area '", study_area, "' in ", ml_rds,
      ".\n  Available layers: ", paste(avail, collapse = " | ")
    )
  }
  poly <- x@.xData[[layer]]
  s <- sf::st_make_valid(sf::st_as_sf(poly))
  if (is.na(sf::st_crs(s))) {
    sf::st_crs(s) <- x@CRS@projargs
  }
  s <- sf::st_transform(s, .fma_target_crs())
  sf::st_sf(source = "v2", study_area = study_area, geometry = sf::st_union(sf::st_geometry(s)))
}

## Read, validate, reproject and dissolve v3's reporting boundary for a study area
## from the branched preamble output. Returns an sf POLYGON (1 feature) in the target
## CRS, or NULL if the v3 preamble output is unavailable.
read_v3_fma_boundary <- function(study_area, outputs_dir = "outputs") {
  shp <- file.path(outputs_dir, study_area, "preamble", "studyAreaReporting_year1.shp")
  if (!file.exists(shp)) {
    return(NULL)
  }
  s <- sf::st_make_valid(sf::st_read(shp, quiet = TRUE))
  if (is.na(sf::st_crs(s))) {
    sf::st_crs(s) <- .fma_target_crs()
  }
  s <- sf::st_transform(s, .fma_target_crs())
  sf::st_sf(source = "v3", study_area = study_area, geometry = sf::st_union(sf::st_geometry(s)))
}

#' v2-vs-v3 FMA reporting-boundary change summary (reports 04, 05)
#'
#' Cache-aware producer for a `format = "file"` target: returns `out_csv`
#' unchanged if it already exists, else computes the v2-vs-v3 reporting-boundary
#' areas, overlap, added and dropped area (ha) for `study_area` and writes a
#' one-row CSV. Areas are planimetric in the LandWeb LCC (equal-ish for these
#' small FMAs; the reports quote them as hectares).
#'
#' @param out_csv path to write (and the value returned, for the file target).
#' @param study_area study-area token (e.g. "SprayLake").
#' @param outputs_dir dir holding the v3 branched outputs (default "outputs").
#' @param v2_root dir holding the v2 baseline runs (default "outputs_v2").
#' @return `out_csv`.
fma_boundary_change_summary <- function(out_csv, study_area,
                                        outputs_dir = "outputs", v2_root = "outputs_v2") {
  if (file.exists(out_csv)) {
    return(out_csv) # already computed -- reuse
  }
  v2 <- read_v2_fma_boundary(study_area, v2_root)
  v3 <- read_v3_fma_boundary(study_area, outputs_dir)
  ha <- function(g) if (is.null(g) || !nrow(g)) NA_real_ else sum(as.numeric(sf::st_area(g))) / 1e4
  a2 <- ha(v2)
  a3 <- ha(v3)
  inter <- add <- drop <- uni <- NA_real_
  if (!is.null(v2) && !is.null(v3)) {
    g2 <- sf::st_geometry(v2)
    g3 <- sf::st_geometry(v3)
    inter <- sum(as.numeric(sf::st_area(sf::st_intersection(g2, g3)))) / 1e4
    uni <- sum(as.numeric(sf::st_area(sf::st_union(g2, g3)))) / 1e4
    dd <- sf::st_difference(g2, g3)
    ad <- sf::st_difference(g3, g2)
    drop <- if (length(dd)) sum(as.numeric(sf::st_area(dd))) / 1e4 else 0
    add <- if (length(ad)) sum(as.numeric(sf::st_area(ad))) / 1e4 else 0
  }
  out <- data.frame(
    study_area = study_area,
    area_v2_ha = a2,
    area_v3_ha = a3,
    area_change_ha = a3 - a2,
    pct_change = 100 * (a3 - a2) / a2,
    area_intersection_ha = inter,
    area_union_ha = uni,
    area_added_ha = add, # in v3, not in v2
    area_dropped_ha = drop, # in v2, not in v3
    pct_overlap_of_v2 = 100 * inter / a2,
    pct_overlap_of_v3 = 100 * inter / a3,
    iou = inter / uni,
    stringsAsFactors = FALSE
  )
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, out_csv, row.names = FALSE)
  out_csv
}

#' v2-vs-v3 FMA reporting-boundary difference map data (reports 04, 05)
#'
#' Cache-aware producer for a `format = "file"` target: writes a GeoPackage with
#' four layers in the LandWeb LCC --- `v2` and `v3` (the two reporting boundaries),
#' `added` (in v3 but not v2) and `dropped` (in v2 but not v3). The report draws v2
#' vs v3 outlines and shades the added/dropped slivers. Returns `out_gpkg` unchanged
#' if it already exists.
#'
#' @param out_gpkg path to write (and the value returned, for the file target).
#' @param study_area study-area token (e.g. "SprayLake").
#' @param outputs_dir dir holding the v3 branched outputs (default "outputs").
#' @param v2_root dir holding the v2 baseline runs (default "outputs_v2").
#' @return `out_gpkg`.
fma_boundary_change_map <- function(out_gpkg, study_area,
                                    outputs_dir = "outputs", v2_root = "outputs_v2") {
  if (file.exists(out_gpkg)) {
    return(out_gpkg)
  }
  v2 <- read_v2_fma_boundary(study_area, v2_root)
  v3 <- read_v3_fma_boundary(study_area, outputs_dir)
  dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_gpkg)) file.remove(out_gpkg)
  wrote <- FALSE
  write_layer <- function(g, layer) {
    if (is.null(g) || !length(sf::st_geometry(g)) || all(sf::st_is_empty(g))) {
      return(invisible())
    }
    sf::st_write(g, out_gpkg, layer = layer, quiet = TRUE, append = wrote)
    wrote <<- TRUE
  }
  write_layer(v2, "v2")
  write_layer(v3, "v3")
  if (!is.null(v2) && !is.null(v3)) {
    g2 <- sf::st_geometry(v2)
    g3 <- sf::st_geometry(v3)
    added <- sf::st_difference(g3, g2)
    dropped <- sf::st_difference(g2, g3)
    if (length(added)) {
      write_layer(sf::st_sf(kind = "added", geometry = added), "added")
    }
    if (length(dropped)) {
      write_layer(sf::st_sf(kind = "dropped", geometry = dropped), "dropped")
    }
  }
  out_gpkg
}
