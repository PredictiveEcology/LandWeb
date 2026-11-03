## FMA reporting-boundary change, v2 -> v3, per tenure (reports 04, 05).
##
## v3 adopts the v10 FMA shapefile, whose tenure boundaries differ from v2's (e.g. SprayLake's v3
## reporting FMA is ~20% larger than v2's). Every NRV summary keys on the reporting polygon, so a
## boundary change shifts every v2-vs-v3 comparison and must be characterized in the v2-vs-v3 diff
## report (04) and in every per-study-area report (05): v2 and v3 area, overlap, added and dropped
## area, and a difference map.
##
## v3 study areas are ecoregion GROUPS of tenures, while v2 ran one FMA per study area, so the
## comparison is per tenure (the v3 reporting unit):
##  - v3: the tenure's polygon in the `FMA` layer of `reportingPolygons_<sa>` (keyed by `Name`, the
##        curated NAME_SHORT code).
##  - v2: the tenure's reporting polygon inside `outputs_v2/<run>_*logROS/ml_preamble.rds`, a named
##        layer of the retired `map`-package object (the only place a v2 run saved it).
## Only tenures in .fma_v2_lookup() are compared; for the rest the result says no v2 run is mapped.
##
## Geometry: repaired with spatialutils::repair_geoms() (terra::makeValid() on the invalid features
## only); sf::st_make_valid() collapses the v10 layer's reversed-winding polygons to slivers. Set
## operations stay in terra for the same reason. Areas are planar, in the LandWeb Lambert conformal
## conic projection, as are the forested areas the reports quote from the simulation grid.

## Canonical LandWeb LCC (matches both the v3 reporting polygons and the v2 `map` CRS; the two differ
## only in datum spelling -- GRS80 vs NAD83 -- which are equivalent).
.fma_target_crs <- function() {
  paste(
    "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77",
    "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  )
}

## v3 tenure code -> the v2 run (base name under `outputs_v2/`) and that run's reporting layer in its
## `map` object. v2 named its layers by informal labels ("Spray Lake", "EdsonFP"), not by the v3 codes.
## Each pair was checked against the data: the v2 layer's `Name` equals the v3 `FMA_NAME`
## ("West Fraser Mills Ltd. (Edson)" in both). Report 04 extends this to the other v2 runs.
.fma_v2_lookup <- function() {
  data.frame(
    tenure = c("SprayLake", "WF_Edson"),
    run = c("SprayLake", "Edson"),
    layer = c("Spray Lake", "EdsonFP"),
    stringsAsFactors = FALSE
  )
}

## Locate the v2 baseline run dir for a v2 run token: base-name match under `v2_root`, EXCLUDING the
## trial `*_v3*` dirs (those are v3 runs, not the v2 baseline), and preferring the canonical
## `*highDispersal_logROS` run when several match. NA if none.
.fma_v2_run_dir <- function(run, v2_root = "outputs_v2") {
  if (is.na(v2_root) || !dir.exists(v2_root)) {
    return(NA_character_)
  }
  hits <- list.dirs(v2_root, recursive = FALSE, full.names = TRUE)
  base <- basename(hits)
  keep <- startsWith(base, paste0(run, "_")) & !grepl("_v3", base, fixed = TRUE)
  hits <- hits[keep]
  if (!length(hits)) {
    return(NA_character_)
  }
  pref <- grepl("highDispersal_logROS$", basename(hits))
  if (any(pref)) {
    hits <- hits[pref]
  }
  hits[[1L]]
}

## Repair, project and dissolve a polygon layer to one feature in the target CRS.
.fma_dissolve <- function(v, crs) {
  v <- spatialutils::repair_geoms(v)
  if (!nrow(v)) {
    return(NULL)
  }
  v <- terra::project(v, crs)
  v <- terra::aggregate(v)
  terra::values(v) <- NULL
  v
}

#' v2 reporting boundary of a tenure
#'
#' @param tenure v3 tenure code (`Name` in the `FMA` reporting layer), e.g. `"WF_Edson"`.
#' @param v2_root Directory holding the v2 baseline runs (default `"outputs_v2"`).
#'
#' @return A list: `geom` (a one-feature `SpatVector` in the LandWeb LCC, or `NULL`), `run` (the v2
#'   run directory's basename, or `NA`), and `reason` (why `geom` is `NULL`, else `NA`).
read_v2_fma_boundary <- function(tenure, v2_root = "outputs_v2") {
  out <- function(geom = NULL, run = NA_character_, reason = NA_character_) {
    list(geom = geom, run = run, reason = reason)
  }
  lk <- .fma_v2_lookup()
  i <- match(tenure, lk$tenure)
  if (is.na(i)) {
    return(out(reason = "no v2 run is mapped to this tenure"))
  }
  run_dir <- .fma_v2_run_dir(lk$run[[i]], v2_root)
  if (is.na(run_dir)) {
    return(out(reason = "the v2 run's outputs were not found"))
  }
  ml_rds <- file.path(run_dir, "ml_preamble.rds")
  if (!file.exists(ml_rds)) {
    return(out(run = basename(run_dir), reason = "the v2 run saved no reporting polygons"))
  }
  x <- readRDS(ml_rds)
  layer <- lk$layer[[i]]
  if (!exists(layer, envir = x@.xData, inherits = FALSE)) {
    stop(
      "No v2 reporting layer '",
      layer,
      "' in ",
      ml_rds,
      ".\n  Available layers: ",
      paste(ls(x@.xData), collapse = " | "),
      call. = FALSE
    )
  }
  v <- terra::vect(sf::st_as_sf(get(layer, envir = x@.xData, inherits = FALSE)))
  if (!nzchar(terra::crs(v))) {
    terra::crs(v) <- x@CRS@projargs
  }
  out(geom = .fma_dissolve(v, .fma_target_crs()), run = basename(run_dir))
}

#' v3 reporting boundary of a tenure
#'
#' @param tenure v3 tenure code.
#' @param fma The `FMA` layer of `reportingPolygons_<sa>` (`sf` or `SpatVector`, with `Name`).
#'
#' @return A one-feature `SpatVector` in the LandWeb LCC, or `NULL` if the tenure is absent.
read_v3_fma_boundary <- function(tenure, fma) {
  v <- if (inherits(fma, "SpatVector")) fma else terra::vect(fma)
  v <- v[v$Name %in% tenure, ]
  if (!nrow(v)) {
    return(NULL)
  }
  .fma_dissolve(v, .fma_target_crs())
}

#' Compare a tenure's v2 and v3 reporting boundaries
#'
#' @inheritParams read_v3_fma_boundary
#' @inheritParams read_v2_fma_boundary
#'
#' @return A list: `stats`, a one-row `data.frame` (tenure, v2 run, the v2 and v3 areas, their
#'   intersection and union, the area added in v3 and dropped from v2, all in ha, and the percent
#'   overlaps; `NA` where not compared); `reason` (`NA` when compared); and `geoms`, a named list of
#'   `sf` layers (`v2`, `v3`, `added`, `dropped`) for the difference map, or `NULL`.
fma_boundary_compare <- function(tenure, fma, v2_root = "outputs_v2") {
  v3 <- read_v3_fma_boundary(tenure, fma)
  v2 <- read_v2_fma_boundary(tenure, v2_root)
  ha <- function(g) {
    if (is.null(g) || !nrow(g)) 0 else sum(terra::expanse(g, unit = "ha", transform = FALSE))
  }
  stats <- data.frame(
    tenure = tenure,
    v2_run = v2$run,
    area_v2_ha = NA_real_,
    area_v3_ha = if (is.null(v3)) NA_real_ else ha(v3),
    area_intersection_ha = NA_real_,
    area_union_ha = NA_real_,
    area_added_ha = NA_real_, ## in v3, not in v2
    area_dropped_ha = NA_real_, ## in v2, not in v3
    stringsAsFactors = FALSE
  )
  if (is.null(v2$geom) || is.null(v3)) {
    stats[c("pct_change", "pct_overlap_of_v2", "pct_overlap_of_v3")] <- NA_real_
    reason <- if (is.null(v3)) "the tenure has no v3 reporting polygon" else v2$reason
    return(list(stats = stats, reason = reason, geoms = NULL))
  }
  g2 <- v2$geom
  inter <- terra::intersect(g2, v3)
  added <- terra::erase(v3, g2)
  dropped <- terra::erase(g2, v3)
  stats$area_v2_ha <- ha(g2)
  stats$area_intersection_ha <- ha(inter)
  stats$area_union_ha <- ha(terra::aggregate(rbind(g2, v3)))
  stats$area_added_ha <- ha(added)
  stats$area_dropped_ha <- ha(dropped)
  stats$pct_change <- 100 * (stats$area_v3_ha - stats$area_v2_ha) / stats$area_v2_ha
  stats$pct_overlap_of_v2 <- 100 * stats$area_intersection_ha / stats$area_v2_ha
  stats$pct_overlap_of_v3 <- 100 * stats$area_intersection_ha / stats$area_v3_ha
  as_sf <- function(g) if (is.null(g) || !nrow(g)) NULL else sf::st_as_sf(g)
  list(
    stats = stats,
    reason = NA_character_,
    geoms = list(v2 = as_sf(g2), v3 = as_sf(v3), added = as_sf(added), dropped = as_sf(dropped))
  )
}

#' v2-vs-v3 reporting-boundary change summary, as a CSV (report 04)
#'
#' Cache-aware producer for a `format = "file"` target: returns `out_csv` unchanged if it exists,
#' else writes one row per tenure from [fma_boundary_compare()].
#'
#' @param out_csv Path to write (and the value returned, for the file target).
#' @param tenures v3 tenure codes.
#' @inheritParams fma_boundary_compare
#'
#' @return `out_csv`.
fma_boundary_change_summary <- function(out_csv, tenures, fma, v2_root = "outputs_v2") {
  if (file.exists(out_csv)) {
    return(out_csv)
  }
  rows <- lapply(tenures, function(t) {
    cmp <- fma_boundary_compare(t, fma, v2_root)
    cbind(cmp$stats, reason = cmp$reason)
  })
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(do.call(rbind, rows), out_csv, row.names = FALSE)
  out_csv
}

#' v2-vs-v3 reporting-boundary difference map, as a GeoPackage (report 04)
#'
#' Cache-aware producer for a `format = "file"` target: writes the `v2`, `v3`, `added` and `dropped`
#' layers of every compared tenure, with a `tenure` column. Returns `out_gpkg` unchanged if it exists.
#'
#' @param out_gpkg Path to write (and the value returned, for the file target).
#' @inheritParams fma_boundary_change_summary
#'
#' @return `out_gpkg`.
fma_boundary_change_map <- function(out_gpkg, tenures, fma, v2_root = "outputs_v2") {
  if (file.exists(out_gpkg)) {
    return(out_gpkg)
  }
  dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)
  layers <- list()
  for (t in tenures) {
    g <- fma_boundary_compare(t, fma, v2_root)$geoms
    for (k in names(g)) {
      if (!is.null(g[[k]])) {
        layers[[k]] <- rbind(layers[[k]], sf::st_sf(tenure = t, geometry = sf::st_geometry(g[[k]])))
      }
    }
  }
  for (k in names(layers)) {
    sf::st_write(layers[[k]], out_gpkg, layer = k, quiet = TRUE, append = file.exists(out_gpkg))
  }
  out_gpkg
}
