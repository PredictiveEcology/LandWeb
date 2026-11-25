## FMA reporting-boundary change, v2 -> v3, per tenure (reports 04, 05).
##
## v3 adopts the v10 FMA shapefile, whose tenure boundaries differ from v2's (e.g. SprayLake's v3
## reporting FMA is ~15% larger than v2's). Every NRV summary keys on the reporting polygon, so a
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
## Only tenures with a primary row in v2_v3_crosswalk() are compared; for the rest the result says no
## v2 run is mapped.
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

## ---- v2 runs -> v3 tenures -------------------------------------------------------------------
##
## v2 ran one study area per tenure or licensee (`outputs_v2/<run>_<dispersal>_logROS/`) and
## summarised each of its reporting layers polygon by polygon; v3 reports by tenure, keyed by the
## curated `NAME_SHORT` code (LandWebUtils::tenureShortNames()). v2_v3_crosswalk() matches each v2
## polygon to the v3 tenure it is, by geometry: each primary pair overlaps by at least 93% of the v2
## polygon and 56% of the v3 tenure, and 20 of the 27 by more than 99% of both (measured 2026-11-25
## against the v10 FMA layer; reports 04 and 05 show each tenure's change). Names are no guide: the
## polygon v2 called "Millar Western Forest Products Ltd." is v3's CanforWhitecourt, and v2's
## "West Fraser Mills Ltd. (Slave Lake)" is v3's MillarWestern.
##
## A tenure that several v2 runs summarised has one `primary` row, the run compared: the run whose
## layer holds only that tenure where there is one, else the run named for the tenure's first-listed
## licensee. The other rows record the alternatives.

#' The v2 runs compared with v3, by v3 tenure
#'
#' @return A `data.frame`, one row per v2 polygon and v3 tenure that are the same unit: `tenure` (v3
#'   `NAME_SHORT`), `run` (the v2 run's base name, see [v2_run_dir()]), `layer` (the reporting layer
#'   in the run's `ml_preamble.rds`; its summary files use the name with spaces replaced by
#'   underscores), `zone` (the polygon's `Name` in that layer and in the summary files),
#'   `primary` (`TRUE` for the one row per tenure that is compared), and `patch_layer` (the layer's
#'   name in the run's patch-area files, `histograms/largePatches_<layer>_<size>.csv`: the same,
#'   except that v2's DMI run wrote those under "Mercer").
v2_v3_crosswalk <- function() {
  row <- function(tenure, run, layer, zone, primary = TRUE, patch_layer = layer) {
    data.frame(
      tenure = tenure,
      run = run,
      layer = layer,
      zone = zone,
      primary = primary,
      patch_layer = patch_layer,
      stringsAsFactors = FALSE
    )
  }
  tvw <- paste(
    "Tolko Industries Ltd., Vanderwell Contractors (1971) Ltd. and",
    "West Fraser Mills Ltd. (Slave Lake)"
  )
  wft <- "West Fraser Mills Ltd. and Tolko Industries Ltd."
  x <- rbind(
    row("ANC", "ANC", "ANC", "ANC Timber Ltd."),
    row("AlPac", "AlPac", "Alpac", "ALPAC Forest Products Incorporated"),
    row("BlueRidge", "BlueRidge", "West Fraser Blue Ridge", "Blue Ridge Lumber Inc."),
    row(
      "CanforWhitecourt",
      "MillarWestern",
      "Millar Western",
      "Millar Western Forest Products Ltd."
    ),
    row("DawsonCreek_TSA", "LP_BC", "LP BC", "Dawson Creek"),
    row("FortStJohn_TSA", "LP_BC", "LP BC", "Fort St. John"),
    row("MB_FML3", "LP_MB", "LP MB", "Mountain"),
    row("Manning", "Manning", "Manning", "Manning Diversified Forest Products Ltd."),
    row("Mercer_E", "DMI", "DMI AB E", "Mercer Peace River Pulp Ltd. (East)", TRUE, "Mercer AB E"),
    row("Mercer_E", "DMI", "DMI Full", "Mercer Peace River Pulp Ltd. (East)", FALSE, "Mercer Full"),
    row("Mercer_W", "DMI", "DMI Full", "Mercer Peace River Pulp Ltd. (West)", TRUE, "Mercer Full"),
    row("MillarWestern", "WestFraser_N", "West Fraser N", "West Fraser Mills Ltd. (Slave Lake)"),
    row("NT_FtProvidence", "FMANWT2", "FMANWT FP", "Fort Providence"),
    row("NT_FtResolution", "FMANWT", "FMANWT FR", "Fort Resolution"),
    row("SK_BronsonGreen", "Tolko_SK", "Tolko SK", "Meadow Lake OSB"),
    row("SK_MeadowLake", "Mistik", "Mistik", "Mistik"),
    row("SK_PasquiaPorc", "WeyCo_SK", "WeyCo SK", "Pasquia-Porcupine"),
    row("SprayLake", "SprayLake", "Spray Lake", "Spray Lake Sawmills (1980) Ltd."),
    row("Sundre", "Sundre", "SundreFP", "Sundre Forest Products Inc."),
    row("Tolko_HP", "Tolko_AB_S", "Tolko AB South", "Tolko Industries Ltd. (High Prairie)"),
    row(
      "Tolko_Norbord_LC",
      "Tolko_AB_N",
      "Tolko AB North",
      "Tolko Industries Ltd., Footner Forest Products Ltd. and La Crete Sawmills Ltd."
    ),
    row("Tolko_Vand_WF_SL", "Tolko_AB_S", "Tolko AB South", tvw),
    row("Tolko_Vand_WF_SL", "Vanderwell", "Vanderwell", tvw, FALSE),
    row("Tolko_Vand_WF_SL", "WestFraser_N", "West Fraser N", tvw, FALSE),
    row("Vanderwell", "Vanderwell", "Vanderwell", "Vanderwell Contractors (1971) Ltd."),
    row("WF_Edson", "Edson", "EdsonFP", "West Fraser Mills Ltd. (Edson)"),
    row("WF_Edson", "WestFraser_S", "West Fraser S", "West Fraser Mills Ltd. (Edson)", FALSE),
    row("WF_Hinton", "WestFraser_S", "West Fraser S", "West Fraser Mills Ltd. (Hinton)"),
    row("WF_Tolko", "WestFraser_N", "West Fraser N", wft),
    row("WF_Tolko", "Tolko_AB_S", "Tolko AB South", wft, FALSE),
    row(
      "WeyCo_GrandePr",
      "WeyCo_GP",
      "WeyCo GP",
      "Weyerhaeuser Company Limited (Grande Prairie)"
    ),
    row(
      "WeyCo_Pembina",
      "WeyCo_PT",
      "WeyCo PT",
      "Weyerhaeuser Company Limited (Pembina Timberland)"
    )
  )
  rownames(x) <- NULL
  x
}

#' v2 polygons with no v3 tenure of their own
#'
#' The polygons of the v2 runs' tenure layers that [v2_v3_crosswalk()] leaves out, and why.
#'
#' @return A `data.frame`: `run`, `layer`, `zone`, `reason`.
v2_unmatched <- function() {
  data.frame(
    run = c("SprayLake", "DMI", "DMI"),
    layer = c("Spray Lake C5", "DMI AB NW", "DMI AB SW"),
    zone = c("C5", "Mercer Peace River Pulp Ltd. (West)", "Mercer Peace River Pulp Ltd. (West)"),
    reason = c(
      "a forest management unit, not a tenure; the Crowsnest FMA lies within it",
      "part of Mercer_W, which is compared as a whole (layer DMI Full)",
      "part of Mercer_W, which is compared as a whole (layer DMI Full)"
    ),
    stringsAsFactors = FALSE
  )
}

## The v2 baseline runs: one per tenure or licensee, with the log rate of spread and high (else
## aspen) seed dispersal. Trial v3 runs (`*_v3*`), FMU, provincial and whole-area runs, and the
## dispersal, succession, resolution and rate-of-spread experiments are not baselines.
.v2_baseline_runs <- c(
  "ANC",
  "AlPac",
  "BlueRidge",
  "DMI",
  "Edson",
  "FMANWT",
  "FMANWT2",
  "LP_BC",
  "LP_MB",
  "Manning",
  "MillarWestern",
  "Mistik",
  "SprayLake",
  "Sundre",
  "Tolko_AB_N",
  "Tolko_AB_S",
  "Tolko_SK",
  "Vanderwell",
  "WestFraser_N",
  "WestFraser_S",
  "WeyCo_GP",
  "WeyCo_PT",
  "WeyCo_SK"
)

#' Locate a v2 baseline run
#'
#' The run directory is `<run>_highDispersal_logROS` where it exists, else
#' `<run>_aspenDispersal_logROS`, matched exactly, so a trial v3 run (`SprayLake_v3`), a variant
#' (`SprayLake_highDispersal_logROS_lthfc_8c`, `ANC_aspenDispersal_logROS_fri1_res50`,
#' `SprayLake_highDispersal_burnyROS`) or a run sharing the prefix (`FMANWT2_...` for `FMANWT`) is
#' never taken.
#'
#' @param run The run's base name (`run` in [v2_v3_crosswalk()]).
#' @param v2_root Directory holding the v2 runs.
#'
#' @return The run directory, or `NA` if neither exists (or `v2_root` is `NA` or missing).
v2_run_dir <- function(run, v2_root = "outputs_v2") {
  if (is.na(v2_root) || !dir.exists(v2_root)) {
    return(NA_character_)
  }
  cand <- file.path(v2_root, paste0(run, c("_highDispersal_logROS", "_aspenDispersal_logROS")))
  cand <- cand[dir.exists(cand)]
  if (length(cand)) cand[[1L]] else NA_character_
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
  lk <- v2_v3_crosswalk()
  lk <- lk[lk$primary, , drop = FALSE]
  i <- match(tenure, lk$tenure)
  if (is.na(i)) {
    return(out(reason = "no v2 run is mapped to this tenure"))
  }
  run_dir <- v2_run_dir(lk$run[[i]], v2_root)
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
  ## a layer can hold several tenures (v2's Tolko AB South holds three): keep this one's polygon
  zone <- lk$zone[[i]]
  if (!"Name" %in% names(v) || !any(v$Name %in% zone)) {
    stop("No polygon '", zone, "' in v2 reporting layer '", layer, "' of ", ml_rds, call. = FALSE)
  }
  v <- v[v$Name %in% zone, ]
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
