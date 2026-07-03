## LTHFC fire-return-interval version comparison over the LandWeb study area.
##
## Summarizes the area of the study area assigned to each fire-return-interval
## (FRI, years) under the three LTHFC layers that report 02 (fire-model inputs)
## compares: the current v10 map (2026, with NW Alberta updated to the Intermediate
## NWAB scenario), the previous v8c map, and the Spray Lake (SLS v3) local layer.
## This is the evidence behind the v8c -> v10 FRI shift documented in report 02:
## NW-AB polygons drop from the original ~100-yr default toward 40-55 yr.
##
## Each layer stores the FRI in a differently-named integer field (v10: LTFC10;
## v8c / SLS: LTHFC; some legacy layers: LTHRC) and in a different CRS (v10 is
## LCC; v8c / SLS are NAD83 / UTM 11N). Areas are therefore computed as geodesic
## areas (transform to EPSG:4326, then sf::st_area) so the three are comparable
## without picking a single projected CRS.
##
## The scan is light (three ~500-polygon shapefiles), but the producer is still
## CACHE-AWARE for parity with scanfi_veg_summary(): if `out_csv` already exists it
## is returned unchanged, so the `format = "file"` target records its hash without
## recomputing. Delete `out_csv` to force a fresh scan.

## version -> shapefile + FRI field + display label.
.lthfc_versions <- function() {
  data.table::data.table(
    version = c("v10", "v8c", "sls_v3"),
    file = c("landweb_ltfc_v10.shp", "landweb_ltfc_v8c.shp", "ltfc_sls_v3.shp"),
    fri_field = c("LTFC10", "LTHFC", "LTHFC"),
    label = c(
      "v10 (2026; NW-AB = NWAB Intermediate)",
      "v8c (previous study-area map)",
      "SLS v3 (Spray Lake Sawmills variant)"
    )
  )
}

## Read one LTHFC layer and return per-FRI-value area (km^2) + polygon counts.
## Tolerant of the field-name variants (LTFC10 / LTHFC / LTHRC).
.lthfc_layer_summary <- function(shp, version, label, fri_field) {
  if (!file.exists(shp)) {
    return(NULL)
  }
  v <- sf::st_make_valid(sf::st_read(shp, quiet = TRUE))
  fld <- if (fri_field %in% names(v)) {
    fri_field
  } else {
    intersect(c("LTFC10", "LTHFC", "LTHRC"), names(v))[1]
  }
  if (is.na(fld)) {
    return(NULL)
  }
  fri <- as.integer(v[[fld]])
  area_km2 <- as.numeric(sf::st_area(sf::st_transform(v, 4326))) / 1e6
  dt <- data.table::data.table(version = version, label = label, fri = fri, area_km2 = area_km2)
  dt <- dt[!is.na(fri)]
  dt[, .(area_km2 = sum(area_km2), n_polys = .N), by = .(version, label, fri)]
}

#' Per-version LTHFC fire-return-interval area summary
#'
#' Cache-aware producer for a `format = "file"` target: returns `out_csv`
#' unchanged if it already exists, else reads the LTHFC layers, computes the
#' study-area area assigned to each FRI value under each version, and writes it.
#'
#' @param out_csv path to write (and the value returned, for the file target).
#' @param inputs_dir directory holding the LTHFC shapefiles.
#' @return `out_csv`.
lthfc_version_summary <- function(out_csv, inputs_dir = "inputs") {
  if (file.exists(out_csv)) {
    return(out_csv) # already computed -- reuse
  }
  vers <- .lthfc_versions()
  res <- lapply(seq_len(nrow(vers)), function(i) {
    .lthfc_layer_summary(
      shp = file.path(inputs_dir, vers$file[i]),
      version = vers$version[i],
      label = vers$label[i],
      fri_field = vers$fri_field[i]
    )
  })
  out <- data.table::rbindlist(res)
  data.table::setorder(out, version, fri)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(out, out_csv)
  out_csv
}

## LandWeb LCC projection used for LTHFC maps (matches the NWAB mapping
## convention in the v2-branch R/study_area_maps.R).
.lthfc_map_crs <- function() {
  paste(
    "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  )
}

#' Spatial data for the v8c-vs-v10 LTHFC change map (report 02)
#'
#' Cache-aware producer for a `format = "file"` target: writes a GeoPackage with
#' two layers --- `lthfc` (v8c and v10 burnable polygons, tagged by `version` with
#' a `fri` field, both reprojected to the LandWeb LCC) and `aoi` (the dissolved
#' Northwest Alberta area-of-interest outline, the region the 2026 update revised).
#' The report facets `lthfc` by `version` and overlays `aoi` to highlight where the
#' fire-return interval changed. Returns `out_gpkg` unchanged if it already exists.
#'
#' @param out_gpkg path to write (and the value returned, for the file target).
#' @param inputs_dir directory holding the LTHFC shapefiles + the NW-AB AOI gpkg.
#' @return `out_gpkg`.
lthfc_change_map_data <- function(out_gpkg, inputs_dir = "inputs") {
  if (file.exists(out_gpkg)) {
    return(out_gpkg)
  }
  target_crs <- .lthfc_map_crs()
  prep <- function(fname, fld, ver) {
    v <- sf::st_make_valid(sf::st_read(file.path(inputs_dir, fname), quiet = TRUE))
    fri <- as.integer(v[[fld]])
    g <- sf::st_transform(sf::st_geometry(v), target_crs)
    out <- sf::st_sf(version = ver, fri = fri, geometry = g)
    out[!is.na(out$fri) & out$fri > 0, ]
  }
  both <- rbind(
    prep("landweb_ltfc_v8c.shp", "LTHFC", "v8c (previous)"),
    prep("landweb_ltfc_v10.shp", "LTFC10", "v10 (2026)")
  )
  dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_gpkg)) file.remove(out_gpkg)
  sf::st_write(both, out_gpkg, layer = "lthfc", quiet = TRUE)

  ## NW-AB area-of-interest outline (the region the 2026 update revised)
  aoi_src <- file.path(inputs_dir, "LTHFC_NW_AB.gpkg")
  if (file.exists(aoi_src)) {
    aoi <- sf::st_transform(sf::st_make_valid(sf::st_read(aoi_src, quiet = TRUE)), target_crs)
    aoi <- sf::st_sf(geometry = sf::st_union(aoi))
    sf::st_write(aoi, out_gpkg, layer = "aoi", quiet = TRUE, append = TRUE)
  }

  ## provincial boundaries (geographic reference), cropped to the study-area bbox
  prov <- .lthfc_provinces(inputs_dir, target_crs)
  if (!is.null(prov)) {
    bb <- sf::st_bbox(both)
    bb["xmin"] <- bb["xmin"] - 50000
    bb["xmax"] <- bb["xmax"] + 50000
    bb["ymin"] <- bb["ymin"] - 50000
    bb["ymax"] <- bb["ymax"] + 50000
    prov <- suppressWarnings(sf::st_crop(sf::st_make_valid(prov), bb))
    sf::st_write(prov["NAME_1"], out_gpkg, layer = "provinces", quiet = TRUE, append = TRUE)
  }
  out_gpkg
}

## Canadian provincial boundaries (GADM level 1) as sf in `target_crs`.
## Reads the cached geodata pack (inputs/gadm41_CAN_1_pk.rds); no download.
.lthfc_provinces <- function(inputs_dir, target_crs) {
  sv <- tryCatch(
    geodata::gadm("CAN", level = 1, path = inputs_dir),
    error = function(e) NULL
  )
  if (is.null(sv)) {
    pk <- file.path(inputs_dir, "gadm41_CAN_1_pk.rds")
    if (file.exists(pk)) sv <- tryCatch(terra::unwrap(readRDS(pk)), error = function(e) NULL)
  }
  if (is.null(sv)) {
    return(NULL)
  }
  sf::st_transform(sf::st_as_sf(sv), target_crs)
}

#' Rasterized v10 - v8c fire-return-interval difference (report 02)
#'
#' Cache-aware producer for a `format = "file"` target: rasterizes the burnable
#' FRI of each LTHFC layer onto a common LCC grid and writes the difference
#' (v10 minus v8c, years) as a GeoTIFF. Cells non-burnable in either layer are NA,
#' so only areas comparable in both versions carry a value (negative = v10 shortened
#' the fire cycle, e.g. the NW Alberta revision).
#'
#' @param out_tif path to write (and the value returned, for the file target).
#' @param inputs_dir directory holding the LTHFC shapefiles.
#' @param res_m raster resolution (m); 5000 = 5 km, adequate for a study-area map.
#' @return `out_tif`.
lthfc_diff_raster <- function(out_tif, inputs_dir = "inputs", res_m = 5000) {
  if (file.exists(out_tif)) {
    return(out_tif)
  }
  target_crs <- .lthfc_map_crs()
  read_fri <- function(fname, fld) {
    v <- terra::project(terra::vect(file.path(inputs_dir, fname)), target_crs)
    v$fri <- as.integer(unlist(v[[fld]], use.names = FALSE))
    v[!is.na(v$fri) & v$fri > 0, ]
  }
  v8 <- read_fri("landweb_ltfc_v8c.shp", "LTHFC")
  v10 <- read_fri("landweb_ltfc_v10.shp", "LTFC10")
  e8 <- terra::ext(v8)
  e10 <- terra::ext(v10)
  e <- terra::ext(
    min(e8[1], e10[1]), max(e8[2], e10[2]),
    min(e8[3], e10[3]), max(e8[4], e10[4])
  )
  r <- terra::rast(e, resolution = res_m, crs = target_crs)
  d <- terra::rasterize(v10, r, field = "fri") - terra::rasterize(v8, r, field = "fri")
  names(d) <- "fri_change"
  dir.create(dirname(out_tif), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(d, out_tif, overwrite = TRUE)
  out_tif
}
