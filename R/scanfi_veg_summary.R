## SCANFI per-species cover summary over the full LandWeb (LTHFC) study area.
##
## Quantifies, for each SCANFI source species that maps to a simulated LandWeb
## species, how many forested domain cells carry any cover (and, for the coastal
## "suspect" species, how many carry >= 10% cover), over the union of the LTHFC
## v10 polygons. This is the evidence behind the THUJ_PLI/TSUG_HET -> Abie_spp
## merge in LandWeb_preamble: western hemlock/redcedar are a localized SW-Rockies
## SCANFI artifact (~0.14% domain-wide vs ~25% at Spray Lake), not pervasive.
##
## The scan is genuinely expensive: the LTHFC v10 domain is ~3.0e6 km2
## (~3.2e9 cells per 30 m raster), so a full pass over the ~18 species rasters
## takes hours. scanfi_veg_summary() is therefore CACHE-AWARE: if `out_csv`
## already exists it returns the path unchanged, so a pre-seeded result is reused
## and the `format = "file"` target records that file's hash without recomputing.
## Delete `out_csv` (or bump the version stamp in its name) to force a fresh scan.

## SCANFI code -> common name + the simulated LandWeb species it merges into.
## (Mirrors the InitSpecies() dictionary in LandWeb_preamble.R; THUJ_PLI/TSUG_HET fold into Abie_spp.)
.scanfi_species_lookup <- function() {
  data.table::data.table(
    species = c(
      "ABIE_BAL",
      "ABIE_LAS",
      "BETU_PAP",
      "LARI_LAR",
      "LARI_OCC",
      "PICE_ENG",
      "PICE_ENG_GLA",
      "PICE_GLA",
      "PICE_MAR",
      "PINU_BAN",
      "PINU_CON_CON",
      "PINU_CON_LAT",
      "POPU_BAL",
      "POPU_TRE",
      "PSEU_MEN",
      "PSEU_MEN_GLA",
      "THUJ_PLI",
      "TSUG_HET"
    ),
    common_name = c(
      "balsam fir",
      "subalpine fir",
      "white birch",
      "tamarack",
      "western larch",
      "Engelmann spruce",
      "Engelmann x white spruce",
      "white spruce",
      "black spruce",
      "jack pine",
      "shore pine",
      "lodgepole pine",
      "balsam poplar",
      "trembling aspen",
      "Douglas-fir",
      "Douglas-fir (interior)",
      "western redcedar",
      "western hemlock"
    ),
    landweb_species = c(
      "Abie_spp",
      "Abie_spp",
      "Popu_spp",
      "Lari_spp",
      "Lari_spp",
      "Pice_gla",
      "Pice_gla",
      "Pice_gla",
      "Pice_mar",
      "Pinu_spp",
      "Pinu_spp",
      "Pinu_spp",
      "Popu_spp",
      "Popu_spp",
      "Pseu_men",
      "Pseu_men",
      "Abie_spp",
      "Abie_spp"
    )
  )
}

## Assemble the tidy summary table from raw counts. Shared by the live scan and
## the pre-seed builder so both emit an identical CSV layout. `counts` is a named
## numeric vector (SCANFI code -> cells with cover > 0); `ge10`/`cover_max` are
## named numeric vectors covering only the detailed (suspect) species.
.scanfi_assemble_summary <- function(
  counts,
  ge10,
  cover_max,
  domain_forested_cells,
  domain_area_km2
) {
  dt <- .scanfi_species_lookup()
  dt <- dt[species %in% names(counts)]
  dt[, n_cover_gt0 := as.numeric(counts[species])]
  dt[, pct_forested := round(100 * n_cover_gt0 / domain_forested_cells, 4)]
  dt[, n_cover_ge10 := as.numeric(ge10[species])]
  dt[, pct_ge10 := round(100 * n_cover_ge10 / domain_forested_cells, 4)]
  dt[, cover_max := as.numeric(cover_max[species])]
  dt[, domain_forested_cells := domain_forested_cells]
  dt[, domain_area_km2 := domain_area_km2]
  data.table::setorder(dt, -n_cover_gt0)
  dt[]
}

#' Per-species SCANFI cover summary over the LandWeb (LTHFC) study area
#'
#' Cache-aware producer for a `format = "file"` target: returns `out_csv`
#' unchanged if it already exists, else runs the full domain scan and writes it.
#'
#' @param out_csv path to write (and the value returned, for the file target).
#' @param inputs_dir directory holding the SCANFI species rasters + LTHFC zip.
#' @param lthfc_zip LTHFC v10 zip (relative to `inputs_dir`); extracted to read
#'   its `.shp` (the zip carries the `.prj`, unlike the loose `.shp` copy).
#' @param year,version,datestamp SCANFI raster name stamps
#'   (`SCANFI_spsCC_<CODE>_<year>_<version>_<datestamp>.tif`).
#' @param suspects SCANFI codes to additionally summarize at the >= 10% threshold
#'   (the coastal-attribution candidates).
#' @return `out_csv` (invisibly via the file target's hash).
scanfi_veg_summary <- function(
  out_csv,
  inputs_dir = "inputs",
  lthfc_zip = "landweb_ltfc_v10.zip",
  year = 2020L,
  version = "v2",
  datestamp = "20260119",
  suspects = c("TSUG_HET", "THUJ_PLI", "PINU_CON_CON")
) {
  if (file.exists(out_csv)) {
    return(out_csv) # pre-seeded / already computed -- skip the multi-hour scan
  }

  terra::terraOptions(memfrac = 0.7, progress = 0)
  lu <- .scanfi_species_lookup()
  fs <- file.path(
    inputs_dir,
    sprintf("SCANFI_spsCC_%s_%d_%s_%s.tif", lu$species, year, version, datestamp)
  )
  have <- file.exists(fs)
  fs <- fs[have]
  codes <- lu$species[have]

  ## LTHFC v10 union (extract zip -> .shp carries the .prj/CRS)
  exdir <- tempfile("lthfc_v10_")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(file.path(inputs_dir, lthfc_zip), exdir = exdir)
  shp <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
  b0 <- terra::aggregate(terra::vect(shp))

  ## rasterize the domain mask ONCE on the SCANFI grid, then reuse for every layer
  r0 <- terra::rast(fs[1])
  b <- terra::project(b0, terra::crs(r0))
  r0c <- terra::crop(r0, b)
  dmask <- terra::rasterize(b, r0c)
  domain_forested_cells <- as.numeric(terra::global(
    !is.na(terra::mask(r0c, dmask)),
    "sum",
    na.rm = TRUE
  )[1, 1])
  domain_area_km2 <- round(as.numeric(terra::expanse(b, unit = "km")))

  counts <- stats::setNames(numeric(length(codes)), codes)
  ge10 <- stats::setNames(rep(NA_real_, length(codes)), codes)
  cmax <- stats::setNames(rep(NA_real_, length(codes)), codes)
  for (i in seq_along(fs)) {
    r <- terra::crop(terra::rast(fs[i]), dmask)
    if (!terra::compareGeom(r, dmask, stopOnError = FALSE, messages = FALSE)) {
      r <- terra::resample(r, dmask, method = "near")
    }
    rm2 <- terra::mask(r, dmask)
    counts[codes[i]] <- as.numeric(terra::global(rm2 > 0, "sum", na.rm = TRUE)[1, 1])
    if (codes[i] %in% suspects) {
      ge10[codes[i]] <- as.numeric(terra::global(rm2 >= 10, "sum", na.rm = TRUE)[1, 1])
      cmax[codes[i]] <- as.numeric(terra::minmax(rm2, compute = TRUE)[2, 1])
    }
  }

  dt <- .scanfi_assemble_summary(counts, ge10, cmax, domain_forested_cells, domain_area_km2)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(dt, out_csv)
  out_csv
}
