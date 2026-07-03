## Beaudoin (2014) kNN per-species cover summary over the LandWeb (LTHFC v10) study area.
##
## The v2 companion to R/scanfi_veg_summary.R: it quantifies, for each Beaudoin
## kNN 2001 tree-species layer (NFI MODIS 250 m), the mean cover and prevalence
## over the LTHFC v10 domain, so report 01 can contrast the *initial conditions*
## the model starts from under kNN (v2) vs SCANFI 2020 (v3).
##
## The kNN product is 250 m (vs SCANFI's 30 m), so this scan is light (~seconds
## per layer). It is still CACHE-AWARE for parity with scanfi_veg_summary(): if
## `out_csv` exists it is returned unchanged. Delete it to force a fresh scan.
##
## Metric note: kNN species values are percent cover (0-100). `mean_cover` (the
## area-weighted mean over the domain) is the resolution-robust comparison metric;
## `pct_domain` (share of domain cells with any cover) is reported too but is not
## directly comparable in magnitude to SCANFI's 30 m prevalence.

## kNN species file stem -> common name + the simulated LandWeb species it maps to.
## (kNN resolves fewer species than SCANFI: no larch (Lari_spp), no Douglas-fir
## (Pseu_men), firs lumped, a single lodgepole/Pinus contorta layer.)
.knn_species_lookup <- function() {
  data.table::data.table(
    species = c(
      "Abie_Bal", "Abie_Las", "Abie_Spp", "Betu_Pap",
      "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Tre"
    ),
    common_name = c(
      "balsam fir", "subalpine fir", "fir (unspecified)", "white birch",
      "white spruce", "black spruce", "jack pine", "lodgepole pine", "trembling aspen"
    ),
    landweb_species = c(
      "Abie_spp", "Abie_spp", "Abie_spp", "Popu_spp",
      "Pice_gla", "Pice_mar", "Pinu_spp", "Pinu_spp", "Popu_spp"
    )
  )
}

#' Per-species Beaudoin kNN cover summary over the LandWeb (LTHFC) study area
#'
#' Cache-aware producer for a `format = "file"` target: returns `out_csv`
#' unchanged if it already exists, else scans the kNN layers and writes it.
#'
#' @param out_csv path to write (and the value returned, for the file target).
#' @param inputs_dir directory holding the kNN rasters + the LTHFC v10 shapefile.
#' @param lthfc_shp LTHFC v10 shapefile (relative to `inputs_dir`); dissolved to
#'   the study-area mask.
#' @param pattern kNN species-layer filename template (`%s` = species stem).
#' @return `out_csv`.
knn_veg_summary <- function(
  out_csv,
  inputs_dir = "inputs",
  lthfc_shp = "landweb_ltfc_v10.shp",
  pattern = "NFI_MODIS250m_2001_kNN_Species_%s_v1.tif"
) {
  if (file.exists(out_csv)) {
    return(out_csv) # pre-seeded / already computed
  }
  terra::terraOptions(memfrac = 0.6, progress = 0)
  lu <- .knn_species_lookup()
  fs <- file.path(inputs_dir, sprintf(pattern, lu$species))
  have <- file.exists(fs)
  lu <- lu[have]
  fs <- fs[have]

  ## LTHFC v10 domain, dissolved, projected to the kNN grid
  b0 <- terra::aggregate(terra::vect(file.path(inputs_dir, lthfc_shp)))
  r0 <- terra::rast(fs[1])
  b <- terra::project(b0, terra::crs(r0))
  domain_area_km2 <- round(as.numeric(terra::expanse(b, unit = "km")))

  n_gt0 <- numeric(nrow(lu))
  meanc <- numeric(nrow(lu))
  cmax <- numeric(nrow(lu))
  dcells <- NA_real_
  for (i in seq_along(fs)) {
    rc <- terra::mask(terra::crop(terra::rast(fs[i]), b), b)
    if (is.na(dcells)) {
      dcells <- as.numeric(terra::global(!is.na(rc), "sum", na.rm = TRUE)[1, 1])
    }
    n_gt0[i] <- as.numeric(terra::global(rc > 0, "sum", na.rm = TRUE)[1, 1])
    meanc[i] <- as.numeric(terra::global(rc, "mean", na.rm = TRUE)[1, 1])
    cmax[i] <- as.numeric(terra::minmax(rc, compute = TRUE)[2, 1])
  }

  dt <- lu[, .(species, common_name, landweb_species)]
  dt[, n_cover_gt0 := n_gt0]
  dt[, pct_domain := round(100 * n_gt0 / dcells, 4)]
  dt[, mean_cover := round(meanc, 3)]
  dt[, cover_max := round(cmax)]
  dt[, domain_cells := dcells]
  dt[, domain_area_km2 := domain_area_km2]
  data.table::setorder(dt, -mean_cover)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(dt, out_csv)
  out_csv
}
