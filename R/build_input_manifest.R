## Build the LandWeb input-data manifest (_input_manifest.json).
##
## A hand-curated provenance record of the key input datasets the v3 pipeline
## consumes, built with the workflowtools manifest API. It is the substrate for
## (a) sync_manifest_to_bibtex() -> citations/data-sources.bib, and (b) the
## per-report provenance appendix / report 06.
##
## Run once (Rscript R/build_input_manifest.R) to (re)generate the manifest; it
## is committed to the repo. `retrieved_at` is taken from the local copy's mtime
## where the file exists (honest "when did we last fetch this"), else the vintage.
## sha256 is omitted deliberately -- several inputs are multi-GB rasters.

build_input_manifest <- function(
  out = "_input_manifest.json",
  inputs_dir = "inputs",
  reports_dir = "reports"
) {
  ## retrieved_at from a representative local file's mtime, else a fallback date.
  when <- function(rel, fallback = "2026-01-01T00:00:00Z") {
    f <- file.path(rel)
    if (file.exists(f)) file.info(f)$mtime else as.POSIXct(fallback, tz = "UTC")
  }

  ds <- list(
    list(
      id = "scanfi-species-2020",
      name = "SCANFI v2 per-species crown closure (2020)",
      source = list(type = "drive", url = "https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf"),
      local_path = file.path(inputs_dir, "SCANFI_spsCC_ABIE_BAL_2020_v2_20260119.tif"),
      version_or_vintage = "2020, v2 (stamp 20260119)",
      license = "OGL-Canada-2.0",
      description = "30 m per-species percent crown-closure rasters; the v3 species initial-conditions source (SCANFI_spsCC_<code>_2020_v2_20260119.tif).",
      citation = list(bibtex_key = "scanfi-v2", external = TRUE)
    ),
    list(
      id = "scanfi-structure-2020",
      name = "SCANFI v2 stand age, biomass, land cover (2020)",
      source = list(type = "drive", url = "https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf"),
      local_path = file.path(inputs_dir, "SCANFI_age_median_2020_v2_20260119.tif"),
      version_or_vintage = "2020, v2 (stamp 20260119)",
      license = "OGL-Canada-2.0",
      description = "30 m SCANFI attribute rasters (median age, biomass, NFI land-cover class) used for v3 age/biomass/LCC initial conditions.",
      citation = list(bibtex_key = "scanfi-methods", external = TRUE)
    ),
    list(
      id = "knn-beaudoin-2001",
      name = "Beaudoin kNN forest attributes (2001 base year)",
      source = list(type = "http_download", url = "https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990"),
      local_path = file.path(inputs_dir, "NFI_MODIS250m_2001_kNN_Species_Pice_Mar_v1.tif"),
      version_or_vintage = "2001 base year, v1 (250 m)",
      license = "OGL-Canada-2.0",
      description = "250 m kNN/MODIS species, biomass and stand-age maps; the v2 species/biomass/age initial-conditions source, superseded by SCANFI in v3.",
      citation = list(bibtex_key = "BeaudoinEtAl2014", external = TRUE)
    ),
    list(
      id = "lthfc-v10",
      name = "LandWeb long-term historic fire-cycle map, v10 (2026)",
      source = list(type = "drive", url = "https://drive.google.com/drive/folders/1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"),
      local_path = file.path(inputs_dir, "landweb_ltfc_v10.shp"),
      version_or_vintage = "v10 (2026; NW Alberta = NWAB Intermediate)",
      license = "Access controlled (LandWeb hosted Drive)",
      description = "Fire-return-interval polygons (field LTFC10) that drive LandMine; v10 incorporates the June-2026 NW Alberta update.",
      citation = list(bibtex_key = "peterson-nwab-data", external = TRUE)
    ),
    list(
      id = "lthfc-v8c",
      name = "LandWeb long-term historic fire-cycle map, v8c (previous)",
      source = list(type = "drive", url = "https://drive.google.com/drive/folders/1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"),
      local_path = file.path(inputs_dir, "landweb_ltfc_v8c.shp"),
      version_or_vintage = "v8c (NAD83 / UTM 11N; field LTHFC)",
      license = "Access controlled (LandWeb hosted Drive)",
      description = "The previous study-area fire-cycle map, retained for the v8c-vs-v10 comparison in report 02."
    ),
    list(
      id = "ltfc-sls-v3",
      name = "Spray Lake Sawmills fire-cycle variant (SLS v3)",
      source = list(type = "drive", url = "https://drive.google.com/drive/folders/1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"),
      local_path = file.path(inputs_dir, "ltfc_sls_v3.shp"),
      version_or_vintage = "v3 (NAD83 / UTM 11N; field LTHFC)",
      license = "Access controlled (LandWeb hosted Drive)",
      description = "Spray Lake / Crowsnest fire-cycle variant, the patch source for the interim v8d map (R/new_lthfc.R)."
    ),
    list(
      id = "nwab-data-report-2026",
      name = "Northwest Alberta NRV -- Data Report (June 2026)",
      source = list(type = "literature", url = "https://friaa.ca"),
      local_path = file.path(reports_dir, "2026", "2026-06-08_NWAB_Data_Report.pdf"),
      version_or_vintage = "June 2026",
      license = "FRIAA report",
      description = "Methods + numeric fire-cycle tables (incl. Table 2-5 FRI-by-NSR) behind the v10 LTHFC update.",
      citation = list(bibtex_key = "peterson-nwab-data", external = TRUE)
    ),
    list(
      id = "nwab-final-report-2026",
      name = "Northwest Alberta NRV -- Final Report (June 2026)",
      source = list(type = "literature", url = "https://friaa.ca"),
      local_path = file.path(reports_dir, "2026", "2026-06-08_NWAB_Final_Report.pdf"),
      version_or_vintage = "June 2026",
      license = "FRIAA report",
      description = "NRV results and scenario comparison for the NW Alberta fire-cycle update.",
      citation = list(bibtex_key = "peterson-nwab-final", external = TRUE)
    ),
    list(
      id = "gadm-canada-provinces",
      name = "GADM Canada provincial/territorial boundaries (level 1)",
      source = list(type = "http_download", url = "https://gadm.org/download_country.html"),
      local_path = file.path(inputs_dir, "gadm41_CAN_1_pk.rds"),
      version_or_vintage = "GADM 4.1",
      license = "GADM (non-commercial)",
      description = "Provincial boundaries used for geographic context in the report-02 fire maps."
    ),
    list(
      id = "lthfc-nwab-aoi",
      name = "Northwest Alberta area-of-interest outline",
      source = list(type = "manual_drop", url = "https://drive.google.com/drive/folders/1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"),
      local_path = file.path(inputs_dir, "LTHFC_NW_AB.gpkg"),
      version_or_vintage = "2026",
      license = "Access controlled (LandWeb hosted Drive)",
      description = "The NW Alberta AOI outline highlighted on the report-02 change maps."
    ),
    list(
      id = "current-condition-v2",
      name = "Current-condition layers, v2 era (CASFRI / Pickell / AVI-derived)",
      source = list(type = "drive", url = "https://drive.google.com/drive/folders/1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"),
      local_path = file.path(inputs_dir, "CurrentCondition"),
      version_or_vintage = "v2 era",
      license = "Access controlled / proprietary (CASFRI)",
      description = "The v2 current-condition species/age rasters (CurrentCondition/, Pickell*.tif, CASFRI GIDs), to be superseded by the 2026 AVI+VRI+SBFI composite (not yet staged locally).",
      extra = list(status = "superseded; 2026 composite pending local staging")
    )
  )

  records <- lapply(ds, function(d) {
    do.call(workflowtools::input_manifest_record, c(
      d[setdiff(names(d), "local_path_when")],
      list(retrieved_at = when(d$local_path))
    ))
  })
  workflowtools::write_input_manifest(records, path = out)
  out
}

if (identical(environment(), globalenv()) && !length(sys.calls())) {
  build_input_manifest()
}
