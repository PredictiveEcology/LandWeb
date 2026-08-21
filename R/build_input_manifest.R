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
      name = "SCANFI v2 stand age and biomass (2020)",
      source = list(type = "drive", url = "https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf"),
      local_path = file.path(inputs_dir, "SCANFI_age_median_2020_v2_20260119.tif"),
      version_or_vintage = "2020, v2 (stamp 20260119)",
      license = "OGL-Canada-2.0",
      description = "30 m SCANFI attribute rasters (median stand age, biomass) used for v3 age/biomass initial conditions. Land cover is NOT this record: the pipeline consumes a derived reclassification, registered separately as scanfi-lcc-2020.",
      citation = list(bibtex_key = "scanfi-methods", external = TRUE)
    ),
    list(
      id = "scanfi-lcc-2020",
      name = "SCANFI v2 land cover, reclassified to NFI/EOSD codes (2020)",
      source = list(type = "drive", url = "https://drive.google.com/file/d/1EGp7LUA7cXMR6KpXDmu617xsjwGM6aIx"),
      local_path = file.path(inputs_dir, "SCANFI_att_nfiLandcover_CanadaLCCclassCodes_2020_v2_20260119.tif"),
      version_or_vintage = "2020, v2 (stamp 20260119); derived, reclassified",
      license = "OGL-Canada-2.0 (upstream); hosted copy access-controlled",
      description = paste0(
        "DERIVED product, not the published SCANFI layer: SCANFI's 8-class nfiLandcover ",
        "relabelled 1:1 into NFI/EOSD numeric codes (1:8 -> 40, 100, 30, 50, 220, 210, 230, 20) ",
        "by LandR::convert_SCANFI_LCC_codes(). Supplies rasterToMatch and the LCC input to ",
        "Biomass_borealDataPrep. Upstream source is ",
        "SCANFI_att_nfiLandcover_2020_v2_20260119.tif on the NRCan FTP; the DOI does not resolve ",
        "to this file, and the hosted copy is access-controlled rather than openly downloadable."
      ),
      citation = list(bibtex_key = "scanfi-methods", external = TRUE),
      extra = list(
        derived = TRUE,
        derived_from = "scanfi-structure-2020",
        derivation = "LandR::convert_SCANFI_LCC_codes()",
        upstream_url = paste0(
          "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/",
          "SCANFI_att_nfiLandcover_2020_v2_20260119.tif"
        )
      )
    ),
    list(
      id = "canlcc-2020",
      name = "2020 Land Cover of Canada (NALCMS, 30 m)",
      source = list(type = "http_download",
                    url = "https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47"),
      local_path = file.path(inputs_dir, "landcover-2020-classification.tif"),
      version_or_vintage = "2020 edition (Landsat OLI, mostly 2020 with some 2019/2021)",
      license = "OGL-Canada-2.0",
      description = paste0(
        "30 m FAO-LCCS land cover (19 level-II classes, 15 applicable in Canada) from the Canada ",
        "Centre for Remote Sensing; Canada's NALCMS contribution. Used as the CURRENT-CONDITIONS ",
        "land cover in v3. Chosen over SCANFI because SCANFI LCC already drives the simulation, ",
        "so reusing it for current conditions would be circular; the two are methodologically ",
        "independent (NFI photo-plot training vs unsupervised clustering + expert interpretation; ",
        "kNN imputation vs per-tile random forest). Two layers are derived: the simulation copy ",
        "reclassifies urban to its nearest type (pre-industrial approximation), while the ",
        "reporting copy retains urban."
      ),
      citation = list(bibtex_key = "LatifovicEtAl2017", external = TRUE),
      extra = list(
        cog_url = paste0("https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/",
                         "store/land/landcover/landcover-2020-classification.tif"),
        crs = "EPSG:3979",
        note = "Series is 2010/2015/2020; no 2025 edition released as of 2026-08."
      )
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

  ## The caribou range sources are DERIVED from LandWebUtils::caribouRangeLayers() rather than
  ## hand-listed here -- the one deliberate exception to this file being hand-curated. There are six
  ## of them, from six different authorities with different vintages and licences, and they are the
  ## dataset most likely to change (a provincial re-delineation). If
  ## the package's source table and this manifest were maintained separately they would drift, and the
  ## manifest is exactly the artifact that must not silently go stale.
  caribou <- tryCatch(LandWebUtils::caribouRangeLayers(), error = function(e) NULL)
  if (!is.null(caribou)) {
    LIC <- c(
      AB = "Alberta Open Government Licence", BC = "OGL-BC-2.0",
      SK = "Saskatchewan Open Data Licence", MB = "By request (Govt. of Manitoba); not openly licensed",
      NWT = "GNWT open data", ON = "OGL-Ontario-1.0"
    )
    VIN <- c(
      AB = "published 2012", BC = "BC Data Catalogue WFS (live)", SK = "published 2020",
      MB = "delineated 2015, provided 2018 (current)", NWT = "GNWT layer 97, modified 2023-06-23",
      ON = "LIO release 2019-09-26"
    )
    ds <- c(ds, lapply(seq_len(nrow(caribou)), function(i) {
      r <- caribou[i, ]
      list(
        id = paste0("caribou-ranges-", tolower(r$juris)),
        name = paste0(r$key, " (caribou reporting ranges: ", r$juris, ")"),
        source = list(type = if (r$source == "drive") "drive" else "http_download", url = r$id),
        local_path = file.path(inputs_dir, "reportingPolygons", "Caribou_Ranges",
                               paste0("caribou_", r$juris)),
        version_or_vintage = unname(VIN[[r$juris]]),
        license = unname(LIC[[r$juris]]),
        description = paste0(
          "Jurisdictional caribou range boundaries for ", r$juris,
          "; one of six sources assembled into LandWeb's caribou reporting layer by ",
          "LandWebUtils::buildCaribouRanges(). Labelled on ", r$labelCols,
          if (!is.na(r$extirpated)) "; locally extirpated herds excluded" else "", "."
        ),
        extra = list(assembled_into = "caribou-ranges", jurisdiction = r$juris)
      )
    }))
  }

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
