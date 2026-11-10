## Data for report 05, the per-study-area report (reports/05-study-area-report.qmd).
##
## build_report05_data() gathers, for one study-area group, everything the template shows, so the
## render only draws: the member tenures (the reporting units) and their areas, the area simulated as
## forest by vegetation type from the current-condition map and the share of it that LCC 2020 maps as
## forest, the per-replicate NRV values and the current condition from NRV_summary's parquet
## datasets, the v2 -> v3 boundary comparison, and where each simulated species' growth traits came
## from. Target `report05Data_<sa>` (R/targets_reports.R) calls it on a worker; the template reads
## the result from the file render_report05() writes.
##
## "Forest" in report 05 is the area simulated as forest: every pixel with a vegetation type, i.e.
## the land SCANFI 2025 maps as treed (or as forest land that recently lost its trees), including
## land that LCC 2020 calls shrubland, grassland or wetland (LandWebUtils::lcc2020_remap_table() lets
## SCANFI decide forest extent). The model burns and regrows all of it as forest.

## ---- registries: measured caveats the run's outputs do not record --------------------------------

#' Share of forest without a current-condition stand age, per study area
#'
#' The preamble measures it at 30 m (LandWebUtils::cc_age_pct_missing()) and stops above
#' `ccAgeMaxMissing`, but saves no record of the value. Measured with LandWeb_preamble 1.0.14 on the
#' October 2026 inputs (preamble NEWS, commit 3983784); only the groups whose gap is large enough to
#' flag. BigTroutLake measured 1.4% and is not flagged. Unaged pixels take LandR's imputed ages.
report05_cc_age_gaps <- function() {
  data.frame(
    study_area = c("InterlakePlain", "MidBorealLowland", "ChurchillRiverUpland"),
    pct_forest_unaged = c(28.9, 14.9, 11.1),
    stringsAsFactors = FALSE
  )
}

#' Known bias of the BC current-condition ages, in years
#'
#' The age layer's BC step counts the years since the inventory photo twice (`2025 - REFERENCE_YEAR +
#' PROJ_AGE_1`, where `PROJ_AGE_1` is already projected to 2025), so BC stands read about this many
#' years too old (found 2026-10-26; the corrected layer is pending). Current-condition numbers for BC
#' tenures are provisional until it arrives.
report05_bc_age_bias_years <- function() 20L

## ---- labels -----------------------------------------------------------------------------------

## the kind of sub-region a crossed reporting layer splits a tenure into
.report05_subregion_kinds <- c(
  ANSR = "natural subregion",
  BEC = "biogeoclimatic zone",
  NT_Ecoregion = "ecoregion",
  Ecoregion = "ecoregion"
)

## a species code (LandR) -> a common name from the species table, else the code
.report05_spp_name <- function(code, sppEquiv) {
  eq <- data.table::as.data.table(sppEquiv)
  i <- match(code, eq[["LandR"]])
  nm <- if ("PSP" %in% names(eq)) eq[["PSP"]][i] else NA_character_
  alt <- if ("EN_generic_full" %in% names(eq)) eq[["EN_generic_full"]][i] else NA_character_
  nm[is.na(nm) | !nzchar(nm)] <- alt[is.na(nm) | !nzchar(nm)]
  nm[is.na(nm) | !nzchar(nm)] <- code[is.na(nm) | !nzchar(nm)]
  paste0(toupper(substr(nm, 1, 1)), substring(nm, 2))
}

## ---- pieces -----------------------------------------------------------------------------------

## The tenures the report covers: the FMA-layer polygons that NRV_summary also crossed with the
## sub-region layers. The tenure layer is clipped to the group boundary, which also catches slivers
## of neighbouring tenures that belong to another group; LandWebUtils::buildCrossedReportingPolygons()
## crosses only the group's members, so a tenure with a crossed layer is a member.
.report05_tenures <- function(rp) {
  fma <- spatialutils::repair_geoms(terra::vect(rp[["FMA"]]))
  codes <- sort(unique(fma$Name))
  members <- codes[vapply(
    codes,
    function(t) any(startsWith(names(rp), paste0(t, " "))),
    logical(1)
  )]
  att <- data.table::as.data.table(terra::values(fma))
  ## each province's tenure name (as LandWebUtils::build_studyarea_crosswalk() reads them)
  name_cols <- intersect(
    c("FMA_NAME", "FML_NAME", "FMU_NAME", "TSA_NUMB_1", "FOREST_NAM"),
    names(att)
  )
  full_name <- vapply(
    members,
    function(t) {
      a <- att[att$Name == t][1L]
      v <- unlist(a[, name_cols, with = FALSE], use.names = FALSE)
      v <- trimws(as.character(v[!is.na(v) & nzchar(trimws(as.character(v)))]))
      if (length(v)) v[[1L]] else t
    },
    character(1)
  )
  is_bc <- vapply(
    members,
    function(t) {
      v <- if ("TSA_NUMB_1" %in% names(att)) att$TSA_NUMB_1[att$Name == t] else NA
      any(!is.na(v) & nzchar(trimws(as.character(v))))
    },
    logical(1)
  )
  diss <- terra::aggregate(fma[fma$Name %in% members, "Name"], by = "Name")
  area <- stats::setNames(terra::expanse(diss, unit = "ha", transform = FALSE), diss$Name)
  data.table::data.table(
    tenure = members,
    name = unname(full_name),
    bc = unname(is_bc),
    area_ha = unname(area[members])
  )
}

## Per tenure, the crossed layer that splits it into ecological sub-regions, by order of preference
## among the kinds present, skipping a layer that covers only part of the tenure (a tenure near a border
## can carry a sliver crossing with the neighbouring jurisdiction's layer). NA if none.
.report05_subregion_layer <- function(rp, tenure) {
  cand <- paste(tenure, names(.report05_subregion_kinds))
  cand <- cand[cand %in% names(rp)]
  if (!length(cand)) {
    return(NA_character_)
  }
  a <- vapply(
    cand,
    function(l) {
      v <- spatialutils::repair_geoms(terra::vect(rp[[l]]))
      sum(terra::expanse(v, unit = "ha", transform = FALSE))
    },
    numeric(1)
  )
  ## the first kind, in .report05_subregion_kinds' order of preference, that covers at least half as
  ## much as the best-covering one (ANSR and Ecoregion both cover an Alberta tenure; ANSR is wanted)
  cand[[which(a >= 0.5 * max(a))[[1L]]]]
}

## The current-condition vegetation-type map NRV_summary summarised: the urban-masked copy when the
## run had an urban mask, else rep01's year-0 map.
.report05_cc_vtm <- function(postprocess_dir) {
  f <- file.path(
    postprocess_dir,
    "_reportingMaps",
    c("_cc/rep01", "rep01"),
    "vegTypeMap_year0000.tif"
  )
  f <- f[file.exists(f)]
  if (!length(f)) {
    stop("no current-condition vegetation-type map under ", postprocess_dir, call. = FALSE)
  }
  f[[1L]]
}

## Area simulated as forest (ha) per polygon name and vegetation type in the current condition.
.report05_forest_area <- function(vtm, polys, layer) {
  a <- nrvtools::subregion_forested_area(vtm, polys, "Name")
  data.table::data.table(
    layer = layer,
    poly = a$poly,
    vegType = a$vegCover,
    area_ha = a$area_ha
  )
}

## Per polygon name, the pixels simulated as forest in the current condition (those with a vegetation
## type, counted as nrvtools::subregion_forested_area() counts them) and the share of them that LCC
## 2020 maps as forest. `lcc` is the preamble's `LandTypeCC_reporting`: LCC 2020 codes on the
## simulation grid (the most common 30 m class in each pixel), urban kept.
.report05_lcc2020_forest <- function(vtm, lcc, polys) {
  if (!terra::compareGeom(vtm, lcc, stopOnError = FALSE)) {
    stop("the LCC 2020 layer is not on the vegetation-type map's grid", call. = FALSE)
  }
  polys <- terra::project(polys, vtm)
  ext <- terra::extract(vtm, polys, cells = TRUE)
  vcol <- setdiff(names(ext), c("ID", "cell"))[[1L]]
  d <- data.table::data.table(
    poly = as.character(polys[["Name"]][[1L]])[ext[["ID"]]],
    veg = !is.na(as.character(ext[[vcol]])),
    ## raw codes, never the active category (labels can be lost on a file round trip)
    lcc = terra::values(lcc, mat = FALSE)[ext[["cell"]]]
  )
  d[
    veg & !is.na(poly),
    .(
      n_pixels = .N,
      lcc2020_forest = mean(lcc %in% LandWebUtils::lcc2020_classes()[["forest"]])
    ),
    by = poly
  ]
}

## Land that starts without trees: land-base pixels (`rtm`) that Biomass_borealDataPrep gave no
## cohorts (pixelGroup 0 in `pgm`, year 0), so they have no vegetation type in the current condition
## and are left out of it; mostly sparse stands (every species at or below `minCoverThreshold`) and
## recently disturbed forest. Per polygon name: land-base and bare pixels, and pixels counted in the
## current condition's age classes (a vegetation type and an age class), all forest and bare, by age
## class of the current-condition time since fire (`tsf`), binned as nrvtools:::.age_class_raster()
## bins it (intervals closed on the right).
.report05_bare_start <- function(vtm, rtm, pgm, tsf, polys, age_classes, age_cutoffs) {
  for (r in list(rtm, pgm, tsf)) {
    if (!terra::compareGeom(vtm, r, stopOnError = FALSE)) {
      stop("a year-0 layer is not on the vegetation-type map's grid", call. = FALSE)
    }
  }
  polys <- terra::project(polys, vtm)
  ext <- terra::extract(vtm, polys, cells = TRUE)
  vcol <- setdiff(names(ext), c("ID", "cell"))[[1L]]
  a <- terra::values(tsf, mat = FALSE)[ext[["cell"]]]
  ac <- findInterval(a, age_cutoffs[-1L], left.open = TRUE) + 1L
  ac[is.na(a) | a <= age_cutoffs[[1L]] - 0.1] <- NA_integer_
  pg <- terra::values(pgm, mat = FALSE)[ext[["cell"]]]
  d <- data.table::data.table(
    poly = as.character(polys[["Name"]][[1L]])[ext[["ID"]]],
    veg = !is.na(as.character(ext[[vcol]])),
    landbase = !is.na(terra::values(rtm, mat = FALSE)[ext[["cell"]]]),
    ageClass = age_classes[ac]
  )
  d[, bare := landbase & !veg & !is.na(pg) & pg == 0]
  d <- d[!is.na(poly)]
  counts <- d[, .(n_landbase = sum(landbase), n_bare = sum(bare)), by = poly]
  by_ac <- d[
    !is.na(ageClass),
    .(n_forest = sum(veg), n_bare = sum(bare)),
    by = .(poly, ageClass)
  ]
  list(counts = counts, by_age_class = by_ac)
}

## NRV_summary's file of zero rows for the vegetation types a polygon shows but lacks in a snapshot
## (`.fillLeadingZeros()`, its default `zerosFile`), one per `replicate=<id>/` of a dataset.
.report05_lw_zeros_file <- "lw_zeros.parquet"

## One reporting layer's per-replicate values and current condition, for the given metrics and
## vegetation types, from NRV_summary's `lw` parquet datasets. `zero_fill` marks the rows NRV_summary
## wrote as 0 for a vegetation type absent from that snapshot.
.report05_lw <- function(agg_dir, layer, metrics, vegTypes = NULL) {
  ref <- LandWebUtils::refCodeFor("lw", layer)
  lyr <- layer ## the datasets have a `layer` column of their own
  read_files <- function(files, zero_fill) {
    ds <- nrvtools::open_nrv_dataset(files)
    if (is.null(ds)) {
      return(NULL)
    }
    d <- data.table::as.data.table(dplyr::collect(dplyr::filter(ds, .data$metric %in% metrics)))
    if (!is.null(vegTypes)) {
      d <- d[d[["metric.1"]] %in% vegTypes]
    }
    d[, .(
      layer = lyr,
      poly = poly,
      metric = metric,
      vegType = metric.1,
      ageClass = class,
      rep = rep,
      time = time,
      value = value,
      zero_fill = zero_fill
    )]
  }
  read <- function(root) {
    f <- list.files(root, "[.]parquet$", recursive = TRUE, full.names = TRUE)
    zero <- basename(f) == .report05_lw_zeros_file
    d <- data.table::rbindlist(
      list(read_files(f[!zero], FALSE), read_files(f[zero], TRUE)),
      use.names = TRUE
    )
    if (nrow(d)) d else NULL
  }
  list(
    nrv = read(file.path(agg_dir, ref)),
    cc = read(file.path(agg_dir, paste0(ref, "_CC")))
  )
}

## `leadingProp` is the share of a vegetation type's own forest in each age class, so a snapshot
## without the type has no value (0 of 0 ha), not 0 in every age class: drop NRV_summary's zero rows
## for it. Area metrics (`leadingShare`, `totalArea_ge*ha`) keep theirs: 0 ha is a real value.
.report05_drop_absent <- function(d) {
  if (is.null(d)) {
    return(d)
  }
  d[!(metric == "leadingProp" & zero_fill)]
}

#' Percentiles of the simulated values, with the current condition's place among them
#'
#' @param nrv Per-replicate values (`value`), one row per replicate and snapshot.
#' @param cc Current-condition values (`value`), one row per group.
#' @param by Grouping columns, present in both.
#'
#' @return One row per group: the minimum, 12.5th, 25th, 50th, 75th and 87.5th percentiles and the
#'   maximum of the simulated values (`min`, `p12.5`, ..., `max`), their number `n`, the current
#'   condition `cc`, its mid-rank percentile among the simulated values `cc_rank` (0-100), and its
#'   `position`: `"below"` / `"above"` the simulated range, in its lower (`"low"`) or upper
#'   (`"high"`) quarter, or in its middle half (`"mid"`). A group with a current condition but no
#'   simulated value (a vegetation type that never leads in the simulations) has `n = 0` and `NA`
#'   percentiles, rank and position.
report05_percentiles <- function(nrv, cc, by) {
  probs <- c(0, 0.125, 0.25, 0.5, 0.75, 0.875, 1)
  qn <- c("min", "p12.5", "p25", "p50", "p75", "p87.5", "max")
  nrv <- data.table::as.data.table(nrv)
  cc <- data.table::as.data.table(cc)[, c(by, "value"), with = FALSE]
  data.table::setnames(cc, "value", "cc")
  q <- nrv[,
    c(
      stats::setNames(as.list(stats::quantile(value, probs, names = FALSE, type = 7)), qn),
      list(n = .N)
    ),
    by = by
  ]
  q <- merge(q, cc, by = by, all = TRUE)
  q[is.na(n), n := 0L]
  rk <- merge(nrv[, c(by, "value"), with = FALSE], cc, by = by)[,
    .(cc_rank = 100 * (mean(value < cc) + mean(value <= cc)) / 2),
    by = by
  ]
  q <- merge(q, rk, by = by, all.x = TRUE)
  q[,
    position := data.table::fcase(
      is.na(cc) | n == 0L , NA_character_ ,
      cc < min            , "below"       ,
      cc > max            , "above"       ,
      cc < p25            , "low"         ,
      cc > p75            , "high"        ,
      default = "mid"
    )
  ]
  q[]
}

## The species units of the area and where each unit's growth-curve traits came from, read from
## `growthTraitSource` (LandWebUtils::landweb_unit_growth_traits(): "<growthCurveSource> (<member>)",
## where growthCurveSource is "estimated" for a curve fitted to plot data and "imputed" for one given
## the hardwood or softwood mean).
.report05_growth_sources <- function(species, spp_list) {
  sp <- data.table::as.data.table(species)
  src <- as.character(sp[["growthTraitSource"]])
  kind <- sub(" [(].*$", "", src)
  from <- ifelse(grepl("[(].*[)]$", src), sub("^.*[(](.*)[)]$", "\\1", src), NA_character_)
  eq <- spp_list[["sppEquiv"]]
  units <- data.table::as.data.table(spp_list[["units"]])
  members <- units[["members"]][match(sp[["species"]], units[["unit"]])]
  members_lab <- vapply(
    strsplit(ifelse(is.na(members), sp[["species"]], members), "+", fixed = TRUE),
    function(m) paste(.report05_spp_name(m, eq), collapse = ", "),
    character(1)
  )
  share <- units[["share"]][match(sp[["species"]], units[["unit"]])]
  out <- data.table::data.table(
    unit = as.character(sp[["species"]]),
    members = members_lab,
    share = share,
    source = data.table::fcase(
      kind == "estimated" , "fitted"  ,
      kind == "imputed"   , "imputed" ,
      default = kind
    ),
    traits_from = ifelse(is.na(from), NA_character_, .report05_spp_name(from, eq)),
    growthTraitSource = src
  )
  out[order(-share, unit)]
}

## ---- main builder -------------------------------------------------------------------------------

#' Gather the data for one study-area group's report 05
#'
#' @param study_area Study-area group name.
#' @param reporting_polygons Value of target `reportingPolygons_<sa>`: a named list of `sf` layers.
#' @param species_traits Value of target `speciesTraits_<sa>` (with `species$growthTraitSource`).
#' @param spp_list Value of target `sppList_<sa>` (`sppEquiv`, `units`).
#' @param postprocess_dir NRV_summary's `postprocess/` directory for the study area.
#' @param v2_root Directory holding the v2 baseline runs, or `NA` to skip the v2 comparison.
#' @param veg_leading_prop The run's `vegLeadingProportion`, reported as the leading-type threshold.
#' @param lcc2020_file The preamble's LCC 2020 layer on the simulation grid (`LandTypeCC_reporting`),
#'   in the `preamble/` directory next to `postprocess_dir`.
#'
#' @return A named list; see the template for its use.
build_report05_data <- function(
  study_area,
  reporting_polygons,
  species_traits,
  spp_list,
  postprocess_dir = file.path("outputs", study_area, "postprocess"),
  v2_root = "outputs_v2",
  veg_leading_prop = NA_real_,
  lcc2020_file = file.path(dirname(postprocess_dir), "preamble", "LandTypeCC_reporting_year1.tif")
) {
  rp <- reporting_polygons
  agg <- file.path(postprocess_dir, "_aggregates")
  tenures <- .report05_tenures(rp)
  if (!nrow(tenures)) {
    stop("no member tenures in the FMA reporting layer of ", study_area, call. = FALSE)
  }
  tenures[,
    sub_layer := vapply(tenure, function(t) .report05_subregion_layer(rp, t), character(1))
  ]
  ## the layer's kind is what follows the tenure code ("WF_Edson ANSR" -> "ANSR")
  tenures[, sub_kind := unname(.report05_subregion_kinds[sub("^[^ ]+ ", "", sub_layer)])]

  ## area simulated as forest by type, current condition, over repaired polygons
  vtm_file <- .report05_cc_vtm(postprocess_dir)
  vtm <- terra::rast(vtm_file)
  repaired <- function(x) spatialutils::repair_geoms(terra::vect(x))
  fma <- repaired(rp[["FMA"]][rp[["FMA"]]$Name %in% tenures$tenure, ])
  forest <- .report05_forest_area(vtm, fma, "FMA")
  sub_layers <- unique(stats::na.omit(tenures$sub_layer))
  forest_sub <- data.table::rbindlist(lapply(sub_layers, function(l) {
    .report05_forest_area(vtm, repaired(rp[[l]]), l)
  }))
  all_forest <- forest[vegType == "All species"]
  tenures[, forest_ha := all_forest$area_ha[match(tenure, all_forest$poly)]]
  tenures[is.na(forest_ha), forest_ha := 0]

  ## group outline and area
  grp <- terra::aggregate(fma)
  terra::values(grp) <- data.frame(Name = study_area)
  group_area_ha <- sum(terra::expanse(grp, unit = "ha", transform = FALSE))
  group_forest <- .report05_forest_area(vtm, grp, "group")
  group_forest_ha <- sum(group_forest[vegType == "All species"]$area_ha)

  ## share of the area simulated as forest that LCC 2020 maps as forest, per tenure and group
  if (!file.exists(lcc2020_file)) {
    stop("no LCC 2020 layer at ", lcc2020_file, call. = FALSE)
  }
  lcc <- terra::rast(lcc2020_file)
  lcc_t <- .report05_lcc2020_forest(vtm, lcc, fma)
  lcc_g <- .report05_lcc2020_forest(vtm, lcc, grp)
  ## the same pixels as the area simulated as forest, or the share describes another area
  cell_ha <- prod(terra::res(vtm)) / 1e4
  n_t <- lcc_t$n_pixels[match(tenures$tenure, lcc_t$poly)]
  n_t[is.na(n_t)] <- 0L
  n_ok <- isTRUE(all.equal(n_t * cell_ha, tenures$forest_ha)) &&
    isTRUE(all.equal(sum(lcc_g$n_pixels) * cell_ha, group_forest_ha))
  if (!n_ok) {
    stop("LCC 2020 share: pixel counts differ from the area simulated as forest", call. = FALSE)
  }
  tenures[, lcc2020_forest := lcc_t$lcc2020_forest[match(tenure, lcc_t$poly)]]
  group_lcc2020_forest <- sum(lcc_g$lcc2020_forest * lcc_g$n_pixels) / sum(lcc_g$n_pixels)

  ## NRV values: age-class shares by vegetation type per tenure, all-forest shares per sub-region,
  ## and old-forest area in large patches per tenure
  patch_metrics <- paste0("totalArea_ge", c(100L, 500L, 1000L, 5000L), "ha")
  lw_fma <- .report05_lw(agg, "FMA", c("leadingProp", patch_metrics))
  keep_t <- function(d) d[poly %in% tenures$tenure]
  ## every snapshot, before the absent types' rows are dropped
  snapshots <- unique(lw_fma$nrv[metric == "leadingProp", .(rep, time)])
  lw_fma <- lapply(lw_fma, .report05_drop_absent)
  type_nrv <- keep_t(lw_fma$nrv[metric == "leadingProp"])
  type_cc <- keep_t(lw_fma$cc[metric == "leadingProp"])
  old_patches <- function(d) {
    d <- keep_t(d[metric %in% patch_metrics & ageClass == "Old" & vegType == "All species"])
    d[, min_patch_ha := as.integer(sub("^totalArea_ge([0-9]+)ha$", "\\1", metric))]
    d[]
  }
  patch_nrv <- old_patches(lw_fma$nrv)
  patch_cc <- old_patches(lw_fma$cc)

  ## the crossed polygons are named "<sub-region> <tenure>", their layers "<tenure> <kind>"
  by_subregion <- function(d) {
    if (!nrow(d)) {
      return(d)
    }
    d[, tenure := sub(" [^ ]+$", "", layer)]
    d[, subregion := sub(paste0(" ", tenure[[1L]], "$"), "", poly), by = tenure]
    d[]
  }
  sub_lw <- lapply(sub_layers, function(l) {
    lapply(.report05_lw(agg, l, "leadingProp", "All species"), .report05_drop_absent)
  })
  sub_nrv <- by_subregion(data.table::rbindlist(lapply(sub_lw, `[[`, "nrv")))
  sub_cc <- by_subregion(data.table::rbindlist(lapply(sub_lw, `[[`, "cc")))
  forest_sub <- by_subregion(forest_sub)

  pct_type <- report05_percentiles(type_nrv, type_cc, c("poly", "vegType", "ageClass"))
  data.table::setnames(pct_type, "poly", "tenure")
  ## in how many snapshots each type leads some of the tenure's forest (the snapshots its
  ## age-class shares summarise), for every type with forest today or in any snapshot
  type_presence <- merge(
    unique(type_nrv[, .(tenure = poly, vegType, rep, time)])[,
      .(n_present = .N),
      by = .(tenure, vegType)
    ],
    unique(forest[area_ha > 0, .(tenure = poly, vegType)]),
    by = c("tenure", "vegType"),
    all = TRUE
  )
  type_presence[is.na(n_present), n_present := 0L]
  type_presence[, n_snapshots := nrow(snapshots)]
  pct_sub <- if (nrow(sub_nrv)) {
    report05_percentiles(sub_nrv, sub_cc, c("tenure", "subregion", "ageClass"))
  } else {
    NULL
  }
  pct_patch <- report05_percentiles(patch_nrv, patch_cc, c("poly", "min_patch_ha"))
  data.table::setnames(pct_patch, "poly", "tenure")

  ## v2 -> v3 boundary change; the map layers are simplified to 100 m, for drawing only
  simplify <- function(x) {
    if (is.null(x)) NULL else sf::st_simplify(x, preserveTopology = TRUE, dTolerance = 100)
  }
  boundary <- lapply(tenures$tenure, function(t) {
    cmp <- fma_boundary_compare(t, fma, v2_root)
    if (!is.null(cmp$geoms)) {
      cmp$geoms <- lapply(cmp$geoms, simplify)
    }
    cmp
  })
  names(boundary) <- tenures$tenure

  ## readable group name: the AAFC ecoregion it is named after, where one matches
  eco <- rp[["Ecoregion"]]
  eco_names <- if (!is.null(eco) && "REGION_NAM" %in% names(eco)) {
    unique(as.character(eco$REGION_NAM))
  } else {
    character(0)
  }
  hit <- eco_names[gsub("[^A-Za-z]", "", eco_names) == study_area]
  label <- if (length(hit)) hit[[1L]] else gsub("([a-z])([A-Z])", "\\1 \\2", study_area)

  age_gaps <- report05_cc_age_gaps()
  ## NRV_summary's `ageClasses` / `ageClassCutOffs` defaults, which _targets.R does not override
  age_classes <- LandWebUtils:::.ageClasses
  age_cutoffs <- LandWebUtils:::.ageClassCutOffs

  ## land that starts without trees (left out of the current condition), from the year-0 maps
  sa_dir <- dirname(postprocess_dir)
  tsf0 <- file.path(sa_dir, "mainSim", c("_cc/rep01", "rep01"), "rstTimeSinceFire_year0000.tif")
  year0 <- c(
    rtm = file.path(sa_dir, "preamble", "rasterToMatch_year1.tif"),
    pgm = file.path(sa_dir, "mainSim", "rep01", "pixelGroupMap_year0000.tif"),
    tsf = tsf0[file.exists(tsf0)][1L] ## the urban-masked copy, as for the vegetation-type map
  )
  if (anyNA(year0) || !all(file.exists(year0))) {
    stop("missing year-0 maps under ", sa_dir, call. = FALSE)
  }
  bs <- .report05_bare_start(
    vtm,
    terra::rast(year0[["rtm"]]),
    terra::rast(year0[["pgm"]]),
    terra::rast(year0[["tsf"]]),
    fma,
    age_classes,
    age_cutoffs
  )
  at <- match(tenures$tenure, bs$counts$poly)
  tenures[, bare := bs$counts$n_bare[at] / bs$counts$n_landbase[at]]
  ## the current all-forest age-class shares with that land counted by its time since fire: the
  ## bare pixels added to the forest pixels behind each share (NRV_summary's share, our counts)
  cc_with_bare <- merge(
    bs$by_age_class[, .(tenure = poly, ageClass, n_forest, n_bare)],
    pct_type[vegType == "All species", .(tenure, ageClass, cc)],
    by = c("tenure", "ageClass")
  )
  cc_with_bare[,
    cc_with_bare := (cc * sum(n_forest) + n_bare) / (sum(n_forest) + sum(n_bare)),
    by = tenure
  ]

  list(
    study_area = study_area,
    study_area_label = label,
    built = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    tenures = tenures,
    group_area_ha = group_area_ha,
    group_forest_ha = group_forest_ha,
    group_lcc2020_forest = group_lcc2020_forest,
    group_forest = group_forest,
    map = list(
      group = simplify(sf::st_as_sf(grp)),
      tenures = simplify(sf::st_as_sf(terra::aggregate(fma[, "Name"], by = "Name")))
    ),
    forest = forest,
    forest_sub = forest_sub,
    type_nrv = type_nrv,
    type_cc = type_cc,
    sub_nrv = sub_nrv,
    sub_cc = sub_cc,
    patch_nrv = patch_nrv,
    patch_cc = patch_cc,
    pct_type = pct_type,
    cc_with_bare = cc_with_bare,
    type_presence = type_presence,
    pct_sub = pct_sub,
    pct_patch = pct_patch,
    boundary = boundary,
    growth = .report05_growth_sources(species_traits$species, spp_list),
    design = list(
      reps = sort(unique(snapshots$rep)),
      times = sort(unique(snapshots$time)),
      pixel_ha = prod(terra::res(vtm)) / 1e4,
      pixel_m = terra::res(vtm)[[1L]],
      age_classes = age_classes,
      age_cutoffs = age_cutoffs,
      veg_leading_prop = veg_leading_prop,
      patch_sizes_ha = sort(unique(patch_nrv$min_patch_ha))
    ),
    caveats = list(
      cc_age_gap = age_gaps$pct_forest_unaged[match(study_area, age_gaps$study_area)],
      bc_tenures = tenures$tenure[tenures$bc],
      bc_age_bias_years = report05_bc_age_bias_years()
    ),
    ## project-relative paths, for the data appendix
    sources = list(
      postprocess_dir = postprocess_dir,
      cc_vtm = vtm_file,
      lcc2020 = lcc2020_file,
      lcc2020_forest_classes = LandWebUtils::lcc2020_classes()[["forest"]],
      lw_zeros_file = .report05_lw_zeros_file,
      year0 = year0,
      sub_layers = sub_layers
    )
  )
}

#' Build report 05's data from the targets store (for a render outside the pipeline)
#'
#' @param study_area Study-area group name.
#' @param ... Passed to [build_report05_data()].
build_report05_data_from_store <- function(study_area, ...) {
  rd <- function(stem) targets::tar_read_raw(paste0(stem, "_", study_area))
  build_report05_data(
    study_area,
    reporting_polygons = rd("reportingPolygons"),
    species_traits = rd("speciesTraits"),
    spp_list = rd("sppList"),
    ...
  )
}
