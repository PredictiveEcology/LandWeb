## SCANFI v3 land cover vs the SCANFI v2 land cover the simulation uses, per study-area group.
##
## Question: could SCANFI v3's 20-class land cover (released 2026-09-09) replace v2's 8-class
## `nfiLandcover` as the simulation's LCC input? For each of the 18 study-area groups this reports
## where each v2 class lands in v3, and how the area of forest classes changes, at 30 m and at the
## 240 m simulation resolution (`fact = 8, fun = "modal"`, as LandWeb_preamble aggregates LCC).
##
## Writes outputs/_extended_analyses/scanfi_v3_landcover/:
##   alignment_check.csv   proof that v3 sits on the v2 grid (see "grid alignment" below)
##   crosstab_30m.csv      group x v2 class x v3 class, km2
##   lcc_by_variant.csv    group x variant x resolution x LandR code, km2
##   forest_extent.csv     forest km2 per group and variant, and % change vs v2
##   forest_extent.png, crosstab.png
##
## Inputs: the public v2 `SCANFI_att_nfiLandcover_2020_v2_20260119.tif` (the pipeline's Drive file
## `..._CanadaLCCclassCodes_...` is a 1:1 relabel of it by LandR::convert_SCANFI_LCC_codes()), the
## v3 `cog_SCANFI_landcover_2020_v3_20260528.tif` (both fetched into inputs/ if missing), and the
## group polygons in outputs/_reference/studyAreaGroups.gpkg (scripts/make_sa_reference.R; groups
## are simplified at 1 km, which is immaterial here because both maps share the same mask).
##
## HEAVY -- national 30 m rasters, ~2 GB download, ~20 min on 6 cores. Run on a compute node from
## the project root (NOT --vanilla: renv must activate), e.g.
##   systemd-run --user --unit=scanfi-v3-lcc --working-directory="$PWD" \
##     Rscript-4.6.1 scripts/scanfi_v3_landcover_compare.R
Sys.setenv(OMP_NUM_THREADS = 1, GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
suppressMessages({
  library(data.table)
  library(ggplot2)
})

YEAR <- 2020L
N_CORES <- as.integer(Sys.getenv("SCANFI_V3_CORES", "6"))
MEMFRAC <- 0.06 ## per worker: 6 x 0.06 of a 503 GB node is ~180 GB

SCANFI_URL <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI"
V2_NAME <- sprintf("SCANFI_att_nfiLandcover_%d_v2_20260119.tif", YEAR)
V3_NAME <- sprintf("cog_SCANFI_landcover_%d_v3_20260528.tif", YEAR)
paths <- list(
  v2 = file.path("inputs", V2_NAME),
  v3 = file.path("inputs", V3_NAME),
  gpkg = Sys.getenv("SCANFI_V3_GPKG", "outputs/_reference/studyAreaGroups.gpkg"),
  out = Sys.getenv("SCANFI_V3_OUT", "outputs/_extended_analyses/scanfi_v3_landcover"),
  tmp = Sys.getenv("SCANFI_V3_TMP", file.path("/mnt/scratch", Sys.getenv("USER"), "scanfi_v3_tmp"))
)
dir.create(paths$out, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$tmp, recursive = TRUE, showWarnings = FALSE)

## ---- class tables ------------------------------------------------------------------------------
## LandR codes as LandWeb_preamble uses them: 20 water, 30 snow/ice + rock + barren, 40 bryoids,
## 50 shrubs, 100 herbs, 210 conifer, 220 broadleaf, 230 mixedwood, 240 recently disturbed (a
## forest class, imputed from neighbours), 99 "unwanted" (imputed by convertUnwantedLCC()).
FOREST_CODES <- c(210L, 220L, 230L)

## v2 `nfiLandcover` -> LandR, as LandR::convert_SCANFI_LCC_codes()
V2_CLASSES <- data.table(
  v2 = 1:8,
  v2_label = c(
    "Bryoids",
    "Herbs",
    "Rock/exposed",
    "Shrubs",
    "Broadleaf",
    "Conifer",
    "Mixedwood",
    "Water"
  ),
  code = c(40L, 100L, 30L, 50L, 220L, 210L, 230L, 20L)
)

## v3 `landcover` -> LandR. TWO variants differ only in the open-conifer classes 12-16, which v3
## assigns to "non-treed pixels with 10-50% coniferous crown closure" (dataset description). NFI
## calls >= 10% crown closure treed, so `open_forest` is the NFI-consistent reading; `open_nonforest`
## files them under their understory. Burn scars -> 240 in both, mirroring the preamble's FAO-2019
## stamp; they are reported separately so they can be left out. Cropland/urban -> 99 follows the
## LCC 2020 design (imputed to a pre-industrial type); roads -> 30 matches v2, which folds roads into
## rock/exposed (the preamble then replaces those 30s where LCC 2020 sees vegetation).
## TODO(curate): these mappings are this prototype's assumptions, not settled decisions.
V3_CLASSES <- data.table(
  v3 = 1:20,
  v3_label = c(
    "Water",
    "Rock",
    "Soil",
    "Burn scars",
    "Lichen",
    "Herbaceous",
    "Low shrubs",
    "Tall shrubs",
    "Treed broadleaf",
    "Treed mixed",
    "Treed coniferous",
    "Treed conif. + lichen",
    "Treed conif. + rock/soil",
    "Treed conif. + herbs",
    "Treed conif. + low shrubs",
    "Treed conif. + tall shrubs",
    "Cropland",
    "Urban",
    "Road",
    "Snow/ice"
  ),
  open_forest = c(
    20L,
    30L,
    30L,
    240L,
    40L,
    100L,
    50L,
    50L,
    220L,
    230L,
    210L,
    210L,
    210L,
    210L,
    210L,
    210L,
    99L,
    99L,
    30L,
    30L
  ),
  open_nonforest = c(
    20L,
    30L,
    30L,
    240L,
    40L,
    100L,
    50L,
    50L,
    220L,
    230L,
    210L,
    40L,
    30L,
    100L,
    50L,
    50L,
    99L,
    99L,
    30L,
    30L
  )
)

## ---- inputs ------------------------------------------------------------------------------------
fetch <- function(dest, url) {
  if (!file.exists(dest)) {
    message("downloading ", url)
    tmp <- paste0(dest, ".part")
    status <- system2(
      "curl",
      c("-sSfL", "--retry", "5", "-C", "-", "-o", shQuote(tmp), shQuote(url))
    )
    stopifnot("download failed" = identical(status, 0L))
    file.rename(tmp, dest)
  }
  dest
}
invisible(fetch(paths$v2, file.path(SCANFI_URL, "v2", V2_NAME)))
invisible(fetch(paths$v3, file.path(SCANFI_URL, "v3", V3_NAME)))

## ---- grid alignment ----------------------------------------------------------------------------
## v3 is on EPSG:3979 (lat_0 = 49, NAD83(CSRS)); v2 on the unregistered lat_0 = 0 Lambert (NAD83).
## At one datum the two differ by a pure northing shift of 6,585,076.02 m (PROJ), and v3's origin
## sits 6,585,000 m below v2's, so every v2 pixel centre falls 1.0 m inside v3 row s + 3. NRCan
## built v3 that way: v3 crown closure equals v2's at a 3-row offset (98-99% of pixels identical,
## ~25% at any other offset; checked 2026-09-28). A 1 m margin is within PROJ's NAD83 ->
## NAD83(CSRS) handling, so project(method = "near") may land a whole row off. Place v3 on the v2
## grid by the index shift instead -- relabel, not resample -- and prove it before use (below).
V3_ROW_SHIFT <- 3L

v3_on_v2_grid <- function(v3, v2) {
  stopifnot(
    all(terra::res(v3) == terra::res(v2)),
    terra::xmin(v3) == terra::xmin(v2)
  )
  dy <- terra::ymax(v2) - terra::ymax(v3) + V3_ROW_SHIFT * terra::yres(v2)
  terra::crs(v3) <- terra::crs(v2)
  terra::shift(v3, dy = dy)
}

## Proof on real data: v3 treed broadleaf must equal v2 broadleaf crown closure once shifted, and
## beat a one-row slip either way by a wide margin. Also records what project(method = "near")
## gives, which is what anyone adopting v3 would otherwise reach for.
check_alignment <- function(v2lc) {
  v2b <- terra::rast(paste0(
    "/vsicurl/",
    SCANFI_URL,
    "/v2/SCANFI_spsCC_broadleaf_",
    YEAR,
    "_v2_20260119.tif"
  ))
  v3b_raw <- terra::rast(paste0(
    "/vsicurl/",
    SCANFI_URL,
    "/v3/cog_SCANFI_treed_broadleaf_",
    YEAR,
    "_v3_20260528.tif"
  ))
  v3b <- v3_on_v2_grid(v3b_raw, v2lc)
  sites <- data.table(
    site = c("AB boreal plains", "SK boreal plains"),
    lon = c(-115, -104),
    lat = c(56, 55.5)
  )
  rbindlist(lapply(seq_len(nrow(sites)), function(i) {
    p <- terra::project(terra::vect(cbind(sites$lon[i], sites$lat[i]), crs = "EPSG:4326"), v2lc)
    xy <- terra::crds(p)
    win <- terra::align(terra::ext(xy[1] - 7500, xy[1] + 7500, xy[2] - 7500, xy[2] + 7500), v2lc)
    a <- terra::values(terra::crop(v2b, win), mat = FALSE)
    same <- function(b) {
      b <- terra::values(b, mat = FALSE)
      ok <- !is.na(a) & !is.na(b)
      mean(a[ok] == b[ok])
    }
    reproj <- terra::project(
      terra::crop(
        v3b_raw,
        terra::project(terra::extend(win, 300), from = terra::crs(v2lc), to = terra::crs(v3b_raw))
      ),
      terra::crop(v2b, win),
      method = "near"
    )
    data.table(
      site = sites$site[i],
      index_shift = same(terra::crop(v3b, win)),
      shift_plus_1_row = same(terra::crop(terra::shift(v3b, dy = 30), win)),
      shift_minus_1_row = same(terra::crop(terra::shift(v3b, dy = -30), win)),
      project_near = same(reproj)
    )
  }))
}

v2lc <- terra::rast(paths$v2)
align <- check_alignment(v2lc)
fwrite(align, file.path(paths$out, "alignment_check.csv"))
print(align)
stopifnot(
  "v3 does not sit on the v2 grid at the expected row shift" = all(align$index_shift > 0.95),
  "a one-row slip matches nearly as well: alignment is not identified" = all(
    align$index_shift - pmax(align$shift_plus_1_row, align$shift_minus_1_row) > 0.3
  )
)

## ---- per-group comparison ----------------------------------------------------------------------
compare_group <- function(g_name) {
  ## one temp dir per worker: forked workers share the parent's terra temp-file prefix, so
  ## terra::tmpFiles(remove = TRUE) in one could delete another's in-use files
  tmp <- file.path(paths$tmp, g_name)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  terra::terraOptions(memfrac = MEMFRAC, tempdir = tmp, progress = 0)
  data.table::setDTthreads(1)
  v2 <- terra::rast(paths$v2)
  v3 <- v3_on_v2_grid(terra::rast(paths$v3), v2)
  grp <- terra::vect(paths$gpkg, layer = "groups")
  g <- terra::project(grp[grp$group == g_name], v2)

  a <- terra::crop(v2, g, snap = "out", mask = TRUE)
  b <- terra::mask(terra::crop(v3, a, snap = "near"), g)
  stopifnot(terra::compareGeom(a, b, stopOnError = FALSE))

  ## 30 m crosstab; 0 = NA (outside the group, or no data in that map)
  code <- terra::lapp(
    c(a, b),
    fun = function(x, y) {
      x[is.na(x)] <- 0
      y[is.na(y)] <- 0
      x * 100 + y
    },
    wopt = list(datatype = "INT2U")
  )
  ct <- as.data.table(terra::freq(code))[value > 0]
  ct <- ct[, .(group = g_name, v2 = value %/% 100, v3 = value %% 100, km2 = count * 9e-4)]

  ## LandR codes per variant, at 30 m and aggregated to 240 m as the preamble does
  lcc <- list(
    v2 = terra::subst(a, V2_CLASSES$v2, V2_CLASSES$code),
    v3_open_forest = terra::subst(b, V3_CLASSES$v3, V3_CLASSES$open_forest),
    v3_open_nonforest = terra::subst(b, V3_CLASSES$v3, V3_CLASSES$open_nonforest)
  )
  by_code <- rbindlist(lapply(names(lcc), function(v) {
    x30 <- lcc[[v]]
    x240 <- terra::aggregate(x30, fact = 8, fun = "modal")
    rbind(
      as.data.table(terra::freq(x30))[, .(res_m = 30L, code = value, km2 = count * 9e-4)],
      as.data.table(terra::freq(x240))[, .(res_m = 240L, code = value, km2 = count * 0.0576)]
    )[, `:=`(group = g_name, variant = v)]
  }))
  list(crosstab = ct, by_code = by_code)
}

groups <- fread("outputs/_reference/studyAreaGroups.csv")[order(-area_km2), group]
only <- Sys.getenv("SCANFI_V3_GROUPS") ## comma-separated subset, for a quick test
if (nzchar(only)) {
  groups <- intersect(groups, strsplit(only, ",")[[1]])
}
t0 <- Sys.time()
res <- parallel::mclapply(groups, compare_group, mc.cores = N_CORES, mc.preschedule = FALSE)
failed <- vapply(res, inherits, logical(1), what = "try-error")
if (any(failed)) {
  stop("groups failed: ", paste(groups[failed], collapse = ", "), "\n", res[failed][[1]])
}
message("compared ", length(groups), " groups in ", format(round(Sys.time() - t0, 1)))

crosstab <- rbindlist(lapply(res, `[[`, "crosstab"))
crosstab <- V2_CLASSES[, .(v2, v2_label)][crosstab, on = "v2"]
crosstab <- V3_CLASSES[, .(v3, v3_label)][crosstab, on = "v3"]
setcolorder(crosstab, c("group", "v2", "v2_label", "v3", "v3_label", "km2"))
fwrite(crosstab[order(group, v2, v3)], file.path(paths$out, "crosstab_30m.csv"))

by_code <- rbindlist(lapply(res, `[[`, "by_code"))
setcolorder(by_code, c("group", "variant", "res_m", "code", "km2"))
fwrite(by_code[order(group, variant, res_m, code)], file.path(paths$out, "lcc_by_variant.csv"))

## ---- forest extent -----------------------------------------------------------------------------
forest <- by_code[,
  .(
    treed_km2 = sum(km2[code %in% FOREST_CODES]),
    burn_240_km2 = sum(km2[code == 240]),
    unwanted_99_km2 = sum(km2[code == 99]),
    total_km2 = sum(km2)
  ),
  by = .(group, variant, res_m)
]
forest[, v2_treed_km2 := treed_km2[variant == "v2"], by = .(group, res_m)]
forest[, `:=`(
  pct_change_treed = 100 * (treed_km2 - v2_treed_km2) / v2_treed_km2,
  pct_change_treed_plus_240 = 100 * (treed_km2 + burn_240_km2 - v2_treed_km2) / v2_treed_km2
)]
fwrite(forest[order(res_m, group, variant)], file.path(paths$out, "forest_extent.csv"))

## ---- figures -----------------------------------------------------------------------------------
fx <- forest[res_m == 240L & variant != "v2"]
fx <- rbind(
  fx[
    variant == "v3_open_nonforest",
    .(group, what = "Open conifer as non-forest", pct = pct_change_treed)
  ],
  fx[
    variant == "v3_open_forest",
    .(group, what = "Open conifer as forest", pct = pct_change_treed)
  ],
  fx[
    variant == "v3_open_forest",
    .(group, what = "Open conifer as forest, + burn scars", pct = pct_change_treed_plus_240)
  ]
)
fx[, group := factor(group, levels = fx[what == "Open conifer as forest"][order(pct), group])]
p1 <- ggplot2::ggplot(fx, ggplot2::aes(pct, group, colour = what)) +
  ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_colour_manual(
    values = c("#b2182b", "#2166ac", "#67a9cf"),
    breaks = c(
      "Open conifer as non-forest",
      "Open conifer as forest",
      "Open conifer as forest, + burn scars"
    )
  ) +
  ggplot2::labs(
    title = "Forest area in SCANFI v3 land cover vs v2, by study-area group",
    subtitle = paste0(
      YEAR,
      ", 240 m (modal of 30 m). Forest = conifer, broadleaf and mixedwood classes."
    ),
    x = "Change in forest area vs SCANFI v2 (%)",
    y = NULL,
    colour = "v3 reading"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
ggplot2::ggsave(file.path(paths$out, "forest_extent.png"), p1, width = 8, height = 7, dpi = 200)

ctd <- crosstab[, .(km2 = sum(km2)), by = .(v2, v2_label, v3, v3_label)]
ctd[, pct := 100 * km2 / sum(km2), by = v2]
ctd[, v2_label := factor(v2_label, levels = rev(V2_CLASSES$v2_label))]
ctd[, v3_label := factor(v3_label, levels = V3_CLASSES$v3_label)]
p2 <- ggplot2::ggplot(ctd, ggplot2::aes(v3_label, v2_label, fill = pct)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(
    data = ctd[pct >= 0.5],
    ggplot2::aes(label = sprintf("%.0f", pct), colour = pct > 50),
    size = 3
  ) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", limits = c(0, 100)) +
  ggplot2::labs(
    title = "Where each SCANFI v2 land-cover class goes in v3",
    subtitle = paste0(
      YEAR,
      ", 30 m, all 18 study-area groups. Cells: % of the v2 class's area (labelled if >= 0.5%)."
    ),
    x = "SCANFI v3 class",
    y = "SCANFI v2 class",
    fill = "% of v2 class"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(file.path(paths$out, "crosstab.png"), p2, width = 11, height = 5.5, dpi = 200)

message("wrote ", paths$out)
