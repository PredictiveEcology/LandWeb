## Build the caribou-range reference artifacts for the data-sources report:
##   outputs/_reference/caribouRanges.gpkg        -- layers `regional`, `eccc`, `aoi`
##   outputs/_reference/caribouRanges.csv         -- per-source unit counts + areas
##   outputs/_reference/caribouRanges.png         -- STANDALONE 2-panel map (title + legend)
##   outputs/_reference/caribouRanges_report.png  -- report map (no title; caption supplies it)
##
## WHAT THIS FIGURE IS FOR:
##   * `regional` -- built by `LandWebUtils::buildCaribouRanges()`, i.e. EXACTLY the layer the
##     pipeline reports on (same sources, same extirpation filter, same name normalisation). If this
##     map and the reporting units ever disagree, that is a bug, not a difference of method.
##   * `eccc` -- ECCC Priority Species at Risk, caribou subset. **Comparison only, NOT a reporting
##     layer.** Kept so the cost of the regional-only choice stays visible: the federal Southern
##     Mountain units are aggregate Local Population Units that MERGE the finer provincial herds, and
##     the federal layer carries whole cross-border ranges as single units where each provincial layer
##     holds only its own share.
## The two are NEVER merged: for ranges wholly inside one province they are geometrically identical
## (AB IoU = 1.0000 for 9 of 12 boreal ranges), so merging would double-count and mix vintages.
##
## Per-source provenance and quirks are documented on `?caribouRangeLayers`, not duplicated here.
##
## RUN WITH `Rscript scripts/make_caribou_reference.R` -- NOT `--vanilla`, which skips .Rprofile and
## so never activates renv (ggpattern is project-library only) and never sets GOOGLEDRIVE_AUTH (the
## Manitoba source is Drive-hosted, by request, with no public endpoint).
## Study-area-INDEPENDENT, so it lives at the outputs/ root under `_reference/` (see CLAUDE.md).
suppressMessages({
  pkgload::load_all("packages/LandWebUtils", quiet = TRUE)
  library(sf)
  library(ggplot2)
})

dp <- reproducible::checkPath("inputs/caribouReference", create = TRUE)
outDir <- reproducible::checkPath("outputs/_reference", create = TRUE)

## NEVER sf::st_make_valid() here -- it collapses reversed ring-winding-order polygons (the v10 FMA
## layer loses Prince Albert: -31,600 km^2 -> a 4 km^2 sliver). terra re-orients them correctly.
.repair <- function(x) spatialutils::repair_geoms(terra::vect(x))

.km2 <- function(x) as.numeric(sum(sf::st_area(sf::st_as_sf(x)))) / 1e6

aoi <- .repair(sf::st_read("inputs/landweb_ltfc_v10.shp", quiet = TRUE)) |>
  terra::project(LandWebCRS) |>
  terra::aggregate()

## ---- regional: the layer the pipeline actually reports on ---------------------------------------
message("assembling the regional caribou layer via LandWebUtils::buildCaribouRanges() ...")
regional <- buildCaribouRanges(aoi, destinationPath = dp)
stopifnot("buildCaribouRanges() returned nothing" = !is.null(regional) && nrow(regional) > 0L)

## ---- eccc: comparison only ----------------------------------------------------------------------
## Priority Species at Risk (2023-12-13), NOT the boreal-only `All51_Caribou_Ranges_Aires_2012` range
## file: the regional assembly includes mountain caribou, so a boreal-only comparator is not
## like-for-like. PrioritySpecies carries the Southern Mountain DU, and its boreal ranges are the same
## 2012 delineation as All51 anyway (geodesic areas agree to a median 0.0006%). Costs of the swap: no
## `(XXX)` range codes, and EPSG:3978 rather than an equal-area CRS (immaterial -- reprojected below).
## NB the real download endpoint is `api/file?path=` -- the `/data/...` paths are a JS SPA that returns
## HTTP 200 with an identical 2,200-byte shell for EVERY url, so liveness checks there are meaningless.
ECCC_PATH <- "/species/plansreports/priority-species-for-species-at-risk/PrioritySpecies.gdb.zip"
ECCC_URL <- paste0(
  "https://data-donnees.az.ec.gc.ca/api/file?path=",
  utils::URLencode(ECCC_PATH, reserved = FALSE)
)

## Designatable unit -> the map's classes. All four are mapped so the classification is explicit, then
## BOREAL AND MOUNTAIN ONLY are kept: barren-ground and Peary caribou are not used in LandWeb. That
## also keeps the comparison like-for-like, since the regional assembly carries no barren-ground either
## (the jurisdictions publish it separately -- GNWT has its own layer, and BC's ECOTYPE has no
## barren-ground class). For the record, 4 barren-ground ranges DO reach this AOI (Bathurst, Beverly,
## Bluenose East, Qamanirjuaq), covering ~495,000 km^2; no Peary range does.
IN_SCOPE <- c("Boreal", "Mountain")
.du <- function(pop, cn) {
  pop <- trimws(pop); cn <- trimws(cn)
  ifelse(pop == "Boreal", "Boreal",
    ifelse(pop == "Southern Mountain", "Mountain",
      ifelse(grepl("Barren-ground|Dolphin", paste(pop, cn)), "Barren-ground",
        ifelse(grepl("Peary", cn), "Peary", NA_character_))))
}

message("loading the ECCC comparison layer ...")
ez <- file.path(dp, "eccc", basename(ECCC_PATH))
reproducible::checkPath(dirname(ez), create = TRUE)
workflowtools::download_once(ECCC_URL, ez)
workflowtools::archive_extract_once(ez, dir = file.path(dp, "eccc"))
gdb <- list.files(file.path(dp, "eccc"), "PrioritySpecies\\.gdb$",
                  full.names = TRUE, include.dirs = TRUE)
gdb <- gdb[dir.exists(gdb)][[1L]]

eccc <- sf::st_read(gdb, quiet = TRUE)
eccc <- eccc[grepl("aribou", eccc$CommName_E), ] ## drops 13 Wood Bison + 2 Sage-grouse
eccc <- .repair(eccc)
eccc$ecotype <- .du(eccc$Population_E, eccc$CommName_E)
eccc$Name <- trimws(eccc$Range_Name_E)
eccc$juris <- "ECCC"
eccc <- eccc[eccc$ecotype %in% IN_SCOPE & !is.na(eccc$Name) & eccc$Name != "", ]
eccc <- spatialutils::prep_vector(eccc[, c("juris", "Name", "ecotype")], aoi, crs = LandWebCRS)

## ---- artifacts ---------------------------------------------------------------------------------
gpkg <- file.path(outDir, "caribouRanges.gpkg")
unlink(gpkg)
sf::st_write(sf::st_as_sf(regional), gpkg, layer = "regional", quiet = TRUE)
sf::st_write(sf::st_as_sf(eccc), gpkg, layer = "eccc", quiet = TRUE, append = TRUE)
sf::st_write(sf::st_as_sf(aoi), gpkg, layer = "aoi", quiet = TRUE, append = TRUE)

.row <- function(d, layer, source) {
  nby <- function(t) length(unique(d$Name[d$ecotype == t]))
  data.frame(
    layer = layer, source = source, n_units = length(unique(d$Name)),
    n_boreal = nby("Boreal"), n_mountain = nby("Mountain"),
    area_km2 = round(.km2(d)), stringsAsFactors = FALSE
  )
}
rdf <- as.data.frame(regional)
tab <- do.call(rbind, c(
  lapply(split(seq_len(nrow(rdf)), rdf$juris), function(i) {
    .row(regional[i, ], "regional", rdf$juris[i[[1L]]])
  }),
  list(.row(eccc, "eccc", "PrioritySpecies"))
))
## Regional TOTAL as its own row so report 06 reads it rather than restating a number. It is NOT the
## column sum: a cross-border range appears under two jurisdictions but is ONE reporting unit (the
## summaries group by name), so the totals count DISTINCT names -- currently 3 fewer than the sum.
tab <- rbind(tab, .row(regional, "regional", "(all sources)"))
tab <- tab[order(tab$layer, tab$source != "(all sources)", tab$source), ]
utils::write.csv(tab, file.path(outDir, "caribouRanges.csv"), row.names = FALSE)
print(tab, row.names = FALSE)

## ---- map ---------------------------------------------------------------------------------------
PANELS <- c(
  "Regional assembly (AB, BC, SK, MB, NWT, ON)",
  "ECCC Priority Species at Risk — caribou (2023)"
)
both <- rbind(
  transform(sf::st_as_sf(regional)[, c("juris", "Name", "ecotype")], panel = PANELS[[1L]]),
  transform(sf::st_as_sf(eccc)[, c("juris", "Name", "ecotype")], panel = PANELS[[2L]])
)
both$panel <- factor(both$panel, levels = PANELS)

## Blue/orange/aqua/violet + two extras, reused from the validated study-area palette where possible.
PAL <- c(AB = "#2a78d6", BC = "#eb6834", SK = "#1baf7a", MB = "#4a3aa7",
         NWT = "#d4a017", ON = "#c2185b", ECCC = "#5c6b73")

## One geometry per reporting unit, then simplified: `geom_sf_pattern()` rasterises a hatch per
## feature, so a few hundred multipart herd slivers make it crawl. 1 km is plenty at page size.
## (The .gpkg keeps the UNSIMPLIFIED geometries -- this is a plotting-only reduction.)
plt <- stats::aggregate(
  both["ecotype"],
  by = list(juris = both$juris, Name = both$Name, ecotype = both$ecotype, panel = both$panel),
  FUN = function(z) z[[1]]
)
plt <- sf::st_simplify(plt, dTolerance = 1000, preserveTopology = TRUE)
plt <- plt[!sf::st_is_empty(plt), ]
plt <- sf::st_as_sf(.repair(plt))
plt$panel <- factor(plt$panel, levels = PANELS)

## Fill = jurisdiction (who supplies the unit), hatch = designatable unit. The hatch is WHITE on the
## saturated fills, as in the study-area map: washing the fills out to carry a dark hatch would break
## the colour separation the palette was picked for.
gg <- ggplot() +
  geom_sf(data = sf::st_as_sf(aoi), fill = "grey96", colour = "grey55", linewidth = 0.3) +
  ggpattern::geom_sf_pattern(
    data = plt,
    aes(fill = juris, pattern = ecotype),
    colour = "white", linewidth = 0.08,
    pattern_colour = NA, pattern_fill = "white", pattern_alpha = 0.85,
    pattern_density = 0.22, pattern_spacing = 0.011, pattern_angle = 45
  ) +
  scale_fill_manual(values = PAL, breaks = names(PAL), name = "Source") +
  ggpattern::scale_pattern_manual(
    values = c(Boreal = "none", Mountain = "stripe"),
    name = "Designatable unit", breaks = IN_SCOPE
  ) +
  guides(
    fill = guide_legend(override.aes = list(pattern = "none"), order = 1),
    pattern = guide_legend(override.aes = list(fill = "grey45", colour = "white"), order = 2)
  ) +
  facet_wrap(~panel, ncol = 2) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_line(colour = "grey92", linewidth = 0.2),
    axis.text = element_text(size = 7, colour = "grey40"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

ggsave(file.path(outDir, "caribouRanges_report.png"), gg,
       width = 11, height = 6.2, dpi = 200, bg = "white")

## Hard-wrapped, not one long string: ggplot does not wrap a subtitle, so an unwrapped one runs off
## the right edge of the device (it did).
SUB <- paste(
  "Clipped to the LandWeb v10 area of interest. Boreal and mountain caribou only (barren-ground and Peary are not used);",
  "locally extirpated herds excluded. Fill = contributing jurisdiction, hatching = mountain caribou.",
  "The regional panel is the layer LandWeb reports on, built by the same LandWebUtils::buildCaribouRanges() the pipeline uses,",
  "because reporting goes to jurisdictional partners who each want their own management-unit names. The federal layer is kept",
  "for comparison only: its Southern Mountain units are aggregate Local Population Units that MERGE the finer provincial herds,",
  "which is why the federal panel has fewer mountain units over essentially the same ground.",
  sep = "\n"
)
ggsave(file.path(outDir, "caribouRanges.png"),
       gg + labs(
         title = "Caribou ranges: regional assembly vs ECCC Priority Species at Risk",
         subtitle = SUB,
         caption = "Sources and per-source unit counts: outputs/_reference/caribouRanges.csv; provenance: ?caribouRangeLayers"
       ) + theme(plot.subtitle = element_text(size = 9, colour = "grey30", lineheight = 1.15)),
       width = 11, height = 7.6, dpi = 200, bg = "white")

message("wrote: ", paste(list.files(outDir, "caribouRanges", full.names = TRUE), collapse = "\n       "))
