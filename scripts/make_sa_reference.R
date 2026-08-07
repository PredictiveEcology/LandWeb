## Build the study-area-group reference artifacts consumed by report 00:
##   outputs/_reference/studyAreaGroups.gpkg           -- provinces / member tenures / group polygons
##   outputs/_reference/studyAreaGroups.csv            -- the group -> member lookup table
##   outputs/_reference/studyAreaGroups.png            -- STANDALONE map (title + legend; shareable)
##   outputs/_reference/studyAreaGroups_report.png     -- report map (no title/legend; the caption
##                                                        and the crosswalk table supply those)
##   outputs/_reference/studyAreaGroups_facets_p{1..3}.png -- one panel per group (tenures named),
##                                                        paginated 6 to a page
##
## RUN WITH `Rscript scripts/make_sa_reference.R` -- NOT `--vanilla`, which skips .Rprofile and so
## never activates renv, leaving ggpattern (project-library only) unavailable.
## Study-area-INDEPENDENT, so it lives at the outputs/ root under `_reference/` (see CLAUDE.md).
suppressMessages({
  pkgload::load_all("packages/LandWebUtils", quiet = TRUE)
  library(sf)
  library(ggplot2)
})
dp <- "inputs/reportingPolygons"
outDir <- reproducible::checkPath("outputs/_reference", create = TRUE)

## sf-friendly adapter for the terra-based repair. NEVER sf::st_make_valid() here: it collapses the
## reversed ring-winding-order polygons in the v10 FMA layer (Prince Albert: -31,600 km^2 -> a
## 4 km^2 sliver), which would silently shrink tenures on this very map.
.repair <- function(x) sf::st_as_sf(spatialutils::repair_geoms(x))

cw <- studyAreaCrosswalk(dp, LandWebCRS)
fmas <- prepFMAs(dp, LandWebCRS)
fv <- LandWebUtils:::.repairGeom(fmas)
ident <- LandWebUtils:::.fmaMemberIdentity(sf::st_drop_geometry(fv))
fv$fma_name <- ident$member
fv <- fv[!is.na(fv$fma_name) & fv$fma_name %in% cw$fma_name, ]

## one geometry per member tenure, tagged with its group
mem <- stats::aggregate(fv["fma_name"], by = list(fma_name = fv$fma_name), FUN = function(x) x[[1]])
mem <- merge(mem[, "fma_name"], cw[, c("fma_name", "name_short", "group", "province")], by = "fma_name")
mem <- .repair(sf::st_simplify(mem, dTolerance = 1000)) ## 1 km: plenty for a page-size map

## group boundaries = union of member tenures
grp <- stats::aggregate(mem["group"], by = list(group = mem$group), FUN = function(x) x[[1]])[, "group"]
grp <- .repair(grp)

## ---- lookup table -----------------------------------------------------------------------------
tab <- do.call(rbind, lapply(split(cw, cw$group), function(d) {
  data.frame(
    group = d$group[[1]],
    province = paste(sort(unique(d$province)), collapse = "/"),
    n_members = nrow(d),
    members = paste(sort(d$name_short), collapse = ", "),
    area_km2 = sum(d$area_km2),
    mpix = round(sum(d$mpix), 2),
    stringsAsFactors = FALSE
  )
}))
## Numbered ALPHABETICALLY by group name, so the map label, the legend and the lookup table all
## share one ordering and the numbers ascend wherever they appear. (Area/Mpix stay as columns, so
## run-cost ordering is still one sort away -- it just no longer drives the numbering.)
tab <- tab[order(tab$group), ]
tab$id <- seq_len(nrow(tab)) ## the map label, and the key into this table
tab <- tab[, c("id", "group", "province", "n_members", "members", "area_km2", "mpix")]

grp <- merge(grp, tab[, c("id", "group")], by = "group")
grp <- grp[order(grp$id), ]

## ---- fill: adjacency-safe graph colouring ------------------------------------------------------
## Fill groups the tenures VISUALLY; it does not name a group. 18 groups cannot each get a hue that
## stays colourblind-separable from all the others, so instead the group adjacency graph is properly
## coloured: neighbouring groups NEVER share a fill, while distant groups may reuse one. Identity is
## the number label (and the lookup table) throughout.
##
## Welsh-Powell (highest degree first) + an explicit verify. An earlier naive greedy silently left
## `NA` when it ran out of colours and a buggy check reported success -- five pairs of *touching*
## groups ended up sharing a fill. Hence: escalate the palette until a verified-clean colouring
## exists, and stop hard if none does.
nearKm <- 60
adj <- sf::st_is_within_distance(grp, dist = nearKm * 1000, sparse = TRUE)
adj <- lapply(seq_along(adj), function(i) setdiff(adj[[i]], i))

colour_graph <- function(adj, k) {
  n <- length(adj)
  col <- rep(NA_integer_, n)
  for (i in order(lengths(adj), decreasing = TRUE)) {
    taken <- stats::na.omit(col[adj[[i]]])
    free <- setdiff(seq_len(k), taken)
    if (!length(free)) return(NULL)
    col[i] <- free[[1L]]
  }
  ## verify explicitly rather than trusting the loop
  if (anyNA(col)) return(NULL)
  ok <- all(vapply(seq_len(n), function(i) !any(col[adj[[i]]] == col[i]), logical(1)))
  if (ok) col else NULL
}

## Blue / orange / aqua / violet. Validated as a set under the ALL-PAIRS rule, not the usual
## adjacent-slot one: a graph colouring can put *any* two of these side by side, so every pair must
## be separable (worst all-pairs CVD dE 9.2, normal-vision 20.5). Adding a 5th hue breaks that --
## none of the obvious candidates clears the all-pairs normal-vision floor -- so if the colouring
## ever needs more than 4, re-validate a new set rather than reaching for another swatch.
PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#4a3aa7")

## Hatch variants. 4 fills x 5 variants = 20 combinations >= 18 groups, so EVERY group gets a
## unique colour+hatch pair: the two channels together identify a group, and texture alone still
## separates same-coloured groups under colour-vision deficiency or greyscale printing. Kept coarse
## and low-contrast -- these tenures are small and fragmented, so denser hatching reads as noise.
##
## The hatch is WHITE, not black. Washing out the fills to let a dark hatch show was tried and
## rejected: lightening these four hues by even 35% drops the blue/violet pair to normal-vision
## dE 10.5 (floor is 15) and pushes two of them out of the lightness band, i.e. it breaks exactly
## the separation the palette was chosen for. A light hatch reads strongly against saturated fills
## instead, so the colours keep their validated separation AND the texture stays legible.
PATTERN <- c("none", "stripe", "stripe", "crosshatch", "stripe")
PATTERN_ANGLE <- c(0, 45, 135, 45, 90)

fill_slot <- NULL
for (k in 3:length(PAL)) {
  fill_slot <- colour_graph(adj, k)
  if (!is.null(fill_slot)) {
    nCol <- k
    break
  }
}
stopifnot(
  "no clean colouring within the validated 4-hue palette -- re-validate a larger set (see PAL)" =
    !is.null(fill_slot)
)

## Rebalance the colour classes, with a HARD CAP. A plain Welsh-Powell colouring is valid but
## lopsided -- it packs the first colour with as many groups as it can -- and since each colour
## class then needs one distinct hatch per member, a lopsided colouring demands more hatch variants
## than exist. Prefer, among the colours no neighbour is using, the least-used one that is still
## under `cap`; try several deterministic vertex orders and keep the first assignment that is both
## a valid colouring and within cap. (The earlier version only *preferred* balance, so it silently
## depended on vertex order -- renumbering the groups was enough to overflow a class.)
balance_colours <- function(adj, k, cap) {
  n <- length(adj)
  orders <- list(
    order(lengths(adj), decreasing = TRUE), order(lengths(adj)),
    seq_len(n), rev(seq_len(n))
  )
  for (ord in orders) {
    col <- rep(NA_integer_, n)
    for (i in ord) {
      free <- setdiff(seq_len(k), stats::na.omit(col[adj[[i]]]))
      if (!length(free)) break
      cnt <- tabulate(col[!is.na(col)], nbins = k)
      under <- free[cnt[free] < cap]
      col[i] <- if (length(under)) under[which.min(cnt[under])] else free[which.min(cnt[free])]
    }
    if (anyNA(col) || max(tabulate(col, nbins = k)) > cap) next
    if (all(vapply(seq_len(n), function(i) !any(col[adj[[i]]] == col[i]), logical(1)))) {
      return(col)
    }
  }
  NULL
}
balanced <- balance_colours(adj, nCol, cap = length(PATTERN))
grp$fill_slot <- if (!is.null(balanced)) balanced else fill_slot

## Within each colour class, hand out distinct hatch variants, so the (colour, pattern) pair is
## unique across all groups while the adjacency guarantee on colour is preserved.
grp$pat_slot <- ave(grp$fill_slot, grp$fill_slot, FUN = seq_along)
stopifnot(
  "not enough hatch variants for the largest colour class -- add one to PATTERN/PATTERN_ANGLE" =
    max(grp$pat_slot) <= length(PATTERN),
  "colour x pattern combination is not unique per group" =
    !anyDuplicated(paste(grp$fill_slot, grp$pat_slot))
)
cat(sprintf("fill: %d colours x up to %d hatch variants; %d groups, all combos unique\n",
            nCol, max(grp$pat_slot), nrow(grp)))
cat(sprintf("      verified no two groups within %d km share a fill colour\n", nearKm))
mem <- merge(mem, sf::st_drop_geometry(grp)[, c("group", "id", "fill_slot", "pat_slot")],
             by = "group")

## ---- context: the ecoregions the groups are named for ------------------------------------------
## Draw the ecoregion each group was formed from, so a reader can SEE the grouping principle rather
## than infer it from the fill: each study area should sit inside (and largely fill) one ecoregion.
## Only the 18 ecoregions that actually name a group are drawn -- the source layer has 218, which
## would be pure noise at page size.
eco <- prepEcoregionLayer(dp, LandWebCRS)
eco <- eco[LandWebUtils:::.groupToken(eco[["REGION_NAM"]]) %in% tab$group, "REGION_NAM"]
names(eco)[names(eco) == "REGION_NAM"] <- "eco_unit"
eco <- .repair(sf::st_simplify(eco, dTolerance = 2000))

## ECOPROVINCES for the per-group panel backdrop: coarse enough (68 units vs the ecoregions' 218)
## to read as context at panel size. The unit that actually DEFINES a group is its ecoregion
## (`build_studyarea_crosswalk(eco_field = "REGION_NAM")`; all 18 group names are ecoregion names),
## so sa_facets() draws that one on top with a darker fill -- coarse context plus the defining unit,
## without the full ecoregion mesh turning the panel into noise.
ecoAll <- prepEcoprovinceLayer(dp, LandWebCRS)[, "ECOPROVINC"]
names(ecoAll)[names(ecoAll) == "ECOPROVINC"] <- "eco_unit"
ecoAll <- .repair(sf::st_simplify(ecoAll, dTolerance = 2000))
ecoAll <- sf::st_crop(ecoAll, sf::st_bbox(sf::st_buffer(grp, 6e5)))

## ---- context: provinces / territories ----------------------------------------------------------
provDir <- reproducible::checkPath(file.path("inputs", "gadm"), create = TRUE)
prov <- sf::st_as_sf(geodata::gadm(country = "CAN", level = 1, path = provDir))
prov <- .repair(sf::st_simplify(sf::st_transform(prov[, "NAME_1"], sf::st_crs(grp)), dTolerance = 2000))
names(prov)[names(prov) == "NAME_1"] <- "province"

## ---- write vector artifacts --------------------------------------------------------------------
sf::st_write(prov, file.path(outDir, "studyAreaGroups.gpkg"), layer = "provinces",
             delete_dsn = TRUE, quiet = TRUE)
sf::st_write(eco, file.path(outDir, "studyAreaGroups.gpkg"), layer = "ecoregions",
             append = TRUE, quiet = TRUE)
sf::st_write(ecoAll, file.path(outDir, "studyAreaGroups.gpkg"), layer = "ecoprovinces",
             append = TRUE, quiet = TRUE)
sf::st_write(mem, file.path(outDir, "studyAreaGroups.gpkg"), layer = "members",
             append = TRUE, quiet = TRUE)
sf::st_write(grp, file.path(outDir, "studyAreaGroups.gpkg"), layer = "groups",
             append = TRUE, quiet = TRUE)
write.csv(tab, file.path(outDir, "studyAreaGroups.csv"), row.names = FALSE)

## Per-group style vectors, keyed by the group id used on the map and in the key.
STYLE <- local({
  g <- sf::st_drop_geometry(grp)[order(grp$id), ]
  nm <- as.character(g$id)
  list(
    fill = stats::setNames(PAL[g$fill_slot], nm),
    pattern = stats::setNames(PATTERN[g$pat_slot], nm),
    angle = stats::setNames(PATTERN_ANGLE[g$pat_slot], nm)
  )
})

## ---- the figure --------------------------------------------------------------------------------
lab <- cbind(sf::st_drop_geometry(grp),
             sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(grp))))
bb <- sf::st_bbox(grp)
pad <- 1.2e5

sa_map <- function(standalone = FALSE) {
  p <- ggplot() +
    ## provinces: light grey fill, faint slightly-darker boundary
    geom_sf(data = prov, fill = "grey96", colour = "grey82", linewidth = 0.3) +
    ## the ecoregions the groups are named for: dashed, so they read as context not as a boundary
    geom_sf(data = eco, fill = "#2a78d6", alpha = 0.05, colour = "grey55",
            linewidth = 0.3, linetype = "22") +
    ## Member tenures: filled by their group, thin black outlines. The hatch pattern CO-VARIES with
    ## the fill, so the four fills are separable by texture alone -- a reader with colour-vision
    ## deficiency, or a greyscale print, loses nothing. (v2's NWAB comparison figures use the same
    ## ggpattern recipe: black hatch, 45 deg, low density.)
    ggpattern::geom_sf_pattern(
      data = mem,
      aes(fill = factor(id), pattern = factor(id), pattern_angle = factor(id)),
      colour = "grey20", linewidth = 0.12,
      pattern_fill = "white", pattern_colour = "white", pattern_density = 0.09,
      pattern_spacing = 0.010, pattern_alpha = 0.9
    ) +
    ## study-area groups: bold outline
    geom_sf(data = grp, fill = NA, colour = "black", linewidth = 0.7) +
    ggrepel::geom_label_repel(
      data = lab, aes(X, Y, label = id), size = 3, fontface = "bold",
      label.padding = unit(0.14, "lines"), label.size = 0.25, colour = "grey10",
      fill = "white", alpha = 0.92, box.padding = 0.45, point.padding = 0.15,
      force = 4, max.overlaps = Inf, min.segment.length = 0,
      segment.colour = "grey30", segment.size = 0.35, seed = 1
    ) +
    scale_fill_manual(values = STYLE$fill, guide = "none") +
    ## Explicit pattern scales: ggpattern's DEFAULT discrete scale reaches for hex/circle patterns,
    ## which need geometry parameters this map does not supply (it errors out).
    ggpattern::scale_pattern_manual(
      values = STYLE$pattern, guide = "none"
    ) +
    ggpattern::scale_pattern_angle_manual(
      values = STYLE$angle, guide = "none"
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    coord_sf(xlim = bb[c("xmin", "xmax")] + c(-1, 1) * pad,
             ylim = bb[c("ymin", "ymax")] + c(-1, 1) * pad, expand = FALSE)

  if (!standalone) {
    ## report version: the figure caption and the adjacent crosswalk table do the explaining
    return(p + theme_bw() + theme(axis.title = element_blank(), panel.grid = element_blank()))
  }

  ## standalone version: must explain itself with no surrounding text, so it carries a title and a
  ## number -> name key (colour cannot serve as the key -- see the colouring note above)
  ## Title/subtitle only; the number -> name key is a separate panel (below), because a ggplot
  ## caption soft-wraps to the panel width no matter how the string is pre-broken, which orphans
  ## each number from its group name.
  p +
    labs(
      title = "LandWeb v3 study-area groups",
      subtitle = paste(
        "A study area is the FILLED tenures inside each bold outline. It takes its NAME from the",
        "ecoregion it mostly falls in (dashed, unfilled) --\nso a number labels the tenure group,",
        "not the ecoregion outline near it. Each group has a unique colour + hatch (key below);",
        "neighbouring groups always differ in colour."
      )
    ) +
    theme_bw() +
    theme(
      axis.title = element_blank(), panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 8, colour = "grey30", lineheight = 1.1)
    )
}

## ---- companion: one panel per study-area group -------------------------------------------------
## The overview map keeps the spatial scale and relative position of the groups, but the dense
## Alberta cluster is unavoidably busy there. This plate answers the other question -- "which
## tenures are in group N, and what do they look like?" -- by giving each group its own panel at
## its own extent, with the member tenures labelled by their short name.
##
## Every panel shares ONE map scale across all three pages, so group sizes stay comparable both
## within and between pages -- the alternative (per-panel or per-page extents) would silently imply
## that a 2,363 km^2 group and a 106,440 km^2 one are the same size. The price is white space around
## the smaller groups, which is the honest depiction. Colour does no work here -- one group per
## panel -- so the fill is uniform and the hatching is dropped.
sa_facets <- function(page = 1L, perPage = 6L, ncol = 3L) {
  ## NB: `facet_wrap(scales = "free")` is impossible with `coord_sf()` (ggplot2 errors outright),
  ## so each group is its own plot and patchwork assembles the plate.
  ids <- tab$id[seq(from = (page - 1L) * perPage + 1L,
                    to = min(page * perPage, nrow(tab)))]
  ## ONE common map scale across every panel on every page: each panel is the same size AND covers
  ## the same ground distance, so the panels are directly comparable and every scale bar comes out
  ## identical. That is the point -- a per-panel extent would silently make a 2,363 km^2 group look
  ## the same size as a 106,440 km^2 one. Window = the largest group's span (x1.08), 4:3.
  aspect <- 4 / 3
  spans <- vapply(tab$id, function(i) {
    b <- sf::st_bbox(mem[mem$id == i, ])
    max(b[["xmax"]] - b[["xmin"]], (b[["ymax"]] - b[["ymin"]]) * aspect)
  }, numeric(1))
  winW <- max(spans) * 1.08
  winH <- winW / aspect
  panels <- lapply(ids, function(i) {
    d <- mem[mem$id == i, ]
    r <- tab[tab$id == i, ]
    b <- sf::st_bbox(d)
    cx <- (b[["xmax"]] + b[["xmin"]]) / 2; cy <- (b[["ymax"]] + b[["ymin"]]) / 2
    w <- winW; h <- winH ## identical for every panel -- see the common-scale note above
    ggplot(d) +
      ## Ecoprovinces behind, faint -- coarse ecological context.
      geom_sf(data = ecoAll, fill = "grey97", colour = "grey85", linewidth = 0.18) +
      ## THIS group's naming ecoregion, darker: the unit that actually defined the group. Warm
      ## neutral, deliberately NOT a blue tint -- the tenures are blue and would be hard to tell
      ## apart from a blue-tinted ecoregion.
      geom_sf(data = eco[LandWebUtils:::.groupToken(eco$eco_unit) == r$group, ],
              fill = "#e6d7bd", colour = "grey35", linewidth = 0.5) +
      ## No colour/hatch here: one group per panel, so neither encodes anything, and both fight the
      ## detail this plate exists to show -- the INTERIOR tenure boundaries and their labels. A pale
      ## fill with a crisp dark stroke maximises both. Slightly transparent so the ecoregion lines
      ## drawn over it stay visible -- the point is to see how much of a tenure sits in which one.
      geom_sf(fill = "#cfe0f2", colour = "grey10", linewidth = 0.32, alpha = 0.8) +
      geom_sf(data = ecoAll, fill = NA, colour = "grey65", linewidth = 0.18, linetype = "22") +
      ggrepel::geom_text_repel(
        aes(label = name_short, geometry = geometry), stat = "sf_coordinates",
        size = 2.1, colour = "grey5", segment.colour = "grey35", segment.size = 0.2,
        min.segment.length = 0, max.overlaps = Inf, seed = 1,
        box.padding = 0.4, point.padding = 0.1, force = 8,
        bg.color = "white", bg.r = 0.2
      ) +
      ggspatial::annotation_scale(
        location = "br", height = unit(0.07, "cm"), text_cex = 0.4, line_width = 0.3
      ) +
      labs(title = sprintf("%d · %s", r$id, r$group),
           subtitle = sprintf("%d tenure%s · %s km²", r$n_members,
                              ifelse(r$n_members == 1L, "", "s"),
                              format(r$area_km2, big.mark = ","))) +
      coord_sf(xlim = c(cx - w / 2, cx + w / 2), ylim = c(cy - h / 2, cy + h / 2),
               expand = FALSE) +
      theme_bw(base_size = 7) +
      theme(
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 7),
        plot.subtitle = element_text(size = 5.5, colour = "grey35")
      )
  })

  nPages <- ceiling(nrow(tab) / perPage)
  patchwork::wrap_plots(panels, ncol = ncol) +
    patchwork::plot_annotation(
      title = sprintf("LandWeb v3 study-area groups: member forest tenures (%d of %d)",
                      page, nPages),
      subtitle = paste(
        "All panels share ONE map scale, so the groups are directly comparable in size",
        "(they differ ~45-fold in area, ~6-fold across).\nTenure labels are the curated short names",
        "used throughout the reporting; panel numbers match the overview map.\nDashed lines are",
        "ecoprovince boundaries -- the unit the PSP trait pool is drawn from; the tan area is the",
        "ECOREGION the group is NAMED for, which is what defines its membership.",
        collapse = " "
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 8, colour = "grey30", lineheight = 1.1)
      )
    )
}

## The key: a swatch of each group's ACTUAL colour+hatch beside its number and name. Drawn at fixed
## coordinates so nothing can reflow (a ggplot caption soft-wraps and orphans numbers from names).
sa_key <- function(ncol = 3L) {
  ## Alphabetical, which is also numeric order (the ids ARE the alphabetical rank), so the legend
  ## can be scanned by either name or number.
  k <- tab[order(tab$id), ]
  n <- nrow(k)
  nrow_ <- ceiling(n / ncol)
  d <- data.frame(
    id = k$id, group = k$group,
    col = (seq_len(n) - 1L) %/% nrow_,
    row = (seq_len(n) - 1L) %% nrow_
  )
  sw <- 0.16 ## swatch width, in column units
  ggplot(d) +
    ggpattern::geom_rect_pattern(
      aes(xmin = col, xmax = col + sw, ymin = -row - 0.32, ymax = -row + 0.32,
          fill = factor(id), pattern = factor(id), pattern_angle = factor(id)),
      colour = "grey25", linewidth = 0.25,
      pattern_fill = "white", pattern_colour = "white", pattern_density = 0.09,
      pattern_spacing = 0.035, pattern_alpha = 0.9
    ) +
    geom_text(aes(x = col + sw + 0.06, y = -row, label = sprintf("%2d  %s", id, group)),
              hjust = 0, size = 2.9, colour = "grey15") +
    scale_fill_manual(values = STYLE$fill, guide = "none") +
    ggpattern::scale_pattern_manual(values = STYLE$pattern, guide = "none") +
    ggpattern::scale_pattern_angle_manual(values = STYLE$angle, guide = "none") +
    scale_x_continuous(limits = c(-0.05, ncol), expand = expansion(mult = 0.01)) +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    theme_void()
}

ggsave(file.path(outDir, "studyAreaGroups.png"),
       patchwork::wrap_plots(sa_map(standalone = TRUE), sa_key(), ncol = 1,
                             heights = grid::unit(c(1, 1.35), c("null", "in"))),
       width = 10, height = 10.5, dpi = 200, bg = "white")
ggsave(file.path(outDir, "studyAreaGroups_report.png"), sa_map(standalone = FALSE),
       width = 7, height = 7.5, dpi = 200, bg = "white")
## Paginated: 3 pages of 6 panels. One 18-panel plate shrinks each map (and its tenure labels)
## past legibility; 6 to a page keeps them readable and matches how the NRV figure sets paginate.
nFacetPages <- ceiling(nrow(tab) / 6L)
for (pg in seq_len(nFacetPages)) {
  ggsave(sprintf("%s/studyAreaGroups_facets_p%d.png", outDir, pg), sa_facets(page = pg),
         width = 11, height = 8, dpi = 200, bg = "white")
}

## ---- guard: geometry must not have silently shrunk ---------------------------------------------
## The failure this catches is real and quiet: `sf::st_make_valid()` on the reversed-winding v10
## polygons returns a valid-but-tiny sliver, so a tenure would just look smaller on the map with no
## error anywhere. Compare the drawn areas against the crosswalk's independently-computed areas.
chk <- merge(
  data.frame(fma_name = mem$fma_name, drawn_km2 = as.numeric(sf::st_area(mem)) / 1e6),
  cw[, c("fma_name", "name_short", "area_km2")], by = "fma_name"
)
chk$ratio <- chk$drawn_km2 / chk$area_km2
bad <- chk[chk$ratio < 0.95 | chk$ratio > 1.05, ]
if (nrow(bad)) {
  warning("geometry area mismatch (drawn vs crosswalk) for ", nrow(bad), " tenure(s): ",
          paste(sprintf("%s %.2fx", bad$name_short, bad$ratio), collapse = "; "), call. = FALSE)
} else {
  cat("area check: all", nrow(chk), "tenures within 5% of their crosswalk area",
      sprintf("(range %.3f-%.3f x)\n", min(chk$ratio), max(chk$ratio)))
}

cat("wrote", nrow(mem), "member polygons /", nrow(grp), "groups\n")
cat("total:", sum(tab$area_km2), "km2,", sum(tab$n_members), "member tenures\n")
