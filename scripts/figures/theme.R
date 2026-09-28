## Shared look for LandWeb presentation and manual figures.
##
## Sourced by the other scripts in scripts/figures/. Colours come from the FOR-CAST deck:
## navy for titles, and one colour per model component (data = slate, vegetation = olive,
## fire = rust). The categorical and ordinal palettes below were checked with the dataviz
## palette validator (2026-09-25):
##   - leading types (all 8, incl. Douglas-fir), in stacking order: every adjacent pair passes
##     the CVD (>= 14.4) and normal-vision (>= 15.8) floors. Eight classes cannot pass ALL
##     pairs, so a map of them must carry a legend (pine vs deciduous is the weakest pair under
##     deuteranopia). A brown Douglas-fir (#8C5A3C) was tried and failed: too grey (chroma
##     0.079) and too close to larch orange (normal-vision 14.4).
##   - age classes: one olive hue, lightness monotone, light end 2.14:1 against white.
## Text uses Carlito, which is metric-compatible with the deck's Calibri.

landweb_colours <- c(
  navy = "#1F5081",
  slate = "#375974",
  olive = "#727A35",
  rust = "#954224",
  today = "#E0452B",
  ink = "#1E2833",
  muted = "#5F6B78",
  grid = "#E3E6EA",
  nodata = "#E9EBEE",
  range = "#C9CFA4"
)

## LandWeb species groups -> display names, in the stacking order the palette was validated in.
landweb_leading <- data.frame(
  code = c(
    "Popu_spp",
    "Mixed",
    "Pinu_spp",
    "Pice_gla",
    "Pice_mar",
    "Abie_spp",
    "Lari_spp",
    "Pseu_men"
  ),
  label = c(
    "Deciduous",
    "Mixed",
    "Pine",
    "White spruce",
    "Black spruce",
    "Fir",
    "Larch & tamarack",
    "Douglas-fir"
  ),
  colour = c(
    "#9BBF3B",
    "#7D5BA6",
    "#E0A100",
    "#1B8A5A",
    "#2A6FB0",
    "#19A7A0",
    "#D8602E",
    "#C77CB0"
  )
)

## Every species group LandWeb simulates needs a display name and colour here; a group added to
## LandWebUtils::landweb_species_map() without one would otherwise plot as an unlabelled NA.
local({
  groups <- unique(LandWebUtils::landweb_species_map())
  missing <- setdiff(groups, landweb_leading$code)
  if (length(missing)) {
    stop("landweb_leading lacks species group(s): ", paste(missing, collapse = ", "))
  }
})

## Age classes (lower bounds 0/40/80/120 y, LandWebUtils:::.ageClassCutOffs), young -> old.
landweb_age <- data.frame(
  class = c("Young", "Immature", "Mature", "Old"),
  label = c("Young (< 40)", "Immature (40-80)", "Mature (80-120)", "Old (120+)"),
  colour = c("#ADB85F", "#88933A", "#626C24", "#3A4210")
)

#' Display name for LandWeb species-group codes
#'
#' @param code character vector of codes (`Pice_mar`, ...); `"All species"` maps to
#'   `"All forest"`.
#' @return character vector of display names; unknown codes are returned unchanged.
leading_label <- function(code) {
  out <- landweb_leading$label[match(code, landweb_leading$code)]
  out[code == "All species"] <- "All forest"
  ifelse(is.na(out), code, out)
}

#' Named colour vector for leading types, keyed by display name
leading_colours <- function() {
  stats::setNames(landweb_leading$colour, landweb_leading$label)
}

#' Named colour vector for age classes, keyed by class name
age_colours <- function() {
  stats::setNames(landweb_age$colour, landweb_age$class)
}

#' ggplot2 theme for LandWeb slides and manual figures
#'
#' @param base_size base font size in points. Figures are saved at the size they occupy on a
#'   13.33 x 7.5 in slide, so 16 pt here is 16 pt on the slide.
#' @param base_family font family.
#' @return a ggplot2 theme.
theme_landweb <- function(base_size = 16, base_family = "Carlito") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = landweb_colours[["ink"]]),
      axis.text = ggplot2::element_text(colour = landweb_colours[["muted"]]),
      axis.title = ggplot2::element_text(colour = landweb_colours[["ink"]]),
      panel.grid.major = ggplot2::element_line(colour = landweb_colours[["grid"]], linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        colour = landweb_colours[["ink"]],
        face = "bold",
        hjust = 0,
        size = ggplot2::rel(0.95)
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = landweb_colours[["ink"]]),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = landweb_colours[["muted"]], hjust = 1),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

#' Theme for maps and schematics: no axes, no grid
theme_landweb_void <- function(base_size = 16, base_family = "Carlito") {
  theme_landweb(base_size, base_family) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )
}

#' Where figures are written
#'
#' `LANDWEB_FIG_DIR` overrides the default, which lets a compute node (whose checkout has no
#' copy of in-progress scripts) write to shared storage for copying back.
#'
#' @param default directory used when `LANDWEB_FIG_DIR` is unset.
fig_dir <- function(default = "manual/figures") {
  d <- Sys.getenv("LANDWEB_FIG_DIR", default)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Save a figure as PNG (ragg) and optionally SVG
#'
#' @param plot a ggplot.
#' @param name file name without extension.
#' @param width,height size in inches: the size the figure occupies on the slide.
#' @param dir output directory.
#' @param svg also write an SVG (for concept diagrams that get edited or rescaled).
#' @param dpi PNG resolution.
#' @return the PNG path, invisibly.
save_figure <- function(plot, name, width, height, dir = fig_dir(), svg = FALSE, dpi = 300) {
  png <- file.path(dir, paste0(name, ".png"))
  ggplot2::ggsave(
    png,
    plot,
    width = width,
    height = height,
    dpi = dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  if (svg) {
    ggplot2::ggsave(
      file.path(dir, paste0(name, ".svg")),
      plot,
      width = width,
      height = height,
      device = svglite::svglite,
      bg = "white"
    )
  }
  message("wrote ", png)
  invisible(png)
}
