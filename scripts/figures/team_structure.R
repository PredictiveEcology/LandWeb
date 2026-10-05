## Where LandWeb sits: the shared SpaDES modelling framework and its domain teams (top tier), and
## the projects built from it (bottom tier), with LandWeb's own advisory and implementation teams.
##
## Writes PNG + SVG to fig_dir() (default manual/figures):
##   landweb_teams
##
## Content follows the team diagram from the May 2022 LandWeb Users Group meeting, with
## organisations only (no people). Vegetation and fire keep the deck's component colours; the other
## three domains were picked with the dataviz validator. Five classes with olive and rust fixed
## cannot pass every pair (olive vs rust is weak under deuteranopia), so every card is labelled.
## Run from the project root: Rscript-4.6.1 scripts/figures/team_structure.R

if (!exists("theme_landweb")) {
  source("scripts/figures/theme.R")
}

out <- fig_dir()
col <- landweb_colours
tint <- "#F2F4F6"

## domain cards, left to right; orgs as in the 2022 diagram
domains <- data.frame(
  name = c("Vegetation", "Fire", "Wildlife", "Insects", "Carbon"),
  sub = c("", "", "land birds, caribou", "", ""),
  advisory = c("CFS and others", "CFS, Univ. Laval, others", "ECCC and others", "CFS", "CFS"),
  implementation = c("PEG, FOR-CAST", "PEG, FOR-CAST", "PEG", "FOR-CAST, PEG", "PEG, FOR-CAST"),
  colour = c(col[["olive"]], col[["rust"]], "#1F9E96", "#A0559E", "#4B5563")
)

## layout in inches: the canvas is the figure, so 1 unit = 1 inch at the saved size
W <- 12.3
card_w <- 2.2
gap <- (W - 0.45 - 5 * card_w) / 4
domains$x0 <- (seq_len(nrow(domains)) - 1) * (card_w + gap)
domains$x1 <- domains$x0 + card_w
domains$xm <- (domains$x0 + domains$x1) / 2
band <- c(4.1, 4.62) ## framework banner
head_y <- c(3.28, 3.78) ## card header
body_y <- c(2.3, 3.28) ## card body
lw_x <- c(0, domains$x1[2]) ## LandWeb card spans the vegetation and fire columns
lw_head <- c(1.28, 1.78)
lw_body <- c(0.3, 1.28)

txt <- function(
  x,
  y,
  label,
  size = 13,
  colour = col[["ink"]],
  hjust = 0,
  vjust = 0.5,
  face = "plain"
) {
  ggplot2::annotate(
    "text",
    x = x,
    y = y,
    label = label,
    size = size / ggplot2::.pt,
    colour = colour,
    hjust = hjust,
    vjust = vjust,
    fontface = face,
    family = "Carlito",
    lineheight = 0.9
  )
}
box <- function(x0, x1, y0, y1, fill, colour = NA, linetype = "solid") {
  ggplot2::annotate(
    "rect",
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = fill,
    colour = colour,
    linetype = linetype,
    linewidth = 0.6
  )
}
## an "Advisory: ..." / "Implementation: ..." pair, label above value
role <- function(x, y, label, value) {
  list(
    txt(x, y, label, size = 11, colour = col[["muted"]], face = "bold"),
    txt(x, y - 0.19, value, size = 13.5)
  )
}

p <- ggplot2::ggplot() +
  ## framework banner and its links to the domain cards
  box(0, W, band[1], band[2], col[["navy"]]) +
  txt(0.2, mean(band), "SpaDES modelling framework", size = 18, colour = "white", face = "bold") +
  txt(
    W - 0.2,
    mean(band),
    "modules built and maintained by PEG and FOR-CAST",
    size = 14,
    colour = "white",
    hjust = 1
  ) +
  ggplot2::annotate(
    "segment",
    x = domains$xm,
    xend = domains$xm,
    y = band[1],
    yend = head_y[2],
    colour = col[["muted"]],
    linewidth = 0.5
  ) +
  ## domain cards
  box(domains$x0, domains$x1, body_y[1], body_y[2], tint) +
  box(domains$x0, domains$x1, head_y[1], head_y[2], domains$colour) +
  txt(
    domains$x0 + 0.15,
    ifelse(domains$sub == "", mean(head_y), mean(head_y) + 0.08),
    domains$name,
    size = 17,
    colour = "white",
    face = "bold"
  ) +
  txt(
    domains$x0[domains$sub != ""] + 0.15,
    mean(head_y) - 0.13,
    domains$sub[domains$sub != ""],
    size = 11.5,
    colour = "white"
  ) +
  role(domains$x0 + 0.15, body_y[2] - 0.2, "Advisory", domains$advisory) +
  role(domains$x0 + 0.15, body_y[2] - 0.63, "Implementation", domains$implementation) +
  txt(
    W - 0.05,
    mean(c(body_y[1], head_y[2])),
    "...",
    size = 22,
    colour = col[["muted"]],
    hjust = 1
  ) +
  ## modules flow from vegetation and fire into LandWeb
  ggplot2::annotate(
    "segment",
    x = domains$xm[1:2],
    xend = domains$xm[1:2],
    y = body_y[1] - 0.04,
    yend = lw_head[2] + 0.04,
    colour = domains$colour[1:2],
    linewidth = 1.3,
    arrow = grid::arrow(length = grid::unit(0.12, "in"), type = "closed")
  ) +
  txt(
    domains$xm[1:2] + 0.1,
    mean(c(body_y[1], lw_head[2])),
    c("vegetation\nmodules", "fire\nmodules"),
    size = 12
  ) +
  ## LandWeb
  box(lw_x[1], lw_x[2], lw_body[1], lw_body[2], tint) +
  box(lw_x[1], lw_x[2], lw_head[1], lw_head[2], col[["navy"]]) +
  txt(0.15, mean(lw_head), "LandWeb", size = 19, colour = "white", face = "bold") +
  txt(
    lw_x[2] - 0.15,
    mean(lw_head),
    "natural range of variation",
    size = 13,
    colour = "white",
    hjust = 1
  ) +
  role(0.15, lw_body[2] - 0.2, "Advisory", "LandWeb Users Group (LUG)") +
  role(0.15, lw_body[2] - 0.63, "Implementation", "FOR-CAST") +
  ## other projects
  box(lw_x[2] + gap, W, lw_body[1], lw_head[2], "white", colour = col[["muted"]], linetype = "22") +
  txt(
    (lw_x[2] + gap + W) / 2,
    mean(c(lw_body[1], lw_head[2])),
    "Other projects draw on\nother modules from the same framework",
    size = 14,
    colour = col[["muted"]],
    hjust = 0.5
  ) +
  txt(
    W,
    0.06,
    paste(
      "CFS: Canadian Forest Service. ECCC: Environment and Climate Change Canada.",
      "PEG: Predictive Ecology Group."
    ),
    size = 10.5,
    colour = col[["muted"]],
    hjust = 1
  ) +
  ggplot2::coord_fixed(
    xlim = c(-0.05, W + 0.05),
    ylim = c(-0.05, 4.67),
    expand = FALSE,
    clip = "off"
  ) +
  theme_landweb_void() +
  ggplot2::theme(plot.margin = ggplot2::margin(4, 4, 4, 4))
save_figure(p, "landweb_teams", width = W + 0.1, height = 4.72, dir = out, svg = TRUE)
