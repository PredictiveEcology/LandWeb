## Concept figures for LandWeb talks and the manual. Synthetic data only: no model outputs.
##
## Writes PNG + SVG to fig_dir() (default manual/figures):
##   nrv_concept           -- 15 simulated runs fluctuating inside a bounded range; today's value
##   nrv_concept_compare   -- the same range with three "today" values: above, within, below
##   model_schematic       -- two landscape layers (year t, t + dt): fire, seed dispersal, cohorts
##   reporting_overlap     -- a management unit + a caribou range = their overlap
##   boxplot_key           -- how to read a LandWeb boxplot
##
## Built from scratch for v3; the v2 schematic was adapted from the LANDIS-II manual.
## Run from the project root: Rscript-4.6.1 scripts/figures/concept_figures.R

if (!exists("theme_landweb")) {
  source("scripts/figures/theme.R")
}

out <- fig_dir()
col <- landweb_colours
DELTA <- "\u0394"
TIMES <- "\u00d7"

## ---- NRV envelope ------------------------------------------------------------------------
## A mean-reverting series with occasional large fire years: bounded, never constant.
set.seed(20261014)
years <- seq(0, 1000, by = 10)
sim_rep <- function(i) {
  x <- numeric(length(years))
  x[1] <- 32 + stats::rnorm(1, 0, 8)
  for (t in seq_along(years)[-1]) {
    x[t] <- 32 + 0.85 * (x[t - 1] - 32) + stats::rnorm(1, 0, 5)
    if (stats::runif(1) < 0.04) x[t] <- x[t] * stats::runif(1, 0.4, 0.7)
  }
  data.frame(rep = i, year = years, old = pmin(pmax(x, 0), 100))
}
runs <- do.call(rbind, lapply(seq_len(15), sim_rep))
q <- stats::quantile(runs$old, c(0.05, 0.25, 0.5, 0.75, 0.95))

lab <- function(
  x,
  y,
  text,
  hjust = 0,
  size = 15,
  colour = col[["ink"]],
  face = "plain",
  vjust = 0.5
) {
  ggplot2::annotate(
    "text",
    x = x,
    y = y,
    label = text,
    hjust = hjust,
    family = "Carlito",
    size = size / ggplot2::.pt,
    colour = colour,
    fontface = face,
    vjust = vjust,
    lineheight = 0.9
  )
}

nrv_base <- function(today, x_max = 1250) {
  xmax <- max(years)
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = xmax,
      ymin = q[[1]],
      ymax = q[[5]],
      fill = col[["range"]],
      alpha = 0.55
    ) +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = xmax,
      ymin = q[[2]],
      ymax = q[[4]],
      fill = col[["range"]],
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      data = runs,
      ggplot2::aes(year, old, group = rep),
      colour = col[["muted"]],
      alpha = 0.35,
      linewidth = 0.3
    ) +
    ggplot2::geom_line(
      data = runs[runs$rep == 3, ],
      ggplot2::aes(year, old),
      colour = col[["olive"]],
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = today,
      ggplot2::aes(x, y),
      shape = 21,
      size = 5.5,
      stroke = 1.2,
      fill = col[["today"]],
      colour = "white"
    ) +
    ggplot2::geom_text(
      data = today,
      ggplot2::aes(x + 22, y, label = label),
      hjust = 0,
      family = "Carlito",
      size = 16 / ggplot2::.pt,
      colour = col[["ink"]]
    ) +
    ggplot2::scale_x_continuous(
      "Simulated years",
      breaks = seq(0, 1000, 200),
      expand = ggplot2::expansion(mult = c(0.01, 0))
    ) +
    ggplot2::scale_y_continuous(
      "Old forest (% of forest area)",
      limits = c(0, 75),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), clip = "off") +
    theme_landweb() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

p <- nrv_base(data.frame(x = 1060, y = q[[5]] + 14, label = "Today")) +
  lab(1030, mean(q[c(1, 5)]), "Natural range\nof variation", size = 16) +
  lab(
    15,
    72,
    "Each grey line is one simulated run; one is highlighted",
    size = 13,
    colour = col[["muted"]]
  )
save_figure(p, "nrv_concept", width = 12.3, height = 5.2, dir = out, svg = TRUE)

cmp <- data.frame(
  x = 1060,
  y = c(q[[5]] + 12, q[[3]], max(q[[1]] - 8, 2)),
  label = c("Above the range", "Within the range", "Below the range")
)
p <- nrv_base(cmp, x_max = 1560)
save_figure(p, "nrv_concept_compare", width = 8.2, height = 5, dir = out, svg = TRUE)

## ---- two-layer model schematic ----------------------------------------------------------------
## Oblique projection of an nx x ny grid; layer z is a vertical offset.
nx <- 14
ny <- 6
shear <- 0.55
squash <- 0.42
iso <- function(x, y, z) data.frame(X = x + shear * y, Y = squash * y + z)
z_top <- 5.2
z_bot <- 0

set.seed(7)
ages <- c("Young", "Immature", "Mature", "Old")
grid <- expand.grid(i = seq_len(nx), j = seq_len(ny))
grid$top <- sample(ages, nrow(grid), replace = TRUE, prob = c(0.1, 0.25, 0.35, 0.3))
grid$bot <- grid$top

fire <- (grid$i %in% 2:4 & grid$j %in% 2:4) |
  (grid$i == 5 & grid$j %in% 2:3) |
  (grid$i == 3 & grid$j == 5) |
  (grid$i == 2 & grid$j == 1)
grid$top[fire] <- "Burning"
grid$bot[fire] <- "Young"

seed_i <- 8
seed_j <- 3
seed <- grid$i == seed_i & grid$j == seed_j
grid$top[seed] <- "Seed source"
grid$bot[seed] <- "Seed source"
seedlings <- abs(grid$i - seed_i) <= 1 &
  abs(grid$j - seed_j) <= 1 &
  !seed &
  !(grid$i == 9 & grid$j == 2)
grid$bot[seedlings] <- "Seedlings"

cell_i <- 12
cell_j <- 3
cohort <- grid$i == cell_i & grid$j == cell_j
grid$top[cohort] <- "Immature"
grid$bot[cohort] <- "Mature"

cells <- function(state, z) {
  do.call(
    rbind,
    lapply(seq_len(nrow(grid)), function(k) {
      i <- grid$i[k]
      j <- grid$j[k]
      p <- iso(c(i - 1, i, i, i - 1), c(j - 1, j - 1, j, j), z)
      p$id <- paste(k, z)
      p$state <- state[k]
      p
    })
  )
}
slab <- function(z, th = 0.3) {
  front <- iso(c(0, nx, nx, 0), c(0, 0, 0, 0), z)
  front$Y <- front$Y - c(0, 0, th, th)
  side <- iso(c(nx, nx, nx, nx), c(0, ny, ny, 0), z)
  side$Y <- side$Y - c(0, 0, th, th)
  rbind(transform(front, id = paste("f", z)), transform(side, id = paste("s", z)))
}
centre <- function(i, j, z) iso(i - 0.5, j - 0.5, z)
outline <- function(i, j, z, id) {
  transform(iso(c(i - 1, i, i, i - 1), c(j - 1, j - 1, j, j), z), id = id)
}

state_fill <- c(
  stats::setNames(landweb_age$colour, landweb_age$class),
  Burning = col[["rust"]],
  `Seed source` = "#1B8A5A",
  Seedlings = "#8CCBA8"
)
cl <- rbind(cells(grid$top, z_top), cells(grid$bot, z_bot))
sl <- rbind(slab(z_top), slab(z_bot))
hl <- rbind(outline(cell_i, cell_j, z_top, "a"), outline(cell_i, cell_j, z_bot, "b"))

fire_c <- centre(3.5, 3, c(z_top, z_bot))
seed_c <- centre(seed_i, seed_j, c(z_top, z_bot))
cell_c <- centre(cell_i, cell_j, c(z_top, z_bot))
drop <- function(cc, text, colour, dx = 0.25) {
  y0 <- cc$Y[1] - 0.55
  y1 <- cc$Y[2] + squash * ny * 0.5 + 0.9
  list(
    ggplot2::annotate(
      "segment",
      x = cc$X[1],
      xend = cc$X[2],
      y = y0,
      yend = y1,
      colour = colour,
      linewidth = 1.3,
      arrow = grid::arrow(length = grid::unit(0.14, "in"), type = "closed")
    ),
    lab(cc$X[1] + dx, mean(c(y0, y1)), text)
  )
}
rays <- do.call(
  rbind,
  lapply(seq(0, 2 * pi, length.out = 9)[-9], function(a) {
    s <- centre(seed_i, seed_j, z_top)
    e <- iso(seed_i - 0.5 + 1.25 * cos(a), seed_j - 0.5 + 1.25 * sin(a), z_top)
    data.frame(x = s$X, y = s$Y, xend = e$X, yend = e$Y)
  })
)
y_above <- z_top + squash * ny + 0.3
y_below <- z_bot - 0.55

p <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = sl, ggplot2::aes(X, Y, group = id), fill = "#C5CBD2") +
  ggplot2::geom_polygon(
    data = cl,
    ggplot2::aes(X, Y, group = id, fill = state),
    colour = "white",
    linewidth = 0.5
  ) +
  ggplot2::geom_polygon(
    data = hl,
    ggplot2::aes(X, Y, group = id),
    fill = NA,
    colour = col[["navy"]],
    linewidth = 1.3
  ) +
  ggplot2::geom_segment(
    data = rays,
    ggplot2::aes(x, y, xend = xend, yend = yend),
    colour = "white",
    linewidth = 0.7,
    arrow = grid::arrow(length = grid::unit(0.06, "in"))
  ) +
  drop(fire_c, "Fire", col[["rust"]]) +
  drop(seed_c, "Seed\ndispersal", "#1B8A5A") +
  drop(cell_c, "Growth &\nmortality", col[["navy"]]) +
  lab(-0.3, z_top + squash * ny / 2, "Year t", hjust = 1, face = "bold") +
  lab(-0.3, z_bot + squash * ny / 2, paste0("Year t + ", DELTA, "t"), hjust = 1, face = "bold") +
  lab(3.5 + shear * ny, y_above, "Burning", hjust = 0.5, vjust = 0) +
  lab(3.5, y_below, "Regenerating", hjust = 0.5, vjust = 1) +
  lab(seed_i - 0.5 + shear * ny, y_above, "Seed source", hjust = 0.5, vjust = 0) +
  lab(seed_i - 0.5, y_below, "Seedlings\nestablish", hjust = 0.5, vjust = 1) +
  lab(cell_i - 0.5 + shear * ny, y_above, "One cell:\ntree cohorts", hjust = 0.5, vjust = 0) +
  lab(cell_i - 0.5, y_below, "Cohorts\nage", hjust = 0.5, vjust = 1) +
  ggplot2::geom_tile(
    data = data.frame(x = -3.1 + 0.42 * (0:3), y = z_bot - 0.95, f = landweb_age$class),
    ggplot2::aes(x, y, fill = f),
    width = 0.4,
    height = 0.32,
    colour = "white"
  ) +
  lab(-3.3, z_bot - 0.4, "Forest age", size = 13, colour = col[["muted"]], vjust = 0) +
  lab(-3.4, z_bot - 0.95, "young", hjust = 1, size = 12, colour = col[["muted"]]) +
  lab(-1.55, z_bot - 0.95, "old", size = 12, colour = col[["muted"]]) +
  ggplot2::scale_fill_manual(values = state_fill, guide = "none") +
  ggplot2::coord_fixed(clip = "off") +
  theme_landweb_void() +
  ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 120))
save_figure(p, "model_schematic", width = 8.4, height = 4.2, dir = out, svg = TRUE)

## ---- reporting-unit overlap -------------------------------------------------------------------
blob <- function(cx, cy, r, seed_val, aspect = 0.8, n = 90) {
  set.seed(seed_val)
  th <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  ph <- stats::runif(2, 0, 2 * pi)
  k <- 1 + 0.12 * sin(3 * th + ph[1]) + 0.07 * sin(5 * th + ph[2])
  xy <- cbind(cx + r * k * cos(th), cy + aspect * r * k * sin(th))
  sf::st_polygon(list(rbind(xy, xy[1, ])))
}
unit_a <- blob(0, 0, 1, 11)
range_1 <- blob(0.95, 0.45, 0.85, 23, aspect = 0.75)
both <- sf::st_intersection(unit_a, range_1)
shift <- function(g, dx) g + c(dx, 0)
gap <- 3.4
unit_col <- col[["navy"]]
range_col <- "#3C8D93"
sfc <- function(g) sf::st_sfc(g)
p <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = sfc(unit_a),
    fill = unit_col,
    alpha = 0.22,
    colour = unit_col,
    linewidth = 1
  ) +
  ggplot2::geom_sf(
    data = sfc(shift(range_1, gap - 0.95)),
    fill = range_col,
    alpha = 0.22,
    colour = range_col,
    linewidth = 1
  ) +
  ggplot2::geom_sf(
    data = sfc(shift(unit_a, 2 * gap)),
    fill = NA,
    colour = unit_col,
    linewidth = 0.6,
    linetype = "22"
  ) +
  ggplot2::geom_sf(
    data = sfc(shift(range_1, 2 * gap)),
    fill = NA,
    colour = range_col,
    linewidth = 0.6,
    linetype = "22"
  ) +
  ggplot2::geom_sf(
    data = sfc(shift(both, 2 * gap)),
    fill = unit_col,
    alpha = 0.85,
    colour = unit_col,
    linewidth = 1
  ) +
  lab(gap / 2 + 0.05, 0.1, "+", hjust = 0.5, size = 40, colour = col[["muted"]]) +
  lab(1.5 * gap + 0.35, 0.1, "=", hjust = 0.5, size = 40, colour = col[["muted"]]) +
  lab(0, -1.3, "FMA A", hjust = 0.5, size = 18, face = "bold") +
  lab(gap, -1.3, "Caribou range 1", hjust = 0.5, size = 18, face = "bold") +
  lab(
    2 * gap + 0.45,
    -1.3,
    paste("FMA A", TIMES, "Caribou range 1"),
    hjust = 0.5,
    size = 18,
    face = "bold"
  ) +
  lab(0, -1.75, "management unit", hjust = 0.5, colour = col[["muted"]]) +
  lab(gap, -1.75, "reporting layer", hjust = 0.5, colour = col[["muted"]]) +
  lab(
    2 * gap + 0.45,
    -1.75,
    "their overlap: its own reporting area",
    hjust = 0.5,
    colour = col[["muted"]]
  ) +
  ggplot2::coord_sf(
    clip = "off",
    expand = FALSE,
    xlim = c(-1.3, 2 * gap + 2.1),
    ylim = c(-1.95, 1.2)
  ) +
  theme_landweb_void()
save_figure(p, "reporting_overlap", width = 11.8, height = 3.9, dir = out, svg = TRUE)

## ---- boxplot key ------------------------------------------------------------------------------
## Same marks as the data boxplots in scripts/figures/nrv_output_figures.R.
bx <- list(lo = 0.08, q1 = 0.22, med = 0.33, q3 = 0.47, hi = 0.72, out = c(0.8, 0.86))
today <- 0.62
lead_line <- function(x, xend, y, yend) {
  ggplot2::annotate(
    "segment",
    x = x,
    xend = xend,
    y = y,
    yend = yend,
    colour = col[["muted"]],
    linewidth = 0.4
  )
}
p <- ggplot2::ggplot() +
  ggplot2::annotate(
    "segment",
    x = bx$lo,
    xend = bx$q1,
    y = 0,
    yend = 0,
    colour = col[["olive"]],
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "segment",
    x = bx$q3,
    xend = bx$hi,
    y = 0,
    yend = 0,
    colour = col[["olive"]],
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "rect",
    xmin = bx$q1,
    xmax = bx$q3,
    ymin = -0.28,
    ymax = 0.28,
    fill = col[["range"]],
    colour = col[["olive"]],
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "segment",
    x = bx$med,
    xend = bx$med,
    y = -0.28,
    yend = 0.28,
    colour = "#3A4210",
    linewidth = 1.6
  ) +
  ggplot2::annotate("point", x = bx$out, y = 0, colour = col[["olive"]], size = 2) +
  ggplot2::annotate(
    "point",
    x = today,
    y = 0,
    shape = 21,
    size = 5.5,
    stroke = 1.2,
    fill = col[["today"]],
    colour = "white"
  ) +
  lead_line((bx$q1 + bx$q3) / 2, (bx$q1 + bx$q3) / 2, 0.3, 0.55) +
  lab(
    (bx$q1 + bx$q3) / 2,
    0.75,
    "Box: the middle half\nof the 105 simulated\nlandscapes",
    hjust = 0.5
  ) +
  lead_line(bx$med, 0.1, -0.3, -0.62) +
  lab(0.1, -0.74, "Line: median", hjust = 0.5) +
  lead_line(0.69, 0.82, 0.02, 0.42) +
  lab(0.86, 0.62, "Whiskers: the rest\nof the usual range", hjust = 0.5) +
  lead_line(0.83, 0.9, -0.05, -0.5) +
  lab(0.92, -0.66, "Dots: unusual\nsnapshots", hjust = 0.5) +
  lead_line(today, 0.5, -0.08, -0.9) +
  lab(0.5, -1.02, "Today's forest", hjust = 0.5, face = "bold") +
  ggplot2::coord_cartesian(xlim = c(-0.02, 1.02), ylim = c(-1.2, 1.05), clip = "off") +
  theme_landweb_void()
save_figure(p, "boxplot_key", width = 4.6, height = 3.7, dir = out, svg = TRUE)
