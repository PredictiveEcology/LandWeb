## Figures from one study area's simulation outputs, for talks on reading LandWeb results.
##
## Reporting units are anonymised: the example unit is shown as "FMA A". Which unit that is comes
## from LANDWEB_EXAMPLE_POLY at run time and is deliberately not recorded here.
##
## Environment (all optional):
##   LANDWEB_SA            study area                                [WesternAlbertaUpland]
##   LANDWEB_REP_DIR       one replicate's mainSim output directory  [outputs/<sa>/mainSim/rep01]
##   LANDWEB_AGG_DIR       NRV_summary per-replicate aggregates      [outputs/<sa>/postprocess/_aggregates]
##   LANDWEB_LAYER         reporting layer for the example unit      [FMA]
##   LANDWEB_EXAMPLE_POLY  reporting unit to show as "FMA A"          [first unit passing the rule below]
##   LANDWEB_FIG_DIR       output directory                          [outputs/_reference/nrv_output_figures]
##   LANDWEB_SKIP_ANIM     "true" skips the 50-frame animation       [false]
##
## Writes (PNG unless noted):
##   veg_transitions            leading-type flows, year 0 -> burn-in -> the 7 sampling years (one rep)
##   landscape_animation.gif    stand age + leading type, years 601-650 (one rep); frame 1 as PNG too
##   build_thumb_<year>         stand-age thumbnails at the sampling years (one rep)
##   build_dots, build_box      old forest in FMA A: the 105 snapshot values, then their boxplot
##   boxplot_example            FMA A, black spruce-leading forest by age class, with today's value
##   boxplot_small_multiples    FMA A, all leading types
##   largepatch_hist            FMA A, number of old-forest patches >= 100/500/1,000/5,000 ha
##   thumb_boxplot, thumb_histogram   text-free miniatures of the two plot types
##
## Run on a compute node from the project root: Rscript-4.6.1 scripts/figures/nrv_output_figures.R

if (!exists("theme_landweb")) {
  source("scripts/figures/theme.R")
}

sa <- Sys.getenv("LANDWEB_SA", "WesternAlbertaUpland")
rep_dir <- Sys.getenv("LANDWEB_REP_DIR", file.path("outputs", sa, "mainSim", "rep01"))
agg_dir <- Sys.getenv("LANDWEB_AGG_DIR", file.path("outputs", sa, "postprocess", "_aggregates"))
layer <- Sys.getenv("LANDWEB_LAYER", "FMA")
out <- fig_dir(file.path("outputs", "_reference", "nrv_output_figures"))
col <- landweb_colours
caption <- "Preliminary LandWeb v3 results"
pixel_km2 <- 0.24^2 ## 240 m pixels

## ---- vegetation transitions (T1) ----------------------------------------------------------------
vt <- data.table::as.data.table(dplyr::collect(arrow::open_dataset(file.path(
  rep_dir,
  "vegetation-transitions"
))))
vt <- vt[!is.na(vegType)]
times <- sort(unique(vt$time))
xpos <- stats::setNames(c(0, seq(2, by = 1, length.out = length(times) - 1)), times)
types <- landweb_leading$code[landweb_leading$code %in% unique(vt$vegType)]
bar_w <- 0.3

stacks <- vt[, .N, by = .(time, vegType)]
stacks[, vegType := factor(vegType, levels = types)]
data.table::setorder(stacks, time, vegType)
stacks[, `:=`(area = N * pixel_km2 / 1000, x = xpos[as.character(time)])]
stacks[, `:=`(ymax = cumsum(area)), by = time]
stacks[, ymin := ymax - area]

sigmoid_ribbon <- function(x0, x1, b0, t0, b1, t1, n = 40) {
  s <- (1 - cos(pi * seq(0, 1, length.out = n))) / 2
  xs <- x0 + (x1 - x0) * seq(0, 1, length.out = n)
  data.frame(x = c(xs, rev(xs)), y = c(b0 + (b1 - b0) * s, rev(t0 + (t1 - t0) * s)))
}
flows <- list()
for (k in seq_along(times)[-length(times)]) {
  a <- vt[time == times[k], .(pixelID, from = vegType)]
  b <- vt[time == times[k + 1], .(pixelID, to = vegType)]
  f <- merge(a, b, by = "pixelID")[, .N, by = .(from, to)]
  f[, `:=`(from = factor(from, levels = types), to = factor(to, levels = types))]
  f[, area := N * pixel_km2 / 1000]
  ## outgoing offsets within each source bar, incoming offsets within each target bar
  data.table::setorder(f, from, to)
  base_from <- stacks[time == times[k], stats::setNames(ymin, vegType)]
  f[, out0 := base_from[as.character(from)] + cumsum(area) - area, by = from]
  data.table::setorder(f, to, from)
  base_to <- stacks[time == times[k + 1], stats::setNames(ymin, vegType)]
  f[, in0 := base_to[as.character(to)] + cumsum(area) - area, by = to]
  x0 <- xpos[[k]] + bar_w / 2
  x1 <- xpos[[k + 1]] - bar_w / 2
  for (r in seq_len(nrow(f))) {
    rb <- sigmoid_ribbon(x0, x1, f$out0[r], f$out0[r] + f$area[r], f$in0[r], f$in0[r] + f$area[r])
    rb$id <- paste(k, r)
    rb$from <- as.character(f$from[r])
    rb$burnin <- k == 1
    flows[[length(flows) + 1]] <- rb
  }
}
flows <- data.table::rbindlist(flows)
flows[, type := leading_label(from)]
stacks[, type := leading_label(as.character(vegType))]
last <- stacks[time == max(times)]
ytop <- max(stacks$ymax)

p <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = flows,
    ggplot2::aes(x, y, group = id, fill = type, alpha = burnin),
    colour = NA
  ) +
  ggplot2::geom_rect(
    data = stacks,
    ggplot2::aes(xmin = x - bar_w / 2, xmax = x + bar_w / 2, ymin = ymin, ymax = ymax, fill = type),
    colour = "white",
    linewidth = 0.3
  ) +
  ggrepel::geom_text_repel(
    data = last,
    ggplot2::aes(x = x + bar_w / 2 + 0.08, y = (ymin + ymax) / 2, label = type),
    hjust = 0,
    direction = "y",
    nudge_x = 0.25,
    segment.colour = col[["muted"]],
    segment.size = 0.3,
    family = "Carlito",
    size = 14 / ggplot2::.pt,
    colour = col[["ink"]],
    min.segment.length = 0
  ) +
  ggplot2::annotate(
    "text",
    x = 1,
    y = ytop * 1.07,
    label = "700-year burn-in",
    family = "Carlito",
    size = 15 / ggplot2::.pt,
    colour = col[["muted"]],
    fontface = "italic"
  ) +
  ggplot2::annotate(
    "segment",
    x = xpos[[2]] - bar_w / 2,
    xend = max(xpos) + bar_w / 2,
    y = ytop * 1.05,
    yend = ytop * 1.05,
    colour = col[["navy"]],
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "text",
    x = mean(xpos[-1]),
    y = ytop * 1.1,
    label = "7 sampling years, every 50 years",
    family = "Carlito",
    size = 15 / ggplot2::.pt,
    colour = col[["navy"]],
    fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(values = leading_colours(), guide = "none") +
  ggplot2::scale_alpha_manual(values = c(`FALSE` = 0.45, `TRUE` = 0.2), guide = "none") +
  ggplot2::scale_x_continuous(
    NULL,
    breaks = xpos,
    labels = ifelse(names(xpos) == "0", "0\n(today)", names(xpos)),
    expand = ggplot2::expansion(add = c(0.3, 1.6))
  ) +
  ggplot2::scale_y_continuous(
    expression(Area ~ (thousand ~ km^2)),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::labs(caption = paste0(caption, "; one simulated run")) +
  theme_landweb() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
save_figure(p, "veg_transitions", width = 12.3, height = 5.1, dir = out)

## ---- shared raster helpers ---------------------------------------------------------------------
age_levels <- c("Burned < 10 y", landweb_age$label)
age_cols <- stats::setNames(c(col[["rust"]], landweb_age$colour), age_levels)
classify_age <- function(r) {
  rcl <- matrix(
    c(-Inf, 10, 1, 10, 40, 2, 40, 80, 3, 80, 120, 4, 120, Inf, 5),
    ncol = 3,
    byrow = TRUE
  )
  terra::classify(r, rcl, right = FALSE)
}
map_df <- function(r, labels) {
  d <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "v"
  d$v <- factor(labels[d$v], levels = labels)
  d
}

## ---- animation frames: stand age + leading type, years 601-650 (T1) ----------------------------
veg_codes <- function(r) {
  ## the vegTypeMap RAT maps integer ids to LandWeb codes; build an id -> display-name lookup
  rat <- terra::cats(r)[[1]]
  stats::setNames(leading_label(as.character(rat[[2]])), rat[[1]])
}
frame_plot <- function(year, fact = 3) {
  a <- terra::rast(file.path(rep_dir, sprintf("standAgeMap_year%04d.tif", year)))
  v <- terra::rast(file.path(rep_dir, sprintf("vegTypeMap_year%04d.tif", year)))
  lut <- veg_codes(v)
  a <- terra::aggregate(classify_age(a), fact = fact, fun = "modal", na.rm = TRUE)
  v <- terra::aggregate(terra::as.int(v), fact = fact, fun = "modal", na.rm = TRUE)
  ## vegTypeMap also covers the study-area buffer; show both panels on the same footprint
  v <- terra::mask(v, a)
  da <- map_df(a, age_levels)
  dv <- terra::as.data.frame(v, xy = TRUE, na.rm = TRUE)
  names(dv)[3] <- "v"
  dv$v <- factor(lut[as.character(dv$v)], levels = landweb_leading$label)
  pa <- ggplot2::ggplot(da, ggplot2::aes(x, y, fill = v)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual("Stand age (years)", values = age_cols, drop = FALSE) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    theme_landweb_void(base_size = 15) +
    ggplot2::theme(legend.position = "right")
  pv <- ggplot2::ggplot(dv, ggplot2::aes(x, y, fill = v)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual("Leading type", values = leading_colours(), drop = TRUE) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    theme_landweb_void(base_size = 15) +
    ggplot2::theme(legend.position = "right")
  patchwork::wrap_plots(pa, pv, nrow = 1) +
    patchwork::plot_annotation(
      title = sprintf("Simulated year %d", year),
      caption = paste0(caption, "; one simulated run"),
      theme = theme_landweb(base_size = 18) +
        ggplot2::theme(plot.title = ggplot2::element_text(colour = col[["navy"]], face = "bold"))
    )
}
if (!identical(tolower(Sys.getenv("LANDWEB_SKIP_ANIM")), "true")) {
  anim_years <- 601:650
  frame_dir <- file.path(out, "animation_frames")
  dir.create(frame_dir, showWarnings = FALSE)
  frames <- parallel::mclapply(
    anim_years,
    function(y) {
      f <- file.path(frame_dir, sprintf("frame_%04d.png", y))
      ggplot2::ggsave(
        f,
        frame_plot(y),
        width = 12,
        height = 5.4,
        dpi = 150,
        device = ragg::agg_png,
        bg = "white"
      )
      f
    },
    mc.cores = 16
  )
  frames <- unlist(frames)
  stopifnot(all(file.exists(frames)))
  file.copy(frames[1], file.path(out, "landscape_animation_frame1.png"), overwrite = TRUE)
  gifski::gifski(
    frames,
    gif_file = file.path(out, "landscape_animation.gif"),
    width = 1800,
    height = 810,
    delay = 0.3,
    loop = TRUE,
    progress = FALSE
  )
  message("wrote ", file.path(out, "landscape_animation.gif"))
}

## ---- stand-age thumbnails at the sampling years (T2 build, step 1) ----------------------------
samp_years <- seq(700, 1000, by = 50)
thumb_window <- function(r, km = 180) {
  e <- terra::ext(r)
  cx <- (e[1] + e[2]) / 2
  cy <- (e[3] + e[4]) / 2
  h <- km * 500
  terra::crop(r, terra::ext(cx - h, cx + h, cy - h, cy + h))
}
for (y in samp_years) {
  r <- terra::rast(file.path(rep_dir, sprintf("rstTimeSinceFire_year%04d.tif", y)))
  r <- thumb_window(classify_age(r))
  p <- ggplot2::ggplot(map_df(r, age_levels), ggplot2::aes(x, y, fill = v)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = age_cols, guide = "none") +
    ggplot2::coord_equal(expand = FALSE) +
    theme_landweb_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  save_figure(p, sprintf("build_thumb_%04d", y), width = 2, height = 2, dir = out)
}

## ---- NRV summaries for the example unit (T2) --------------------------------------------------
read_lw <- function(cc = FALSE) {
  ds <- nrvtools::open_nrv_dataset(file.path(agg_dir, paste0("lw_", layer, if (cc) "_CC")))
  data.table::as.data.table(dplyr::collect(ds))
}
lw <- read_lw()
lw_cc <- read_lw(cc = TRUE)
lw[, class := factor(class, levels = landweb_age$class)]
lw_cc[, class := factor(class, levels = landweb_age$class)]

## Example-unit rule: an Alberta FMA (not a BC TSA) whose black spruce-leading forest has today's
## value outside the whiskers (1.5 x IQR) in at least one age class; most such classes first.
outside <- function(v, today) {
  w <- grDevices::boxplot.stats(v)$stats[c(1, 5)]
  today < w[1] | today > w[2]
}
bs <- lw[metric == "leadingProp" & metric.1 == "Pice_mar"]
bs_cc <- lw_cc[metric == "leadingProp" & metric.1 == "Pice_mar", .(poly, class, today = value)]
cand <- merge(bs, bs_cc, by = c("poly", "class"))[,
  .(n = .N, out = outside(value, today[1])),
  by = .(poly, class)
][, .(n_min = min(n), n_out = sum(out)), by = poly][!grepl("_TSA$", poly)]
data.table::setorder(cand, -n_out, poly)
print(cand)
ex <- Sys.getenv("LANDWEB_EXAMPLE_POLY", cand[n_out > 0 & n_min == max(n_min)]$poly[1])
stopifnot(!is.na(ex), ex %in% lw$poly)
message("example unit chosen (shown as 'FMA A')")

lead <- lw[poly == ex & metric == "leadingProp"]
lead_cc <- lw_cc[poly == ex & metric == "leadingProp"]
lead[, type := factor(leading_label(metric.1), levels = c("All forest", landweb_leading$label))]
lead_cc[, type := factor(leading_label(metric.1), levels = levels(lead$type))]
## a leading type that never occurs in the simulation has only today's value: no panel for it
sim_types <- lead[, .(n = sum(!is.na(value) & value > 0)), by = type][n > 0]$type
lead <- lead[type %in% sim_types]
lead_cc <- lead_cc[type %in% sim_types]
lead[, type := droplevels(type)]
lead_cc[, type := factor(type, levels = levels(lead$type))]

box_layers <- function(d, dcc, point_size = 5) {
  list(
    ggplot2::geom_boxplot(
      data = d,
      ggplot2::aes(x = value, y = class),
      fill = col[["range"]],
      colour = col[["olive"]],
      median.colour = "#3A4210",
      median.linewidth = 1.4,
      outlier.colour = col[["olive"]],
      outlier.size = 1.4,
      width = 0.6,
      staplewidth = 0
    ),
    ggplot2::geom_point(
      data = dcc,
      ggplot2::aes(x = value, y = class),
      shape = 21,
      size = point_size,
      stroke = 1,
      fill = col[["today"]],
      colour = "white"
    )
  )
}

## build steps 2 and 3: old forest (all types) in FMA A -- the 105 values, then their boxplot
old <- lead[type == "All forest" & class == "Old"]
xlim_old <- range(c(0, old$value)) + c(0, 0.02)
set.seed(1)
p <- ggplot2::ggplot(old, ggplot2::aes(value, 0)) +
  ggplot2::geom_jitter(
    height = 0.35,
    width = 0,
    colour = col[["olive"]],
    alpha = 0.75,
    size = 2.4
  ) +
  ggplot2::scale_x_continuous("Old forest (share of forest area)", limits = xlim_old) +
  ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(-0.6, 0.6)) +
  ggplot2::labs(title = sprintf("%d simulated landscapes, FMA A", nrow(old))) +
  theme_landweb(base_size = 15) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, colour = col[["ink"]]))
save_figure(p, "build_dots", width = 4.3, height = 3.2, dir = out)
p <- ggplot2::ggplot(old, ggplot2::aes(value, 0)) +
  ggplot2::geom_boxplot(
    fill = col[["range"]],
    colour = col[["olive"]],
    median.colour = "#3A4210",
    median.linewidth = 1.4,
    outlier.colour = col[["olive"]],
    width = 0.5,
    staplewidth = 0
  ) +
  ggplot2::scale_x_continuous("Old forest (share of forest area)", limits = xlim_old) +
  ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(-0.6, 0.6)) +
  ggplot2::labs(title = "The natural range, FMA A") +
  theme_landweb(base_size = 15) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, colour = col[["ink"]]))
save_figure(p, "build_box", width = 4.3, height = 3.2, dir = out)

## one leading type, annotated on the slide by the boxplot key
d1 <- lead[type == "Black spruce"]
p <- ggplot2::ggplot() +
  box_layers(d1, lead_cc[type == "Black spruce"]) +
  ggplot2::scale_x_continuous("Share of black spruce-leading forest", limits = c(0, 1)) +
  ggplot2::labs(y = NULL, title = "FMA A: black spruce-leading forest", caption = caption) +
  theme_landweb() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(colour = col[["ink"]], face = "bold")
  )
save_figure(p, "boxplot_example", width = 7.4, height = 4.9, dir = out)

p <- ggplot2::ggplot() +
  box_layers(lead, lead_cc, point_size = 3.4) +
  ggplot2::facet_wrap(~type, nrow = 2) +
  ggplot2::scale_x_continuous(
    "Share of each leading type's forest area",
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0", "0.5", "1")
  ) +
  ggplot2::labs(y = NULL, caption = caption) +
  theme_landweb(base_size = 14) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(1.2, "lines")
  )
save_figure(p, "boxplot_small_multiples", width = 12.3, height = 5.2, dir = out)

## ---- large old-forest patches ------------------------------------------------------------------
sizes <- c(100, 500, 1000, 5000)
size_lab <- stats::setNames(
  sprintf("%s+ ha", format(sizes, big.mark = ",", trim = TRUE)),
  sprintf("Npatch_ge%dha", sizes)
)
lp <- lw[poly == ex & class == "Old" & metric.1 == "All species" & metric %in% names(size_lab)]
lp_cc <- lw_cc[
  poly == ex & class == "Old" & metric.1 == "All species" & metric %in% names(size_lab)
]
lp[, size := factor(size_lab[metric], levels = size_lab)]
lp_cc[, size := factor(size_lab[metric], levels = size_lab)]
hist_d <- lp[, .N, by = .(size, value)][, share := N / sum(N), by = size]
p <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = hist_d,
    ggplot2::aes(value, share),
    fill = col[["range"]],
    colour = col[["olive"]],
    width = 0.8
  ) +
  ggplot2::geom_vline(
    data = lp_cc,
    ggplot2::aes(xintercept = value),
    colour = col[["today"]],
    linewidth = 1.4
  ) +
  ggplot2::facet_wrap(~size, nrow = 1, scales = "free_x") +
  ggplot2::scale_x_continuous(
    "Number of old-forest patches",
    breaks = function(l) unique(round(pretty(l)))
  ) +
  ggplot2::scale_y_continuous(
    "Share of simulated landscapes",
    labels = function(v) paste0(round(100 * v), "%"),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  ) +
  ggplot2::labs(
    title = "FMA A: large patches of old forest",
    subtitle = "Bars: the 105 simulated landscapes. Red line: today.",
    caption = caption
  ) +
  theme_landweb() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(colour = col[["ink"]], face = "bold"),
    plot.subtitle = ggplot2::element_text(colour = col[["muted"]])
  )
save_figure(p, "largepatch_hist", width = 12.3, height = 5, dir = out)

## ---- text-free miniatures for the "what is summarized" slide ---------------------------------
mini <- ggplot2::theme(
  axis.text = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(6, 6, 6, 6),
  panel.border = ggplot2::element_rect(fill = NA, colour = col[["grid"]])
)
p <- ggplot2::ggplot() +
  box_layers(d1, lead_cc[type == "Black spruce"], point_size = 3.5) +
  theme_landweb() +
  mini
save_figure(p, "thumb_boxplot", width = 3, height = 2.1, dir = out)
p <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = hist_d[size == size_lab[["Npatch_ge1000ha"]]],
    ggplot2::aes(value, share),
    fill = col[["range"]],
    colour = col[["olive"]],
    width = 0.8
  ) +
  ggplot2::geom_vline(
    data = lp_cc[size == size_lab[["Npatch_ge1000ha"]]],
    ggplot2::aes(xintercept = value),
    colour = col[["today"]],
    linewidth = 1.2
  ) +
  theme_landweb() +
  mini
save_figure(p, "thumb_histogram", width = 3, height = 2.1, dir = out)
