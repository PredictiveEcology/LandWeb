## Figures for report 04 (reports/04-output-differences.qmd), which sources this file after
## R/report05_helpers.R (labels, number formats, the boundary map and the PNG writer come from there).
## Version 2 is drawn in blue, version 3 in report 05's green; each version's current condition is a
## hollow red circle or a red line, as in report 05 and the 2021 reports.

## version fills (checked for colour-vision deficiency with the dataviz palette validator)
.report04_fills <- c(v2 = "#2a78d6", v3 = "#32cd32")
.report04_version_labels <- c(v2 = "Version 2", v3 = "Version 3")

.report04_version_scale <- function(aes = "fill") {
  ggplot2::scale_discrete_manual(
    aesthetics = aes,
    values = .report04_fills,
    labels = .report04_version_labels,
    name = NULL,
    drop = FALSE
  )
}

#' Box plots of each version's simulated age-class shares, side by side
#'
#' @param nrv,cc `data.table`s with `panel` (facet label), `ageClass`, `version` and `value` (a
#'   proportion).
#' @param age_classes Age classes, youngest first.
#' @param xlab X-axis label.
#' @param ncol Facet columns.
report04_boxplot <- function(nrv, cc, age_classes, xlab, ncol = 2L) {
  lv <- rev(age_classes)
  prep <- function(d) {
    d <- data.table::copy(d)
    d[, ageClass := factor(ageClass, levels = lv)]
    d[, version := factor(version, levels = names(.report04_fills))]
    d
  }
  nrv <- prep(nrv)
  cc <- prep(cc)
  dodge <- ggplot2::position_dodge(width = 0.8, preserve = "single", orientation = "y")
  ggplot2::ggplot(nrv, ggplot2::aes(x = 100 * value, y = ageClass, fill = version)) +
    ggplot2::geom_boxplot(
      position = dodge,
      width = 0.7,
      outlier.size = 0.5,
      linewidth = 0.25,
      orientation = "y"
    ) +
    ggplot2::geom_point(
      data = cc,
      ggplot2::aes(x = 100 * value, y = ageClass, group = version),
      position = dodge,
      shape = 21,
      colour = "red",
      fill = NA,
      stroke = 0.9,
      size = 2,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~panel, ncol = ncol) +
    .report04_version_scale() +
    ggplot2::coord_cartesian(xlim = c(0, 100)) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(legend.position = "top")
}

#' Histograms of each version's simulated old-forest area in large patches
#'
#' One column per minimum patch size, one row per version, sharing the area axis within a column;
#' each version's current condition is a red line.
#'
#' @param nrv,cc `data.table`s with `min_patch_ha`, `version` and `value` (ha).
report04_patch_hist <- function(nrv, cc) {
  sizes <- sort(unique(nrv$min_patch_ha))
  prep <- function(d) {
    d <- data.table::copy(d)
    d[,
      size := factor(
        sprintf("%s ha or more", report05_ha(min_patch_ha)),
        levels = sprintf("%s ha or more", report05_ha(sizes))
      )
    ]
    d[, version := factor(.report04_version_labels[version], levels = .report04_version_labels)]
    d
  }
  nrv <- prep(nrv)
  cc <- prep(cc)
  fills <- stats::setNames(.report04_fills, .report04_version_labels)
  ggplot2::ggplot(nrv, ggplot2::aes(x = value / 1000, fill = version)) +
    ggplot2::geom_histogram(bins = 15, colour = "grey25", linewidth = 0.15) +
    ggplot2::geom_vline(
      data = cc,
      ggplot2::aes(xintercept = value / 1000),
      colour = "red",
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(version ~ size, scales = "free") +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x = "Area of old forest in patches of at least this size (thousand ha)",
      y = "Simulated landscapes"
    ) +
    ggplot2::theme_bw(base_size = 8)
}

#' Change in each tenure's reporting area, v2 to v3
#'
#' @param stats Boundary statistics ([fma_boundary_compare()]'s `stats`, one row per tenure) with a
#'   `name` column.
report04_boundary_bars <- function(stats) {
  d <- data.table::copy(data.table::as.data.table(stats))[!is.na(pct_change)]
  d[, name := .report04_name_axis(name, order_by = pct_change)]
  d[, lab := paste0(report04_signed(pct_change), "%")]
  lo <- min(0, d$pct_change)
  hi <- max(0, d$pct_change)
  pad <- 0.25 * (hi - lo)
  ggplot2::ggplot(d, ggplot2::aes(x = pct_change, y = name)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_col(fill = "grey60", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = lab, hjust = ifelse(round(pct_change, 1) >= 0, -0.15, 1.15)),
      size = 2.3
    ) +
    ggplot2::coord_cartesian(xlim = c(lo - pad, hi + pad)) +
    ggplot2::labs(x = "Change in reporting area (%)", y = NULL) +
    ggplot2::theme_bw(base_size = 8)
}

## a signed number with one decimal, never "-0.0"
report04_signed <- function(x) {
  r <- round(x, 1)
  r[!is.na(r) & r == 0] <- 0
  sprintf("%+.1f", r)
}

## Tenure names as axis labels, without the companies' legal suffixes ("Ltd.", "Inc.") and wrapped:
## in alphabetical order from the top, or with the largest `order_by` at the top.
.report04_name_axis <- function(x, width = 40L, order_by = NULL) {
  short <- gsub(" (Ltd\\.|Inc\\.|Limited|Incorporated)(?=[ ,)]|$)", "", x, perl = TRUE)
  w <- vapply(short, function(n) paste(strwrap(n, width), collapse = "\n"), character(1))
  lv <- if (is.null(order_by)) rev(unique(w[order(x)])) else unique(w[order(order_by)])
  factor(w, levels = lv)
}

#' Median and middle half of each version's simulated values, by tenure
#'
#' @param long Percentiles by version ([report04_compare()]'s `long`), with `name` (tenure label)
#'   and the facet column `facet`.
#' @param scale Multiplier for the values (100 for proportions shown as percent).
#' @param xlab X-axis label.
#' @param ncol Facet columns.
#' @param free_x Whether each facet has its own x axis.
report04_range_plot <- function(long, scale = 100, xlab = "", ncol = 4L, free_x = FALSE) {
  d <- data.table::copy(data.table::as.data.table(long))
  d[, version := factor(version, levels = names(.report04_fills))]
  d[, name := .report04_name_axis(name)]
  dodge <- ggplot2::position_dodge(width = 0.7, orientation = "y")
  ggplot2::ggplot(d, ggplot2::aes(y = name, colour = version, group = version)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = scale * p25, xmax = scale * p75),
      position = dodge,
      linewidth = 0.9
    ) +
    ggplot2::geom_point(ggplot2::aes(x = scale * p50), position = dodge, size = 1.1) +
    ggplot2::geom_point(
      ggplot2::aes(x = scale * cc),
      position = dodge,
      shape = 21,
      colour = "red",
      fill = NA,
      stroke = 0.6,
      size = 1.5
    ) +
    ggplot2::facet_wrap(~facet, ncol = ncol, scales = if (free_x) "free_x" else "fixed") +
    .report04_version_scale("colour") +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(legend.position = "top")
}

#' Departure of the simulated forest composition from the current condition, by version
#'
#' @param delta One row per tenure, vegetation type and version: `name`, `facet` (type label),
#'   `version`, `dep` (simulated median share minus the version's current share, percentage points).
report04_departure_plot <- function(delta) {
  d <- data.table::copy(data.table::as.data.table(delta))
  d[, version := factor(version, levels = names(.report04_fills))]
  d[, name := .report04_name_axis(name)]
  dodge <- ggplot2::position_dodge(width = 0.6, orientation = "y")
  ggplot2::ggplot(d, ggplot2::aes(x = dep, y = name, colour = version)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_point(position = dodge, size = 1.2) +
    ggplot2::facet_wrap(~facet, nrow = 1L) +
    ggplot2::scale_x_continuous(breaks = c(-50, 0, 50)) +
    .report04_version_scale("colour") +
    ggplot2::labs(
      x = "Simulated median minus current share of forest (percentage points)",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(legend.position = "top")
}

#' Version 3's simulated forest composition against both versions' current conditions
#'
#' @param d One row per tenure and vegetation type: `name`, `facet` (type label), `p25_v3`, `p50_v3`,
#'   `p75_v3`, `cc_v3`, `cc_v2` (proportions).
report04_yardstick_plot <- function(d) {
  d <- data.table::copy(data.table::as.data.table(d))
  d[, name := .report04_name_axis(name)]
  ggplot2::ggplot(d, ggplot2::aes(y = name)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = 100 * p25_v3, xmax = 100 * p75_v3),
      colour = .report04_fills[["v3"]],
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * p50_v3),
      colour = .report04_fills[["v3"]],
      size = 1.1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * cc_v3, shape = "Version 3 current condition"),
      colour = "red",
      fill = NA,
      stroke = 0.6,
      size = 1.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * cc_v2, shape = "Version 2 current condition"),
      colour = .report04_fills[["v2"]],
      fill = NA,
      stroke = 0.6,
      size = 1.6
    ) +
    ggplot2::scale_shape_manual(
      values = c(`Version 3 current condition` = 21, `Version 2 current condition` = 23),
      name = NULL
    ) +
    ggplot2::facet_wrap(~facet, nrow = 1L) +
    ggplot2::scale_x_continuous(breaks = c(0, 50, 100)) +
    ggplot2::labs(x = "Share of forest (%)", y = NULL) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(legend.position = "top")
}
