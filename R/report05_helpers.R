## Figures and text helpers for report 05 (reports/05-study-area-report.qmd), which sources this file.
## The figures follow NRV_summary's own: green boxes for the simulated range, the current condition
## as a hollow red circle (a solid dot hid simulated values at the same place) or a red line.

## a proportion as a whole percent
report05_pct <- function(x) sprintf("%.0f", 100 * x)

## hectares, with thousands separators
report05_ha <- function(x) formatC(round(x), big.mark = ",", format = "d")

## a project-relative path (memory: never show absolute NFS paths in a report)
report05_relpath <- function(x) sub("^.*?/(outputs|inputs)/", "\\1/", x, perl = TRUE)

## reporting vegetation-type codes (sppEquiv `LandWebReport`) -> report labels
report05_veg_label <- function(x) {
  lab <- c(
    `All species` = "All forest",
    Bl_Spruce = "Black spruce",
    Decid = "Deciduous",
    Fir = "Fir",
    Mixed = "Mixedwood",
    Pine = "Pine",
    Wh_Spruce = "White spruce"
  )
  out <- unname(lab[x])
  out[is.na(out)] <- gsub("_", " ", x[is.na(out)])
  out
}

## where the current condition sits in the simulated range, in words: outside it, against its bounds;
## inside it, as its (mid-rank) percentile among the simulated values
report05_position_phrase <- function(position, min, max, cc_rank, pct = report05_pct) {
  data.table::fcase(
    position == "below" , sprintf("below the simulated range (minimum %s%%)", pct(min))                             ,
    position == "above" , sprintf("above the simulated range (maximum %s%%)", pct(max))                             ,
    !is.na(cc_rank)     , sprintf("at the %s percentile of the simulated values", report05_ordinal(round(cc_rank))) ,
    default = "not available"
  )
}

## short form of the position, for tables
report05_position_short <- function(position) {
  unname(c(
    below = "below range",
    low = "lower quarter",
    mid = "middle half",
    high = "upper quarter",
    above = "above range"
  )[position])
}

## age classes as "Young (< 40 years)", ... from the lower bounds
report05_age_class_labels <- function(classes, cutoffs) {
  n <- length(classes)
  lab <- character(n)
  for (i in seq_len(n)) {
    lab[i] <- if (i == 1L) {
      sprintf("%s (under %d years)", classes[i], cutoffs[i + 1L])
    } else if (i == n) {
      sprintf("%s (%d years and older)", classes[i], cutoffs[i])
    } else {
      sprintf("%s (%d to %d years)", classes[i], cutoffs[i], cutoffs[i + 1L])
    }
  }
  lab
}

#' Box plots of the simulated age-class shares, current condition as red circles
#'
#' @param nrv,cc `data.table`s with `panel` (facet label), `ageClass` and `value` (a proportion).
#' @param age_classes Age classes, youngest first.
#' @param xlab X-axis label.
#' @param ncol Facet columns.
report05_boxplot <- function(nrv, cc, age_classes, xlab, ncol = 2L) {
  lv <- rev(age_classes)
  nrv <- data.table::copy(nrv)[, ageClass := factor(ageClass, levels = lv)]
  cc <- data.table::copy(cc)[, ageClass := factor(ageClass, levels = lv)]
  ggplot2::ggplot(nrv, ggplot2::aes(x = 100 * value, y = ageClass)) +
    ggplot2::geom_boxplot(fill = "limegreen", outlier.size = 0.6, linewidth = 0.3) +
    ggplot2::geom_point(
      data = cc,
      ggplot2::aes(x = 100 * value, y = ageClass),
      shape = 21,
      colour = "red",
      fill = NA,
      stroke = 1.1,
      size = 2.6
    ) +
    ggplot2::facet_wrap(~panel, ncol = ncol) +
    ggplot2::coord_cartesian(xlim = c(0, 100)) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_bw(base_size = 9)
}

#' Histograms of the simulated old-forest area in large patches, current condition as a red line
#'
#' @param nrv,cc `data.table`s with `min_patch_ha` and `value` (ha).
report05_patch_hist <- function(nrv, cc) {
  sizes <- sort(unique(nrv$min_patch_ha))
  lab <- function(s) {
    factor(
      sprintf("Patches of %s ha or more", report05_ha(s)),
      levels = sprintf("Patches of %s ha or more", report05_ha(sizes))
    )
  }
  nrv <- data.table::copy(nrv)[, panel := lab(min_patch_ha)]
  cc <- data.table::copy(cc)[, panel := lab(min_patch_ha)]
  ggplot2::ggplot(nrv, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 20, fill = "grey70", colour = "grey35", linewidth = 0.2) +
    ggplot2::geom_vline(
      data = cc,
      ggplot2::aes(xintercept = value),
      colour = "red",
      linewidth = 0.9
    ) +
    ggplot2::facet_wrap(~panel, ncol = 2, scales = "free") +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x = "Area of old forest in patches of at least this size (ha)",
      y = "Simulated landscapes"
    ) +
    ggplot2::theme_bw(base_size = 9)
}

#' Map of the study-area group and its tenures
#'
#' @param group,tenures `sf` layers (`tenures` with a `label` column).
report05_map <- function(group, tenures) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = tenures, fill = "grey85", colour = "grey30", linewidth = 0.25) +
    ggplot2::geom_sf(data = group, fill = NA, colour = "black", linewidth = 0.5) +
    ggplot2::geom_sf_label(
      data = tenures,
      ggplot2::aes(label = label),
      size = 3,
      label.size = 0.1,
      fun.geometry = sf::st_point_on_surface
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      height = ggplot2::unit(0.15, "cm"),
      text_cex = 0.6
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      style = ggspatial::north_arrow_fancy_orienteering(text_size = 6),
      height = ggplot2::unit(0.9, "cm"),
      width = ggplot2::unit(0.9, "cm")
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 9)
}

#' Map of a tenure's v2 and v3 reporting boundaries and the area between them
#'
#' @param geoms The `geoms` list from [fma_boundary_compare()].
report05_boundary_map <- function(geoms) {
  fills <- c(`Added in v3` = "#0072B2", `Dropped from v2` = "#E69F00")
  gg <- ggplot2::ggplot()
  if (!is.null(geoms$added)) {
    gg <- gg + ggplot2::geom_sf(data = geoms$added, ggplot2::aes(fill = "Added in v3"), colour = NA)
  }
  if (!is.null(geoms$dropped)) {
    gg <- gg +
      ggplot2::geom_sf(data = geoms$dropped, ggplot2::aes(fill = "Dropped from v2"), colour = NA)
  }
  gg +
    ggplot2::geom_sf(data = geoms$v2, ggplot2::aes(linetype = "v2"), fill = NA, colour = "grey20") +
    ggplot2::geom_sf(data = geoms$v3, ggplot2::aes(linetype = "v3"), fill = NA, colour = "black") +
    ggplot2::scale_fill_manual(values = fills, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(v2 = "dashed", v3 = "solid"), name = "Boundary") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(legend.position = "right")
}

## save a figure as a standalone PNG next to the report (repo convention: reference figures get one)
report05_save_png <- function(p, dir, name, width, height) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, paste0(gsub("[^A-Za-z0-9_.-]+", "_", name), ".png"))
  ggplot2::ggsave(f, p, width = width, height = height, dpi = 200)
  invisible(f)
}

## 1 -> "1st", 22 -> "22nd", 13 -> "13th"
report05_ordinal <- function(n) {
  suf <- ifelse(
    n %% 100L %in% 11:13,
    "th",
    c("th", "st", "nd", "rd", rep("th", 6L))[n %% 10L + 1L]
  )
  paste0(n, suf)
}
