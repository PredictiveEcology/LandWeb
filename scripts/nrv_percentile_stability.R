## How much report 05's NRV percentiles, and the current condition's place among them, depend on the
## number of replicates. Read by report 07 (reports/07-best-practices.qmd, "Number of replicates").
##
## For each study-area group, the per-replicate values report 05 summarises (target
## report05Data_<sa>: age-class shares within each vegetation type by tenure, and old forest in large
## patches by tenure) are summarised again from subsets of k of the replicates, with report 05's own
## function (report05_percentiles(), R/report_data.R), and compared with the summary of all of them.
## The subsets come from the replicates of one run, so they show how much a result depends on which
## replicates it rests on; they cannot show what replicates beyond those would add.
##
## Per study area, indicator and k (mean over the subsets):
##   pct_position_changed  % of tenure x class rows whose position (below / lower quarter / middle half /
##                         upper quarter / above the simulated range) differs from the full-run position
##   quartile_shift        mean |change| of the 25th and 75th percentiles, as a share of the full-run
##                         range (rows whose range is 0 left out)
##   range_shift           the same for the minimum and maximum
##
## Writes outputs/_extended_analyses/nrv_percentile_stability/{summary.csv, nrv_percentile_stability.png}.
## Light (minutes), but it reads the targets store: run it from the project root on a compute node,
##   Rscript-4.6.1 scripts/nrv_percentile_stability.R [subsets per k, default 200]

source("R/report_data.R")

args <- commandArgs(trailingOnly = TRUE)
n_subsets <- if (length(args)) as.integer(args[[1L]]) else 200L
ks <- c(3L, 5L, 8L, 10L, 12L, 14L)
out_dir <- file.path("outputs", "_extended_analyses", "nrv_percentile_stability")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

meta <- targets::tar_meta(fields = "error")
study_areas <- sort(sub("^report05Data_", "", grep("^report05Data_", meta$name, value = TRUE)))

## the subsets of `reps` of size k: all of them when there are at most `n`, else `n` drawn at random
subsets_of <- function(reps, k, n) {
  if (choose(length(reps), k) <= n) {
    utils::combn(reps, k, simplify = FALSE)
  } else {
    lapply(seq_len(n), function(i) sort(sample(reps, k)))
  }
}

compare_to_full <- function(full, sub, by) {
  cols <- c(by, "position", "min", "p25", "p75", "max")
  m <- merge(
    full[!is.na(position), cols, with = FALSE],
    sub[, cols, with = FALSE],
    by = by,
    all.x = TRUE,
    suffixes = c("", "_k")
  )
  ## both shifts on the same scale, the full-run range, so the quartiles and the extremes compare
  rng <- m$max - m$min
  r <- rng > 0
  shift <- function(lo, hi) {
    mean(
      ((abs(m[[paste0(lo, "_k")]] - m[[lo]]) + abs(m[[paste0(hi, "_k")]] - m[[hi]])) / 2 / rng)[r],
      na.rm = TRUE
    )
  }
  data.table::data.table(
    n_rows = nrow(m),
    changed = 100 * mean(is.na(m$position_k) | m$position_k != m$position),
    quartile_shift = shift("p25", "p75"),
    range_shift = shift("min", "max")
  )
}

one_area <- function(sa) {
  data.table::setDTthreads(1L)
  set.seed(1L)
  rd <- targets::tar_read_raw(paste0("report05Data_", sa))
  sets <- list(
    age_shares = list(nrv = rd$type_nrv, cc = rd$type_cc, by = c("poly", "vegType", "ageClass")),
    old_patches = list(nrv = rd$patch_nrv, cc = rd$patch_cc, by = c("poly", "min_patch_ha"))
  )
  reps <- sort(unique(rd$type_nrv$rep))
  data.table::rbindlist(lapply(names(sets), function(ind) {
    s <- sets[[ind]]
    if (is.null(s$nrv) || !nrow(s$nrv)) {
      return(NULL)
    }
    full <- report05_percentiles(s$nrv, s$cc, s$by)
    data.table::rbindlist(lapply(ks[ks < length(reps)], function(k) {
      subs <- subsets_of(reps, k, n_subsets)
      res <- data.table::rbindlist(lapply(subs, function(r) {
        compare_to_full(full, report05_percentiles(s$nrv[rep %in% r], s$cc, s$by), s$by)
      }))
      data.table::data.table(
        study_area = sa,
        indicator = ind,
        k = k,
        n_reps = length(reps),
        n_snapshots_per_rep = data.table::uniqueN(rd$type_nrv$time),
        n_subsets = length(subs),
        n_rows = res$n_rows[[1L]],
        pct_position_changed = mean(res$changed),
        quartile_shift = mean(res$quartile_shift, na.rm = TRUE),
        range_shift = mean(res$range_shift, na.rm = TRUE)
      )
    }))
  }))
}

res <- parallel::mclapply(study_areas, one_area, mc.cores = min(length(study_areas), 18L))
bad <- vapply(res, inherits, logical(1), what = "try-error")
if (any(bad)) {
  stop(
    "failed for: ",
    paste(study_areas[bad], collapse = ", "),
    "\n",
    paste(unlist(res[bad]), collapse = "\n")
  )
}
summary_dt <- data.table::rbindlist(res)
data.table::fwrite(summary_dt, file.path(out_dir, "summary.csv"))

## standalone copy of report 07's figure (the report draws its own from summary.csv)
lab <- c(
  age_shares = "Age-class shares within vegetation types",
  old_patches = "Old forest in large patches"
)
p <- ggplot2::ggplot(
  summary_dt,
  ggplot2::aes(x = k, y = pct_position_changed, group = study_area)
) +
  ggplot2::geom_line(colour = "grey70", linewidth = 0.4) +
  ggplot2::stat_summary(
    ggplot2::aes(group = 1),
    fun = stats::median,
    geom = "line",
    colour = "black",
    linewidth = 0.9
  ) +
  ggplot2::facet_wrap(~indicator, labeller = ggplot2::as_labeller(lab)) +
  ggplot2::labs(
    x = sprintf("Replicates used (of %d)", max(summary_dt$n_reps)),
    y = "Positions that differ from the full run (%)"
  ) +
  ggplot2::theme_bw()
ggplot2::ggsave(
  file.path(out_dir, "nrv_percentile_stability.png"),
  p,
  width = 8,
  height = 3.8,
  dpi = 200
)

cat("NRV PERCENTILE STABILITY OK:", nrow(summary_dt), "rows,", length(study_areas), "study areas\n")
