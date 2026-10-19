## Uncertainty of the shared growth-curve fit: for each species and growth trait, how the bootstrap
## refits (target growthTraitsResampled) compare with the fit to all plots (growthTraits). A
## project helper for now: as part of LandWebUtils, any change to it would re-run every refit,
## because the refit stage fingerprints that package. Move it there with the next refit re-run.

## @param resampled data.frame of LandWebUtils::landweb_growth_traits() rows from each refit, with
##   the refit's number in `resample`.
## @param fitted LandWebUtils::landweb_growth_traits() of the fit to all plots.
## @return data.table, one row per species and trait: `full` (the fit to all plots) and its
##   `fullSource`; `refits`, and `fittedShare`, the share of refits that fitted the species rather
##   than give it group means; over the refits that fitted it, `q05`, `median`, `q95`, the number
##   of `distinct` values, and `sameAsFull`, the share equal to the full fit's value.
growth_trait_spread <- function(resampled, fitted) {
  traits <- c("growthcurve", "mortalityshape", "mANPPproportion", "inflationFactor", "longevity")
  rs <- data.table::as.data.table(resampled)
  ft <- data.table::as.data.table(fitted)
  rows <- list()
  for (sp in sort(unique(ft[["species"]]))) {
    r <- rs[rs[["species"]] == sp]
    f <- ft[ft[["species"]] == sp]
    est <- r[r[["growthCurveSource"]] == "estimated"]
    for (tr in traits) {
      v <- as.numeric(est[[tr]])
      full <- as.numeric(f[[tr]])
      rows[[length(rows) + 1L]] <- data.table::data.table(
        species = sp,
        trait = tr,
        full = full,
        fullSource = f[["growthCurveSource"]],
        refits = nrow(r),
        fittedShare = nrow(est) / nrow(r),
        q05 = if (length(v)) stats::quantile(v, 0.05, names = FALSE) else NA_real_,
        median = if (length(v)) stats::median(v) else NA_real_,
        q95 = if (length(v)) stats::quantile(v, 0.95, names = FALSE) else NA_real_,
        distinct = length(unique(v)),
        sameAsFull = if (length(v)) mean(v == full) else NA_real_
      )
    }
  }
  data.table::rbindlist(rows)
}
