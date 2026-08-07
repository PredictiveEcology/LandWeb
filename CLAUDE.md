# LandWeb — repository conventions

Guidance for contributors and coding agents working in this repo. Keep changes consistent
with the conventions below.

## Output directory layout — **one directory per run, keyed by study area**

All simulation outputs are organized so that **multiple study areas never collide** and a human
can easily review a run's files (especially figures). This mirrors the historical `outputs_v2/`
one-dir-per-run layout.

```
outputs/
  <studyArea>/          # e.g. SprayLake/ — ALL study-area-specific outputs for a run
    preamble/           # data-prep stages, each with figures/<module>/
    speciesData/        #   (incl. the SCANFI_*.tif species layers)
    dataPrep/
    mainSim/            # simulation + mode="multi" summaries share this dir
      rep01/ … repNN/   # per-replicate sim outputs + figures/
      figures/{Biomass_core,LandMine,burnSummaries,NRV_summary}/   # aggregate + summary figures
      _aggregates/      # per-refCode parquet (NRV envelopes)
      *.csv             # envelope + fire-summary tables
    reports/            # rendered Quarto reports for this study area
  _factorial/           # SHARED, study-area-INDEPENDENT — DO NOT nest per study area
  _extended_analyses/   # SHARED, LTHFC domain-wide analyses
  _reference/           # shared reference layers
logs/                   # run + crew worker logs — NEVER put logs in outputs/
```

### Rules (please preserve these)

- **Study-area-specific** stages (`preamble`, `speciesData`, `dataPrep`, `mainSim`, the
  `mode="multi"` summaries, rendered `reports`) write under `outputs/<studyArea>/…`.
- **Study-area-INDEPENDENT** outputs stay at the `outputs/` root with an **underscore prefix**
  (so they sort/read apart from the per-study-area run dirs):
  - `_factorial` — the `Biomass_speciesFactorial` trait table is built with a fixed
    `.studyAreaName = "_factorial_"` sentinel so it is built **once and reused across study
    areas**. Nesting it per-area would defeat that and force a costly (~2 h) rebuild per area.
  - `_extended_analyses` — LTHFC domain-wide, not per-FMA.
- **Logs go in `logs/`, never in `outputs/`.** Both `outputs/` and `logs/` are git-ignored.
- Figures are **stage-based**: they land under each stage's `out_dir/figures/<module>/`
  (SpaDES `figurePath` follows `outputPath`), so everything for a study area is under
  `outputs/<studyArea>/…`.
- **Reference figures also get a standalone `.png`.** A figure that exists only inside a rendered
  report can't be inspected or shared without rebuilding the report, and these get passed around on
  their own. So a reference figure's producer script writes the PNG alongside its data artifacts,
  and the report renders its own copy of the same figure at render time — e.g.
  `scripts/make_sa_reference.R` → `outputs/_reference/studyAreaGroups.{gpkg,csv,png}` plus a
  report-sized copy and a paginated per-group plate, consumed by report 00's `fig-sa-groups` /
  `fig-sa-facets`. Use `ggplot2` for these (see the reporting conventions).

### Where this is wired

- `_targets.R` defines `sa_dir <- local$study_areas` and passes
  `out_dir = file.path("outputs", sa_dir, "<stage>")` to each study-area-specific
  `tar_simspades()` call (the branched `mainSim` uses `bquote(... .(sa_dir) ...)` so the
  study area is baked into the per-replicate path). `_factorial`/`_extended_analyses` omit the
  `sa_dir` prefix. Phase-0 runs a single study area, so `sa_dir` is a scalar; when stages
  branch per-FMA this becomes the per-branch study-area name.
- `_quarto.yml` `project.output-dir` points at `outputs/<studyArea>/reports/`.
- The local-fallback crew controller logs to `logs/crew/`.

When adding a new output-producing stage, decide whether it is study-area-specific (→ nest
under `outputs/<studyArea>/`) or independent (→ `outputs/` root), and set `out_dir` accordingly.
