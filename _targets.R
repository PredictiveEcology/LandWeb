# _targets.R -- LandWeb pipeline (Phase-0 spike: 1 study area, N replicates via map(rep_index)).
#
# Proves the outputs-manifest pattern (Part J of the refactor plan): each stage
# runs simInitAndSpades in-process, declares what to save via outputs_spec()
# (simInit `outputs`), and emits (a) a primary target holding the manifest of
# files the run saved -- discovered dynamically from outputs(sim), so per-stage
# saves, module registerOutputs() dumps, and Plots() figures are all captured --
# plus any small in-memory `plain` objects, and (b) a companion `<name>_files`
# `format = "file"` target that hashes every saved file. Downstream stages reload
# the files via sim_inputs() (simInit `inputs`) and read plain objects as
# `<name>$obj`. No simList crosses a target boundary.
#
# Module params are ported from 00-main.R / 03-main-sim.R + box/landweb.R for a
# single small FMA. The mainSim module set is the v3 5-module set; the NRV_summary
# consolidation has retired timeSinceFire + LandWeb_output (submodules removed --
# burnSummaries owns rstTimeSinceFire, NRV_summary owns the per-year veg dumps).
# LandWeb_summary is retained (as a submodule, NOT in the module set) ONLY as the
# porting reference for its Leading (LeadingVegTypeByAgeClass) + LargePatches
# analyses, which are not yet ported to NRV_summary/nrvtools (Part G #7).

source("_local.R") # per-user/host knobs, BEFORE tar_source()

library(targets)
library(SpaDES.targets)

## Terra per-process memory cap -- applied ONLY to the concurrent stage (mainSim). With N
## crew workers sharing a node, terra's default per-process memfrac (0.6 of RAM, no
## cross-worker coordination) lets concurrent workers collectively OOM the node -- this
## SIGKILLed concurrent mainSim reps in LandMine fire-spread. Only mainSim runs its stages
## concurrently (5 reps/node via map(rep_index)); preamble/speciesData/factorial/dataPrep run
## sequentially or singly (<=2 concurrent), so they don't need the cap. Crucially, mem_workers
## is a RUNTIME resource knob but tar_simspades() bakes it into the target's command hash, so
## a global option would tie the expensive cached factorial (~2.2h) to local_workers and
## needlessly rebuild it whenever the worker count changes. So the cap is passed explicitly to
## the mainSim tar_simspades() call below (mem_workers = local$local_workers) and NOT set as a
## global option -- the cached stages stay immune to worker-count changes.

## Gated "extended analyses" (SCANFI study-area vegetation summary + report).
## These define functions only; R/ also holds standalone scripts, so source the
## specific files rather than tar_source()-ing the whole dir.
source("R/scanfi_veg_summary.R")
source("R/knn_veg_summary.R")
source("R/lthfc_summary.R")
source("R/module_changelog.R")
source("R/publish_pdf.R")
source("R/targets_extended_analyses.R")

## Optional multi-node SSH cluster (CONTROL NODE ONLY). When _hosts.R defines crew.ssh.nodes,
## heavy module execution (preamble/speciesData/dataPrep/mainSim) is dispatched to the compute
## nodes via crew.ssh; otherwise a local crew pool is used. _hosts.R is gitignored and lives
## only on the control node -- it holds the real hostnames + per-node worker caps, never committed.
if (file.exists("_hosts.R")) {
  source("_hosts.R")
}

primary_controller <- if (length(getOption("crew.ssh.nodes"))) {
  crew.ssh::crew_controller_ssh(
    name = "primary",
    nodes = getOption("crew.ssh.nodes"),
    projdir = getOption("crew.ssh.projdir"),
    ## reverse SSH tunnel by default: dispatcher binds 127.0.0.1, no inbound port
    tunnel = getOption("crew.ssh.tunnel", TRUE),
    ## NULL -> this session's Rscript path (homogeneous installs); override via crew.ssh.rscript
    rscript = getOption("crew.ssh.rscript"),
    seconds_idle = Inf,
    crashes_max = 25L
  )
} else {
  ## local fallback -- a pool sized by local$local_workers on whatever machine runs tar_make (e.g.
  ## running directly on a compute node with no _hosts.R present); set it >= n_reps so all mainSim
  ## branches run in one wave. Still capped by availableCores. seconds_idle = Inf keeps workers for
  ## the whole run.
  crew::crew_controller_local(
    name = "primary",
    workers = min(parallelly::availableCores(omit = 1), local$local_workers),
    seconds_idle = Inf,
    options_local = crew::crew_options_local(log_directory = file.path("logs", "crew"))
  )
}

tar_option_set(
  packages = "SpaDES.targets", # for unqualified sim_inputs()/outputs_spec() in target commands
  format = "rds",
  controller = primary_controller,
  workspace_on_error = TRUE ## save a workspace on error (tar_traceback()/tar_workspace()) while the pipeline is under active development
)

res <- local$res

## Per-run output directory = the study area. All STUDY-AREA-SPECIFIC stages write under
## outputs/<sa_dir>/<stage>/ so multiple study areas never collide (matches the outputs_v2
## one-dir-per-run layout). Study-area-INDEPENDENT outputs stay at the outputs/ root with an
## underscore prefix (so they sort/read apart from the per-study-area run dirs): `_factorial`
## (the `_factorial_` sentinel, built once + reused across areas) and the gated
## `_extended_analyses` (LTHFC domain-wide). Phase-0 runs a single area, so sa_dir is scalar;
## when stages branch per-FMA this becomes the per-branch study-area name.
sa_dir <- local$study_areas

## ---- shared (.globals) + per-module parameters --------------------------------
globals <- list(
  dataYear = 2020L,
  fireTimestep = 1L,
  initialB = 10,
  sppEquivCol = "LandWeb",
  successionTimestep = 10L,
  summaryInterval = 50L,
  summaryPeriod = c(700, 1000),
  vegLeadingProportion = 0.8,
  .plotInitialTime = 0,
  .plots = "png",
  .sslVerify = 0L,
  .studyAreaName = local$study_areas,
  .useParallel = local$dt_threads
)

p_preamble <- list(
  .globals = globals,
  LandWeb_preamble = list(
    bufferDist = 20000, bufferDistLarge = 50000, dispersalType = "default",
    friMultiple = 1L, pixelSize = res, mergeSlivers = FALSE, minFRI = 25L,
    ROStype = "default", treeClassesLCC = c(81, 210, 220, 230, 240),
    .plotInitialTime = 0, .useCache = FALSE
  )
)

p_speciesData <- list(
  .globals = globals,
  ## .useParallel = 1 (serial): the within-module parallel cluster (PSOCK) spawns
  ## fresh R sessions that carry neither the in-memory googledrive SA token nor the
  ## absolute GOOGLEDRIVE_AUTH (no .Rprofile from a scratch cwd), so the SCANFI Drive
  ## download fails auth in a sub-process. crew already parallelizes across stages.
  Biomass_speciesData = list(types = "SCANFI", .plots = "png", .useCache = FALSE, .useParallel = 1)
)

## Biomass_speciesFactorial is a GENERIC trait-space lookup table: it fabricates its own synthetic
## single-ecoregion studyArea and synthetic (trait-combination) species, and reads `.studyAreaName`
## nowhere (confirmed by module audit). Its output depends only on `factorialSize` + the numeric
## params -- NOT on the study area or the real species (those enter downstream in
## Biomass_speciesParameters via PSP matching). So pin `.studyAreaName` to a fixed sentinel here:
## tar_simspades bakes the `params` VALUE into the factorial command, and `.studyAreaName` is the
## only study-area-coupled entry in `globals`, so this decouples the factorial from
## `local$study_areas`. It is then built once and reused across study areas; it still rebuilds on
## its true inputs (factorialSize, initialB, maxBInFactorial, minCohortBiomass) or a module change.
globals_factorial <- modifyList(globals, list(.studyAreaName = "_factorial_"))

p_factorial <- list(
  .globals = globals_factorial,
  Biomass_speciesFactorial = list(factorialSize = "large")
)

p_dataPrep <- list(
  .globals = globals,
  Biomass_borealDataPrep = list(
    adjustAgeAndLongevity = TRUE,
    biomassModel = quote(lme4::lmer(
      B ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)
    )),
    dataSource = "SCANFI", earliestFireYear = 1950L, ecoregionLayerField = "ECOREGION",
    exportModels = "none", fixModelBiomass = TRUE,
    forestedLCCClasses = c(81, 210, 220, 230, 240), LCCClassesToReplaceNN = 240,
    pixelGroupAgeClass = 20L, pixelGroupBiomassClass = 1000 / (250 / res)^2,
    speciesTableAreas = c("BSW", "BP", "MC"),
    subsetDataAgeModel = 100L, subsetDataBiomassModel = 100L,
    useCloudCacheForStats = FALSE, .plotInitialTime = 0, .useCache = FALSE
    ## TODO: speciesUpdateFunction (2 quotes) + minRelativeBFunction =
    ## quote(myMinRelativeB(pixelCohortData)) -- port verbatim from box/landweb.R
    ## + 00-main.R:299-301 when wiring the real run.
  ),
  Biomass_speciesParameters = list(
    PSPdataTypes = "NFI", quantileAgeSubset = 98L, speciesFittingApproach = "focal"
  )
)

p_mainSim <- list(
  .globals = globals,
  Biomass_core = list(
    growthInitialTime = 0, initialBiomassSource = "cohortData", mixedType = 2L,
    seedingAlgorithm = "wardDispersal", .plotInitialTime = 0, .plotInterval = 100L,
    ## Biomass_core-only param (no other pipeline module uses it): keep it OUT of the
    ## shared `globals` so it doesn't invalidate the cached factorial/dataPrep. Default
    ## "LandR" has a one-to-many mapping to the 7 LandWeb groups (e.g. Abie_spp <-
    ## Abie_bal/Abie_las/Thuj_pli/Tsug_het), which fails plotSummaryBySpecies' assertion.
    sppEquivPlotCol = "LandWeb",
    ## vegetation-transition plots, built natively by Biomass_core (supersedes the
    ## former standalone transition-plot script). Times mirror the NRV summary outputs
    ## (year 0 + `summaryPeriod` at `summaryInterval`); `.plotTransitionField = NA`
    ## dissolves `studyAreaReporting` into a single zone per study area.
    .plotTransitionTimes = as.integer(c(0, seq(globals$summaryPeriod[1L], globals$summaryPeriod[2L],
                                               by = globals$summaryInterval))),
    .plotTransitionField = NA_character_,
    .useCache = FALSE
  ),
  Biomass_regeneration = list(
    calibrate = FALSE, fireInitialTime = 1, .plotInitialTime = 0, .useCache = FALSE
  ),
  LandMine = list(
    biggestPossibleFireSizeHa = 3e5, burnInitialTime = 1L, maxReburns = c(1L, 20L),
    maxRetriesPerID = 9L, minPropBurn = 0.90, mode = "single", ROSother = 30L,
    ROStype = "default", useSeed = NULL, .plotInitialTime = 1, .plotInterval = 100,
    .studyAreaName = local$study_areas, .unitTest = FALSE, .useCache = FALSE
  ),
  ## burnSummaries + NRV_summary run IN the sim in mode="single" (two-phase Phase 1):
  ## they generate + save the per-summary-year files (into out_dir = .../rep%02d) that
  ## the mode="multi" summaries targets later aggregate across reps. burnSummaries owns
  ## rstTimeSinceFire (increments yearly, resets on burn) + fire summaries -> supersedes
  ## timeSinceFire; NRV_summary generates the vegTypeMap/standAgeMap per-year dumps ->
  ## supersedes LandWeb_output. So both retired modules drop out of the module set below.
  burnSummaries = list(mode = "single", .useCache = FALSE),
  NRV_summary = list(mode = "single", .useCache = FALSE)
)

## ---- gated extended analyses --------------------------------------------------
## SCANFI per-species cover over the full LTHFC domain + a study-area vegetation
## report. Multi-hour scan, but cache-aware + pre-seeded so it is normally a
## no-op. Off by default; enable with options(landweb.extended_analyses = TRUE)
## in _local.R (workers do not source _local.R, so the gate + the quarto
## inspection only fire on the control node).
extended_targets <- list()
if (isTRUE(getOption("landweb.extended_analyses", FALSE))) {
  extended_targets <- get_targets_extended_analyses(local)
}

## ---- pipeline -----------------------------------------------------------------
list(
  ## branch sources
  tar_target(study_areas, local$study_areas), # Phase-0: one area
  tar_target(rep_index, seq_len(local$n_reps), iteration = "vector"),

  ## Stage 1: preamble. Saves its spatial handoff objects (so they appear in
  ## outputs(sim)) and exposes small tables in-memory via `plain`.
  tar_simspades(
    "preamble",
    modules = "LandWeb_preamble",
    params = p_preamble,
    paths = local$paths,
    out_dir = file.path("outputs", sa_dir, "preamble"),
    log_file = file.path("outputs", sa_dir, "logs", "preamble.log"),
    plain = c("sppEquiv", "sppColorVect", "speciesParams", "speciesTable", "ROSTable"),
    outputs = quote(outputs_spec(
      raster = c(
        "rasterToMatch", "rasterToMatch_biomassParam",
        "rstLCC", "standAgeMap", "flammableMap", "fireReturnInterval"
      ),
      vect = c("studyArea", "studyArea_biomassParam", "studyAreaReporting", "studyAreaANPP")
    ))
  ),

  ## Stage 2: speciesData. Loads upstream files via sim_inputs() and small
  ## tables via `objects`; saves speciesLayers.
  tar_simspades(
    "speciesData",
    modules = "Biomass_speciesData",
    params = p_speciesData,
    paths = local$paths,
    out_dir = file.path("outputs", sa_dir, "speciesData"),
    log_file = file.path("outputs", sa_dir, "logs", "speciesData.log"),
    objects = quote(list(
      sppEquiv = preamble$sppEquiv,
      sppColorVect = preamble$sppColorVect
    )),
    inputs = quote(sim_inputs(
      preamble,
      objects = c(
        "rasterToMatch", "rasterToMatch_biomassParam",
        "studyArea", "studyArea_biomassParam", "studyAreaReporting"
      ),
      files = preamble_files
    )),
    outputs = quote(outputs_spec(raster = "speciesLayers"))
  ),

  ## Stage 3a: factorial -- self-contained species-trait calibration (its only input,
  ## argsForFactorial, is defaulted via `factorialSize`). Split into its own target so the
  ## heavy "large" build runs ONCE and is cached by targets: iterating on dataPrep (e.g. the
  ## borealDataPrep code-235) does NOT re-run it -- the firewall sets reproducible.useCache =
  ## FALSE, so a bundled factorial would rebuild on every dataPrep run. Biomass_speciesFactorial
  ## writes its arrow datasets under outputPath (the shared-NFS `outputs` symlink) and emits the
  ## paths, which persist across the target boundary and across compute nodes.
  tar_simspades(
    "factorial",
    modules = "Biomass_speciesFactorial",
    params = p_factorial,
    paths = local$paths,
    ## SHARED, study-area-independent -> underscore-prefixed dir at the outputs/ root (NOT nested
    ## per study area): built once with the `_factorial_` sentinel and reused across areas.
    out_dir = file.path("outputs", "_factorial"),
    log_file = file.path("outputs", "_factorial", "logs", "factorial.log"),
    plain = c("cohortDataFactorial_path", "speciesTableFactorial_path")
  ),

  ## Stage 3b: dataPrep -- Biomass_borealDataPrep + Biomass_speciesParameters, consuming the
  ## factorial paths from the cached `factorial` target via `objects`.
  tar_simspades(
    "dataPrep",
    modules = c("Biomass_borealDataPrep", "Biomass_speciesParameters"),
    params = p_dataPrep,
    paths = local$paths,
    out_dir = file.path("outputs", sa_dir, "dataPrep"),
    log_file = file.path("outputs", sa_dir, "logs", "dataPrep.log"),
    ## Spatial handoff objects pass in-memory via sim_objects() (loaded on the worker),
    ## NOT as file inputs: Biomass_borealDataPrep/Biomass_speciesParameters read several
    ## (studyArea, rasterToMatch, ...) in .inputObjects(), which runs during simInit() --
    ## before inputs= load. terra layers load lazily, so this stays cheap.
    objects = quote(c(
      list(
        speciesParams = preamble$speciesParams,
        speciesTable = preamble$speciesTable,
        sppColorVect = preamble$sppColorVect,
        sppEquiv = preamble$sppEquiv,
        cohortDataFactorial_path = factorial$cohortDataFactorial_path,
        speciesTableFactorial_path = factorial$speciesTableFactorial_path
      ),
      sim_objects(
        preamble,
        objects = c(
          "rstLCC", "rasterToMatch", "rasterToMatch_biomassParam", "standAgeMap",
          "studyArea", "studyAreaANPP", "studyArea_biomassParam", "studyAreaReporting"
        ),
        files = preamble_files
      ),
      sim_objects(speciesData, objects = "speciesLayers", files = speciesData_files)
    )),
    plain = c(
      "cohortData", "species", "speciesEcoregion", "ecoregion", "minRelativeB",
      "sufficientLight", "sppEquiv", "sppColorVect", "speciesParams", "speciesTable"
    ),
    outputs = quote(outputs_spec(
      raster = c(
        "biomassMap", "rawBiomassMap", "ecoregionMap", "pixelGroupMap", "rstLCC",
        "standAgeMap", "speciesLayers", "rasterToMatch", "rasterToMatch_biomassParam"
      ),
      vect = c("studyArea", "studyArea_biomassParam")
    ))
  ),

  ## Stage 4: mainSim (two-phase Phase 1). burnSummaries + NRV_summary run here in
  ## mode="single" (params above), replacing timeSinceFire (burnSummaries owns
  ## rstTimeSinceFire -- increments yearly, resets on burn) and LandWeb_output
  ## (NRV_summary owns the per-year veg dumps). Branched over rep_index via
  ## pattern = map(rep_index): each stochastic replicate runs as its own branch with
  ## a per-rep deterministic seed, writing its outputs into outputs/<sa>/mainSim/rep%02d/
  ## so the mode="multi" summaries targets aggregate across all rep dirs.
  ## iteration = "list" because each branch returns a run_simspades() list.
  tar_simspades(
    "mainSim",
    modules = c("Biomass_core", "LandMine", "Biomass_regeneration", "burnSummaries", "NRV_summary"),
    ## explicit load order: Biomass_core's `after = "Biomass_speciesParameters"` metadata refers to a
    ## module absent from this stage, breaking auto-inference; set it like the old 03-main-sim.R did.
    loadOrder = c("Biomass_core", "LandMine", "Biomass_regeneration", "burnSummaries", "NRV_summary"),
    pattern = quote(map(rep_index)),
    iteration = "list",
    out_dir = bquote(file.path("outputs", .(sa_dir), "mainSim", sprintf("rep%02d", rep_index))),
    log_file = bquote(file.path("outputs", .(sa_dir), "logs", sprintf("mainSim_rep%02d.log", rep_index))),
    params = p_mainSim,
    times = list(start = 0, end = local$sim_end),
    paths = local$paths,
    ## terra memory cap: mainSim is the ONLY concurrent stage (5 reps/node), so it alone
    ## needs the per-worker cap (mem_frac * node RAM / mem_workers) to avoid collective OOM
    ## in LandMine fire-spread. Passed here (not as a global option) so the cached upstream
    ## stages stay immune to worker-count changes -- see the note near the top of this file.
    mem_workers = local$local_workers,
    objects = quote(c(
      list(
        cohortData = dataPrep$cohortData,
        species = dataPrep$species,
        speciesEcoregion = dataPrep$speciesEcoregion,
        ecoregion = dataPrep$ecoregion,
        minRelativeB = dataPrep$minRelativeB,
        sufficientLight = dataPrep$sufficientLight,
        sppEquiv = dataPrep$sppEquiv,
        sppColorVect = dataPrep$sppColorVect,
        speciesParams = dataPrep$speciesParams,
        speciesTable = dataPrep$speciesTable,
        ROSTable = preamble$ROSTable
      ),
      ## Fire layers + reporting polygon are touched in .inputObjects() (burnSummaries
      ## derives rstTimeSinceFire from fireReturnInterval; LandMine reads flammableMap/
      ## studyAreaReporting), which runs during simInit() -- before inputs= load. Pass
      ## them in-memory via sim_objects() (loaded lazily on the worker), matching the
      ## dataPrep stage; otherwise rstTimeSinceFire is NULL at LandMine's compareGeom.
      sim_objects(
        preamble,
        objects = c("flammableMap", "fireReturnInterval", "studyAreaReporting"),
        files = preamble_files
      ),
      ## studyArea + rasterToMatch are ALSO touched in Biomass_core's .inputObjects()
      ## (it CRS-compares studyArea vs rasterToMatch and vs studyAreaReporting, e.g.
      ## Biomass_core.R:2539/2555) -- which runs during simInit() BEFORE inputs= load.
      ## Supplying them via inputs= leaves them NULL at that point, so the compare sees
      ## an NA-CRS studyArea vs the loaded studyAreaReporting and spuriously reprojects
      ## (modifies) studyAreaReporting. Pass them in-memory too, matching studyAreaReporting.
      sim_objects(
        dataPrep,
        objects = c("studyArea", "rasterToMatch"),
        files = dataPrep_files
      )
    )),
    inputs = quote(
      sim_inputs(
        dataPrep,
        objects = c(
          "biomassMap", "rawBiomassMap", "ecoregionMap", "pixelGroupMap", "rstLCC",
          "standAgeMap", "speciesLayers", "rasterToMatch_biomassParam",
          "studyArea_biomassParam"
        ),
        files = dataPrep_files
      )
    ),
    seed = quote(rep_index), # per-rep deterministic seed (rep 1 -> 1L, matches the validated single rep)
    plain = c("cohortData", "simulationOutput")
    ## NO outputs_spec here: in the two-phase design NRV_summary + burnSummaries
    ## (mode="single") OWN all per-rep raster saves -- pixelGroupMap/standAgeMap/
    ## vegTypeMap/flammableMap (NRV_summary save_single), rstTimeSinceFire/burnMap/
    ## flammableMap (burnSummaries save_single), vegTypeMap (Biomass_core buildVTM) --
    ## and registerOutputs() them. A redundant outputs= would make SpaDES saveFiles()
    ## re-writeRaster the same paths at end(sim) WITHOUT overwrite=TRUE ->
    ## "[writeRaster] file exists" (the 3h32m crash). mainSim_files still captures
    ## every saved file via the modules' registerOutputs().
  ),

  ## ---- Stage 5: post-processing (NRV_summary) --------------------------------

  ## Reporting polygons: candidate FMA/FMU/ANSR/Caribou/Parks/ecoregion layers
  ## (LandWebUtils::reportingPolygonLayers()) fetched from Drive/URL, clipped to the
  ## study area, kept only where they intersect -- the polygon sets NRV_summary
  ## summarizes metrics over. Stored as a named list of `sf` (serializable;
  ## NRV_summary st_as_sf()-es each element anyway), so a plain cached target. The
  ## sim-sourced "CC SAM"/"CC TSF"/"ecoregionLayer" entries are merged in at the
  ## summaries stage, not here. NOTE: swapping the reporting datasets later is a
  ## light edit to LandWebUtils::reportingPolygonLayers(); this wiring is unaffected.
  tar_target(
    reportingPolygons,
    {
      sa <- sim_objects(preamble, objects = "studyArea", files = preamble_files)[["studyArea"]]
      polys <- LandWebUtils::buildReportingPolygons(
        studyArea = sa,
        destinationPath = file.path(local$paths$inputPath, "reportingPolygons"),
        targetCRS = LandWebUtils::LandWebCRS
      )
      lapply(polys, sf::st_as_sf)
    }
  ),

  ## ---- Stage 5b: post-processing summaries (mode="multi") --------------------
  ## Phase 2 of the two-phase design: NRV_summary + burnSummaries re-run as their
  ## OWN targets in mode="multi", aggregating the per-rep files Phase 1 (mainSim)
  ## saved under outputs/<sa>/mainSim/rep%02d/ into the NRV envelopes + fire summaries.
  ##
  ## out_dir = outputs/<sa>/mainSim -- the PARENT of the rep dirs: both modules read
  ## outputPath(sim)/rep%02d/ and write their aggregates back into outputPath(sim)
  ## (the v2 postprocess pattern from box/landweb.R, where the summary outputPath
  ## is the per-study-area parent, not a per-rep subdir). clean_out_dir = FALSE so
  ## run_simspades does NOT wipe outputs/<sa>/mainSim first -- that would delete the rep
  ## outputs being aggregated. The bare `mainSim_files` reference is a dependency
  ## anchor: the modules discover the per-rep files via dir_ls() at run time (not
  ## sim_inputs), so referencing it forces Phase 1 to run first and re-runs the
  ## summaries whenever a rep output changes.

  ## NRV_summary (mode="multi"): landscape (lm) + patch (pm) metric envelopes per
  ## reporting polygon, via nrvtools' arrow-native summarize_nrv() path.
  tar_simspades(
    "summaries_nrv",
    modules = "NRV_summary",
    out_dir = file.path("outputs", sa_dir, "mainSim"),
    clean_out_dir = FALSE,
    log_file = file.path("outputs", sa_dir, "logs", "summaries_nrv.log"),
    params = list(
      .globals = globals,
      NRV_summary = list(
        mode = "multi",
        reps = seq_len(local$n_reps),
        simTimes = c(0, local$sim_end),
        postprocessEvents = c("lm", "pm"),
        .useCache = FALSE
      )
    ),
    times = list(start = 0, end = local$sim_end),
    paths = local$paths,
    ## reportingPolygons (geographic layers) + the sim objects the lm/pm events
    ## read in-memory (studyAreaReporting, sppEquiv, sppColorVect; speciesLayers
    ## via inputs). TODO (module-contract, resolve at the first data-backed run):
    ## merge the sim-sourced "CC SAM" (current-condition standAgeMap) / "CC TSF" /
    ## "ecoregionLayer" entries into reportingPolygons -- patchMetrics reads
    ## reportingPolygons[["CC SAM"]] -- AND have NRV_summary exclude those special
    ## keys from mod$rptPolyNames so they are not iterated as reporting polygons.
    objects = quote({
      mainSim_files # dependency anchor: Phase-1 per-rep files must exist first
      c(
        list(
          reportingPolygons = reportingPolygons,
          sppEquiv = dataPrep$sppEquiv,
          sppColorVect = dataPrep$sppColorVect
        ),
        sim_objects(preamble, objects = "studyAreaReporting", files = preamble_files)
      )
    }),
    inputs = quote(sim_inputs(dataPrep, objects = "speciesLayers", files = dataPrep_files))
  ),

  ## burnSummaries (mode="multi"): mean-annual cumulative burn maps + across-rep
  ## fire-size distribution (fireregimetools' arrow-native open_burn_dataset /
  ## fire_size_histogram). Reads per-rep burnMap/flammableMap + the fire-size
  ## parquet partitions from outputs/<sa>/mainSim/rep%02d/ and downloads NFDB polygons
  ## into inputPath; needs no in-memory sim objects.
  tar_simspades(
    "summaries_burn",
    modules = "burnSummaries",
    out_dir = file.path("outputs", sa_dir, "mainSim"),
    clean_out_dir = FALSE,
    log_file = file.path("outputs", sa_dir, "logs", "summaries_burn.log"),
    params = list(
      .globals = globals,
      burnSummaries = list(
        mode = "multi",
        reps = seq_len(local$n_reps),
        simTimes = c(0, local$sim_end),
        .useCache = FALSE
      )
    ),
    times = list(start = 0, end = local$sim_end),
    paths = local$paths,
    ## burnSummaries mode="multi" .inputObjects requires reportingPolygons
    ## (stopifnot(suppliedElsewhere(...))). It is not otherwise used by the multi
    ## events, but must be supplied. mainSim_files is the Phase-1 dependency anchor.
    objects = quote({
      mainSim_files # dependency anchor: Phase-1 per-rep burn/flammable maps first
      list(reportingPolygons = reportingPolygons)
    })
  ),

  ## Gated extended analyses (empty list unless landweb.extended_analyses is set
  ## in _local.R). targets flattens nested lists, so this splices in cleanly.
  extended_targets
)
