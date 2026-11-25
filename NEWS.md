**Known issues:**
<a href="https://github.com/PredictiveEcology/LandWeb/issues" class="uri">https://github.com/PredictiveEcology/LandWeb/issues</a>

------------------------------------------------------------------------

### LandWeb v3.0.0 (in development)

v3 rebuilds the modelling infrastructure and refreshes the input data;
some changes also alter the simulated ecology. Report 03
(`reports/pdf/03-code-and-module-changes.pdf`) sets out the changes and
why they were made, and each module’s own `NEWS.md` lists its changes.

-   **Inputs:** species cover and biomass from SCANFI, replacing the
    2001 *k*NN maps; a current-condition stand age layer; fire-cycle map
    v10; and fire records to 2025.
-   **Vegetation:** species are simulated individually where they are
    common enough, with growth curves from one fit shared by all study
    areas; each species regenerates after fire by its own mode, where v2
    made every species resprout.
-   **Vegetation types:** a species or group leads a stand at 75% (80%
    in v2), and mixedwood is judged on the summed share of broadleaf
    species.
-   **Study areas:** the model runs on 18 ecoregion-based groups of
    forest tenures, rather than on the whole LandWeb area or on single
    tenures, with results for each tenure from the reporting polygons.
-   **Workflow:** a `targets` pipeline replaces the hand-run scripts;
    `NRV_summary` and `burnSummaries` replace `timeSinceFire`,
    `LandWeb_output` and `LandWeb_summary`.
-   **Outputs:** PDF reports (00 to 07) replace the web app, which is no
    longer hosted.

------------------------------------------------------------------------

### LandWeb v2.0.1

**App changes**

-   fixed issue with downloading model outputs.
-   minor performance enhancements.

### LandWeb v2.0.0

**Input data**

-   reworked data preparation steps and fixed many data errors
-   implemented and fit various statistical models to impute missing and
    inconsistent data

**Model improvements**

-   reparameterized the model to emulate a state-transition model to
    ensure burned pixels regenerate
-   several bug fixes and performance enhancements

**App changes**

-   reworked app to use pre-generated results only instead of computing
    on-the-fly; as a result, only selected results per FMA are available
    in-app.
-   several other performance enhancements.
-   removed shapefile uploads, as custom areas require rerunning various
    analyses, which were slowing down the app server and blocking other
    users from interacting with the app.

------------------------------------------------------------------------

### LandWeb v1.0.1

-   added axis labels and titles to several figures (\#57)
-   added text to clarify meaning of “years” in simulations (\#58)
-   added boxplots that summarize the vegetation cover histograms (\#59)
-   added news feed and notifications (\#60)
-   some performance improvements loading maps
-   fixed file-naming bug when downloadling model outputs (with \#62)
-   improved caching
-   server upgrades have reduced several performance and disconnection
    issues

**NOTE:** There appears to be a bias towards deciduous species in some
of the results. This may be due to the interaction between the LandMine
fire model and the LANDIS Biomass-Succession models: deciduous species
burn less than conifers but regenerate more quickly. I am currently
taking a closer look at the simulations and code to verify these
results.

------------------------------------------------------------------------

### LandWeb v1.0.0

-   time since fire maps, and histograms for all combinations of stand
    age class, leading vegetation type, and spatial area at a particular
    patch size;
-   vegetation cover histograms for all combinations of stand age class,
    leading vegetation type, and spatial area;
-   user-selectable spatial areas and patch sizes;
-   log in using your Google account to view current conditions, upload
    custom polygons, and download model results.

**NOTE:** There appears to be a bias towards deciduous species in some
of the results. This may be due to the interaction between the LandMine
fire model and the LANDIS Biomass-Succession models: deciduous species
burn less than conifers but regenerate more quickly. We are currently
taking a closer look at the simulations and code to verify these
results.
