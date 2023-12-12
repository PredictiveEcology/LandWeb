# # Require::pkgSnapshot("packages_2023-08-18.txt")
# pkgs <- read.csv("packages_2023-08-18.txt")
# cranPkgs <- pkgs[is.na(pkgs$GithubUsername), ]
# ghPkgs <- pkgs[!is.na(pkgs$GithubUsername), ]

library(data.table)

options(Ncpus = min(parallel::detectCores() / 2, 24L))

renv::snapshot(type = "all")

pkgs <- jsonlite::fromJSON(txt = "renv.lock")[["Packages"]] |>
  lapply(as.data.frame) |>
  rbindlist(fill = TRUE)
set(pkgs, NULL, "Requirements", NULL)
pkgs <- pkgs[!duplicated(pkgs)]
cranPkgs <- ghPkgs <- pkgs[is.na(RemoteUsername), ]
ghPkgs <- pkgs[!is.na(pkgs$RemoteUsername), ]

## TODO: use SpaDES.project::description()
usethis::use_description(fields = list(
  Type = "project",
  Title = "LandWeb: Exploring Natural Range of Variation in the Western Boreal Forest",
  Description = paste(
    "The LandWeb model integrates two well-used models for forest stand succession and fire simulation,",
    "implemented in the SpaDES simulation platform (Chubaty & McIntire, 2022).",
    "Vegetation dynamics are modeled using an implementation of the LANDIS-II Biomass Succession model",
    "(Scheller & Mladenoff, 2004; Scheller et al., 2007).",
    "Fire dynamics are modeled using an implementation of LandMine (Andison, 1996).\n",
    "Simulations were run for the entire LandWeb study area, which spans most of the western Canadian boreal forest.",
    "Input data were derived from several publically available remote-sensed datasets (Beaudoin et al., 2014),",
    "as well as proprietary data complied by Pickell et al. (2016).\n",
    "Simulation outputs consist of maps showing the time since fire as well as histogram summaries of",
    "1) number of large patches (i.e., patches above the number of hectares specified by the user) contained within the selected spatial area;",
    "and 2) the vegetation cover within the selected spatial area. Histograms are provided for each spatial area by polygon, age class, and species.",
    "Simulation outputs are summarized for several forestry management areas/units."),
  Package = NULL,
  `Authors@R` = "c(
    person('Eliot J B', 'McIntire', , 'eliot.mcintire@nrcan-rncan.gc.ca', role = 'aut',
           comment = c(ORCID = '0000-0002-6914-8316')),
    person('Alex M', 'Chubaty', , 'achubaty@for-cast.ca', role = c('aut', 'cre'),
           comment = c(ORCID = '0000-0001-7146-8135'))
  )",
  Version = "3.0.0",
  Language = "en-CA",
  License = "GPL-3",
  Depends = paste0("R (== 4.2)", collapse = ",\n    "),
  Imports = paste0(pkgs$Package, " (== ", pkgs$Version, ")", collapse = ",\n    "),
  Remotes = paste0(ghPkgs$RemoteUsername, "/", ghPkgs$RemoteRepo, "@", ghPkgs$RemoteSha, collapse = ",\n    ")
), check_name = FALSE, roxygen = FALSE)

if (FALSE) {
  ## first run only ------------------------
  renv::init(bare = TRUE)

  ## ignore the SpaDES and SpaDES.project b/c they aren't needed
  renv::settings$ignored.packages(c("SpaDES", "SpaDES.shiny"))

  renv::install(paste0(cranPkgs$Package, "@", cranPkgs$Version))
  renv::install(paste0(ghPkgs$GithubUsername, "/", ghPkgs$GithubRepo, "@", ghPkgs$GithubSHA1))

  renv::settings$snapshot.type("explicit")

  renv::snapshot()

  renv::activate()

  ## first + subsequent runs ---------------
  renv::snapshot()

  ## restoring packages from snapshot ------
  options(renv.config.cache.symlinks = FALSE)
  options(renv.config.mran.enabled = FALSE)
  options(Ncpus = min(parallel::detectCores() / 2, 24L))

  renv::restore()
  # renv::isolate() ## not needed if symlinks option set to FALSE per above
}
