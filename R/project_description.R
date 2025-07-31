options(renv.lockfile.version = 1) ## TODO: fix workflowtools#1

workflowtools::description(
  fields = list(
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
    `Authors@R` = "c(
      person('Eliot J B', 'McIntire', , 'eliot.mcintire@nrcan-rncan.gc.ca', role = 'aut',
             comment = c(ORCID = '0000-0002-6914-8316')),
      person('Alex M', 'Chubaty', , 'achubaty@for-cast.ca', role = c('aut', 'cre'),
             comment = c(ORCID = '0000-0001-7146-8135'))
    )",
    Version = "3.0.0",
    Language = "en-CA",
    License = "GPL-3",
    Depends = "R (== 4.5)"
  ),
  snapshot = "renv.lock"
)
