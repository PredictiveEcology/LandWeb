###############################################################################
## additional simulation object definitions
################################################################################

data("sppEquivalencies_CA", package = "LandR")
sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                             EN_generic_full = "Pine",
                                             Leading = "Pine leading")]

# Make LandWeb spp equivalencies
sppEquivalencies_CA[, LandWeb := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                                   Pinu_con = "Pinu_sp", Pinu_ban = "Pinu_sp",
                                   Popu_tre = "Popu_sp", Betu_pap = "Popu_sp",
                                   Abie_bal = "Abie_sp", Abie_las = "Abie_sp", Abie_sp = "Abie_sp")[LandR]]

sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_full := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_short := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", Leading := "Fir leading"]

sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_full := "Deciduous"]
sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_short := "Decid"]
sppEquivalencies_CA[LandWeb == "Popu_sp", Leading := "Deciduous leading"]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(LandWeb), ]

sppColorVect <- sppColors(sppEquivalencies_CA, sppEquivCol, newVals = "Mixed", palette = "Accent")

LandMineROStable <- rbindlist(list(
  list("mature", "decid", 9L),
  list("immature_young", "decid", 6L),
  list("immature_young", "mixed", 12L),
  list("mature", "mixed", 17L),
  list("immature", "pine", 14L),
  list("mature", "pine", 21L),
  list("young", "pine", 22L),
  list("immature_young", "softwood", 18L),
  list("mature", "softwood", 27L),
  list("immature_young", "spruce", 20L),
  list("mature", "spruce", 30L)
))
setnames(LandMineROStable, old = 1:3, new = c("age", "leading", "ros"))

if (grepl("equalROS", runName)) {
  LandMineROStable$ros <- 1L
} else if (grepl("logROS", runName)) {
  LandMineROStable$ros <- log(LandMineROStable$ros)
}

speciesTable <- getSpeciesTable(dPath = paths1$inputPath) ## uses default URL
speciesParams <- list(
  growthcurve = list(Abie_sp = 0, Pice_gla = 1, Pice_mar = 1, Pinu_sp = 0, Popu_sp = 0),
  mortalityshape = list(Abie_sp = 15L, Pice_gla = 15L, Pice_mar = 15L, Pinu_sp = 15L, Popu_sp = 25L),
  resproutage_min = list(Popu_sp = 25L), # default 10L
  #resproutprob = list(Popu_sp = 0.1), # default 0.5
  shadetolerance = list(Abie_sp = 3, Pice_gla = 2, Pice_mar = 3, Pinu_sp = 1, Popu_sp = 1) # defaults 4, 3, 4, 1, 1
)
speciesParams <- append(speciesParams, if (grepl("aspenDispersal", runName)) {
  ## seed dispersal (see LandWeb#96, LandWeb#112)
  list(
    postfireregen = list(Abie_sp = "resprout", Pice_gla = "resprout", Pice_mar = "resprout",
                         Pinu_sp = "resprout", Popu_sp = "resprout"),
    resproutage_max = list(Abie_sp = 400L, Pice_gla = 400L, Pice_mar = 400L, Pinu_sp = 400L, Popu_sp = 400L),
    resproutage_min = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 0L),
    resproutprob = list(Abie_sp = 1.0, Pice_gla = 1.0, Pice_mar = 1.0, Pinu_sp = 1.0, Popu_sp = 1.0),
    seeddistance_eff = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 100L),
    seeddistance_max = list(Abie_sp = 125L, Pice_gla = 125L, Pice_mar = 125L, Pinu_sp = 125L, Popu_sp = 235L)
  )
} else if (grepl("highDispersal", runName)) {
  list(
    postfireregen = list(Abie_sp = "resprout", Pice_gla = "resprout", Pice_mar = "resprout",
                         Pinu_sp = "resprout", Popu_sp = "resprout"),
    resproutage_max = list(Abie_sp = 400L, Pice_gla = 400L, Pice_mar = 400L, Pinu_sp = 400L, Popu_sp = 400L),
    resproutage_min = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 0L),
    resproutprob = list(Abie_sp = 1.0, Pice_gla = 1.0, Pice_mar = 1.0, Pinu_sp = 1.0, Popu_sp = 1.0),
    seeddistance_eff = list(Abie_sp = 250L, Pice_gla = 100L, Pice_mar = 320L, Pinu_sp = 300L, Popu_sp = 500L),
    seeddistance_max = list(Abie_sp = 1250L, Pice_gla = 1250L, Pice_mar = 1250L, Pinu_sp = 3000L, Popu_sp = 3000L)
  )
} else if (grepl("noDispersal", runName)) {
  list(
    postfireregen = list(Abie_sp = "resprout", Pice_gla = "resprout", Pice_mar = "resprout",
                         Pinu_sp = "resprout", Popu_sp = "resprout"),
    resproutage_max = list(Abie_sp = 400L, Pice_gla = 400L, Pice_mar = 400L, Pinu_sp = 400L, Popu_sp = 400L),
    resproutage_min = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 0L),
    resproutprob = list(Abie_sp = 1.0, Pice_gla = 1.0, Pice_mar = 1.0, Pinu_sp = 1.0, Popu_sp = 1.0),
    seeddistance_eff = list(Abie_sp = 25L, Pice_gla = 100L, Pice_mar = 80L, Pinu_sp = 30L, Popu_sp = 200L),
    seeddistance_max = list(Abie_sp = 160L, Pice_gla = 303L, Pice_mar = 200L, Pinu_sp = 100L, Popu_sp = 2000L)
  )
} else {
  ## defaults
  list(
    seeddistance_eff = list(Abie_sp = 25L, Pice_gla = 100L, Pice_mar = 80L, Pinu_sp = 30L, Popu_sp = 200L),
    seeddistance_max = list(Abie_sp = 160L, Pice_gla = 303L, Pice_mar = 200L, Pinu_sp = 100L, Popu_sp = 2000L)
  )
})

if (grepl("SprayLake", runName)) {
  message(crayon::red("Fir shade tolerance lowered below default (3). Using value 2."))
  message(crayon::red("Spruce shade tolerance raised above default (2, 3). Using values 3, 4."))
  speciesParams <- append(speciesParams, list(
    shadetolerance = list(Abie_sp = 2, Pice_gla = 3, Pice_mar = 4))
  )
}
