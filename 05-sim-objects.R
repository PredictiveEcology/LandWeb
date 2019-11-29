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
speciesParams <- if (grepl("aspenDispersal", runName)) {
  ## seed dispersal (see LandWeb#96, LandWeb#112)
  list(
    seeddistance_eff = list(Abie_sp = 0, Pice_gla = 0, Pice_mar = 0,
                            Pinu_ban = 0, Pinu_con = 0, Pinu_sp = 0, Popu_sp = 100),
    seeddistance_max = list(Abie_sp = 125, Pice_gla = 125, Pice_mar = 125,
                            Pinu_ban = 125, Pinu_con = 125, Pinu_sp = 125, Popu_sp = 235)
  )
} else if (grepl("highDispersal", runName)) {
  list(
    seeddistance_eff = list(Abie_sp = 250, Pice_gla = 100, Pice_mar = 320,
                            Pinu_ban = 300, Pinu_con = 300, Pinu_sp = 300, Popu_sp = 500),
    seeddistance_max = list(Abie_sp = 1250, Pice_gla = 1250, Pice_mar = 1250,
                            Pinu_ban = 3000, Pinu_con = 3000, Pinu_sp = 3000, Popu_sp = 3000)
  )
} else {
  ## defaults
  list(
    seeddistance_eff = list(Abie_sp = 25, Pice_gla = 100, Pice_mar = 80,
                            Pinu_ban = 30, Pinu_con = 30, Pinu_sp = 30, Popu_sp = 200),
    seeddistance_max = list(Abie_sp = 160, Pice_gla = 303, Pice_mar = 200,
                            Pinu_ban = 100, Pinu_con = 100, Pinu_sp = 100, Popu_sp = 2000)
  )
}
