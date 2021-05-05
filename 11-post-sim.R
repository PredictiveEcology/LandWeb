################################################################################
## post-simulation object creation (for documentation)
################################################################################

modules_landweb <- c("LandWeb_preamble", "Biomass_speciesData", "Biomass_borealDataPrep", modules3)

mm <- unique(unlist(lapply(modules_landweb, function(m) {
  moduleMetadata(module = m, path = "m")$inputObjects$objectName
})))
names(mm) <- mm

oo <- lapply(mm, function(x) NULL)
mySim_landweb <- simInit(objects = oo, modules = modules_landweb, paths = paths3)

fsim <- simFile("mySim_landweb", "docs", 0, ext = "qs")
saveSimList(Copy(mySim_landweb), fsim, fileBackend = 2)

#mySim_landweb <- loadSimList(fsim)

png(file.path("docs", "LandWeb_module_diagram.png"), height = 1000, width = 1000)
  moduleDiagram(mySim_landweb)
dev.off()

## currently not working: see https://github.com/rich-iannone/DiagrammeR/issues/207
## workaround is to use Rstudio's manual export ability :S
objectDiagram(mySim_landweb, height = 2500, width = 1250) # %>%
#   DiagrammeRsvg::export_svg() %>%
#   charToRaw() %>%
#   rsvg::rsvg() %>%
#   png::writePNG(file.path("docs", "LandWeb_object_diagram.png"))
