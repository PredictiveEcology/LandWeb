################################################################################
## post-simulation object creation (for documentation)
################################################################################

modules_landweb <- c("LandWeb_preamble", "Biomass_speciesData", modules3)

fsim <- simFile("mySim_landweb", "docs", 0)
mySim_landweb <- if (!file.exists(fsim)) {
  Cache(simInit,
        times = times3,
        params = parameters3,
        modules = modules_landweb,
        outputs = outputs3,
        objects = objects3,
        paths = paths3,
        loadOrder = unlist(modules_landweb),
        omitArgs = c("debug", "paths", ".plotInitialTime")
  ) ## TODO: spurious error re: params needs to be a list of lists (it is!)
  saveSimList(Copy(mySim_landweb), fsim)
} else {
  loadSimList(fsim)
}

png(file.path("docs", "LandWeb_module_diagram.png"), height = 800, width = 800)
  moduleDiagram(mySim_landweb)
dev.off()

## currently not working: see https://github.com/rich-iannone/DiagrammeR/issues/207
## workaround is to use Rstudio's manual export ability :S
objectDiagram(mySim_landweb, height = 2500, width = 1250) # %>%
#   DiagrammeRsvg::export_svg() %>%
#   charToRaw() %>%
#   rsvg::rsvg() %>%
#   png::writePNG(file.path("docs", "LandWeb_object_diagram.png"))
