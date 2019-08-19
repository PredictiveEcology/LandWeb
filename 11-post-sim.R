################################################################################
## post-simulation object creation (for documentation)
################################################################################

#mySimOut <- readRDS(simFile("mySimOut", Paths$outputPath, NULL))

png(file.path("docs", "LandWeb_module_diagram.png"), height = 800, width = 800)
  moduleDiagram(mySimOut)
dev.off()

## currently not working: see https://github.com/rich-iannone/DiagrammeR/issues/207
## workaround is to use Rstudio's manual export ability :S
objectDiagram(mySimOut, height = 2500, width = 1250) # %>%
#   DiagrammeRsvg::export_svg() %>%
#   charToRaw() %>%
#   rsvg::rsvg() %>%
#   png::writePNG(file.path("docs", "LandWeb_object_diagram.png"))

