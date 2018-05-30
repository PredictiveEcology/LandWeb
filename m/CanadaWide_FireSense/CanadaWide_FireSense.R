
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "CanadaWide_FireSense",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = c("Boreal_FireSenseDataPrep", "fireSense_FrequencyFit", "fireSense_FrequencyPredict", "fireSense_SizeFit", "fireSense_SizePredict", "fireSense_EscapeFit", "fireSense_EscapePredict", "fireSense_SpreadFit", "fireSense_SpreadPredict", "fireSense"),
  version = list(SpaDES.core = "0.1.1", CanadaWide_FireSense = "0.0.1",
                 Boreal_FireSenseDataPrep = "0.0.1", fireSense_FrequencyFit = "0.0.1",
                 fireSense_FrequencyPredict = "0.0.1", fireSense_SizeFit = "0.0.1",
                 fireSense_SizePredict = "0.0.1", fireSense_EscapeFit = "0.0.1",
                 fireSense_EscapePredict = "0.0.1", fireSense_SpreadFit = "0.0.1",
                 fireSense_SpreadPredict = "0.0.1", fireSense = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CanadaWide_FireSense.Rmd")
))
