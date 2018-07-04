appInfo <- list(
  name = "LandWeb",
  version = numeric_version("1.0.0"),
  appdir = file.path("/mnt/shared/"), ## TODO: change this on the landweb.ca machine
  authors = c(
    person("Eliot J B", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "alex.chubaty@gmail.com", role = "aut")
  ),
  copyright = paste(icon("copyright"), format(Sys.time(), "%Y"),
                    "Her Majesty the Queen in Right of Canada,",
                    "as represented by the Minister of Natural Resources Canada."),
  support = list(
    model.email = "eliot.mcintire@canada.ca",
    model.name = "Eliot McIntire",
    tech.email = "achubaty@friresearch.ca",
    tech.name = "Alex Chubaty"
  ),

  ### list of authorized users
  users = c(
    "alex.chubaty@gmail.com",
    "andison.d.w@gmail.com",
    "bob.christian@forcorp.com",
    "Chris.Bater@gov.ab.ca",
    "dmiwoodlands@gmail.com",
    "eliot.mcintire@gmail.com",
    "fri.LandWeb@gmail.com",
    "gwhitm@gmail.com",
    "jamesn@albertanewsprint.com",
    "John.Stadt@gov.ab.ca",
    "kathleen.groenewegen@gmail.com",
    "lisa_smithereen@yahoo.ca",
    "mwfp.nrv@gmail.com",
    "Neal.McLoughlin@gov.ab.ca",
    "shanew.sadoway@gmail.com",
    "westfrasertimber@gmail.com"
  )
)
