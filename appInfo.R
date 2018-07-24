appDir <- switch(Sys.info()["nodename"],
                 "kahless" = file.path("/mnt/"),
                 "landweb.ca" =  file.path("/srv/shiny-server/"),
                 "landweb" =  file.path("/mnt/data/"),
                 "W-VIC-A105342" = file.path("/mnt/shared/"),
                 file.path("~/GitHub/")
) # needs trailing slash!

appModDir <- switch(Sys.info()["nodename"],
                    "kahless" = file.path("~/GitHub/m"),
                    "landweb.ca" = file.path(appDir, "m"),
                    "landweb" = file.path(appDir, "m"),
                    "W-VIC-A105342" = file.path("~/GitHub/m"),
                    file.path("~/GitHub/m")
) # no trailing slash!

appInfo <- list(
  name = "LandWeb",
  version = numeric_version("1.0.0.9000"),
  appdir = appDir,
  appmoddir = appModDir,
  authors = c(
    person("Eliot J B", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@friresearch.ca", role = "aut")
  ),
  copyright = paste(icon("copyright"), format(Sys.time(), "%Y"),
                    "Her Majesty the Queen in Right of Canada,",
                    "as represented by the Minister of Natural Resources Canada."),
  support = list(
    model.email = "eliot.mcintire@canada.ca",  ## TODO: update support info
    model.name = "Eliot McIntire",             ## TODO: update support info
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
    "sonya@fuseconsulting.ca",
    "westfrasertimber@gmail.com"
  )
)
