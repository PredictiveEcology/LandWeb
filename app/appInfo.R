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

authorized_users <- read.csv("authorized_users.csv", stringsAsFactors = FALSE)
user.emails <- authorized_users$email %>% tolower() ## these should be all lowercase
                                                    ## (resolved in https://github.com/MarkEdmondson1234/googleID/issues/6)
user.group <- authorized_users$company

appInfo <- list(
  name = "LandWeb",
  version = numeric_version("1.0.1"),
  appdir = appDir,
  appmoddir = appModDir,
  authors = c(
    person("Eliot J B", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@friresearch.ca", role = "aut")
  ),
  copyright = paste(shiny::icon("copyright"), format(Sys.time(), "%Y"),
                    "Her Majesty the Queen in Right of Canada,",
                    "as represented by the Minister of Natural Resources Canada."),
  support = list(
    model.email = "eliot.mcintire@canada.ca",  ## TODO: update support info
    model.name = "Eliot McIntire",             ## TODO: update support info
    tech.email = "achubaty@friresearch.ca",
    tech.name = "Alex Chubaty"
  ),

  ### list of authorized users
  users = user.emails,
  user.group = user.group
)
