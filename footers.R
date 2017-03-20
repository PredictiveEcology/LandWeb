### footers
copyrightFooter <- function() {
  copyrightInfo <- paste(
    shiny::icon("copyright",  lib = "font-awesome"), "Copyright ",
    format(Sys.time(), "%Y"),
    paste("Her Majesty the Queen in Right of Canada,",
          "as represented by the Minister of Natural Resources Canada.")
  )
  
  HTML(paste(
    "<footer>", "<div id=\"copyright\">", copyrightInfo, "</div>", "</footer>"
  ))
}

sidebarFooter <- function() {
  HTML(paste(
    "<footer>",
    "<div id=\"sidebar\">",
    "Powered by <a href=\"http://SpaDES.PredictiveEcology.org\", target=\"_blank\">\u2660 SpaDES</a> ",
    "and <a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a>",
    "</div>",
    "</footer>"
  ))
}
