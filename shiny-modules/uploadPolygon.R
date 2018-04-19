uploadPolygonUI <- function(id) {
  ns <- NS(id)

  if (isTRUE(session$userData$userAuthorized())) {
    fluidRow(
      p("Upload a shapefile by selecting .shp and its associated files (or upload a single .zip file)"),
      p("Each polygon should have a \"LABEL\" attribute, otherwise numeric polygon ids will be used."),

      # user inputs the .shp file (plus aux files) -- multiple select
      # user inputs a name for the polygon (if blank, use filename without extension)
      fileInput(ns("shpFiles"), "Upload shapefile:", multiple = TRUE,
                accept = c(".dbf", ".prj", ".sbn", ".sbx", ".shp", ".shx", ".zip")),
      textInput(ns("polyName"), "Name to use for this polygon.", value = "", placeholder = "filename.shp")
    )
  }
}

#' @param input          shiny server input object
#' @param output         shiny server output object
#' @param session        shiny server session object
#' @param userDir        User-specific directory in which to store uploaded files.
#'
#' @return               Reactive object containing the uploaded polygon.
#'
uploadPolygon <- function(input, output, session, userDir) {
  # create inputs/user-polygons/USERNAME
  if (!dir.exists(userDir)) dir.create(userDir, recursive = TRUE)

  # do GIS checks etc.
  rctUserPoly <- reactive({
    req(input$shpFiles)

    filenames <- input$shpFiles$datapath

    zipFile <- filenames[which(fileExt(filenames) == ".zip")]
    shpFile <- filenames[which(fileExt(filenames) == ".shp")]

    # save polygon to the USERNAME dir with timestamp
    if (length(zipFile)) {
      unzip(zipFile, exdir = userDir, overwrite = TRUE, junkpaths = TRUE)
    }

    if (length(shpFile)) {
      polyName <- if (is.null(input$polyName) || input$polyname == "") {
        shpFile
      } else {
        input$polyName ## TODO: check that this name isn't already in use!!!
      }

      userPoly <- shapefile(shpFile) %>% gBuffer(byid = TRUE, width = 0)
      ## TODO: reproject to correct CRS
      ## TODO: check extent overlap with LandWeb study region and intersect

      ## TODO: check that attribute 'shinyLabel' exists, if not, create it

      ## TODO: should all polygons in the user's dir be added to the list???
      ## TODO: allow a user to remove old uploaded polygons
      #fname <- file.path(userDir, .prefix(basename(shpFile), paste0(format(Sys.Date(), "%Y-%m-%d"), "_")))
      fname <- file.path(userDir, basename(shpFile))
      shapeFile(userPoly, filename = fname)
      userPoly
    } else {
      warning("Invalid or missing shopefile (.shp).")
    }
  })

  # return the cleanedup/verified polygon [outside the module: add this poly to the palyList]
  return(rctUserPoly)
}
