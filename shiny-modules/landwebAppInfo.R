landwebAppInfoUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appInfoUI"))
}

landwebAppInfo <- function(input, output, session, appInfo) {
  output$appInfoUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Funding & Partners", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          column(
            width = 12, align = "center",
            div(style = "display: inline-block;",
                ## fRI Healthy Landscapes
                a(href = "https://friresearch.ca/program/healthy-landscapes-program",
                  img(src = "images/logos/fRI_healthy_landscapes.png", width = 400)
                )
            )
          ),
          column(width = 12, align = "center", HTML(paste(br(), "&nbsp;", br()))),
          column(
            width = 12, align = "center",
            div(style = "display: inline-block;",
                # Gov't of Alberta (Ag and Forestry, plus Environment)
                a(href = "https://friresearch.ca/partner/alberta-agriculture-forestry",
                  img(src = "images/logos/Alberta.jpg", align = "left", width = 165)
                ),
                a(href = "https://friresearch.ca/partner/alberta-environment-and-parks",
                  img(src = "images/logos/Alberta.jpg", align = "left", width = 165)
                ),

                # Gov't of Saskatchewan
                a(href = "https://friresearch.ca/partner/government-saskatchewan-ministry-envrionment",
                  img(src = "images/logos/Saskatchewan.jpg", align = "left", width = 165)
                ),

                # Gov't of NWT
                a(href = "https://friresearch.ca/partner/government-northwest-territories-environment-natural-resources",
                  img(src = "images/logos/NWT.jpg", align = "left", width = 165)
                ),

                # Ducks Unlimited
                a(href = "https://friresearch.ca/partner/ducks-unlimited-canada",
                  img(src = "images/logos/ducks-unlimited.png", align = "left", width = 165)
                ),

                # West Fraser Mills Ltd.
                a(href = "https://friresearch.ca/partner/west-fraser-mills-ltd",
                  img(src = "images/logos/WestFraser.jpg", align = "left", width = 165)
                )
            )
          ),

          column(
            width = 12, align = "center",
            div(style = "display: inline-block;",
                # Diashowa-Marubeni International
                a(href = "https://friresearch.ca/partner/daishowa-marubeni-international-dmi",
                  img(src = "images/logos/DaishowaMarubeni.jpg", align = "left", width = 165)
                ),

                # Alberta Pacific Forest Industries Inc.
                a(href = "https://friresearch.ca/partner/alberta-pacific-forest-industries-inc",
                  img(src = "images/logos/AlbertaPacificForestIndustries.jpg", align = "left", width = 165)
                ),

                # Weyerhaueser Company
                a(href = "https://friresearch.ca/partner/weyerhaeuser-company",
                  img(src = "images/logos/Weyerhaeuser.jpg", align = "left", width = 165)
                ),

                # Canfor Corporation
                a(href = "https://friresearch.ca/partner/canfor-corporation",
                  img(src = "images/logos/Canfor.jpg", align = "left", width = 165)
                ),
                # Millar Western Forest Products Ltd.
                a(href = "https://friresearch.ca/partner/millar-western-forest-products-ltd",
                  img(src = "images/logos/MillarWestern.jpg", align = "left", width = 165)
                ),

                # Alberta Newsprint Company
                a(href = "https://friresearch.ca/partner/alberta-newsprint-company",
                  img(src = "images/logos/Alberta_News_Print_Company.png", align = "left", width = 165)
                )
            )
          ),

          column(
            width = 12, align = "center",
            div(style = "display: inline-block;",
                # Mistik Management
                a(href = "https://friresearch.ca/partner/mistik-management",
                  img(src = "images/logos/MistikMgmt.jpg", align = "left", width = 165)
                ),

                # Tolko Industries Ltd.
                a(href = "https://friresearch.ca/partner/tolko-industries-ltd",
                  img(src = "images/logos/Tolko.jpg", align = "left", width = 165)
                ),

                # Vanderwall Forest Products
                ## TODO: is this Vanderwell Contractors? http://www.vanderwell.com/
                a(href = "https://friresearch.ca/partner/nolink",
                  img(src = "images/logos/noimg.png", align = "left", width = 165) ###
                ),

                # Louisiana Pacific (LP BUilding Products)
                a(href = "https://friresearch.ca/partner/lp-building-products",
                  img(src = "images/logos/LP-building-products.jpg", align = "left", width = 165)
                ),

                # FRIAA FRIP FUND
                a(href = "https://friaa.ab.ca/programs/forest-resource-improvement-program/",
                  img(src = "images/logos/FRIAA.svg", align = "left", width = 165)
                )
            )
          )
        )
      ),
      copyrightAuthorsUI(ns("about-app")) ## already in a fluidRow
    )
  })

  callModule(copyrightAuthors, "about-app",
             appName = paste0(appInfo$name, " v", appInfo$version),
             authorInfo = appInfo$authors,
             copyrightInfo = appInfo$copyright,
             licenseFile = NULL, #"LICENSE",
             status = "success" ## make the boxes have green colour
  )
}
