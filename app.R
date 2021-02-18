## Appsilon R Shiny Task master script file: Aarsh Batra=======================

# metadata---------------------------------------------------------------------
# author: Aarsh Batra
# Start Date: February 2, 2021
# R version: 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)
# R Studio version: 1.4.1103
# e-mail: aarshbatra.in@gmail.com

# libraries--------------------------------------------------------------------
library(shiny)
library(shiny.semantic)
library(magrittr)
library(stringr)
library(testthat)

# setting global options
options(shiny.maxRequestSize = 900*1024^2)
options(semantic.themes =  TRUE)

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "all.RData")
load("all.RData", .GlobalEnv)

# running tests from the inst/tests/tests.R file (uses 'testthat' package)
# I tested this before deploying, all tests were passed. But, because the
# 'test_file' function takes a local file path, I have commented it before
# deployment to R Shiny, to avoid local path errors.

# testthat::test_file(path = "inst/tests/tests.R")


gridTemplateMarineData <- shiny.semantic::grid_template(
  default = list(
    areas = rbind(
      c("appLogo", "leafletMap"),
      c("appTitle", "leafletMap"),
      c("appPurposeInfo", "leafletMap"),
      c("appPurposeInfo", "leafletMap"),
      c("contactInfo", "contactInfo")
    ),
    cols_width = c("600px",  "auto"),
    rows_height = c("200px", "60px", "400px", "800px", "auto")
  ),

  mobile = list(
    areas = rbind(
      "appLogo",
      "appTitle",
      "appPurposeInfo",
      "leafletMap",
      "contactInfo"
    ),

    rows_height = c("150px", "50px", "500px", "300px", "auto"),
    cols_width = c("100%")
  )
)








# making a marine data ui module-----------------------------------------------

# Contains a header followed by a card, a map, and 2 dropdown menus.
# "Ship Name" dropdown menu list gets automatically updated, once a value for
# "Ship Type" is selected. The Map gets rendered automatically.
marine_ui <- function(id){
  shiny.semantic::semanticPage(

    grid(
      gridTemplateMarineData,
      appLogo = tagList(
        img(src = "logo.png", height = 150, width = 150)
        ),
      appTitle = tagList(
        h1("Ships Travel Data Visualizer"),
        shiny::tags$hr()
        ),
      appPurposeInfo =
        tagList(
          shiny::tags$br(),
          shiny::tags$h3("Ship Type"),
          shiny.semantic::dropdown_input(NS(id, "ship_type_dropdown"),
                                         unique(cleaned_data$ship_type), value = "Cargo"),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$h3("Ship Name"),
          shiny.semantic::dropdown_input(NS(id, "ship_name_dd_selected"),
                                         unique((dplyr::filter(cleaned_data, ship_type == "Cargo"))$SHIPNAME),
                                         value = (unique((dplyr::filter(cleaned_data, ship_type == "Cargo"))$SHIPNAME))[1]),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$h3("Travel Statistics"),
          shiny::tags$hr(),
          textOutput(NS(id, "shipStatistics"))

        ),
      leafletMap = leaflet::leafletOutput(NS(id, "leafletMap"), height = "95vh", width = "75vw"),
      contactInfo = textOutput(NS(id, "contactInfo"))

    )

)

}

# marine data server module----------------------------------------------------
marine_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

   observeEvent(input$ship_type_dropdown, {
     update_dropdown_input(session, "ship_name_dd_selected",
                           choices = unique((dplyr::filter(cleaned_data, ship_type == input$ship_type_dropdown))$SHIPNAME),
                           value = (unique((dplyr::filter(cleaned_data, ship_type == input$ship_type_dropdown))$SHIPNAME))[1])

   })





    # Automatically update "ship_name" dropdown menu list once
    # "ship_type" is selected.
    # output$ship_name_dropdown <- shiny::renderUI({
    #
    #   ns <- session$ns
    #   shiny.semantic::dropdown_input(ns("ship_name_dd_selected"),
    #                                  unique(cleaned_data[(cleaned_data$ship_type == input$ship_type_dropdown),
    #                                                      "SHIPNAME"]),
    #                                  value = unique(
    #                                    cleaned_data[(cleaned_data$ship_type == input$ship_type_dropdown),
    #                                                 "SHIPNAME"])[[1]][1]
    #   )
    #
    # })

    # creating a filtered dataset that calculates max distance for choices selected.
    data_for_leaflet_map <- shiny::reactive({max_dist_travelled(
      cleaned_data = cleaned_data, filter_ship_type = input$ship_type_dropdown,
      filter_ship_name = input$ship_name_dd_selected)})

    # render the leaflet map
    output$leafletMap <- leaflet::renderLeaflet({


      icons <- leaflet::awesomeIcons(
        icon = 'fa-ship',
        iconColor = 'black',
        library = 'fa'
      )

      leafletMapDisp <- eventReactive(c(input$ship_type_dropdpwn, input$ship_name_dd_selected ), {

        leaflet::leaflet(width = 10, height = 10) %>%
          leaflet::setView(
            lng = unlist(data_for_leaflet_map()[[2]][(1:2), 1])[[1]][1],
            lat = unlist(data_for_leaflet_map()[[2]][(1:2), 2])[[1]][1] ,
            zoom = 12) %>%
          leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
          leaflet::addAwesomeMarkers(
            data = data_for_leaflet_map()[[2]][, (1:2)], icon = icons,
            label = stringr::str_c("Shipname: ",
                                   data_for_leaflet_map()[[2]]$SHIPNAME[1], "---",
                                   "Departing from: ", data_for_leaflet_map()[[2]]$DESTINATION[1],
                                   "---", "Arriving at: ", data_for_leaflet_map()[[2]]$DESTINATION[2],
                                   "---", "Distance Travelled (in meters): ",
                                   data_for_leaflet_map()[[1]])
          ) %>% leaflet::addPolylines(lng =
                                        unlist(data_for_leaflet_map()[[2]][(1:2), 1]),
                                      lat = unlist(data_for_leaflet_map()[[2]][(1:2), 2]))

      })

      return(leafletMapDisp())

    })

    output$shipStatistics <- renderText({

     sprintf(str_c("The maximum distance travelled by the ", input$ship_type_dropdown, " ship: ", input$ship_name_dd_selected,
     " was ", round(data_for_leaflet_map()[[1]]), " meters ", " from ",
     data_for_leaflet_map()[[2]]$DESTINATION[1], " to ", data_for_leaflet_map()[[2]]$DESTINATION[2]))


    })


  })

}

# Modules definition complete=================================================#
###############################################################################


# ui for the shiny app containing the above "marine data ui module"
# Note that: id for the marine data ui module matches the marine data
# server module.
ui <- shiny.semantic::semanticPage(
  theme = "cerulean",
  marine_ui("m1")
)

# server script for the shiny app containg the "marine data server module".
# Note that: id for the marine data ui module matches the marine data
# server module.
server <- shinyServer(function(input, output, session){
  marine_server("m1")
})

# create a Shiny app object
shinyApp(ui = ui, server = server)

