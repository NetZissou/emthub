equityMapUI <- function(id) {

  bslib::layout_columns(

    col_widths = c(9, 3),

    bslib::card(

      bslib::card_body(
        class = "p-0",
        leaflet::leafletOutput(shiny::NS(id, "equity_map"))
      ),
      full_screen = TRUE
    ),

    bslib::card(
      title = NULL,
      full_screen = TRUE,

      # shiny::varSelectInput(
      #   "var", "Select variable",
      #   mtcars
      # ),

      bslib::card_body(
        min_height = 80,
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::actionButton(
            shiny::NS(id, "apply"),
            label = "Apply"
          ),

          shiny::actionButton(
            shiny::NS(id, "reset"),
            label = "Reset"
          )
        )
      ),
      bslib::card_body(
        shiny::selectInput(
          shiny::NS(id, "selection_hub"), label = "Hub Selection",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
          multiple = TRUE
        ),

        shiny::selectInput(
          shiny::NS(id, "selection_county"), label = "County Selection",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
          multiple = TRUE
        ),

        shiny::selectInput(
          shiny::NS(id, "selection_vax_type"), label = "Vaccine Type",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_type,
          multiple = TRUE
        ),

        shiny::selectInput(
          shiny::NS(id, "selection_point_of_interest"), label = "Point of Interest Type",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$point_of_interest,
          multiple = TRUE
        ),

        shiny::sliderInput(
          shiny::NS(id, "range_svi"),
          "Social Volnerability Index-2018 (Recalculated)",
          min = 0,
          max = 1,
          value = c(0,1)
        ),

        # shiny::selectInput(
        #   shiny::NS(id, "selection_last_3wks_quantile"), label = "Cases Last 3 Weeks Quantile",
        #   choices = NULL,
        #   multiple = TRUE
        # ),

        # shiny::selectInput(
        #   shiny::NS(id, "selection_booster_updake_category"), label = "Bivalent Booster Uptake Category",
        #   choices = NULL,
        #   multiple = TRUE
        # ),

        shiny::sliderInput(
          shiny::NS(id, "range_hispanic_latino"),
          "% Hispanic or Latino",
          min = 0,
          max = 1,
          value = c(0,1)
        ),

        shiny::sliderInput(
          shiny::NS(id, "range_english"),
          "% Households Speaking Limited English",
          min = 0,
          max = 1,
          value = c(0,1)
        ),


        shiny::selectInput(
          shiny::NS(id, "selection_nearest_vax_by_car"), label = "Car Time to Nearest Pediatric Vax Provider",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_car,
          multiple = TRUE
        ),

        shiny::selectInput(
          shiny::NS(id, "selection_nearest_vax_by_transit"), label = "Transit Time to Nearest Pediatric Vax Provider",
          choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_transit,
          multiple = TRUE
        )
      )
    )

  )

}

equityMapServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

    vax_provider <- get_vax_provider()


    # ======================= #
    # ---- Reset Filters ----
    # ======================= #

    shiny::observeEvent(input$reset, {

      # Reset Select Input
      purrr::walk(
        .x = c(
          "selection_hub",
          "selection_county",
          "selection_vax_type",
          "selection_point_of_interest",
          "selection_nearest_vax_by_car",
          "selection_nearest_vax_by_transit"
        ),
        .f = function(id) {

          shiny::updateSelectInput(
            session = session,
            inputId = id,
            selected = ""
          )
        }
      )


      # Reset Slider Input
      purrr::walk(
        .x = c(
          "range_svi",
          "range_hispanic_latino",
          "range_english"
        ),
        .f = function(id) {

          shiny::updateSliderInput(
            session = session,
            inputId = id,
            value = c(0,1)
          )
        }
      )

    })




    output$equity_map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        # =================== #
        # ---- Map Pane ----
      # =================== #
      leaflet::addMapPane("layer_top", zIndex=420) %>%
        leaflet::addMapPane("layer_bottom",zIndex=410) %>%

        # ====================== #
        # ---- Vax Provider ----
      # ======================= #
      leaflet::addCircleMarkers(
        data = vax_provider,
        lat = ~latitude,
        lng = ~longitude,
        #label = ~ address,
        fillColor = "gray",
        fillOpacity = 1,
        stroke = F,
        group = "Vaccine Providers",
        options = leaflet::pathOptions(pane = "layer_top")
      ) %>%

        leaflet::addLayersControl(
          baseGroups = c(

          ),
          overlayGroups = c(
            "Vaccine Providers"
          ),
          position = "topleft"
        ) %>%
        leaflet::hideGroup(
          c(
            "Vaccine Providers"
          )
        )


    })





  })
}
