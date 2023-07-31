equityMapUI <- function(id) {


  # bslib::layout_sidebar(
  #
  #   sidebar = bslib::sidebar(
  #     position = "right",
  #
  #     title = "Controls",
  #
  #     # shiny::varSelectInput(
  #     #   "var", "Select variable",
  #     #   mtcars
  #     # ),
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_hub"), label = "Hub Selection",
  #       choices = names(mtcars),
  #       multiple = TRUE
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_county"), label = "County Selection",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_vax_type"), label = "Vaccine Type",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_point_of_interest"), label = "Point of Interest Type",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::sliderInput(
  #       shiny::NS(id, "range_svi"),
  #       "Social Volnerability Index-2018 (Recalculated)",
  #       min = 0,
  #       max = 1,
  #       value = c(0,1)
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_last_3wks_quantile"), label = "Cases Last 3 Weeks Quantile",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_booster_updake_category"), label = "Bivalent Booster Uptake Category",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::sliderInput(
  #       shiny::NS(id, "range_hispanic_latino"),
  #       "% Hispanic or Latino",
  #       min = 0,
  #       max = 1,
  #       value = c(0,1)
  #     ),
  #
  #     shiny::sliderInput(
  #       shiny::NS(id, "range_english"),
  #       "% Households Speaking Limited English",
  #       min = 0,
  #       max = 1,
  #       value = c(0,1)
  #     ),
  #
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_nearest_vax_by_car"), label = "Car Time to Nearest Pediatric Vax Provider",
  #       choices = NULL,
  #       multiple = TRUE
  #     ),
  #
  #     shiny::selectInput(
  #       shiny::NS(id, "selection_nearest_vax_by_transit"), label = "Transit Time to Nearest Pediatric Vax Provider",
  #       choices = NULL,
  #       multiple = TRUE
  #     )
  #   ),
  #
  #
  #   bslib::card(
  #
  #     bslib::card_body(
  #       class = "p-0",
  #       leaflet::leafletOutput(shiny::NS(id, "test_map"))
  #     ),
  #     full_screen = TRUE
  #   )
  # )


  bslib::layout_columns(

    col_widths = c(9, 3),

    bslib::card(

      bslib::card_body(
        class = "p-0",
        leaflet::leafletOutput(shiny::NS(id, "test_map"))
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
      shiny::selectInput(
        shiny::NS(id, "selection_hub"), label = "Hub Selection",
        choices = names(mtcars),
        multiple = TRUE
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_county"), label = "County Selection",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_vax_type"), label = "Vaccine Type",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_point_of_interest"), label = "Point of Interest Type",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::sliderInput(
        shiny::NS(id, "range_svi"),
        "Social Volnerability Index-2018 (Recalculated)",
        min = 0,
        max = 1,
        value = c(0,1)
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_last_3wks_quantile"), label = "Cases Last 3 Weeks Quantile",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_booster_updake_category"), label = "Bivalent Booster Uptake Category",
        choices = NULL,
        multiple = TRUE
      ),

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
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectInput(
        shiny::NS(id, "selection_nearest_vax_by_transit"), label = "Transit Time to Nearest Pediatric Vax Provider",
        choices = NULL,
        multiple = TRUE
      )
    )

  )

}

equityMapServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session){

    output$test_map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })
  })
}
