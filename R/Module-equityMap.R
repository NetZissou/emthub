# equityMapUItest <- function(id) {
#
#   bslib::layout_columns(
#
#     col_widths = c(9, 3),
#
#     bslib::card(
#
#       bslib::card_body(
#         class = "p-0",
#         leaflet::leafletOutput(shiny::NS(id, "equity_map"))
#       ),
#       full_screen = TRUE
#     ),
#
#     bslib::card(
#       title = NULL,
#       full_screen = TRUE,
#
#       # shiny::varSelectInput(
#       #   "var", "Select variable",
#       #   mtcars
#       # ),
#
#       bslib::card_body(
#         min_height = 50,
#         bslib::layout_column_wrap(
#           width = 1/2,
#           shiny::actionButton(
#             shiny::NS(id, "apply"),
#             label = "Apply"
#           ),
#
#           shiny::actionButton(
#             shiny::NS(id, "reset"),
#             label = "Reset"
#           )
#         )
#       ),
#       bslib::card_body(
#         shiny::selectInput(
#           shiny::NS(id, "selection_hub"), label = "Hub Selection",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
#           multiple = TRUE
#         ),
#
#         shiny::selectInput(
#           shiny::NS(id, "selection_county"), label = "County Selection",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
#           multiple = TRUE
#         ),
#
#         shiny::selectInput(
#           shiny::NS(id, "selection_vax_type"), label = "Vaccine Type",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_type,
#           multiple = TRUE
#         ),
#
#         shiny::selectInput(
#           shiny::NS(id, "selection_point_of_interest"), label = "Point of Interest Type",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$point_of_interest,
#           multiple = TRUE
#         ),
#
#         shiny::sliderInput(
#           shiny::NS(id, "range_svi"),
#           "Social Volnerability Index-2018 (Recalculated)",
#           min = 0,
#           max = 1,
#           value = c(0,1)
#         ),
#
#         # shiny::selectInput(
#         #   shiny::NS(id, "selection_last_3wks_quantile"), label = "Cases Last 3 Weeks Quantile",
#         #   choices = NULL,
#         #   multiple = TRUE
#         # ),
#
#         # shiny::selectInput(
#         #   shiny::NS(id, "selection_booster_updake_category"), label = "Bivalent Booster Uptake Category",
#         #   choices = NULL,
#         #   multiple = TRUE
#         # ),
#
#         shiny::sliderInput(
#           shiny::NS(id, "range_hispanic_latino"),
#           "% Hispanic or Latino",
#           min = 0,
#           max = 1,
#           value = c(0,1)
#         ),
#
#         shiny::sliderInput(
#           shiny::NS(id, "range_english"),
#           "% Households Speaking Limited English",
#           min = 0,
#           max = 1,
#           value = c(0,1)
#         ),
#
#
#         shiny::selectInput(
#           shiny::NS(id, "selection_nearest_vax_by_car"), label = "Car Time to Nearest Pediatric Vax Provider",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_car,
#           multiple = TRUE
#         ),
#
#         shiny::selectInput(
#           shiny::NS(id, "selection_nearest_vax_by_transit"), label = "Transit Time to Nearest Pediatric Vax Provider",
#           choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_transit,
#           multiple = TRUE
#         )
#       )
#     )
#
#   )
#
# }

equityMapUI <- function(id) {

  bslib::layout_columns(

    col_widths = c(8, 4),

    bslib::card(

      bslib::card_body(
        class = "p-0",
        leaflet::leafletOutput(shiny::NS(id, "equity_map"))
      ),
      full_screen = TRUE
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      # bslib::card_header(
      #   bslib::card_body(
      #     min_height = 60,
      #     shiny::actionButton(
      #       shiny::NS(id, "reset"),
      #       label = "Reset"
      #     )
      #     # bslib::layout_column_wrap(
      #     #   width = 1/2,
      #     #   shiny::actionButton(
      #     #     shiny::NS(id, "apply"),
      #     #     label = "Apply"
      #     #   ),
      #     #
      #     #   shiny::actionButton(
      #     #     shiny::NS(id, "reset"),
      #     #     label = "Reset"
      #     #   )
      #     # )
      #   )
      # ),
      bslib::nav_panel(
        title = "Layers Control",

        bslib::card_body(

          shiny::actionButton(
            shiny::NS(id, "reset"),
            label = "Reset"
          ),

          shiny::selectInput(
            shiny::NS(id, "selection_hub"), label = "Hub Selection",
            choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
            multiple = TRUE,
            width = "100%"
          ),

          shiny::selectInput(
            shiny::NS(id, "selection_county"), label = "County Selection",
            choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
            multiple = TRUE,
            width = "100%"
          ),


          shiny::sliderInput(
            shiny::NS(id, "range_svi"),
            "Social Volnerability Index-2018 (Recalculated)",
            min = 0,
            max = 1,
            value = c(0,1),
            width = "100%"
          ),

          shiny::sliderInput(
            shiny::NS(id, "range_hispanic_latino"),
            "% Hispanic or Latino",
            min = 0,
            max = 1,
            value = c(0,1),
            width = "100%"
          ),

          shiny::sliderInput(
            shiny::NS(id, "range_english"),
            "Pct Households Speaking Limited English",
            min = 0,
            max = 1,
            value = c(0,1),
            width = "100%"
          ),


          shiny::selectInput(
            shiny::NS(id, "selection_nearest_vax_by_car"), label = "Car Time to Nearest Pediatric Vax Provider",
            choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_car,
            multiple = TRUE,
            width = "100%"
          ),

          shiny::selectInput(
            shiny::NS(id, "selection_nearest_vax_by_transit"), label = "Transit Time to Nearest Pediatric Vax Provider",
            choices = emthub::EQUITY_MAP_FILTER_CHOICES$nearest_vax_by_transit,
            multiple = TRUE,
            width = "100%"
          )
        )
      ),
      bslib::nav_panel(
        title = "Vaccine Providers",

        shiny::fluidRow(
          #width = 1/2,
          shiny::column(
            width = 6,
            shiny::selectInput(
              shiny::NS(id, "selection_vax_provider_type"),
              label = "Bivalent Booster Type",
              choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_type,
              multiple = TRUE
            )
          ),

          shiny::column(
            width = 6,
            shiny::selectInput(
              shiny::NS(id, "selection_vax_provider_hub"),
              label = "Hub",
              choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
              multiple = TRUE
            )
          )

        ),

        # bslib::layout_columns(
        #   col_widths = c(6,6),
        #   #row_heights = c(1,1,1,1,1,1,3),
        #   shiny::selectInput(
        #     shiny::NS(id, "selection_vax_provider_type"),
        #     label = "Type",
        #     choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_type,
        #     multiple = TRUE
        #   ),
        #
        #   shiny::selectInput(
        #     shiny::NS(id, "selection_vax_provider_hub"),
        #     label = "Hub",
        #     choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
        #     multiple = TRUE
        #   )
        # ),

        # shiny::selectInput(
        #   shiny::NS(id, "selection_vax_ct"),
        #   label = "Census Tract",
        #   choices = emthub::EQUITY_MAP_FILTER_CHOICES$ct,
        #   multiple = TRUE
        # ),
        #
        # shiny::selectInput(
        #   shiny::NS(id, "selection_vax_city"),
        #   label = "City",
        #   choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_city,
        #   multiple = TRUE
        # ),
        #
        # shiny::selectInput(
        #   shiny::NS(id, "selection_vax_zip"),
        #   label = "Zip Code",
        #   choices = emthub::EQUITY_MAP_FILTER_CHOICES$zip,
        #   multiple = TRUE
        # ),
        #
        # shiny::selectInput(
        #   shiny::NS(id, "selection_vax_county"),
        #   label = "County",
        #   choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
        #   multiple = TRUE
        # ),


        # shiny::actionButton(
        #   shiny::NS(id, "update_vax_provider_tbl"),
        #   label = "Find Provider"
        # ),

        reactable_csvDownloadButton(shiny::NS(id, "vax_provider_table"), filename = "vaccine_provider.csv"),
        #shiny::tags$hr(),
        reactable_searchBar(shiny::NS(id, "vax_provider_table"), placeholder = "Search for providers ..."),
        shiny::helpText("Toggle to add places to the map"),
        reactable::reactableOutput(shiny::NS(id, "vax_provider_table"))
      ),


      bslib::nav_panel(
        title = "Places",
        bslib::card_body(
        )
      ),

      bslib::nav_panel(
        title = "ISO",

        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::textInput(
              shiny::NS(id, "iso_location"),
              label = "Full Address",
              placeholder = "street, city, state, zip",
              width = "100%"
            ),
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              shiny::NS(id, "iso_lat"),
              label = "Latitude ",
              value = NULL
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              shiny::NS(id, "iso_lng"),
              label = "Longitude ",
              value = NULL
            )
          ),
          shiny::helpText("Click location on the map to obtain lat, lng. Leave empty if full address is provided.")
        ),

        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::selectInput(
              shiny::NS(id, "iso_range_type"),
              label = "Measurement",
              choices = c("distance", "time"),
              multiple = FALSE
            ),
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              shiny::NS(id, "iso_range"),
              label = "Range (min/km)",
              value = 5
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              shiny::NS(id, "iso_type"),
              label = "Transportation",
              choices = c("car", "walk", "cycle"),
              multiple = FALSE
            )
          )
        ),

        shiny::actionButton(
          shiny::NS(id, "iso_add"),
          label = "Add Isochrones"
        )

        # bslib::layout_columns(
        #   col_widths = c(12,6,6,12,6,6,6,6),
        #   shiny::textInput(
        #     shiny::NS(id, "iso_location"),
        #     label = "Full Address",
        #     placeholder = "street, city, state, zip"
        #   ),
        #
        #   shiny::numericInput(
        #     shiny::NS(id, "iso_lat"),
        #     label = "Latitude ",
        #     value = NULL
        #   ),
        #   shiny::numericInput(
        #     shiny::NS(id, "iso_lng"),
        #     label = "Longitude ",
        #     value = NULL
        #   ),
        #   shiny::helpText("Click location on the map to obtain lat, lng. Leave empty if full address is provided."),
        #
        #   shiny::selectInput(
        #     shiny::NS(id, "iso_range_type"),
        #     label = "Measurement",
        #     choices = c("distance", "time"),
        #     multiple = FALSE
        #   ),
        #
        #   shiny::numericInput(
        #     shiny::NS(id, "iso_range"),
        #     label = "Range (min/km)",
        #     value = 5
        #   ),
        #
        #   shiny::selectInput(
        #     shiny::NS(id, "iso_type"),
        #     label = "Transportation Type",
        #     choices = c("car", "walk", "cycle"),
        #     multiple = FALSE
        #   ),
        #
        #   shiny::actionButton(
        #     shiny::NS(id, "iso_add"),
        #     label = "Add Isochrones"
        #   )
        # )
      )
    )

  )

}

equityMapServer <- function(id, ct_level_data) {

  shiny::moduleServer(id, function(input, output, session){

    # ============== #
    # ---- Data ----
    # ============== #

    # > Shapefiles
    SF_HUB <- get_sf_hub()
    SF_CT <- get_sf_ct()
    SF_COUNTY <- get_sf_county()


    # > Point Level
    vax_provider <- get_vax_provider()
    vax_provider_reactive <- shiny::reactiveValues(
      filtered = NULL
    )
    #point_of_interest <-get_point_of_interest()


    # > Regional Rate
    # > County Level
    covid_data <- get_covid_data_county()

    # > Census Tract Level
    # NOTE: USE ct_level_data()

    # svi_data <- get_SVI()
    # household_english_data <- get_pct_household_limited_english()
    # vax_provider_travel_time_by_car <-
    #   get_vax_provider_travel_time_by_car()
    # vax_provider_travel_time_by_transit <-
    #   get_vax_provider_travel_time_by_transit()





    # > Pal
    # pal_sf_hub <-
    #   leaflet::colorFactor(
    #     palette = c('#a6cee3', "#808080", '#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'),
    #     domain = unique(SF_HUB$HUB_Name),
    #     levels = unique(SF_HUB$HUB_Name)
    #   )

    pal_svi <-
      emthub::PAL$pal_svi

    pal_household_english <-
      emthub::PAL$pal_household_english

    pal_covid_case_rate <-
      emthub::PAL$pal_covid_case_rate

    pal_booster_rate <-
      emthub::PAL$pal_booster_rate

    pal_vax_provider_travel_time_by_car <-
      emthub::PAL$pal_vax_provider_travel_time_by_car

    pal_vax_provider_travel_time_by_transit <-
      emthub::PAL$pal_vax_provider_travel_time_by_transit

    # ======================= #
    # ---- Reset Filters ----
    # ======================= #

    shiny::observeEvent(input$reset, {

      shiny::showNotification("Map Resetting ...", type = "message")
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
            selected = character(0)
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


      # Apply Changes
      update_HUB_highlight(input$selection_hub)
      update_county_highlight(input$selection_county)

      update_SVI(input$range_svi)
      update_household_english(input$range_english)

      update_vax_provider_travel_time_by_car(input$selection_nearest_vax_by_car)
      update_vax_provider_travel_time_by_transit(input$selection_nearest_vax_by_transit)


      #shiny::showNotification("Completed", type = "message")
    })

    # ======================= #
    # ---- Apply Filters ----
    # ======================= #
    update_vax_provider <- function(
      type, ct, city, zip, county, hubs
    ) {

      filtered <- vax_provider

      if (!rlang::is_empty(type)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Vaccine_Type %in% type
          )
      }

      # if (!rlang::is_empty(ct)) {
      #
      #   filtered <-
      #     filtered %>%
      #     dplyr::filter(
      #       .data$Census_Tract %in% ct
      #     )
      # }
      #
      # if (!rlang::is_empty(city)) {
      #
      #   filtered <-
      #     filtered %>%
      #     dplyr::filter(
      #       .data$City %in% city
      #     )
      # }
      #
      # if (!rlang::is_empty(zip)) {
      #
      #   filtered <-
      #     filtered %>%
      #     dplyr::filter(
      #       .data$Zip %in% zip
      #     )
      # }
      #
      # if (!rlang::is_empty(county)) {
      #
      #   filtered <-
      #     filtered %>%
      #     dplyr::filter(
      #       .data$County %in% county
      #     )
      # }

      if (!rlang::is_empty(hubs)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Hub %in% hubs
          )
      }

      vax_provider_reactive$filtered <- filtered
    }


    update_HUB_highlight <- function(hub) {

      if (!rlang::is_empty(hub) && all(hub != "")) {
        SF_HUB_highlight <-
          SF_HUB %>%
          dplyr::filter(
            .data$HUB_Name %in% hub
          )

        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup( "Hub Highlight") %>%
          leaflet::addPolygons(
            data = SF_HUB_highlight,
            group = "Hub Highlight",
            color = "black",
            weight = 2,
            opacity = 1,
            dashArray = "1",
            fillOpacity = 0.1,
            #options = leaflet::pathOptions(pane = "County_districts_polyline"),

            label = ~ paste0(
              "<b>", HUB_Name, "</b>"
            ) %>% lapply(htmltools::HTML),

            labelOptions = leaflet::labelOptions(
              style = list(
                "font-weight" = "normal",
                padding = "3px 8px"
              ),
              textsize = "15px",
              direction = "auto"
            ),

            highlight = leaflet::highlightOptions(
              weight = 3,
              fillOpacity = 0.1,
              color = "black",
              dashArray = "",
              opacity = 0.5,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            options = leaflet::pathOptions(pane = "layer_top")
          )
      } else {
        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup( "Hub Highlight")
      }

    }

    update_county_highlight <- function(county) {

      if (!rlang::is_empty(county) && all(county != "")) {
        SF_COUNTY_highlight <-
          SF_COUNTY %>%
          dplyr::filter(
            .data$COUNTY %in% county
          )

        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup("County Highlight") %>%
          leaflet::addPolygons(
            data = SF_COUNTY_highlight,
            group = "County Highlight",
            color = "#08306b",
            weight = 2,
            opacity = 1,
            dashArray = "1",
            fillOpacity = 0.1,
            #options = leaflet::pathOptions(pane = "County_districts_polyline"),

            label = ~ paste0(
              "<b>", COUNTY, "</b>", "</br>", HUB_Name
            ) %>% lapply(htmltools::HTML),

            labelOptions = leaflet::labelOptions(
              style = list(
                "font-weight" = "normal",
                padding = "3px 8px"
              ),
              textsize = "15px",
              direction = "auto"
            ),

            highlight = leaflet::highlightOptions(
              weight = 3,
              fillOpacity = 0.1,
              color = "black",
              dashArray = "",
              opacity = 0.5,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            options = leaflet::pathOptions(pane = "layer_top")
          )
      } else {
        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup("County Highlight")
      }

    }

    update_SVI <- function(range) {


      svi_data_filtered <-
        ct_level_data() %>%
        dplyr::select(.data$GEOID, .data$recalc_svi_2018) %>%
        dplyr::filter(
          .data$recalc_svi_2018 >= range[1],
          .data$recalc_svi_2018 <= range[2],
        )

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Social Vulnerability Index (2018)") %>%
        leaflet::addPolygons(
          data = svi_data_filtered,
          group = "Social Vulnerability Index (2018)",
          stroke = TRUE,
          color = ~pal_svi(recalc_svi_2018),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>", "<b>SVI: </b>", round(recalc_svi_2018, 4)
          ) %>% lapply(htmltools::HTML),

          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),

          highlight = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.1,
            color = "black",
            dashArray = "",
            opacity = 0.5,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet.extras2::stopSpinner()
    }

    update_household_english <- function(range) {


      household_english_data_filtered <-
        ct_level_data() %>%
        dplyr::select(
          .data$GEOID, .data$prcnt_limited_english_speaking_households
        ) %>%
        dplyr::filter(
          .data$prcnt_limited_english_speaking_households >= 100*range[1],
          .data$prcnt_limited_english_speaking_households <= 100*range[2],
        )

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Pct Households Speaking Limited English") %>%
        leaflet::addPolygons(
          data = household_english_data_filtered,
          group = "Pct Households Speaking Limited English",
          stroke = TRUE,
          color = ~pal_household_english(prcnt_limited_english_speaking_households),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            "<b>Pct Households Speaking Limited English: </b>",
            round(prcnt_limited_english_speaking_households, 4), "%"
          ) %>% lapply(htmltools::HTML),

          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),

          highlight = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.1,
            color = "black",
            dashArray = "",
            opacity = 0.5,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet.extras2::stopSpinner()
    }

    update_vax_provider_travel_time_by_car <- function(type) {

      if (!rlang::is_empty(type) && all(type != "")) {

        vax_provider_travel_time_by_car_filtered <-
          ct_level_data() %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_car
          ) %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_car %in% type
          )
      } else {
        vax_provider_travel_time_by_car_filtered <-
          ct_level_data() %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_car
          )
      }

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Min. (Car) to Nearest Pediatric Vax Provider") %>%
        leaflet::addPolygons(
          data = vax_provider_travel_time_by_car_filtered,
          group = "Min. (Car) to Nearest Pediatric Vax Provider",
          stroke = TRUE,
          color = ~pal_vax_provider_travel_time_by_car(travel_time_to_nearest_ped_vacc_provider_by_car),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            "<b>Travel Time to Nearest Pediatric Vaccine Provider (Car): </b> </br>",
            travel_time_to_nearest_ped_vacc_provider_by_car
          ) %>% lapply(htmltools::HTML),

          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),

          highlight = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.1,
            color = "black",
            dashArray = "",
            opacity = 0.5,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet.extras2::stopSpinner()

    }


    update_vax_provider_travel_time_by_transit <- function(type) {

      if (!rlang::is_empty(type) && all(type != "")) {

        vax_provider_travel_time_by_transit_filtered <-
          ct_level_data() %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit
          ) %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit %in% type
          )
      } else {
        vax_provider_travel_time_by_transit_filtered <-
          ct_level_data() %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit
          )
      }

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Min. (Tranist) to Nearest Pediatric Vax Provider") %>%
        leaflet::addPolygons(
          data = vax_provider_travel_time_by_transit_filtered,
          group = "Min. (Tranist) to Nearest Pediatric Vax Provider",
          stroke = TRUE,
          color = ~pal_vax_provider_travel_time_by_transit(travel_time_to_nearest_ped_vacc_provider_by_transit),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            "<b>Travel Time to Nearest Pediatric Vaccine Provider (Transit): </b> </br>",
            travel_time_to_nearest_ped_vacc_provider_by_transit
          ) %>% lapply(htmltools::HTML),

          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),

          highlight = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.1,
            color = "black",
            dashArray = "",
            opacity = 0.5,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet.extras2::stopSpinner()
    }
    # update_point_of_interest <- function(type) {
    #
    #   leaflet::addCircleMarkers(
    #     data =
    #       point_of_interest %>%
    #       dplyr::filter(
    #         Type %in% input$selection_point_of_interest
    #       ),
    #     lat = ~Latitude,
    #     lng = ~Longitude,
    #     label = ~paste0("<b>", Type, "</b>", "</br>", Company) %>% lapply(htmltools::HTML),
    #     popup = ~popup,
    #     labelOptions = leaflet::labelOptions(
    #       style = list(
    #         "font-weight" = "normal",
    #         padding = "3px 8px"
    #       ),
    #       textsize = "15px",
    #       direction = "auto"
    #     ),
    #     fillColor = "#74a9cf",
    #     fillOpacity = 1,
    #     stroke = F,
    #     group = "Point of Interest",
    #     options = leaflet::pathOptions(pane = "layer_top")
    #   )
    # }


    shiny::observeEvent(input$apply, {

      # if (!rlang::is_empty(input$selection_vax_type)) {
      #
      #   update_vax_provider(type = input$selection_vax_type)
      # }

      # update_HUB_highlight(input$selection_hub)
      # update_county_highlight(input$selection_county)
      #
      # update_SVI(input$range_svi)
      # update_household_english(input$range_english)
      #
      # update_vax_provider_travel_time_by_car(input$selection_nearest_vax_by_car)
      # update_vax_provider_travel_time_by_transit(input$selection_nearest_vax_by_transit)
    })

    shiny::observe({
      update_HUB_highlight(input$selection_hub)
    })

    shiny::observe({
      update_county_highlight(input$selection_county)
    })

    shiny::observe({
      update_SVI(input$range_svi)
    })

    shiny::observe({
      update_household_english(input$range_english)
    })

    shiny::observe({
      update_vax_provider_travel_time_by_car(input$selection_nearest_vax_by_car)
    })

    shiny::observe({
      update_vax_provider_travel_time_by_transit(input$selection_nearest_vax_by_transit)
    })

    shiny::observeEvent(input$iso_add, {
      shiny::req(!rlang::is_empty(input$iso_range_type))

      # Change Units
      if (input$iso_range_type == "distance") {
        # To KM
        range <- input$iso_range*1000
      } else {
        # To Sec
        range <- input$iso_range*60
      }

      addISO(
        map_id = "equity_map",
        full_address = input$iso_location,
        location = c(input$iso_lng, input$iso_lat),
        range = range,
        range_type = input$iso_range_type,
        type = input$iso_type
      )
    })

    shiny::observe({

      click <- input$equity_map_click

      shiny::updateNumericInput(
        session = session,
        inputId = "iso_lat",
        value = click$lat
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "iso_lng",
        value = click$lng
      )
    })

    shiny::observe({
      update_vax_provider(
        input$selection_vax_provider_type,
        input$selection_vax_ct,
        input$selection_vax_city,
        input$selection_vax_zip,
        input$selection_vax_county,
        input$selection_vax_provider_hub
      )
    })




    output$vax_provider_table <- reactable::renderReactable({
      shiny::req(!rlang::is_empty(vax_provider_reactive$filtered))

      vax_provider_reactive$filtered %>%
        dplyr::select(
          Name = .data$Place_Name,
          Type = .data$Vaccine_Type,
          Phone = .data$Phone,
          Website = .data$Website,
          Prescreening = .data$Website,
          Hub = .data$Hub,
          Address = .data$Address,
          City = .data$City,
          County = .data$County,
          State = .data$State,
          Zip = .data$Zip,
          `Census Tract` = .data$Census_Tract
        ) %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          # Selection
          selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = 5, minRows = 5
        )
    })

    selected_vax_provider_index <- shiny::reactive(reactable::getReactableState("vax_provider_table", "selected"))
    selected_vax_provider <- shiny::reactiveValues(value = NULL)
    shiny::observe({

      if (!is.null(selected_vax_provider_index())) {

        selected_vax_provider$value <-
          vax_provider_reactive$filtered[selected_vax_provider_index(),]


      } else {
        selected_vax_provider$value <- NULL
      }


      if (!rlang::is_empty(selected_vax_provider$value)) {

        leaflet::leafletProxy("equity_map") %>%
          leaflet.extras2::addSpinner() %>%
          leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
          leaflet::clearGroup("Vaccine Providers") %>%
          leaflet::addAwesomeMarkers(
            data = selected_vax_provider$value,
            group = "Vaccine Providers",
            lng = ~longitude, lat = ~latitude,
            icon = leaflet::makeAwesomeIcon(
              text = fontawesome::fa("house-medical"),
              iconColor = 'black',
              markerColor = "blue"
            ),
            popup = ~popup,
            clusterOptions = leaflet::markerClusterOptions(),
            labelOptions = leaflet::labelOptions(
              style = list(
                "font-size" = "15px",
                "font-style" = "bold",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            ),
            options = leaflet::pathOptions(pane = "layer_top")
          ) %>%
          leaflet.extras2::stopSpinner()

      } else {

        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup("Vaccine Providers")
      }

      #print(selected_business$value)
    })


    # ==================== #
    # ---- Equity Map ----
    # ==================== #
    output$equity_map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        data = ct_level_data(),
        options = leaflet::leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri, group = "Base Map") %>%
        # =================== #
        # ---- Map Pane ----
      # =================== #
      leaflet::addMapPane("layer_top", zIndex=420) %>%
        leaflet::addMapPane("layer_bottom",zIndex=410) %>%

      # =========================== #
      # ---- Point of Interest ----
      # ============================ #
      # leaflet::addCircleMarkers(
      #   data = point_of_interest,
      #   lat = ~Latitude,
      #   lng = ~Longitude,
      #   label = ~paste0("<b>", Type, "</b>", "</br>", Company) %>% lapply(htmltools::HTML),
      #   popup = ~popup,
      #   labelOptions = leaflet::labelOptions(
      #     style = list(
      #       "font-weight" = "normal",
      #       padding = "3px 8px"
      #     ),
      #     textsize = "15px",
      #     direction = "auto"
      #   ),
      #   fillColor = "#74a9cf",
      #   fillOpacity = 1,
      #   stroke = F,
      #   group = "Point of Interest",
      #   options = leaflet::pathOptions(pane = "layer_top")
      # ) %>%

      # ========================= #
      # ---- COVID Case Rate ----
      # ========================= #
      leaflet::addPolygons(
        data = covid_data,
        group = "County COVID-19 Case Rate (Last 3wk)",
        stroke = TRUE,
        color = ~pal_covid_case_rate(caserate_last3weeks),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", county, "</b>", "</br>",
          "<b>Case Rate (Overall) Per 100K Residents: </b></br>",
          round(caserate_last3weeks, 4)
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%
        # ====================== #
        # ---- Booster Rate ----
      # ====================== #
      leaflet::addPolygons(
        data = covid_data,
        group = "County Bivalent Booster Uptake",
        stroke = TRUE,
        color = ~pal_booster_rate(bivalent_booster_percentage),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", county, "</b>", "</br>",
          "<b>Bivalent Booster Uptake Percentage: </b></br>",
          round(bivalent_booster_percentage, 2), "%"
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%
        # ========================== #
        # ---- HUB Service Area ----
      # ========================== #

      leaflet::addPolygons(
        data = SF_HUB,
        group = "Hub Service Area",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", HUB_Name, "</b>"
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_top")
      ) %>%

        # ============= #
        # ---- SVI ----
      # =============== #
      leaflet::addPolygons(
        #data = svi_data,
        group = "Social Vulnerability Index (2018)",
        stroke = TRUE,
        color = ~pal_svi(recalc_svi_2018),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>", "<b>SVI: </b>", round(recalc_svi_2018, 4)
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        # =========================== #
        # ---- Household English ----
      # ============================ #
      leaflet::addPolygons(
        #data = household_english_data,
        group = "Pct Households Speaking Limited English",
        stroke = TRUE,
        color = ~pal_household_english(prcnt_limited_english_speaking_households),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>",
          "<b>Pct Households Speaking Limited English: </b>",
          round(prcnt_limited_english_speaking_households, 4), "%"
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        # ====================== #
        # ---- Transit Time ----
      # ======================= #

      leaflet::addPolygons(
        #data = vax_provider_travel_time_by_car,
        group = "Min. (Car) to Nearest Pediatric Vax Provider",
        stroke = TRUE,
        color = ~pal_vax_provider_travel_time_by_car(travel_time_to_nearest_ped_vacc_provider_by_car),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>",
          "<b>Travel Time to Nearest Pediatric Vaccine Provider (Car): </b> </br>",
          travel_time_to_nearest_ped_vacc_provider_by_car
        ) %>% lapply(htmltools::HTML),

        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),

        highlight = leaflet::highlightOptions(
          weight = 3,
          fillOpacity = 0.1,
          color = "black",
          dashArray = "",
          opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        leaflet::addPolygons(
          #data = vax_provider_travel_time_by_transit,
          group = "Min. (Tranist) to Nearest Pediatric Vax Provider",
          stroke = TRUE,
          color = ~pal_vax_provider_travel_time_by_transit(travel_time_to_nearest_ped_vacc_provider_by_transit),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            "<b>Travel Time to Nearest Pediatric Vaccine Provider (Transit): </b> </br>",
            travel_time_to_nearest_ped_vacc_provider_by_transit
          ) %>% lapply(htmltools::HTML),

          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),

          highlight = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.1,
            color = "black",
            dashArray = "",
            opacity = 0.5,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%


        leaflet::addLayersControl(
          baseGroups = c(
            "Social Vulnerability Index (2018)",
            "County COVID-19 Case Rate (Last 3wk)",
            "County Bivalent Booster Uptake",
            "Pct Households Speaking Limited English",
            "Min. (Car) to Nearest Pediatric Vax Provider",
            "Min. (Tranist) to Nearest Pediatric Vax Provider",
            "Base Map"
          ),
          overlayGroups = c(
            "Hub Service Area",
            "Vaccine Providers",
            "Point of Interest",
            "Isochron"
          ),
          position = "topleft"
        ) %>%
        leaflet::hideGroup(
          c(
            "Hub Service Area",
            "Point of Interest"
          )
        )


    })





  })
}
