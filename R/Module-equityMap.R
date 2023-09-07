
equityMapUI <- function(id) {

  bslib::page_fillable(

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
        bslib::card_header(
          shiny::helpText(
            "THE INFORMATION BELOW IS SUBJECT TO CHANGE. PLEASE READ THE DOCUMENTATION ON THE DATA SOURCE PAGE AND WATCH THE TRAINING VIDEO BEFORE USING THE TABS AND FUNCTIONS BELOW."
          )
        ),
        bslib::nav_panel(
          title = "Map Filter",

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

            shiny::helpText("Please activate the map layer before filtering the data."),


            shiny::sliderInput(
              shiny::NS(id, "range_svi"),
              "Social Vulnerability Index-2018 (Recalculated)",
              min = 0,
              max = 1,
              value = c(0,1),
              width = "100%"
            ),

            shiny::sliderInput(
              shiny::NS(id, "range_hispanic_latino"),
              "Percent (%) Hispanic or Latino",
              min = 0,
              max = 100,
              value = c(0,100),
              width = "100%"
            ),

            shiny::sliderInput(
              shiny::NS(id, "range_english"),
              "Percent (%) Households Speaking Limited English",
              min = 0,
              max = 100,
              value = c(0,100),
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
          title = "Vax Places",

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
          reactable_searchBar(shiny::NS(id, "vax_provider_table"), placeholder = "Search for providers ..."),
          reactable_csvDownloadButton(shiny::NS(id, "vax_provider_table"), filename = "vaccine_provider.csv"),
          shiny::helpText("Toggle to add places to the map. If you do not see the table below please click the expand button in the bottom-right corner."),
          reactable::reactableOutput(shiny::NS(id, "vax_provider_table"), inline = TRUE)
        ),


        bslib::nav_panel(
          title = "Place Finder",
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                shiny::NS(id, "selection_poi_type"),
                label = "Type",
                choices = emthub::EQUITY_MAP_FILTER_CHOICES$point_of_interest,
                multiple = TRUE
              )
            ),

            shiny::column(
              width = 6,
              shiny::selectInput(
                shiny::NS(id, "selection_poi_hub"),
                label = "Hub",
                choices = emthub::EQUITY_MAP_FILTER_CHOICES$hub,
                multiple = TRUE
              )
            )
          ),

          reactable_searchBar(shiny::NS(id, "poi_table"), placeholder = "Search for Place of Interest ..."),
          reactable_csvDownloadButton(shiny::NS(id, "poi_table"), filename = "poi.csv"),
          shiny::helpText("Toggle to add places to the map. If you do not see the table below please click the expand button in the bottom-right corner."),
          reactable::reactableOutput(shiny::NS(id, "poi_table"), inline = TRUE)
        ),

        bslib::nav_panel(
          title = "Access Tool",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::textInput(
                shiny::NS(id, "iso_location"),
                label = "Full Address",
                placeholder = "street, city, state, zip",
                width = "100%"
              ),
              shiny::textInput(
                shiny::NS(id, "iso_label"),
                label = "Label",
                placeholder = "Label the isochrone, not required",
                width = "100%"
              )
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

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::actionButton(
                shiny::NS(id, "iso_add"),
                label = "Add Isochrones",
                width = "100%"
              )
            ),

            shiny::column(
              width = 6,
              shiny::actionButton(
                shiny::NS(id, "iso_clear"),
                label = "Clear Isochrones",
                width = "100%"
              )
            )
          )

        )
      )

    )
  )

}

equityMapServer <- function(id, ct_level_data, shapefile_list) {

  shiny::moduleServer(id, function(input, output, session){

    # ============== #
    # ---- Data ----
    # ============== #

    # > Point Level
    vax_provider <- get_vax_provider(parquet = TRUE)
    vax_provider_reactive <- shiny::reactiveValues(
      filtered = NULL
    )
    poi <- get_point_of_interest(parquet = TRUE)
    poi_reactive <- shiny::reactiveValues(
      filtered = NULL
    )

    # > Regional Rate
    # > County Level
    covid_data <- get_covid_data_county(parquet = TRUE)

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

    pal_hispanic_latino <-
      emthub::PAL$pal_hispanic_latino

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


      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Vaccine Providers") %>%
        leaflet::clearGroup("Point of Interest") %>%
        leaflet::clearGroup("Isochron") %>%
        leaflet::clearGroup("ISO Resource - Vaccine Providers") %>%
        leaflet::clearGroup("ISO Resource - POI") %>%
        leaflet.extras2::stopSpinner()

      #shiny::showNotification("Completed", type = "message")
    })

    # ======================= #
    # ---- Apply Filters ----
    # ======================= #
    update_vax_provider <- function(
      type, hubs
    ) {

      filtered <- vax_provider

      if (!rlang::is_empty(type)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Vaccine_Type %in% type
          )
      }

      if (!rlang::is_empty(hubs)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Hub %in% hubs
          )
      }

      if (rlang::is_empty(type) && rlang::is_empty(hubs)) {
        vax_provider_reactive$filtered <- NULL
      } else {
        vax_provider_reactive$filtered <- filtered
      }
    }

    update_poi <- function(type, hubs) {

      filtered <- poi

      if (!rlang::is_empty(type)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Type %in% type
          )
      }

      if (!rlang::is_empty(hubs)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$hub %in% hubs
          )
      }

      if (rlang::is_empty(type) && rlang::is_empty(hubs)) {
        poi_reactive$filtered <- NULL
      } else {
        poi_reactive$filtered <- filtered
      }

    }


    update_HUB_highlight <- function(hub) {

      if (!rlang::is_empty(hub) && all(hub != "")) {
        SF_HUB_highlight <-
          shapefile_list$SF_HUB %>%
          dplyr::filter(
            .data$HUB_Name %in% hub
          )

        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup( "Hub Highlight") %>%
          leaflet::addPolylines(
            data = SF_HUB_highlight,
            group = "Hub Highlight",
            color = "black",
            weight = 3,
            opacity = 1,
            dashArray = "1",
            fillOpacity = 0,

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
          shapefile_list$SF_COUNTY %>%
          dplyr::filter(
            .data$COUNTY %in% county
          )

        leaflet::leafletProxy("equity_map") %>%
          leaflet::clearGroup("County Highlight") %>%
          leaflet::addPolylines(
            data = SF_COUNTY_highlight,
            group = "County Highlight",
            color = "#08306b",
            weight = 3,
            opacity = 1,
            dashArray = "1",
            fillOpacity = 0,

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
        ct_level_data %>%
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
          fillOpacity = 0.7,
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
          options = leaflet::pathOptions(pane = "layer_middle")
        ) %>%
        leaflet.extras2::stopSpinner()
    }

    update_household_english <- function(range) {


      household_english_data_filtered <-
        ct_level_data %>%
        dplyr::select(
          .data$GEOID, .data$prcnt_limited_english_speaking_households
        ) %>%
        dplyr::filter(
          .data$prcnt_limited_english_speaking_households >= range[1],
          .data$prcnt_limited_english_speaking_households <= range[2],
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
          fillOpacity = 0.7,
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
          options = leaflet::pathOptions(pane = "layer_middle")
        ) %>%
        leaflet.extras2::stopSpinner()
    }

    update_hispanic_latino <- function(range) {

      hispanic_latino_data_filtered <-
        ct_level_data %>%
        dplyr::select(
          .data$GEOID, .data$percent_hispanic_or_latino
        ) %>%
        dplyr::filter(
          .data$percent_hispanic_or_latino >= range[1],
          .data$percent_hispanic_or_latino <= range[2],
        )

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Pct Hispanic or Latino") %>%
        leaflet::addPolygons(
          data = hispanic_latino_data_filtered,
          group = "Pct Hispanic or Latino",
          stroke = TRUE,
          color = ~pal_hispanic_latino(percent_hispanic_or_latino),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.7,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            "<b>Pct Hispanic Or Latino: </b>",
            round(percent_hispanic_or_latino, 2), "%"
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
          options = leaflet::pathOptions(pane = "layer_middle")
        ) %>%
        leaflet.extras2::stopSpinner()
    }

    update_vax_provider_travel_time_by_car <- function(type) {

      if (!rlang::is_empty(type) && all(type != "")) {

        vax_provider_travel_time_by_car_filtered <-
          ct_level_data %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_car
          ) %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_car %in% type
          )
      } else {
        vax_provider_travel_time_by_car_filtered <-
          ct_level_data %>%
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
          fillOpacity = 0.7,
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
          options = leaflet::pathOptions(pane = "layer_middle")
        ) %>%
        leaflet.extras2::stopSpinner()

    }


    update_vax_provider_travel_time_by_transit <- function(type) {

      if (!rlang::is_empty(type) && all(type != "")) {

        vax_provider_travel_time_by_transit_filtered <-
          ct_level_data %>%
          dplyr::select(
            .data$GEOID,
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit
          ) %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit %in% type
          )
      } else {
        vax_provider_travel_time_by_transit_filtered <-
          ct_level_data %>%
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
          fillOpacity = 0.7,
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
          options = leaflet::pathOptions(pane = "layer_middle")
        ) %>%
        leaflet.extras2::stopSpinner()
    }


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
      update_hispanic_latino(input$range_hispanic_latino)
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
        type = input$iso_type,
        resource_params = list(
          index_sf = shapefile_list$SF_CT,
          index_sf_key = "GEOID",
          resource_tbl = vax_provider,
          resource_tbl_key = "Census_Tract",
          resource_tbl_coords = c("longitude", "latitude")
        ),
        label = input$iso_label
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

    shiny::observeEvent(input$iso_clear, {

      leaflet::leafletProxy("equity_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Isochron") %>%
        leaflet::clearGroup("ISO Resource - Vaccine Providers") %>%
        leaflet::clearGroup("ISO Resource - POI") %>%
        leaflet.extras2::stopSpinner()
    })

    shiny::observe({
      update_vax_provider(
        input$selection_vax_provider_type,
        input$selection_vax_provider_hub
      )
    })

    shiny::observe({
      update_poi(
        input$selection_poi_type,
        input$selection_poi_hub
      )
    })



    # ============================ #
    # ---- Vax Provider Table ----
    # ============================ #

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
        dplyr::collect() %>%
        reactable::reactable(
          # Table Format
          # height = 600,
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
          vax_provider_reactive$filtered[selected_vax_provider_index(),] %>%
          dplyr::select(
            .data$provider_location_guid,
            .data$latitude,
            .data$longitude,
            .data$popup
          ) %>%
          dplyr::collect() %>%
          dplyr::distinct()


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


    # =================== #
    # ---- POI Table ----
    # =================== #
    output$poi_table <- reactable::renderReactable({
      shiny::req(!rlang::is_empty(poi_reactive$filtered))

      poi_reactive$filtered %>%
        dplyr::arrange(.data$Company) %>%
        dplyr::select(
          Name = .data$Company,
          Type = .data$Type,
          Hub = .data$hub,
          City = .data$City,
          County = .data$county,
          State = .data$State,
          Zip = .data$Zipcode,
          `Census Tract` = .data$census_tract,
          Addr = .data$`Address Line 1`
        ) %>%
        dplyr::collect() %>%
        reactable::reactable(
          # height = 600,
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

    selected_poi_index <- shiny::reactive(reactable::getReactableState("poi_table", "selected"))
    selected_poi <- shiny::reactiveValues(value = NULL)
    shiny::observe({

      if (!is.null(selected_poi_index())) {

        selected_poi$value <-
          poi_reactive$filtered[selected_poi_index(),] %>%
          dplyr::collect()


      } else {
        selected_poi$value <- NULL
      }


      if (!rlang::is_empty(selected_poi$value)) {

        leaflet::leafletProxy("equity_map") %>%
          leaflet.extras2::addSpinner() %>%
          leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
          leaflet::clearGroup("Point of Interest") %>%
          leaflet::addAwesomeMarkers(
            data = selected_poi$value,
            group = "Point of Interest",
            lng = ~Longitude, lat = ~Latitude,
            icon = leaflet::makeAwesomeIcon(
              text = fontawesome::fa("location-crosshairs"),
              iconColor = 'white',
              markerColor = "black"
            ),
            popup = ~popup,
            clusterOptions = leaflet::markerClusterOptions(),
            clusterId = "vaxCluster",
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
          leaflet::clearGroup("Point of Interest")
      }

    })


    # ==================== #
    # ---- Equity Map ----
    # ==================== #
    output$equity_map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        data = ct_level_data,
        options = leaflet::leafletOptions(
          zoomControl = TRUE
        )
      ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        #leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri, group = "Base Map") %>%
        # =================== #
        # ---- Map Pane ----
      # =================== #
      leaflet::addMapPane("layer_top", zIndex=420) %>%
        leaflet::addMapPane("layer_middle",zIndex=410) %>%
        leaflet::addMapPane("layer_bottom",zIndex=400) %>%

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
        fillOpacity = 0.7,
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
        options = leaflet::pathOptions(pane = "layer_middle")
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
        fillOpacity = 0.7,
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
        options = leaflet::pathOptions(pane = "layer_middle")
      ) %>%
        # ========================== #
        # ---- HUB Service Area ----
      # ========================== #

      leaflet::addPolygons(
        data = shapefile_list$SF_HUB,
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

        leaflet::addLayersControl(
          baseGroups = c(
            "Social Vulnerability Index (2018)",
            "County COVID-19 Case Rate (Last 3wk)",
            "County Bivalent Booster Uptake",
            "Pct Households Speaking Limited English",
            "Pct Hispanic or Latino",
            "Min. (Car) to Nearest Pediatric Vax Provider",
            "Min. (Tranist) to Nearest Pediatric Vax Provider",
            "Base Map"
          ),
          overlayGroups = c(
            "Hub Service Area",
            "Vaccine Providers",
            "Point of Interest",
            "Isochron",
            "ISO Resource - Vaccine Providers",
            "ISO Resource - POI"
          ),
          position = "topleft"
        ) %>%
        leaflet::hideGroup(
          c(
            "Hub Service Area"
          )
        ) %>%
        # leaflet::addEasyButton(leaflet::easyButton(
        #   position = "bottomleft",
        #   states = list(
        #     leaflet::easyButtonState(
        #       stateName="unfrozen-markers",
        #       icon="ion-toggle",
        #       title="Freeze Clusters",
        #       onClick = leaflet::JS("
        #   function(btn, map) {
        #     var clusterManager =
        #       map.layerManager.getLayer('cluster', 'vaxCluster');
        #     clusterManager.freezeAtZoom();
        #     btn.state('frozen-markers');
        #   }")
        #     ),
        #     leaflet::easyButtonState(
        #       stateName="frozen-markers",
        #       icon="ion-toggle-filled",
        #       title="UnFreeze Clusters",
        #       onClick = leaflet::JS("
        #   function(btn, map) {
        #     var clusterManager =
        #       map.layerManager.getLayer('cluster', 'vaxCluster');
        #     clusterManager.unfreeze();
        #     btn.state('unfrozen-markers');
        #   }")
        #     )
        #   )
        # )) %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = 'Print Map',
            position = 'bottomleft',
            exportOnly = TRUE
          )
        )


    })





  })
}
