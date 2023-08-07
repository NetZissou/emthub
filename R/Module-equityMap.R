equityMapUItest <- function(id) {

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

    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::card_header(
        bslib::card_body(
          min_height = 60,
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
        )
      ),
      bslib::nav_panel(
        title = "Layers Control",

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


          shiny::sliderInput(
            shiny::NS(id, "range_svi"),
            "Social Volnerability Index-2018 (Recalculated)",
            min = 0,
            max = 1,
            value = c(0,1)
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
            "Pct Households Speaking Limited English",
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
      ),
      bslib::nav_panel(
        title = "Vaccine Providers",
        bslib::card_body(
          min_height = 20,
          bslib::layout_column_wrap(
            width = 1/2,
            shiny::selectInput(
              shiny::NS(id, "selection_vax_provider_type"),
              label = "Type",
              choices = emthub::EQUITY_MAP_FILTER_CHOICES$vax_type,
              multiple = TRUE
            ),

            shiny::selectInput(
              shiny::NS(id, "selection_vax_provider_hub"),
              label = "Hub",
              choices = NULL,
              multiple = TRUE
            )

          )
        ),

        bslib::card_body(
          shiny::tags$hr(),
          shiny::helpText("Toggle to add places to the map"),
          reactable::reactableOutput(shiny::NS(id, "vax_provider_table"))
        )
      ),

      bslib::nav_panel(
        title = "Places",
        bslib::card_body(


        )
      )
    )

  )

}

equityMapServer <- function(id) {

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
    #point_of_interest <-get_point_of_interest()
    svi_data <- get_SVI()

    # > Regional Rate
    # > County Level
    covid_data <- get_covid_data_county()
    # > Census Tract Level
    household_english_data <- get_pct_household_limited_english()
    vax_provider_travel_time_by_car <-
      get_vax_provider_travel_time_by_car()
    vax_provider_travel_time_by_transit <-
      get_vax_provider_travel_time_by_transit()


    # > Pal
    # pal_sf_hub <-
    #   leaflet::colorFactor(
    #     palette = c('#a6cee3', "#808080", '#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'),
    #     domain = unique(SF_HUB$HUB_Name),
    #     levels = unique(SF_HUB$HUB_Name)
    #   )

    pal_svi <-
      leaflet::colorNumeric(
        palette = "Reds",
        domain = svi_data$recalc_RPL_THEMES
      )

    pal_household_english <-
      leaflet::colorNumeric(
        palette = "Purples",
        domain = household_english_data$prcnt_limited_english_speaking_households
      )

    pal_covid_case_rate <-
      leaflet::colorNumeric(
        palette = "OrRd",
        domain = covid_data$caserate_last3weeks
      )

    pal_booster_rate <-
      leaflet::colorNumeric(
        palette = "YlGn",
        domain = covid_data$bivalent_booster_percentage
      )

    pal_vax_provider_travel_time_by_car <-
      leaflet::colorFactor(
        palette = c("#fdae6b", "#e6550d"),
        domain = unique(vax_provider_travel_time_by_car$travel_time_to_nearest_ped_vacc_provider_by_car),
        levels = unique(vax_provider_travel_time_by_car$travel_time_to_nearest_ped_vacc_provider_by_car),
        ordered = TRUE
      )

    pal_vax_provider_travel_time_by_transit <-
      leaflet::colorFactor(
        palette = c("#feedde", "#d94701", "#fdbe85", "#fd8d3c"),
        domain = unique(vax_provider_travel_time_by_transit$travel_time_to_nearest_ped_vacc_provider_by_transit),
        levels = unique(vax_provider_travel_time_by_transit$travel_time_to_nearest_ped_vacc_provider_by_transit),
        ordered = TRUE
      )

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

    })

    # ======================= #
    # ---- Apply Filters ----
    # ======================= #
    update_vax_provider <- function(type) {

      leaflet::leafletProxy("equity_map") %>%
        leaflet::clearGroup( "Vaccine Providers") %>%
        leaflet::addCircleMarkers(
          data = vax_provider %>%
            dplyr::filter(.data$Vaccine_Type %in% type),
          lat = ~latitude,
          lng = ~longitude,
          label = ~Place_Name,
          popup = ~popup,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),
          fillColor = "gray",
          fillOpacity = 1,
          stroke = F,
          group = "Vaccine Providers",
          options = leaflet::pathOptions(pane = "layer_top")
        )
    }


    update_HUB_highlight <- function(hub) {

      if (!rlang::is_empty(hub) && hub != "") {
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

      if (!rlang::is_empty(county) && county != "") {
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
        svi_data %>%
        dplyr::filter(
          .data$recalc_RPL_THEMES >= range[1],
          .data$recalc_RPL_THEMES <= range[2],
        )

      leaflet::leafletProxy("equity_map") %>%
        leaflet::clearGroup("Social Vulnerability Index (2018)") %>%
        leaflet::addPolygons(
          data = svi_data_filtered,
          group = "Social Vulnerability Index (2018)",
          stroke = TRUE,
          color = ~pal_svi(recalc_RPL_THEMES),
          weight = 1,
          opacity = 0.5,
          dashArray = "3",
          fillOpacity = 0.5,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>", "<b>SVI: </b>", round(recalc_RPL_THEMES, 4)
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
        )
    }

    update_household_english <- function(range) {


      household_english_data_filtered <-
        household_english_data %>%
        dplyr::filter(
          .data$prcnt_limited_english_speaking_households >= 100*range[1],
          .data$prcnt_limited_english_speaking_households <= 100*range[2],
        )

      leaflet::leafletProxy("equity_map") %>%
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
        )
    }

    update_vax_provider_travel_time_by_car <- function(type) {

      if (!rlang::is_empty(type) && type != "") {

        vax_provider_travel_time_by_car_filtered <-
          vax_provider_travel_time_by_car %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_car %in% type
          )
      } else {
        vax_provider_travel_time_by_car_filtered <-
          vax_provider_travel_time_by_car
      }

      leaflet::leafletProxy("equity_map") %>%
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
        )

    }


    update_vax_provider_travel_time_by_transit <- function(type) {

      if (!rlang::is_empty(type) && type != "") {

        vax_provider_travel_time_by_transit_filtered <-
          vax_provider_travel_time_by_transit %>%
          dplyr::filter(
            .data$travel_time_to_nearest_ped_vacc_provider_by_transit %in% type
          )
      } else {
        vax_provider_travel_time_by_transit_filtered <-
          vax_provider_travel_time_by_transit
      }

      leaflet::leafletProxy("equity_map") %>%
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
        )
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
    # output$vax_provider_table <- reactable::renderReactable({
    #
    #   vax_provider %>%
    #     reactable::reactable(
    #       # Table Format
    #       filterable = TRUE,
    #       outlined = TRUE,
    #       # Selection
    #       selection = "multiple", onClick = "select",
    #       highlight = TRUE,
    #       theme = reactable::reactableTheme(
    #         rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
    #       ),
    #       # Table Size
    #       defaultPageSize = 5, minRows = 5
    #     )
    # })



    # ==================== #
    # ---- Equity Map ----
    # ==================== #
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
      # leaflet::addCircleMarkers(
      #   data = vax_provider,
      #   lat = ~latitude,
      #   lng = ~longitude,
      #   label = ~Place_Name,
      #   popup = ~popup,
      #   labelOptions = leaflet::labelOptions(
      #     style = list(
      #       "font-weight" = "normal",
      #       padding = "3px 8px"
      #     ),
      #     textsize = "15px",
      #     direction = "auto"
      #   ),
      #   fillColor = "gray",
      #   fillOpacity = 1,
      #   stroke = F,
      #   group = "Vaccine Providers",
      #   options = leaflet::pathOptions(pane = "layer_top")
      # ) %>%

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
        data = svi_data,
        group = "Social Vulnerability Index (2018)",
        stroke = TRUE,
        color = ~pal_svi(recalc_RPL_THEMES),
        weight = 1,
        opacity = 0.5,
        dashArray = "3",
        fillOpacity = 0.5,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>", "<b>SVI: </b>", round(recalc_RPL_THEMES, 4)
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
        data = household_english_data,
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
        data = vax_provider_travel_time_by_car,
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
          data = vax_provider_travel_time_by_transit,
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
            "Min. (Tranist) to Nearest Pediatric Vax Provider"
          ),
          overlayGroups = c(
            "Hub Service Area",
            "Vaccine Providers",
            "Point of Interest"
          ),
          position = "topleft"
        ) %>%
        leaflet::hideGroup(
          c(
            "Hub Service Area",
            "Vaccine Providers",
            "Point of Interest"
          )
        )


    })





  })
}
