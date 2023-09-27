
diseaseOutcomesUI <- function(id) {
  bslib::navset_card_tab(
    title = NULL,
    bslib::nav_panel(
      "Map",
      #card_title("Map"),
      bslib::layout_columns(
        bslib::card(

          bslib::card_body(
            class = "p-0",
            leaflet::leafletOutput(shiny::NS(id, "index_map"))
          ),
          full_screen = TRUE
        ),
        bslib::navset_card_tab(
          title = NULL,
          bslib::card_header(
            bslib::card_body(
              #min_height = 20,
              shiny::helpText(
                #"THIS TOOL ONLY WORKS FOR MAHONING COUNTY. ADDITIONAL COUNTIES WILL BE AVAILABLE SOON. PLEASE CHECK BACK LATER."
                "THE INFORMATION BELOW IS SUBJECT TO CHANGE. PLEASE READ THE DOCUMENTATION ON THE DATA SOURCE PAGE AND WATCH THE TRAINING VIDEO BEFORE USING THE TABS AND FUNCTIONS BELOW."
              )
            )
          ),
          bslib::nav_panel(
            'Info',
            shiny::selectInput(
              shiny::NS(id, "global_county"),
              label = "Select County",
              choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
              multiple = TRUE,
              selected = NULL,
              width = "100%"
            ),
            shiny::htmlOutput(shiny::NS(id, "index_map_info"))
            # bslib::card(
            #   bslib::card_body(
            #     min_height = 500,
            #     shiny::selectInput(
            #       shiny::NS(id, "global_county"),
            #       label = "Select County",
            #       choices = emthub::EQUITY_MAP_FILTER_CHOICES$county,
            #       multiple = TRUE,
            #       selected = NULL,
            #       width = "100%"
            #     ),
            #     shiny::htmlOutput(shiny::NS(id, "index_map_info"))
            #   )
            # )
          ),
          bslib::nav_panel(
            'Place Finder',

            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = shiny::NS(id, "filter_type"),
                  label = "Type",
                  choices = emthub::EQUITY_MAP_FILTER_CHOICES$point_of_interest,
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = shiny::NS(id, "filter_city"),
                  label = "City",
                  choices = emthub::EQUITY_MAP_FILTER_CHOICES$city,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),


            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = shiny::NS(id, "filter_ct"),
                  label = "Census Tract",
                  choices = emthub::EQUITY_MAP_FILTER_CHOICES$ct,
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = shiny::NS(id, "filter_zip"),
                  label = "Zip",
                  choices = emthub::EQUITY_MAP_FILTER_CHOICES$zip,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),

            reactable_searchBar(shiny::NS(id, "poi_table"), placeholder = "Search for Place of Interest ..."),
            reactable_csvDownloadButton(shiny::NS(id, "poi_table"), filename = "poi_disease_outcomes.csv"),
            shiny::helpText("Toggle to add places to the map"),
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
                width = 3,
                shiny::selectInput(
                  shiny::NS(id, "iso_type"),
                  label = "Transportation",
                  choices = c("car", "walk", "transit", "cycle"),
                  multiple = FALSE
                )
              ),
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

          # bslib::nav_panel(
          #   "Accessibility",
          #   shiny::selectInput(
          #     inputId = shiny::NS(id, "filter_acc_type"),
          #     label = "Business Type",
          #     choices = emthub::ACC_BUSINESS_TYPE,
          #     multiple = FALSE,
          #     width = "100%"
          #   ),
          #
          #   shiny::selectInput(
          #     inputId = shiny::NS(id, "filter_transportation_method"),
          #     label = "Transportation Methods",
          #     choices = emthub::ACC_TRANSPORTATION_METHODS,
          #     multiple = FALSE,
          #     width = "100%"
          #   ),
          #
          #   shiny::actionButton(
          #     inputId = shiny::NS(id, "apply_filter_acc"),
          #     label = "Apply Filters"
          #   )
          #
          #   # bslib::card(
          #   #   height = 500,
          #   #
          #   #   shiny::selectInput(
          #   #     inputId = shiny::NS(id, "filter_acc_type"),
          #   #     label = "Business Type",
          #   #     choices = emthub::ACC_BUSINESS_TYPE,
          #   #     multiple = FALSE,
          #   #     width = "100%"
          #   #   ),
          #   #
          #   #   shiny::selectInput(
          #   #     inputId = shiny::NS(id, "filter_transportation_method"),
          #   #     label = "Transportation Methods",
          #   #     choices = emthub::ACC_TRANSPORTATION_METHODS,
          #   #     multiple = FALSE,
          #   #     width = "100%"
          #   #   ),
          #   #
          #   #   shiny::actionButton(
          #   #     inputId = shiny::NS(id, "apply_filter_acc"),
          #   #     label = "Apply Filters"
          #   #   )
          #   # )
          # )
        ),
        col_widths = c(8,4)

      )
    ),

    bslib::nav_panel(
      "Controls",
      sortable::bucket_list(
        header = "Disease Outcomes of Interest",
        group_name = "bucket_list_group",
        orientation = "horizontal",

        sortable::add_rank_list(
          text = "Outcomes",
          labels = emthub::DISEASE_OUTCOMES,
          input_id = shiny::NS(id, "outcomes")
        ),
        sortable::add_rank_list(
          text = "Tier 1",
          labels = NULL,
          input_id = shiny::NS(id, "rank_list_1")
        ),
        sortable::add_rank_list(
          text = "Tier 2",
          labels = NULL,
          input_id = shiny::NS(id, "rank_list_2")
        ),
        sortable::add_rank_list(
          text = "Tier 3",
          labels = NULL,
          input_id = shiny::NS(id, "rank_list_3")
        ),
        options = sortable::sortable_options(multiDrag = TRUE)
      )
    )


  )
}

diseaseOutcomesServer <- function(id, poi, ct_level_data_all, app_county, shapefile_list) {

  shiny::moduleServer(id, function(input, output, session){


    # ============== #
    # ---- Data ----
    # ============== #

    # > Params
    tier_weight_list <- shiny::reactiveValues(
      tier_1 = 0,
      tier_2 = 0,
      tier_3 = 0
    )

    tier_score_tbl <- shiny::reactiveValues(
      value = tibble::tibble(
        GEOID = character(),
        tier_1 = numeric(),
        tier_2 = numeric(),
        tier_3 = numeric(),
        weighted_score = numeric(),
        weighted_score_scaled = numeric()
      ),
      pal = NULL
    )

    shiny::observe({

      shiny::updateSelectInput(
        inputId = "global_county",
        selected = glue::glue(
          "{name} County",
          name = app_county$value
        )
      )

      # if (app_county$value == "Mahoning") {
      #   shinyjs::hide(
      #     id = "global_county",
      #     anim = FALSE
      #   )
      # }
      #print("=======================")
      #print(input$global_county)
    })

    # > Point Level
    #BUSINESS_LOCATION_DATA <- get_business_location(parquet = TRUE)

    poi_reactive <-
      shiny::reactiveValues(
        filtered = NULL
      )
    vax_provider <- get_vax_provider(parquet = TRUE)

    # > Regional Rates
    # ACCESSIBILITY_DATA <- get_acc_data(parquet = TRUE)
    # acc_data <-
    #   shiny::reactiveValues(
    #     value = NULL,
    #     pal = NULL
    #   )

    ct_level_data <- shiny::reactive({
      shiny::req(!rlang::is_empty(input$global_county))
      ct_level_data_all %>%
        dplyr::filter(
          stringr::str_to_lower(.data$CountyName) %in% stringr::str_to_lower(stringr::str_remove(input$global_county, " County"))
        )
    })

    sf_zip <- shiny::reactive({
      shiny::req(!rlang::is_empty(input$global_county))
      get_sf_zip_by_county(input$global_county)
    })

    # > Shapefiles
    #SF_ZIP <- get_sf_zip()
    #SF_CT <- get_sf_ct()
    #SF_COUNTY <- get_sf_county()

    # > Pal
    pal_scaled_sum_rank <- emthub::PAL$pal_scaled_sum_rank

    pal_poverty_rate <- emthub::PAL$pal_poverty_rate

    pal_birth_outcomes <- emthub::PAL$pal_birth_outcomes


    # ======================= #
    # ---- Event Handler ----
    # ======================= #
    # > Accessibility ----
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
        map_id = "index_map",
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

      click <- input$index_map_click

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

      leaflet::leafletProxy("index_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup("Isochron") %>%
        leaflet::clearGroup("ISO Resource - Vaccine Providers") %>%
        leaflet::clearGroup("ISO Resource - POI") %>%
        leaflet.extras2::stopSpinner()
    })


    # updateAcc <-
    #   function() {
    #
    #     acc_data_filtered <- ACCESSIBILITY_DATA
    #
    #     if (!rlang::is_empty(input$filter_acc_type)) {
    #
    #       acc_data_filtered <-
    #         acc_data_filtered %>%
    #         dplyr::filter(.data$Business_type_condensed == input$filter_acc_type)
    #     }
    #
    #     if (!rlang::is_empty(input$filter_transportation_method)) {
    #
    #       acc_data_filtered <-
    #         acc_data_filtered %>%
    #         dplyr::transmute(
    #           .data$censustract,
    #           .data$Business_type_condensed,
    #           value = .data[[input$filter_transportation_method]],
    #           value_fct = emthub::ACC_PARAM_LIST$case[[input$filter_transportation_method]](.data$value)
    #         )
    #     }
    #
    #     acc_data$value <- acc_data_filtered
    #     acc_data$pal <-
    #       leaflet::colorFactor(
    #         palette = emthub::ACC_PARAM_LIST$color[[input$filter_transportation_method]],
    #         # domain = factor(
    #         #   emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]],
    #         #   levels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]]
    #         # ),
    #         domain = emthub::ACC_PARAM_LIST$level_n[[input$filter_transportation_method]]
    #         #levels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]],
    #         #ordered = TRUE
    #       )
    #
    #
    #
    #     leaflet::leafletProxy("index_map") %>%
    #       leaflet.extras2::addSpinner() %>%
    #       leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
    #       leaflet::clearGroup(group = "Accessibility") %>%
    #       leaflet::removeControl(layerId = "acc_legend") %>%
    #       leaflet::addPolygons(
    #         data = acc_data$value,
    #         group = "Accessibility",
    #         stroke = TRUE,
    #         color = ~acc_data$pal(value_fct),
    #         weight = 1,
    #         #opacity = 0.8,
    #         dashArray = "3",
    #         fillOpacity = 0.8,
    #         #options = leaflet::pathOptions(pane = "County_districts_polyline"),
    #
    #         label = ~ paste0(
    #           "<b>", censustract, "</b>", "</br>", value, " mins"
    #         ) %>% lapply(htmltools::HTML),
    #
    #         labelOptions = leaflet::labelOptions(
    #           style = list(
    #             "font-weight" = "normal",
    #             padding = "3px 8px"
    #           ),
    #           textsize = "15px",
    #           direction = "auto"
    #         ),
    #
    #         highlight = leaflet::highlightOptions(
    #           weight = 3,
    #           fillOpacity = 0.1,
    #           color = "black",
    #           dashArray = "",
    #           opacity = 0.5,
    #           bringToFront = TRUE,
    #           sendToBack = TRUE
    #         ),
    #
    #         # TODO: Process Layer ID
    #         layerId = ~paste0("acc_", censustract),
    #         options = leaflet::pathOptions(pane = "layer_bottom")
    #       ) %>%
    #       leaflet::addLegend(
    #         "bottomleft",
    #         group = "Accessibility",
    #         layerId = "acc_legend",
    #         data = acc_data$value,
    #         #pal = acc_data$pal,
    #         colors = emthub::ACC_PARAM_LIST$color[[input$filter_transportation_method]],
    #         values = ~value_fct,
    #         title = emthub::ACC_PARAM_LIST$legend_title[[input$filter_transportation_method]],
    #         opacity = 1,
    #         labels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]]
    #         #labFormat = leaflet::labelFormat(suffix = " Mins")
    #       ) %>%
    #       leaflet.extras2::stopSpinner()
    #   }
    #
    # shiny::observeEvent(input$apply_filter_acc, {
    #
    #   updateAcc()
    #
    # })

    # > Places ----
    update_poi <-
      function () {

        filtered <- poi

        if (!rlang::is_empty(input$filter_ct)) {

          filtered <-
            filtered %>%
            dplyr::filter(
              .data$census_tract %in% input$filter_ct
            )
        }

        if (!rlang::is_empty(input$filter_zip)) {

          filtered <-
            filtered %>%
            dplyr::filter(
              .data$zip %in% input$filter_zip
            )
        }

        if (!rlang::is_empty(input$filter_city)) {

          filtered <-
            filtered %>%
            dplyr::filter(
              .data$city %in% input$filter_city
            )
        }

        if (!rlang::is_empty(input$filter_type)) {

          filtered <-
            filtered %>%
            dplyr::filter(
              .data$type %in% input$filter_type
            )
        }

        if (rlang::is_empty(input$filter_ct) && rlang::is_empty(input$filter_zip) &&
            rlang::is_empty(input$filter_city) && rlang::is_empty(input$filter_type)) {
          poi_reactive$filtered <- NULL
        } else {
          poi_reactive$filtered <- filtered
        }
      }

    shiny::observe({
      update_poi()
    }) %>%
      shiny::bindEvent(
        input$filter_type,
        input$filter_city,
        input$filter_ct,
        input$filter_zip
      )


    # > Weight Score ----

    get_tier_score <-
      function(data, tier_list, tier_name, weight) {
        if (!rlang::is_empty(tier_list)) {

          data %>%
            dplyr::as_tibble() %>%
            dplyr::select(
              .data$GEOID,
              dplyr::all_of(
                #emthub::DISEASE_DATA_VARS,
                # TODO: Fix
                tier_list
              )
            ) %>%
            tidyr::pivot_longer(
              tidyselect:::where(is.numeric),
              names_to = "outcome",
              values_to = "value"
            ) %>%
            dplyr::group_by(.data$GEOID) %>%
            dplyr::summarise(value = sum(.data$value) * weight) %>%
            purrr::set_names(
              c("GEOID", tier_name)
            )

        } else {

          data %>%
            dplyr::as_tibble() %>%
            dplyr::transmute(
              GEOID = .data$GEOID,
              value = 0
            ) %>%
            purrr::set_names(
              c("GEOID", tier_name)
            )

        }
      }

    updateWeights <- function() {
      # Get the input values
      rank_list_1 <- input$rank_list_1
      rank_list_2 <- input$rank_list_2
      rank_list_3 <- input$rank_list_3

      is_null_tier_1 <- rlang::is_empty(rank_list_1)
      is_null_tier_2 <- rlang::is_empty(rank_list_2)
      is_null_tier_3 <- rlang::is_empty(rank_list_3)

      # Determine the weights based on the inputs
      if (all(is_null_tier_1, is_null_tier_2, is_null_tier_3)) {

        tier_weight_list$tier_1 <- 0
        tier_weight_list$tier_2 <- 0
        tier_weight_list$tier_3 <- 0

      } else if (is_null_tier_1 && is_null_tier_2 && !is_null_tier_3) {

        tier_weight_list$tier_1 <- 0
        tier_weight_list$tier_2 <- 0
        tier_weight_list$tier_3 <- 1

      } else if (is_null_tier_1 && !is_null_tier_2 && is_null_tier_3) {

        tier_weight_list$tier_1 <- 0
        tier_weight_list$tier_2 <- 1
        tier_weight_list$tier_3 <- 0

      } else if (!is_null_tier_1 && is_null_tier_2 && is_null_tier_3) {

        tier_weight_list$tier_1 <- 1
        tier_weight_list$tier_2 <- 0
        tier_weight_list$tier_3 <- 0

      } else if (is_null_tier_1 && !is_null_tier_2 && !is_null_tier_3) {

        tier_weight_list$tier_1 <- 0
        tier_weight_list$tier_2 <- 0.7
        tier_weight_list$tier_3 <- 0.3

      } else if (!is_null_tier_1 && is_null_tier_2 && !is_null_tier_3) {

        tier_weight_list$tier_1 <- 0.7
        tier_weight_list$tier_2 <- 0
        tier_weight_list$tier_3 <- 0.3

      } else if (!is_null_tier_1 && !is_null_tier_2 && is_null_tier_3) {

        tier_weight_list$tier_1 <- 0.7
        tier_weight_list$tier_2 <- 0.3
        tier_weight_list$tier_3 <- 0

      } else if (!is_null_tier_1 && !is_null_tier_2 && !is_null_tier_3) {

        tier_weight_list$tier_1 <- 0.5
        tier_weight_list$tier_2 <- 0.3
        tier_weight_list$tier_3 <- 0.2
      }

    }

    updateTierScore <- function() {

      tier_score_tbl$value <-
        get_tier_score(
          #DISEASE_DATA,
          ct_level_data(),
          input$rank_list_1,
          "tier_1",
          weight = tier_weight_list$tier_1
        ) %>% dplyr::left_join(

          get_tier_score(
            #DISEASE_DATA,
            ct_level_data(),
            input$rank_list_2,
            "tier_2",
            weight = tier_weight_list$tier_2
          ),
          by = "GEOID"
        ) %>% dplyr::left_join(

          get_tier_score(
            #DISEASE_DATA,
            ct_level_data(),
            input$rank_list_3,
            "tier_3",
            weight = tier_weight_list$tier_3
          ),
          by = "GEOID"
        ) %>%
        dplyr::mutate(
          weighted_score = .data$tier_1 + .data$tier_2 + .data$tier_3,
          weighted_score_scaled = (.data$weighted_score - min(.data$weighted_score)) / (max(.data$weighted_score) - min(.data$weighted_score)),
          weighted_score_scaled = ifelse(is.nan(.data$weighted_score_scaled), NA, .data$weighted_score_scaled)
        )

      leaflet::leafletProxy("index_map") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::clearGroup(group = "Disease Outcomes Weighted Rank Score") %>%
        leaflet::addPolygons(
          data = ct_level_data() %>% dplyr::select(.data$GEOID) %>%
            dplyr::inner_join(tier_score_tbl$value, by = "GEOID"),
          #data = tier_score_tbl$value,
          group = "Disease Outcomes Weighted Rank Score",
          stroke = TRUE,
          color = ~pal_scaled_sum_rank(weighted_score_scaled),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          fillOpacity = 0.8,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", GEOID, "</b>", "</br>",
            round(weighted_score_scaled, 4), "</br>"
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

          # TODO: Process Layer ID
          layerId = ~paste0("weighted_", GEOID),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet.extras2::stopSpinner()


    }


    # shiny::observeEvent(input$rank_list_1, {
    #   updateWeights()
    #   updateTierScore()
    #
    # })
    #
    # shiny::observeEvent(input$rank_list_2, {
    #   updateWeights()
    #   updateTierScore()
    # })
    #
    # shiny::observeEvent(input$rank_list_3, {
    #   updateWeights()
    #   updateTierScore()
    #
    # })

    shiny::observe({
      updateWeights()
      updateTierScore()
    }) %>%
      shiny::bindEvent(
        input$rank_list_1,
        input$rank_list_2,
        input$rank_list_3
      )



    # ============= #
    # ---- Map ----
    # ============= #

    output$index_map <- leaflet::renderLeaflet({
      shiny::req(!rlang::is_empty(input$global_county))

      centroid <-
        shapefile_list$SF_COUNTY %>%
        dplyr::filter(.data$COUNTY == input$global_county) %>%
        dplyr::pull(.data$geometry) %>%
        sf::st_centroid() %>%
        unlist()

      leaflet::leaflet(
        data = ct_level_data(),
        options = leaflet::leafletOptions(
          zoomControl = TRUE
        )
      ) %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
        leaflet::setView(
          lat = centroid[2],
          lng = centroid[1],
          zoom = 10
        ) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri, group = "Base Map") %>%

        leaflet::addMapPane("layer_top", zIndex=420) %>%
        leaflet::addMapPane("layer_middle",zIndex=410) %>%
        leaflet::addMapPane("layer_bottom",zIndex=400) %>%

        # ============================ #
        # > Disease Outcomes Rank ----

      leaflet::addPolygons(
        group = "Disease Outcomes Rank Score",
        stroke = TRUE,
        color = ~pal_scaled_sum_rank(scaled_rank_sum),
        weight = 1,
        #opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.8,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>", round(scaled_rank_sum, 4)
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
        layerId = ~GEOID,
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        # ==================== #
        #  > Poverty Rate ----
      leaflet::addPolygons(
        group = "Poverty Rate",
        stroke = TRUE,
        color = ~pal_poverty_rate(PovertyRate),
        weight = 1,
        #opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.8,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>", round(PovertyRate, 4)
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

        # TODO: Process Layer ID
        layerId = ~paste0("pov_", GEOID),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        # ========================== #
        # > Infant Health Score ----
      leaflet::addPolygons(
        group = "Infant Health Score",
        stroke = TRUE,
        color = ~pal_birth_outcomes(infant_health_score_quantile),
        weight = 1,
        #opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.8,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>",
          "Infant Health Score: ", infant_health_score_quantile, "</br>",
          "Quantile: ", round(infant_health_score, 4)
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

        # TODO: Process Layer ID
        layerId = ~paste0("infant_health_score", GEOID),
        options = leaflet::pathOptions(pane = "layer_bottom")
      ) %>%

        # =================== #
        # > Overlay: Zip ----

      leaflet::addPolygons(
        data = sf_zip(),
        group = "Zip Code",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID20, "</b>"
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

        # ========================================== #
        # > Overlay: Low Income Low Food Access ----
      leaflet::addPolylines(
        data = ct_level_data() %>%
          dplyr::filter(.data$low_income_low_food_access_1_and_10_miles == 1) %>%
          dplyr::select(.data$geometry) %>%
          sf::as_Spatial() %>%
          HatchedPolygons::hatched.SpatialPolygons(
            density = 150,
            angle = 45
          ),

        group = "Low income & Low food access (1-10 miles)",
        stroke = TRUE,
        color = "black", #"#e6550d",
        weight = 2.5,
        dashArray = "1",
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "layer_top")
      ) %>%

        leaflet::addPolylines(
          data = ct_level_data() %>%
            dplyr::filter(.data$low_income_low_food_access_half_and_10_miles == 1) %>%
            dplyr::select(.data$geometry) %>%
            sf::as_Spatial() %>%
            HatchedPolygons::hatched.SpatialPolygons(
              density = 150,
              angle = 60,
            ),

          group = "Low income & Low food access (half-10 miles)",
          stroke = TRUE,
          color = "black", #"#e6550d",
          weight = 2.5,
          dashArray = "1",
          fillOpacity = 0,
          options = leaflet::pathOptions(pane = "layer_top")
        ) %>%

        leaflet::addPolylines(
          data = ct_level_data() %>%
            dplyr::filter(.data$low_income_low_food_access_1_and_20_miles == 1) %>%
            dplyr::select(.data$geometry) %>%
            sf::as_Spatial() %>%
            HatchedPolygons::hatched.SpatialPolygons(
              density = 150,
              angle = 135
            ),

          group = "Low income & Low food access (1-20 miles)",
          stroke = TRUE,
          color = "black", #"#e6550d",
          weight = 2.5,
          dashArray = "1",
          fillOpacity = 0,
          options = leaflet::pathOptions(pane = "layer_top")
        ) %>%

        # ============= #
        # > Legend ----
      leaflet::addLegend(
        "bottomright",
        group = "Infant Health Score",
        #data = ct_level_data,
        pal = pal_birth_outcomes, values = ~infant_health_score_quantile,
        title = "Infant</br>Health Score</br>Quantile",
        opacity = 1
      ) %>%

        leaflet::addLegend(
          "bottomright",
          group = "Poverty Rate",
          #data = DISEASE_DATA,
          pal = pal_poverty_rate, values = ~PovertyRate,
          title = "Poverty Rate",
          opacity = 1
        ) %>%

        leaflet::addLegend(
          "bottomright",
          group = "Disease Outcomes Rank Score",
          #data = DISEASE_DATA,
          pal = pal_scaled_sum_rank, values = ~scaled_rank_sum,
          title = "Rank Score",
          opacity = 1
        ) %>%

        leaflet::addLayersControl(
          baseGroups = c(
            "Disease Outcomes Rank Score",
            "Disease Outcomes Weighted Rank Score",
            "Poverty Rate",
            #"Accessibility",
            "Infant Health Score",
            "Base Map"
          ),
          overlayGroups = c(
            "Place",
            "Zip Code",
            "Low income & Low food access (1-10 miles)",
            "Low income & Low food access (half-10 miles)",
            "Low income & Low food access (1-20 miles)",
            "Isochron",
            "ISO Resource - Vaccine Providers",
            "ISO Resource - POI"
          ),
          position = "topleft"
        ) %>%
        leaflet::hideGroup(
          c(
            #"Place",
            "Zip Code",
            "Low income & Low food access (1-10 miles)",
            "Low income & Low food access (half-10 miles)",
            "Low income & Low food access (1-20 miles)"
          )
        ) %>%
        leaflet.extras2::addEasyprint(
          options = leaflet.extras2::easyprintOptions(
            title = 'Print Map',
            position = 'bottomleft',
            exportOnly = TRUE
          )
        ) %>%
        leaflet.extras2::stopSpinner()

    })


    # ================== #
    # Panel Output ----

    ct_selected <- shiny::reactive({
      #input$index_map_shape_click$id
      stringr::str_extract(
        input$index_map_shape_click$id,
        pattern = '\\d+'
      )
    })

    output$index_map_info <- shiny::renderUI({

      if (!rlang::is_empty(ct_selected())) {

        selected_census_tract <- ct_selected()
        n_total <- nrow(ct_level_data())

        ct_disease_rank_data <-
          ct_level_data() %>%
          dplyr::as_tibble() %>%
          dplyr::filter(.data$GEOID == as.character(selected_census_tract)) %>%
          dplyr::select(dplyr::all_of(c(
            #DISEASE_OUTCOMES,
            emthub::DISEASE_DATA_VARS,
            "recalc_svi_2018"
          ))) %>%
          tidyr::pivot_longer(-.data$recalc_svi_2018, names_to = "disease", values_to = "rank") %>%
          dplyr::mutate(
            disease = stringr::str_replace(.data$disease, "\\.\\.", " ("),
            disease = stringr::str_replace(.data$disease, "\\.", " "),
            disease = stringr::str_remove(.data$disease, "rank_"),
            disease = stringr::str_to_title(.data$disease)
          ) %>%
          dplyr::slice_max(.data$rank, n = 5) %>%
          dplyr::mutate(
            html_output = glue::glue(
              '<h5>{disease}: {disease_rank}/{n}</h5><meter value="{disease_rank}" min="0" max="{n}"></meter>',
              disease = stringr::str_to_title(
                stringr::str_replace_all(
                  stringr::str_remove(.data$disease, "rank_"),
                  pattern = "\\.",
                  replacement = " "
                )
              ),
              disease_rank = .data$rank,
              n = n_total
            )
          )

        shiny::HTML(
          glue::glue(
            "
          <h2>Census Tract: {census_tract}</h2>
          <h4>SVI: {SVI_value}</h4>
          <hr>
          <h4>Top Ranked Disease Outcomes: </h4>
          {disease_score_output}
          ",
            census_tract = ct_selected(),
            SVI_value = unique(ct_disease_rank_data$recalc_svi_2018),
            #gauge = shiny_gauge(unique(ct_disease_rank_data$recalc_svi_2018)),
            disease_score_output = stringr::str_c(ct_disease_rank_data$html_output, collapse = ""),
          )
        )
      }
    })


    # =================== #
    # Business Table ----

    output$poi_table <- reactable::renderReactable({
      shiny::req(!rlang::is_empty(poi_reactive$filtered))

      poi_reactive$filtered %>%
        dplyr::select(
          Name = .data$name,
          Type = .data$type,
          Hub = .data$hub,
          City = .data$city,
          County = .data$county,
          Zip = .data$zip,
          `Census Tract` = .data$census_tract,
          Addr = .data$street_addr
        ) %>%
        dplyr::collect() %>%
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

        leaflet::leafletProxy("index_map") %>%
          leaflet.extras2::addSpinner() %>%
          leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
          leaflet::clearGroup("Place") %>%
          leaflet::addMarkers(
            data = selected_poi$value,
            group = "Place",
            lng = ~lng, lat = ~lat,
            popup = ~popup,
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

        leaflet::leafletProxy("index_map") %>%
          leaflet.extras2::addSpinner() %>%
          leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
          leaflet::clearGroup("Place") %>%
          leaflet.extras2::stopSpinner()
      }

      #print(selected_poi$value)
    })




  })
}
