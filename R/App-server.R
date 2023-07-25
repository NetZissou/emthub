#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  #bslib::bs_themer()
  # Input
  business_data <-
    shiny::reactiveValues(
      filtered = emthub::BUSINESS_LOCATION_DATA
    )

  acc_data <-
    shiny::reactiveValues(
      value = NULL,
      pal = NULL
    )


  updateAcc <-
    function() {

      acc_data_filtered <- emthub::ACCESSIBILITY_DATA

      if (!rlang::is_empty(input$filter_acc_type)) {

        acc_data_filtered <-
          acc_data_filtered %>%
          dplyr::filter(.data$Business_type == input$filter_acc_type)
      }

      if (!rlang::is_empty(input$filter_transportation_method)) {

        acc_data_filtered <-
          acc_data_filtered %>%
          dplyr::transmute(
            .data$censustract,
            .data$Business_type,
            value = .data[[input$filter_transportation_method]],
            value_fct = emthub::ACC_PARAM_LIST$case[[input$filter_transportation_method]](.data$value)
          )
      }

      acc_data$value <- acc_data_filtered
      # acc_data$pal <-
      #   leaflet::colorNumeric(
      #     palette = "Reds",
      #     domain = acc_data_filtered$value,
      #     na.color = "#808080"
      #   )
      acc_data$pal <-
        leaflet::colorFactor(
          palette = emthub::ACC_PARAM_LIST$color[[input$filter_transportation_method]],
          # domain = factor(
          #   emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]],
          #   levels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]]
          # ),
          domain = emthub::ACC_PARAM_LIST$level_n[[input$filter_transportation_method]]
          #levels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]],
          #ordered = TRUE
        )

      # test_type <- "car"
      # pal <-
      #   leaflet::colorFactor(
      #     palette = emthub::ACC_PARAM_LIST$color[[test_type]],
      #     domain = emthub::ACC_PARAM_LIST$level[[test_type]],
      #     levels = emthub::ACC_PARAM_LIST$level[[test_type]],
      #     ordered = TRUE
      #   )



      leaflet::leafletProxy("index_map") %>%
        leaflet::clearGroup(group = "Accessibility") %>%
        leaflet::removeControl(layerId = "acc_legend") %>%
        leaflet::addPolygons(
          data = acc_data$value,
          group = "Accessibility",
          stroke = TRUE,
          color = ~acc_data$pal(value_fct),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          fillOpacity = 0.8,
          #options = leaflet::pathOptions(pane = "County_districts_polyline"),

          label = ~ paste0(
            "<b>", censustract, "</b>", "</br>", value, " mins"
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
          layerId = ~paste0("acc_", censustract),
          options = leaflet::pathOptions(pane = "layer_bottom")
        ) %>%
        leaflet::addLegend(
          "bottomleft",
          group = "Accessibility",
          layerId = "acc_legend",
          data = acc_data$value,
          #pal = acc_data$pal,
          colors = emthub::ACC_PARAM_LIST$color[[input$filter_transportation_method]],
          values = ~value_fct,
          title = emthub::ACC_PARAM_LIST$legend_title[[input$filter_transportation_method]],
          opacity = 1,
          labels = emthub::ACC_PARAM_LIST$level[[input$filter_transportation_method]]
          #labFormat = leaflet::labelFormat(suffix = " Mins")
        )
    }

  updateBusiness <-
    function () {

      filtered <- emthub::BUSINESS_LOCATION_DATA

      if (!rlang::is_empty(input$filter_ct)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$`Census Tract` %in% input$filter_ct
          )
      }

      if (!rlang::is_empty(input$filter_zip)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$`Zip Code` %in% input$filter_zip
          )
      }

      if (!rlang::is_empty(input$filter_city)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$City %in% input$filter_city
          )
      }

      if (!rlang::is_empty(input$filter_type)) {

        filtered <-
          filtered %>%
          dplyr::filter(
            .data$Business_Type %in% input$filter_type
          )
      }

      business_data$filtered <- filtered
    }

  shiny::observeEvent(input$apply_filter_business,{

    updateBusiness()

  })

  shiny::observeEvent(input$apply_filter_acc, {

    updateAcc()

  })

  # Weight Score
  get_tier_score <-
    function(data, tier_list, tier_name, weight) {
      if (!rlang::is_empty(tier_list)) {

        data %>%
          dplyr::select(
            .data$censustract,
            dplyr::all_of(
              stringr::str_c(
                "rank_",
                tier_list
              )
            )
          ) %>%
          tidyr::pivot_longer(
            tidyselect:::where(is.numeric),
            names_to = "outcome",
            values_to = "value"
          ) %>%
          dplyr::group_by(.data$censustract) %>%
          dplyr::summarise(value = sum(.data$value) * weight) %>%
          purrr::set_names(
            c("censustract", tier_name)
          )

      } else {

        data %>%
          dplyr::transmute(
            censustract = .data$censustract,
            value = 0
          ) %>%
          purrr::set_names(
            c("censustract", tier_name)
          )

      }
    }



  tier_weight_list <- shiny::reactiveValues(
    tier_1 = 0,
    tier_2 = 0,
    tier_3 = 0
  )

  tier_score_tbl <- shiny::reactiveValues(
    value = tibble::tibble(
      censustract = character(),
      tier_1 = numeric(),
      tier_2 = numeric(),
      tier_3 = numeric(),
      weighted_score = numeric(),
      weighted_score_scaled = numeric()
    ),
    pal = NULL
  )

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
        emthub::DISEASE_DATA,
        input$rank_list_1,
        "tier_1",
        weight = tier_weight_list$tier_1
      ) %>% dplyr::left_join(

        get_tier_score(
          emthub::DISEASE_DATA,
          input$rank_list_2,
          "tier_2",
          weight = tier_weight_list$tier_2
        ),
        by = "censustract"
      ) %>% dplyr::left_join(

        get_tier_score(
          emthub::DISEASE_DATA,
          input$rank_list_3,
          "tier_3",
          weight = tier_weight_list$tier_3
        ),
        by = "censustract"
      ) %>%
      dplyr::mutate(
        weighted_score = .data$tier_1 + .data$tier_2 + .data$tier_3,
        weighted_score_scaled = (.data$weighted_score - min(.data$weighted_score)) / (max(.data$weighted_score) - min(.data$weighted_score)),
        weighted_score_scaled = ifelse(is.nan(.data$weighted_score_scaled), NA, .data$weighted_score_scaled)
      )

    # tier_score_tbl$pal <-
    #   leaflet::colorNumeric(
    #     palette = "Reds",
    #     domain = tier_score_tbl$value$weighted_score_scaled
    #   )

    leaflet::leafletProxy("index_map") %>%
      leaflet::clearGroup(group = "Disease Outcomes Weighted Rank Score") %>%
      leaflet::addPolygons(
        data = emthub::SF_CENSUS_TRACT %>% dplyr::left_join(tier_score_tbl$value, by = c("GEOID" = "censustract")),
        group = "Disease Outcomes Weighted Rank Score",
        stroke = TRUE,
        color = ~pal_scaled_sum_rank(weighted_score_scaled),
        weight = 1,
        #opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.8,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>", "</br>", round(weighted_score_scaled, 4)
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
      )


  }



  shiny::observeEvent(input$rank_list_1, {
    updateWeights()
    updateTierScore()

    #print(tier_score_tbl$value)
  })

  shiny::observeEvent(input$rank_list_2, {
    updateWeights()
    updateTierScore()

    #print(tier_score_tbl$value)
  })

  shiny::observeEvent(input$rank_list_3, {
    updateWeights()
    updateTierScore()

    #print(tier_score_tbl$value)
  })


  # Map

  pal_scaled_sum_rank <- leaflet::colorNumeric(
    palette = "Reds",
    domain = emthub::DISEASE_DATA$scaled_rank_sum,
    na.color = "#808080"
  )

  pal_poverty_rate <- leaflet::colorNumeric(
    palette = "RdPu",
    domain = emthub::DISEASE_DATA$PovertyRate
  )

  SF_DISEASE_DATA <-
    emthub::SF_CENSUS_TRACT %>%
    dplyr::left_join(emthub::DISEASE_DATA, by = c("GEOID" = "censustract"))

  # leaflet::leaflet() %>%
  #   leaflet::addTiles() %>%
  #   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  #   leaflet::addPolylines(
  #     data = SF_DISEASE_DATA %>%
  #       dplyr::filter(.data$low_income_low_food_access_1_and_10_miles == 1) %>%
  #       dplyr::select(.data$geometry) %>%
  #       sf:::as_Spatial() %>%
  #       HatchedPolygons::hatched.SpatialPolygons(
  #         density = 70,
  #         angle = 135
  #       ),
  #
  #     group = "Low income & Low food access (1-10 miles)",
  #     stroke = TRUE,
  #     color = "black", #"#e6550d",
  #     weight = 2.5,
  #     dashArray = "2",
  #     #fillOpacity = 0
  #   )


  output$index_map <- leaflet::renderLeaflet({

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

      leaflet::addMapPane("layer_top", zIndex=420) %>%
      leaflet::addMapPane("layer_bottom",zIndex=410) %>%

      leaflet::addPolygons(
        data = SF_DISEASE_DATA,
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

      leaflet::addPolygons(
        data = SF_DISEASE_DATA,
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
      leaflet::addPolygons(
        data = emthub::SF_ZIP,
        group = "Zip Code",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID10, "</b>"
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
      leaflet::addPolylines(
        data = SF_DISEASE_DATA %>%
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
        data = SF_DISEASE_DATA %>%
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
        data = SF_DISEASE_DATA %>%
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
      # leaflet::addPolygons(
      #   data = SF_DISEASE_DATA %>%
      #     dplyr::filter(.data$low_income_low_food_access_1_and_10_miles == 1),
      #
      #   group = "Low income & Low food access (1-10 miles)",
      #   stroke = TRUE,
      #   color = "black", #"#e6550d",
      #   weight = 2.5,
      #   dashArray = "1",
      #   fillOpacity = 0,
      #   options = leaflet::pathOptions(pane = "layer_top"),
    #   # highlight = leaflet::highlightOptions(
    #   #   weight = 3,
    #   #   fillOpacity = 0.1,
    #   #   color = "black",
    #   #   dashArray = "",
    #   #   opacity = 0.5,
    #   #   bringToFront = TRUE,
    #   #   sendToBack = TRUE
    #   # ),
    #
    #   # TODO: Process Layer ID
    #   layerId = ~paste0("acctime_", GEOID)
    # ) %>%
    #
    # leaflet::addPolygons(
    #   data = SF_DISEASE_DATA %>%
    #     dplyr::filter(.data$low_income_low_food_access_half_and_10_miles == 1),
    #
    #   group = "Low income & Low food access (half-10 miles)",
    #   stroke = TRUE,
    #   color = "black", #"#e6550d",
    #   weight = 2.5,
    #   dashArray = "1",
    #   fillOpacity = 0,
    #   options = leaflet::pathOptions(pane = "layer_top"),
    #   # highlight = leaflet::highlightOptions(
    #   #   weight = 3,
    #   #   fillOpacity = 0.1,
    #   #   color = "black",
    #   #   dashArray = "",
    #   #   opacity = 0.5,
    #   #   bringToFront = TRUE,
    #   #   sendToBack = TRUE
    #   # ),
    #
    #   # TODO: Process Layer ID
    #   layerId = ~paste0("acctime_", GEOID)
    # ) %>%
    #
    # leaflet::addPolygons(
    #   data = SF_DISEASE_DATA %>%
    #     dplyr::filter(.data$low_income_low_food_access_1_and_20_miles == 1),
    #
    #   group = "Low income & Low food access (1-20 miles)",
    #   stroke = TRUE,
    #   color = "black", #"#e6550d",
    #   weight = 2.5,
    #   dashArray = "1",
    #   fillOpacity = 0,
    #   options = leaflet::pathOptions(pane = "layer_top"),
    #   # highlight = leaflet::highlightOptions(
    #   #   weight = 3,
    #   #   fillOpacity = 0.1,
    #   #   color = "black",
    #   #   dashArray = "",
    #   #   opacity = 0.5,
    #   #   bringToFront = TRUE,
    #   #   sendToBack = TRUE
    #   # ),
    #
    #   # TODO: Process Layer ID
    #   layerId = ~paste0("acctime_", GEOID)
    # ) %>%
    leaflet::addLegend(
      "bottomright",
      group = "Disease Outcomes Rank Score",
      data = emthub::DISEASE_DATA,
      pal = pal_scaled_sum_rank, values = ~scaled_rank_sum,
      title = "Rank Score",
      opacity = 1
    ) %>%

      leaflet::addLegend(
        "bottomright",
        group = "Poverty Rate",
        data = emthub::DISEASE_DATA,
        pal = pal_poverty_rate, values = ~PovertyRate,
        title = "Poverty Rate",
        opacity = 1
      ) %>%

      leaflet::addLayersControl(
        baseGroups = c(
          "Disease Outcomes Rank Score",
          "Disease Outcomes Weighted Rank Score",
          "Poverty Rate",
          "Accessibility"
        ),
        overlayGroups = c(
          "Business Location",
          "Zip Code",
          "Low income & Low food access (1-10 miles)",
          "Low income & Low food access (half-10 miles)",
          "Low income & Low food access (1-20 miles)"
        ),
        position = "topleft"
      ) %>%
      leaflet::hideGroup(
        c(
          #"Business Location",
          "Zip Code",
          "Low income & Low food access (1-10 miles)",
          "Low income & Low food access (half-10 miles)",
          "Low income & Low food access (1-20 miles)"
        )
      )

  })

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

      ct_disease_rank_data <-
        emthub::DISEASE_DATA %>%
        dplyr::filter(.data$censustract == as.character(selected_census_tract)) %>%
        dplyr::select(dplyr::all_of(c(
          #DISEASE_OUTCOMES,
          stringr::str_c("rank_", emthub::DISEASE_OUTCOMES),
          "recalc_svi_2018"
        ))) %>%
        tidyr::pivot_longer(-.data$recalc_svi_2018, names_to = "disease", values_to = "rank") %>%
        dplyr::slice_max(.data$rank, n = 5) %>%
        dplyr::mutate(
          html_output = glue::glue(
            '<h5>{disease}: {disease_rank}/70</h5><meter value="{disease_rank}" min="0" max="70"></meter>',
            disease = stringr::str_remove(.data$disease, "rank_"),
            disease_rank = .data$rank
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

  output$business_table <- reactable::renderReactable({

    business_data$filtered %>%
      dplyr::select(
        .data$`Census Tract`,
        `Type` = .data$Business_Type,
        .data$Name,
        .data$Address,
        .data$City,
        `Zip` = .data$`Zip Code`
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

  selected_business_index <- shiny::reactive(reactable::getReactableState("business_table", "selected"))
  selected_business <- shiny::reactiveValues(value = NULL)
  shiny::observe({

    if (!is.null(selected_business_index())) {

      selected_business$value <-
        business_data$filtered[selected_business_index(),]


    } else {
      selected_business$value <- NULL
    }


    if (!rlang::is_empty(selected_business$value)) {

      leaflet::leafletProxy("index_map") %>%
        leaflet::clearGroup("Business Location") %>%
        leaflet::addMarkers(
          data = selected_business$value,
          group = "Business Location",
          lng = ~Longitude, lat = ~Latitude,
          popup = ~popup,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-size" = "15px",
              "font-style" = "bold",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          ),
          options = leaflet::pathOptions(pane = "layer_top")
        )

    } else {

      leaflet::leafletProxy("index_map") %>%
        leaflet::clearGroup("Business Location")
    }

    #print(selected_business$value)
  })

  shiny::observe({
    # print(input$index_map_shape_click)
    # print(input$rank_list_1)
    # print(input$rank_list_2)
    # print(input$rank_list_3)

    # if (!rlang::is_empty(input$rank_list_1)) {
    #
    #   print(
    #     get_tier_score(
    #       emthub::DISEASE_DATA,
    #       input$rank_list_1,
    #       "tier_1",
    #       0.5
    #     )
    #   )
    # }

  })


  # Table
}
