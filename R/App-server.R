#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  # Input


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
        layerId = ~paste0("weighted_", GEOID)
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
    emthub::SF_CENSUS_TRACT %>% dplyr::left_join(emthub::DISEASE_DATA, by = c("GEOID" = "censustract"))

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
      layerId = ~GEOID
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
        layerId = ~paste0("pov_", GEOID)
      ) %>%

      leaflet::addPolygons(
        data = SF_DISEASE_DATA %>%
          dplyr::filter(.data$low_income_low_food_access_1_and_10_miles == 1),

        group = "Low income & Low food access (1-10 miles)",
        stroke = TRUE,
        color = "black", #"#e6550d",
        weight = 2.5,
        dashArray = "1",
        fillOpacity = 0
      ) %>%

      leaflet::addPolygons(
        data = SF_DISEASE_DATA %>%
          dplyr::filter(.data$low_income_low_food_access_half_and_10_miles == 1),

        group = "Low income & Low food access (half-10 miles)",
        stroke = TRUE,
        color = "black", #"#e6550d",
        weight = 2.5,
        dashArray = "1",
        fillOpacity = 0
      ) %>%

      leaflet::addPolygons(
        data = SF_DISEASE_DATA %>%
          dplyr::filter(.data$low_income_low_food_access_1_and_20_miles == 1),

        group = "Low income & Low food access (1-20 miles)",
        stroke = TRUE,
        color = "black", #"#e6550d",
        weight = 2.5,
        dashArray = "1",
        fillOpacity = 0
      ) %>%

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
          "Poverty Rate"
        ),
        overlayGroups = c(
          "Low income & Low food access (1-10 miles)",
          "Low income & Low food access (half-10 miles)",
          "Low income & Low food access (1-20 miles)"
        ),
        position = "topleft"
      ) %>%
      leaflet::hideGroup(
        c(
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
