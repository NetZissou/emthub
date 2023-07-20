opioidOverdoseMapUI <- function(id) {

  # shinydashboard::box(
  #   width = 12, solidHeader = TRUE,
  #   title = "Opioid Overdose Map",
  #   leaflet::leafletOutput(
  #     outputId = shiny::NS(id, "overdose_map"),
  #     height = 700
  #   )
  # )
  shiny::tagList(

    # shinyWidgets::actionBttn(
    #   inputId = shiny::NS(id, "screenshot_overdose_map"),
    #   label = "Take a map screenshot",
    #   style = "minimal",
    #   color = "success"
    # ),

    shinyWidgets::addSpinner(
      leaflet::leafletOutput(
        outputId = shiny::NS(id, "overdose_map"),
        height = 600
      ),
      spin = "fading-circle"
    )
  )
}

opioidOverdoseMapServer <- function(
  id, filtered_overdose_data,
  filtered_drug_crime_data, filtered_treatment_providers_data, filter_selection
) {
  shiny::req(shiny::is.reactivevalues(filtered_overdose_data))
  shiny::req(shiny::is.reactivevalues(filtered_drug_crime_data))
  shiny::req(shiny::is.reactivevalues(filtered_treatment_providers_data))
  shiny::req(shiny::is.reactivevalues(filter_selection))

  shiny::moduleServer(id, function(input, output, session){

    overdose_data <-
      shiny::reactive({
        return(
          filtered_overdose_data$data
        )
      })

    drug_crime_data <-
      shiny::reactive({
        return(
          filtered_drug_crime_data$data
        )
      })
    treatment_providers_data <-
      shiny::reactive({
        return(
          filtered_treatment_providers_data$data
        )
      })

    franklin_county_school_district_sf <-
      get_franklin_county_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    # ============================================================ #
    # --------------------- Overdose Map -------------------------
    # ============================================================ #

    output$overdose_map <- leaflet::renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      opioid_overdose_map_init_bounds <- opioidDashboard::opioid_overdose_map_init_bounds

      leaflet::leaflet() %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        # ========================= #
        # ---- Map init Bounds ----
      # ========================= #
      leaflet::fitBounds(
        lng1 = opioid_overdose_map_init_bounds$min_lng,
        lat1 = opioid_overdose_map_init_bounds$min_lat,
        lng2 = opioid_overdose_map_init_bounds$max_lng,
        lat2 = opioid_overdose_map_init_bounds$max_lat
      ) %>%
        leaflet::addMapPane("County_districts_polyline", zIndex = 420) %>%
        # ================================================ #
        # ---- Shapefile outline: FC School Districts ----
      # ================================================ #
      leaflet::addPolygons(
        data = franklin_county_school_district_sf,
        group = "FC School Districts",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", NAME, "</b>"
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
        )
      ) %>%
        # =========================================== #
        # ---- Shapefile outline: Fire Districts ----
      # =========================================== #
      leaflet::addPolygons(
        data = fire_districts_sf,
        group = "Fire Districts",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", DEPARTMENT, "</b>"
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
        )
      ) %>%
        # ========================================= #
        # ---- Shapefile outline: Census Tract ----
      # ========================================= #
      leaflet::addPolygons(
        data = census_tract_sf,
        group = "Census Tract",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>"
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
        )
      ) %>%
        # ===================================== #
        # ---- Shapefile outline: Zip Code ----
      # ===================================== #
      leaflet::addPolygons(
        data = zipcode_sf,
        group = "Zip Code",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", GEOID, "</b>"
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
        )
      ) %>%

        # ======================== #
        # ---- Layers Control ----
      # ======================== #
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c(
          "Opioid Overdose Cases",
          "Drug Crime Cases"
        ),
        overlayGroups = c(
          "Treatment Providers",
          "FC School Districts",
          "Fire Districts",
          "Census Tract",
          "Zip Code"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
        leaflet::hideGroup(
          c(
            "Treatment Providers",
            "FC School Districts",
            "Fire Districts",
            "Census Tract",
            "Zip Code"
          )
        ) %>%
        # --- Full Screen Control --- #
        leaflet.extras::addFullscreenControl(
          position = "topleft",
          pseudoFullscreen = FALSE
        ) %>%
        leaflet.extras::addResetMapButton() # reset

    })

    # =================================================== #
    # ---- Update: Highligh selected zip code region ----
    # =================================================== #
    shiny::observe({
      selected_zip <- filter_selection$zip
      if (!is.null(selected_zip)) {
        leaflet::leafletProxy("overdose_map") %>%
          leaflet::clearGroup(group = "Zip Code") %>%
          leaflet::addPolygons(
            data = zipcode_sf %>%
              dplyr::mutate(
                color = ifelse(.data$GEOID %in% selected_zip, "orange", "#555555")
              ),
            group = "Zip Code",
            stroke = TRUE,
            color = ~color,
            weight = 1,
            opacity = 0.8,
            dashArray = "3",
            fillOpacity = 0.1,
            options = leaflet::pathOptions(pane = "County_districts_polyline"),

            label = ~ paste0(
              "<b>", GEOID, "</b>"
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
            )
          )
      } else {
        leaflet::leafletProxy("overdose_map") %>%
          leaflet::clearGroup(group = "Zip Code") %>%
          leaflet::addPolygons(
            data = zipcode_sf %>%
              dplyr::mutate(
                color =  "#555555"
              ),
            group = "Zip Code",
            stroke = TRUE,
            color = ~color,
            weight = 1,
            opacity = 0.8,
            dashArray = "3",
            fillOpacity = 0.1,
            options = leaflet::pathOptions(pane = "County_districts_polyline"),

            label = ~ paste0(
              "<b>", GEOID, "</b>"
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
            )
          )
      }
    })

    # ============================================= #
    # ---- Update: Overdose markers & clusters ----
    # ============================================= #

    shiny::observe({
      leaflet::leafletProxy("overdose_map") %>%
        leaflet::clearGroup(group = "Opioid Overdose Cases") %>%
        leaflet::addCircleMarkers(
          data = overdose_data(),
          lng = ~lng, lat = ~lat,
          stroke = FALSE,
          fillColor = "#de2d26",
          fillOpacity = 0.3,
          clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
          group = "Opioid Overdose Cases"
        )
    })

    # ==================================================== #
    # ---- Update: Drug crime data markers & clusters ----
    # ==================================================== #

    shiny::observe({
      leaflet::leafletProxy("overdose_map") %>%
        leaflet::clearGroup(group = "Drug Crime Cases") %>%
        leaflet::addCircleMarkers(
          data = drug_crime_data(),
          lng = ~lng, lat = ~lat,
          stroke = FALSE,
          fillColor = "#756bb1",
          fillOpacity = 0.3,
          clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
          group = "Drug Crime Cases"
        )
    })

    # ================================================== #
    # ---- Update: Treatment data providers markers ----
    # ================================================== #
    shiny::observe({
      leaflet::leafletProxy("overdose_map") %>%
        leaflet::clearGroup(group = "Treatment Providers") %>%
        leaflet::addMarkers(
          data = treatment_providers_data(),
          lng = ~lng, lat = ~lat,
          popup = ~popup,
          #label = ~label,
          group = "Treatment Providers",
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-size" = "15px",
              "font-style" = "bold",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        )
    })


    # shiny::observe({
    #
    #   leaflet::leafletProxy("overdose_map") %>%
    #     leaflet::clearMarkerClusters() %>%
    #     leaflet::clearMarkers() %>%
    #     leaflet::addCircleMarkers(
    #       data = overdose_data(),
    #       lng = ~lng, lat = ~lat,
    #       stroke = FALSE,
    #       fillColor = "#de2d26",
    #       fillOpacity = 0.3,
    #       clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
    #       group = "Opioid Overdose Cases"
    #     ) %>%
    #     leaflet:::addCircleMarkers(
    #       data = drug_crime_data(),
    #       lng = ~lng, lat = ~lat,
    #       stroke = FALSE,
    #       fillColor = "#756bb1",
    #       fillOpacity = 0.3,
    #       clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
    #       group = "Drug Crime Cases"
    #     )
    # })
  })
}



