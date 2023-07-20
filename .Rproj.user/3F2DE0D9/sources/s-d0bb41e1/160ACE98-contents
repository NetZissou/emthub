fcphUI <- function(id) {
  shiny::tagList(
    shiny::div(
      class = "fcph_map",

      # ==================== #
      # ---- CCS Format ----
      # ==================== #
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          "
          div.fcph_map {
            position: fixed;
            top: 85px;
            left: 0;
            right: 0;
            bottom: 0;
            overflow: hidden;
            padding: 0;
          }

          #controlBox {
            opacity: 0.95;
          }

          #businessTable {
            opacity: 0.95;
          }
          "
        ))
      ),


      # ============================================ #
      # ---- Result: Hot Spot Map (Interactive) ----
      # ============================================ #

      leaflet::leafletOutput(
        outputId = shiny::NS(id, "fcph_map"),
        width="100%", height="100%"
      ),

      shiny::absolutePanel(
        id = "controls",
        fixed = TRUE,
        draggable = FALSE,
        top = 100, left = 20,
        right = "auto", bottom = "auto",
        width = 700, height = "auto",

        shinydashboard::box(
          id = "controlBox",
          title = shiny::tagList(shiny::icon("gear"), "Toolkit"),
          collapsible = TRUE, width = 12, solidHeader = TRUE, collapsed = TRUE,

          shiny::tabsetPanel(

            # ================================== #
            # ---- Tab: Base Layer Controls ----
            # ================================== #
            shiny::tabPanel(
              "Base Layer",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = shiny::NS(id, "base_layer_data_source"),
                    label = "Source",
                    choices = c(
                      "Raw Overdose Case Data",
                      "Filtered Overdose Case Data",
                      "Drug-related Crime Data"
                      # TODO: Make 311 Data available
                      #"311 Request Data"
                    ),
                    selected = NULL
                  )
                ),

                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = shiny::NS(id, "base_layer_display_option"),
                    label = "Display Option",
                    choices = c(
                      "nested cluster",
                      "spatial rate"
                    ),
                    selected = NULL
                  )
                )
              ),

              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shinyWidgets::sliderTextInput(
                    inputId = shiny::NS(id, "date_range"),
                    label = "Date Range",
                    choices = purrr::map_chr(
                      .x = seq(
                        from = as.Date("2008-01-01"),
                        to = Sys.Date(),
                        by = "1 month"),
                      .f = function(date) {
                        year <- lubridate::year(date)
                        month_num <- lubridate::month(date)
                        return(paste0(year, "-", month_num))
                      }
                    ),
                    selected = purrr::map_chr(
                      .x = c(as.Date("2008-01-01"), Sys.Date()),
                      .f = function(date) {
                        year <- lubridate::year(date)
                        month_num <- lubridate::month(date)
                        return(paste0(year, "-", month_num))
                      }
                    )
                  )
                )
              ),



              shiny::fluidRow(

                shiny::column(
                  width = 12,
                  # ======================= #
                  # ---- Param: Update ---- #
                  # ======================= #
                  shiny::actionButton(
                    inputId = shiny::NS(id, "update_base_layer"),
                    label = "Update Base Layer"
                  )
                )
              )
            ),

            # ============================= #
            # ---- Tab: Hot Spot Layer ----
            # ============================= #

            shiny::tabPanel(
              "Hot Spot Layer",

              shiny::fluidRow(

                shiny::column(
                  width = 6,
                  # =========================== #
                  # ---- Param: Bin Width ----  #
                  # =========================== #

                  shiny::numericInput(
                    inputId = shiny::NS(id, "hot_spot_bin_width"),
                    label = "Bin Size",
                    min = 0,
                    value = 0.01,
                    step = 0.001
                  )
                ),
                shiny::column(
                  width = 6,
                  # ========================= #
                  # ---- Param: Quantile ---- #
                  # ========================= #
                  shiny::numericInput(
                    inputId = shiny::NS(id, "hot_spot_quantile"),
                    label = "Hot Spot Quantile",
                    min = 0,
                    max = 0.99,
                    value = 0.75,
                    step = 0.01
                  )
                )

              ),

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  # ===================================== #
                  # ---- Param: Overdose Data Source ---- #
                  # ===================================== #
                  shiny::selectInput(
                    inputId = shiny::NS(id, "hot_spot_od_data_source"),
                    label = "Choose overdose cases data source",
                    choices = c(
                      "Raw Overdose Case Data",
                      "Filtered Overdose Case Data"
                    ),
                    selected = NULL
                  )
                ),

                shiny::column(
                  width = 6,
                  # ======================= #
                  # ---- Param: Update ---- #
                  # ======================= #
                  shiny::actionButton(
                    inputId = shiny::NS(id, "update_param_hot_spot"),
                    label = "Update Hyperparameters"
                  )
                )
              ),

              shiny::plotOutput(
                outputId = shiny::NS(id, "hot_spot_map_mini"),
                height = "500px",
                width = "100%"
              )
            ),

            # ================================= #
            # ---- Tab: Region of Interest ----
            # ================================= #
            shiny::tabPanel(
              "Region Analysis",

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = shiny::NS(id, "region_type"),
                    label = "Region Type",
                    multiple = FALSE,
                    choices = list(
                      `Zip` = "zip",
                      `Census Tract` = "census_tract",
                      `School District` = "school_district",
                      `EMS Agency` = "agency"
                    ),
                    selected = ""
                  ),
                ),
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = shiny::NS(id, "region_select"),
                    label = "Choices",
                    multiple = TRUE,
                    choices = c(),
                    selected = ""
                  )
                )
              ),

              shiny::tabsetPanel(
                type = "tabs",

                shiny::tabPanel(
                  "Aggregation Plot",
                  shinyWidgets::radioGroupButtons(
                    inputId = shiny::NS(id, "agg_choice"),
                    label = NULL,
                    choices = list(
                      Year = "year",
                      Month = "month",
                      Week = "week"
                    ),
                    status = "danger"
                  ),

                  highcharter::highchartOutput(
                    shiny::NS(id, "agg_plot")
                  )
                ),

                shiny::tabPanel(
                  "Count by Agency",

                  reactable::reactableOutput(
                    outputId = shiny::NS(id, "od_count_by_agency_table")
                  )
                )
              ),


              # EWMA Chart

              # Resource List

              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_school_district"),
              #   label = "School District",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_fire_district"),
              #   label = "Fire District",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_census_tract"),
              #   label = "Census Tract",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # ),
              #
              # shiny::selectizeInput(
              #   inputId = shiny::NS(id, "selector_zip"),
              #   label = "Zip",
              #   multiple = TRUE,
              #   choices = c(), #TODO
              #   selected = ""
              # )
            ),


            shiny::tabPanel(
              "Search",
              shiny::fluidRow(
                shiny::column(
                  width = 8,
                  shiny::textInput(
                    shiny::NS(id, "street_address"),
                    label = "Street"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    shiny::NS(id, "city"),
                    label = "City"
                  )
                ),
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    shiny::NS(id, "county"),
                    label = "County"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::textInput(
                    shiny::NS(id, "zip"),
                    label = "Zip"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                    shiny::NS(id, "search"),
                    "Search"
                  ),
                  shiny::downloadButton(
                    shiny::NS(id, "export"),
                    label = "Export",
                    icon = NULL
                  )
                )
              ),

              reactable::reactableOutput(shiny::NS(id, "search_table"))
            )
          )
        )
      )

    )
  )

}


fcphSERVER <- function(id, filtered_overdose_data, od_data_all, drug_crime_data_all) {

  shiny::moduleServer(id, function(input, output, session){

    shiny::req(
      shiny::is.reactivevalues(filtered_overdose_data)
    )

    # ================================ #
    # ---- Load Data & Shapefiles ----
    # ================================ #

    franklin_county_school_district_sf <-
      get_franklin_county_school_district_sf()

    fire_districts_sf <-
      get_fire_district_sf()

    census_tract_sf <-
      get_census_tract_sf()

    zipcode_sf <-
      get_zipcode_sf()

    jurisdiction_sf <-
      get_jurisdictions_sf()

    # > Food Pantries
    food_pantries <-
      data_food_pantries()

    food_pantries_icon <-
      leaflet::makeIcon(
        iconUrl  = "inst/icons/food.png",
        iconWidth = 36,
        iconHeight = 36
      )

    # > HIV Testing Locations
    hiv_testing_locations <-
      data_hiv_testing_locations()

    hiv_testing_locations_icon <-
      leaflet::makeAwesomeIcon(
        icon = 'health',
        library = 'ion',
        iconColor = 'white',
        markerColor = 'red'
      )

    # > Treatment Providers
    treatment_providers_data_all <- opioidDashboard::treatment_providers_data(tier_2_only = TRUE)

    # > Hep C Treatment
    hepc_treatment <- data_hepc_treatment()
    hepc_treatmetn_facilities <-
      hepc_treatment %>%
      dplyr::count(
        .data$adrs_id,
        .data$org_nm,
        .data$adr_geocode,
        .data$lat,
        .data$lng,
        .data$school_district,
        .data$EMS,
        .data$census_tract
      ) %>%
      dplyr::mutate(
        popup = paste(sep = "<br/>",
                      "<b>Hep C Treatment Resource<b>",
                      glue::glue(
                        "<b>{org_name}</b>", org_name = .data$org_nm
                      ),
                      glue::glue(
                        "<b>Address: </b>{org_addr}", org_addr = .data$adr_geocode
                      ),
                      glue::glue(
                        "<b>Number of Doctors: </b>{n_doc}", n_doc = .data$n
                      )
        )
      )

    # > COTA stops
    sf_cota_stops <-
      get_cota_bus_stops_sf()

    # > COTA lines
    sf_cota_lines <-
      get_cota_bus_lines_sf()

    # > 311 Heatmap
    heatmap_data_311 <-
      data_columbus_311_heatmap()

    # > Public Places
    public_places <-
      data_public_places()

    # > OD Death Data
    od_death_rate_ct <-
      data_drug_overdose_deaths_ct_10yr_rate()
    od_death_rate_zip <-
      data_drug_overdose_deaths_zip_5yr_rate()

    # ========================= #
    # ---- Hyperparameters ----
    # ========================= #


    hyper_params <- shiny::reactiveValues(
      base_layer_data_source = od_data_all,
      base_layer_display_option = "nested cluster",
      hot_spot_data_source = od_data_all,
      hot_spot_bin_width = 0.01,
      hot_spot_quantile = 0.75
    )

    hyper_params_sf_rate <-
      shiny::reactiveValues(
        zip = list(
          region_sf = get_region_od_rate(
            "zip",
            zipcode_sf,
            "GEOID",
            "GEOID",
            "TOTAL_POP",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$zip
        ),
        census_tract = list(
          region_sf = get_region_od_rate(
            "census_tract",
            census_tract_sf,
            "GEOID",
            "GEOID",
            "TOTAL_POP",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$census_tract
        ),
        school_district = list(
          region_sf = get_region_od_rate(
            "school_district",
            franklin_county_school_district_sf,
            "NAME",
            "NAME",
            "district_pop",
            od_data = od_data_all
          ),
          pal = opioidDashboard::od_case_rate_init_pal_list$school_district
        )
        # TODO: No population
        # EMS = list(
        #   region_sf = get_region_od_rate(
        #     "EMS",
        #     EMS_sf,
        #     "DEPARTMENT",
        #     "DEPARTMENT",
        #     #"TOTAL_POP",
        #     od_data = hyper_params$data_source
        #   )
        # )
      )

    pal_death_rate_ct <- leaflet::colorQuantile(
      palette = "YlOrRd",
      domain = od_death_rate_ct$rate,
      n = 5
    )

    pal_death_rate_zip <- leaflet::colorQuantile(
      palette = "YlOrRd",
      domain = od_death_rate_zip$rate,
      n = 5
    )

    # ======================================= #
    # > Update param: hot spot detection ----

    shiny::observeEvent(input$update_param_hot_spot, {

      # ===================================== #
      # ---- Handle overdose data source ---- #
      # ===================================== #
      od_data_source <- input$hot_spot_od_data_source

      if (od_data_source == "Raw Overdose Case Data") {
        hyper_params$data_source <- od_data_all
      } else {
        hyper_params$data_source <- filtered_overdose_data$data
      }

      # ========================== #
      # ---- Handle bin width ---- #
      # ========================== #
      hyper_params$hot_spot_bin_width <- input$hot_spot_bin_width

      # ========================= #
      # ---- Handle quantile ---- #
      # ========================= #
      hyper_params$hot_spot_quantile <- input$hot_spot_quantile

    })

    # ===================================== #
    # > Update param: date range input ----
    shiny::observe({

      base_layer_data_source <- input$base_layer_data_source

      if (base_layer_data_source == "Raw Overdose Case Data") {
        date_min <- min(od_data_all$date)
        date_max <- max(od_data_all$date)
      } else if (base_layer_data_source == "Filtered Overdose Case Data") {
        date_min <- min(filtered_overdose_data$data$date)
        date_max <- max(filtered_overdose_data$data$date)
      } else if (base_layer_data_source == "Drug-related Crime Data") {
        date_min <- min(drug_crime_data_all$date, na.rm = T)
        date_max <- max(drug_crime_data_all$date, na.rm = T)
      }

      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "date_range",
        selected = purrr::map_chr(
          .x = c(date_min, date_max),
          .f = function(date) {
            year <- lubridate::year(date)
            month_num <- lubridate::month(date)
            return(paste0(year, "-", month_num))
          }
        ),
        choices = purrr::map_chr(
          .x = seq(
            from = date_min,
            to = date_max,
            by = "1 month"),
          .f = function(date) {
            year <- lubridate::year(date)
            month_num <- lubridate::month(date)
            return(paste0(year, "-", month_num))
          }
        )
      )

    })

    # =========================================== #
    # > Update param: base layer data source ----

    shiny::observeEvent(input$update_base_layer, {

      base_layer_data_source <- input$base_layer_data_source

      data_min <- input$date_range[1]
      data_max <- input$date_range[2]

      if (base_layer_data_source == "Raw Overdose Case Data") {
        hyper_params$base_layer_data_source <- od_data_all %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      } else if (base_layer_data_source == "Filtered Overdose Case Data") {
        hyper_params$base_layer_data_source <- filtered_overdose_data$data %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      } else if (base_layer_data_source == "Drug-related Crime Data") {
        hyper_params$base_layer_data_source <- drug_crime_data_all %>%
          dplyr::filter(
            .data$date >= lubridate::ym(data_min),
            .data$date <= lubridate::ym(data_max)
          )
      }


      hyper_params$base_layer_display_option <-
        input$base_layer_display_option

    })

    # ===================================== #
    # > Update param: Region Selection ----

    shiny::observeEvent(input$region_type, {

      region_type <- input$region_type

      if (region_type == "zip") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = zipcode_sf$GEOID
        )
      } else if (region_type == "census_tract") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = census_tract_sf$GEOID
        )
      } else if (region_type == "school_district") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = franklin_county_school_district_sf$NAME
        )
      } else if (region_type == "agency") {

        shiny::updateSelectizeInput(
          inputId = "region_select",
          choices = opioidDashboard::AGENCY
        )
      }

    })


    # =================================== #
    # ---- Hot Spot Algorithm Result ----
    # =================================== #

    # ---- Hot Spot Region Data for heatmap ---- #
    hot_spot_region <- shiny::reactiveValues(
      value = opioidDashboard::get_hot_spot_region(
        od_data = od_data_all,
        percent_tile = 0.75,
        bin_width = c(0.01, 0.01)
      )
    )



    shiny::observeEvent(input$update_param_hot_spot, {
      hot_spot_region$value <-
        opioidDashboard::get_hot_spot_region(
          od_data = hyper_params$hot_spot_data_source,
          percent_tile = hyper_params$hot_spot_quantile,
          bin_width = c(hyper_params$hot_spot_bin_width, hyper_params$hot_spot_bin_width)
        )
    })


    # ---- Hot Spot Map (mini) ---- #

    output$hot_spot_map_mini <- shiny::renderPlot({

      hot_spot_data_source <- hyper_params$hot_spot_data_source
      hot_spot_bin_width <- hyper_params$hot_spot_bin_width
      hot_spot_quantile <- hyper_params$hot_spot_quantile


      ggmap::ggmap(opioidDashboard::COLUMBUS_STATIC_ROADMAP) +
        ggplot2::geom_bin_2d(
          ggplot2::aes(x = .data$lng, y = .data$lat),
          binwidth = c(hot_spot_bin_width, hot_spot_bin_width),
          alpha = 0.75,
          data = hot_spot_data_source
        ) +
        ggplot2::geom_text(
          ggplot2::aes(x = .data$lng, y = .data$lat, label = .data$hot_spot),
          data =  opioidDashboard::get_hot_spot_region(
            od_data = hot_spot_data_source,
            percent_tile = hot_spot_quantile,
            bin_width = c(hot_spot_bin_width, hot_spot_bin_width)
          ),
          alpha = 0.75
        ) +
        ggplot2::scale_fill_gradient('Overdose Cases',
                                     low = "#ffeda0",
                                     high = "#f03b20") +
        ggplot2::coord_quickmap(
          xlim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lng-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lng+0.15
          ),
          ylim = c(
            opioidDashboard::opioid_overdose_map_init_bounds$min_lat-0.1,
            opioidDashboard::opioid_overdose_map_init_bounds$max_lat+0.1
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "", title = NULL) +
        ggplot2::theme(
          legend.position = "top",
          plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        )
    })

    # ================== #
    # ---- Map Init ----
    # ================== #

    output$fcph_map <- leaflet::renderLeaflet({

      opioid_overdose_map_init_bounds <- opioidDashboard::opioid_overdose_map_init_bounds

      hot_spot_data_source <- hyper_params$hot_spot_data_source
      hot_spot_bin_width <- hyper_params$hot_spot_bin_width
      hot_spot_quantile <- hyper_params$hot_spot_quantile

      heatmap_data <-
        ggplot2::layer_data(
          ggplot2::ggplot() +
            ggplot2::geom_bin_2d(
              ggplot2::aes(x = .data$lng, y = .data$lat),
              binwidth = c(hot_spot_bin_width, hot_spot_bin_width),
              data = hot_spot_data_source
            ) +
            ggplot2::scale_fill_gradient('Overdose Cases',
                                         low = "#ffeda0",
                                         high = "#f03b20")
        )

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        leaflet::setView(
          lat = 39.9612,
          lng = -82.9988,
          zoom = 10
        ) %>%
        # =================== #
        # ---- Map Tiles ----
      # =================== #
      leaflet::addTiles() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%

        # ==================== #
        # ---- Base Layer ----
      # ===================== #
      leaflet::addCircleMarkers(
        data = hyper_params$base_layer_data_source,
        lng = ~lng, lat = ~lat,
        stroke = FALSE,
        fillColor = "#de2d26",
        fillOpacity = 0.3,
        clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
        group = "Base Layer",
        clusterId = "baseCluster"
      ) %>%

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

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
        # ========================================= #
        # ---- Shapefile outline: Jurisdiction ----
      # ======================================== #
      leaflet::addPolygons(
        data = jurisdiction_sf,
        group = "Jurisdiction",
        stroke = TRUE,
        color = "#555555",
        weight = 1,
        opacity = 0.8,
        dashArray = "3",
        fillOpacity = 0.1,
        #options = leaflet::pathOptions(pane = "County_districts_polyline"),

        label = ~ paste0(
          "<b>", name, "</b>"
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
        # =========================== #
        # ---- Hot Spot Heat Map ----
      # ============================ #
      leaflet::addRectangles(
        data = heatmap_data,
        group = "Heatmap",
        lng1 = ~xmin, lng2 = ~xmax,
        lat1 = ~ymin, lat2 = ~ymax,
        color = ~fill,
        dashArray = "3",
        fillColor = ~fill,
        fillOpacity = 0.40,
        opacity = 0
      ) %>%
        # ====================== #
        # ---- Food Pantries ----
      # ======================= #
      leaflet::addMarkers(
        data = food_pantries,
        lat = ~lat,
        lng = ~lng,
        popup = ~popup_label,
        icon = food_pantries_icon,
        group = "Food Pantries"
      ) %>%
        # =============================== #
        # ---- HIV Testing Locations ----
      # ================================ #
      leaflet::addAwesomeMarkers(
        data = hiv_testing_locations,
        lat = ~lat,
        lng = ~lng,
        popup = ~popup_label,
        icon = hiv_testing_locations_icon,
        group = "HIV Testing Sites"
      ) %>%
        # ============================= #
        # ---- Treatment Providers ----
      # ============================== #
      leaflet::addMarkers(
        data = treatment_providers_data_all,
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
      ) %>%
        # =================================== #
        # ---- HepC Treatment Facilities ----
      # ==================================== #
      leaflet::addMarkers(
        data = hepc_treatmetn_facilities,
        lng = ~lng, lat = ~lat,
        popup = ~popup,
        #label = ~label,
        group = "Hep C Treatment Facilities"
        # labelOptions = leaflet::labelOptions(
        #   style = list(
        #     "font-size" = "15px",
        #     "font-style" = "bold",
        #     "border-color" = "rgba(0,0,0,0.5)"
        #   )
        # )
      ) %>%

        # ======================= #
        # ---- COTA BUS Info ----
      # ======================== #
      leaflet::addPolylines(
        data = sf_cota_lines,
        color = "black",
        label = ~LineName,
        highlight = leaflet::highlightOptions(
          weight = 8,
          #fillOpacity = 0.1,
          color = "blue",
          #dashArray = "",
          #opacity = 0.5,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        group = "COTA Lines"
      ) %>%
        leaflet::addCircles(
          data = sf_cota_stops,
          color = "red",
          popup = ~StopName,
          group = "COTA Stops"
        ) %>%

        # ============================== #
        # ---- Columbus 311 Heatmap ----
      # =============================== #
      leaflet::addRectangles(
        data = heatmap_data_311,
        group = "Columbus 311 Request Heatmap (2022)",
        lng1 = ~xmin, lng2 = ~xmax,
        lat1 = ~ymin, lat2 = ~ymax,
        color = ~fill,
        dashArray = "3",
        fillColor = ~fill,
        fillOpacity = 0.60,
        opacity = 0
      ) %>%

        # =================================== #
        # ---- FCPH Location of Interest ----
      # ==================================== #
      leaflet::addMarkers(
        data = opioidDashboard::FCPH_LOCATION_OF_INTEREST,
        #lng = ~lng, lat = ~lat,
        popup = ~popup,
        #label = ~label,
        group = "FCPH Location of Interest"
      ) %>%

        # ======================= #
        # ---- Public Places ----
      # ======================= #
      # leaflet::addAwesomeMarkers(
      #   data = public_places,
      #   #lng = ~lng, lat = ~lat,
      #   label = ~POI_NAME,
      #   popup = ~popup,
      #   icon = public_places_icons,
      #   #label = ~label,
      #   group = "Public Places"
      # ) %>%

      # > Arena/Statdium
      leaflet::addAwesomeMarkers(
        data = public_places %>% dplyr::filter(type == "Arena/Stadium"),
        #lng = ~lng, lat = ~lat,
        label = ~POI_NAME,
        popup = ~popup,
        icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Arena/Stadium`,
        #label = ~label,
        group = "Public Places - Arena/Stadium"
      ) %>%
        # > Cemetery
        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Cemetery"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Cemetery`,
          #label = ~label,
          group = "Public Places - Cemetery"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Community/Recreation Center"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Community/Recreation Center`,
          #label = ~label,
          group = "Public Places - Community/Recreation Center"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Convention Center"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Convention Center`,
          #label = ~label,
          group = "Public Places - Convention Center"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Historic Site"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Historic Site`,
          #label = ~label,
          group = "Public Places - Historic Site"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "House of Worship"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`House of Worship`,
          #label = ~label,
          group = "Public Places - House of Worship"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Library"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Library`,
          #label = ~label,
          group = "Public Places - Library"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Museum"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Museum`,
          #label = ~label,
          group = "Public Places - Museum"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Other"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Other`,
          #label = ~label,
          group = "Public Places - Other"
        ) %>%

        leaflet::addAwesomeMarkers(
          data = public_places %>% dplyr::filter(type == "Theater/Concert Hall"),
          #lng = ~lng, lat = ~lat,
          label = ~POI_NAME,
          popup = ~popup,
          icon = opioidDashboard::PUBLIC_PLACES_ICON_LIST$`Theater/Concert Hall`,
          #label = ~label,
          group = "Public Places - Theater/Concert Hall"
        ) %>%

        # ============================================ #
        # ---- Unintentional Drug Overdose Deaths ----
      # ============================================= #

      leaflet::addPolygons(
        data = od_death_rate_ct,
        group = "OD Death 10yrs Rate by CT",
        stroke = TRUE,
        color = ~pal_death_rate_ct(rate), #~hyper_params_sf_rate$census_tract$pal(rate_pal),
        weight = 1,
        #opacity = 0.8,
        dashArray = "3",
        #fillOpacity = 0.1,

        label = ~ label %>% lapply(htmltools::HTML),

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

        leaflet::addPolygons(
          data = od_death_rate_zip,
          group = "OD Death 5yrs Rate by Zip",
          stroke = TRUE,
          color = ~pal_death_rate_zip(rate), #~hyper_params_sf_rate$census_tract$pal(rate_pal),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          #fillOpacity = 0.1,

          label = ~ label %>% lapply(htmltools::HTML),

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
          "Base Layer"
        ),
        overlayGroups = c(
          "Heatmap",
          "FC School Districts",
          "Fire Districts",
          "Census Tract",
          "Zip Code",
          "Jurisdiction",
          "Food Pantries",
          "HIV Testing Sites",
          "Treatment Providers",
          "Hep C Treatment Facilities",
          "COTA Lines",
          "COTA Stops",
          "Columbus 311 Request Heatmap (2022)",
          "FCPH Location of Interest",
          "OD Death 5yrs Rate by Zip",
          "OD Death 10yrs Rate by CT",
          #"Public Places"
          stringr::str_c("Public Places - ", names(opioidDashboard::PUBLIC_PLACES_ICON_LIST))
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
        leaflet::hideGroup(
          c(
            "Heatmap",
            "FC School Districts",
            "Fire Districts",
            "Census Tract",
            "Zip Code",
            "Jurisdiction",
            "Food Pantries",
            "HIV Testing Sites",
            "Treatment Providers",
            "Hep C Treatment Facilities",
            "COTA Lines",
            "COTA Stops",
            "Columbus 311 Request Heatmap (2022)",
            "FCPH Location of Interest",
            "OD Death 5yrs Rate by Zip",
            "OD Death 10yrs Rate by CT",
            #"Public Places"
            stringr::str_c("Public Places - ", names(opioidDashboard::PUBLIC_PLACES_ICON_LIST))
          )
        ) %>%

        # ========================= #
        # ---- Cluster Control ----
      # ========================= #

      leaflet::addEasyButton(leaflet::easyButton(
        position = "topright",
        states = list(
          leaflet::easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'baseCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          leaflet::easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'baseCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))
      # --- Full Screen Control --- #
      # leaflet.extras::addFullscreenControl(
      #   position = "topleft",
      #   pseudoFullscreen = FALSE
      # ) %>%
      # leaflet.extras::addResetMapButton() # reset
    })


    # ==================================== #
    # ---- Map Update (click observe) ----
    # ==================================== #

    map_select_region <- shiny::reactiveValues(
      value = NULL,
      group = NULL,
      click = NULL
    )

    shiny::observe({

      click <- input$fcph_map_shape_click

      selected_region <- NULL
      group<- NULL

      if (!is.null(click)) {

        if (click$group == "FC School Districts") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = franklin_county_school_district_sf, id = "NAME")

          group <- "school_district"

        } else if (click$group == "Fire Districts") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = fire_districts_sf, id = "DEPARTMENT")

          group <- "EMS"

        } else if (click$group == "Census Tract") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = census_tract_sf, id = "GEOID")

          group <- "census_tract"

        } else if (click$group == "Zip Code") {

          selected_region <-
            geocoding_sf(click$lat, click$lng, sf = zipcode_sf, id = "GEOID")

          group <- "zip"

        }

      }

      print(click)

      #print(selected_region)
      map_select_region$value <- selected_region
      map_select_region$group <- group
      #map_select_region$click <- click

    })

    shiny::observe({

      print(map_select_region$value)
      print(map_select_region$group)

      # if (!is.null(map_select_region$value)) {
      #   print("not null")
      #   region <- map_select_region$value
      #   attr <- map_select_region$group
      #   if (region %in% map_select_stack$value$name) {
      #     map_select_stack$value <-
      #       map_select_stack$value %>%
      #       dplyr::filter(
      #         !name %in% region
      #       )
      #   } else {
      #     map_select_stack$value <-
      #       map_select_stack$value %>%
      #       rbind(
      #         c(region, attr)
      #       )
      #   }
      # }
    })

    # shiny::observe({
    #   print(map_select_stack$value)
    # })


    shiny::observeEvent(input$update_base_layer, {

      if (hyper_params$base_layer_display_option == "spatial rate") {

        print("spatial rate")

        leaflet::leafletProxy("fcph_map") %>%
          leaflet::clearGroup(
            c(
              "Base Layer",
              "FC School Districts",
              "Census Tract",
              "Zip Code"
            )
          ) %>%
          # ================== #
          # ---- Zip Code ----
        # ================== #
        leaflet::addPolygons(
          data = hyper_params_sf_rate$zip$region_sf,
          group = "Zip Code",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$zip$pal(rate_pal),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          #fillOpacity = 0.1,

          label = ~ label %>% lapply(htmltools::HTML),

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

          # ============================ #
          # ---- FC School District ----
        # ============================ #
        leaflet::addPolygons(
          data = hyper_params_sf_rate$school_district$region_sf,
          group = "FC School Districts",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$school_district$pal(rate_pal),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          #fillOpacity = 0.1,

          label = ~ label %>% lapply(htmltools::HTML),

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
          # ====================== #
          # ---- Census Tract ----
        # ====================== #
        leaflet::addPolygons(
          data = hyper_params_sf_rate$census_tract$region_sf,
          group = "Census Tract",
          stroke = TRUE,
          color = ~hyper_params_sf_rate$census_tract$pal(rate_pal),
          weight = 1,
          #opacity = 0.8,
          dashArray = "3",
          #fillOpacity = 0.1,

          label = ~ label %>% lapply(htmltools::HTML),

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
            "Zip Code",
            "FC School Districts",
            "Census Tract"
          ),
          overlayGroups = c(
            "Jurisdiction",
            "Heatmap",
            "Fire Districts",
            "Food Pantries",
            "HIV Testing Sites",
            "Treatment Providers",
            "Hep C Treatment Facilities",
            "COTA Lines",
            "COTA Stops",
            "Columbus 311 Request Heatmap (2022)",
            "FCPH Location of Interest",
            "OD Death 5yrs Rate by Zip",
            "OD Death 10yrs Rate by CT",
            #"Public Places"
            stringr::str_c("Public Places - ", names(opioidDashboard::PUBLIC_PLACES_ICON_LIST))
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
          leaflet::showGroup("Zip Code") %>%
          leaflet::hideGroup(
            c(
              "Jurisdiction",
              "Heatmap",
              "Fire Districts",
              "FC School Districts",
              "Census Tract",
              "Food Pantries",
              "HIV Testing Sites",
              "Treatment Providers",
              "Hep C Treatment Facilities",
              "COTA Lines",
              "COTA Stops",
              "Columbus 311 Request Heatmap (2022)",
              "FCPH Location of Interest",
              "OD Death 5yrs Rate by Zip",
              "OD Death 10yrs Rate by CT",
              #"Public Places"
              stringr::str_c("Public Places - ", names(opioidDashboard::PUBLIC_PLACES_ICON_LIST))
            )
          )


      }

    })


    # ========================= #
    # ---- Region Analysis ----
    # ========================= #

    # ====================== #
    # > Region Od Data ----- #
    region_od_data <- shiny::reactiveValues(
      value = NULL
    )

    shiny::observe({
      region_type <- input$region_type
      region_select <- input$region_select

      if (!rlang::is_empty(region_select)) {

        filter_result <-
          od_data_all %>%
          dplyr::filter(
            .data[[region_type]] %in% region_select
          )

        if (nrow(filter_result) != 0) {
          region_od_data$value <-
            filter_result
        } else {
          region_od_data$value <- NULL
        }


      } else {
        region_od_data$value <- NULL
      }

    })

    # =============== #
    # > Agg plot ---- #
    output$agg_plot <- highcharter::renderHighchart({

      if (!rlang::is_empty(region_od_data$value)) {
        agg_data <-
          region_od_data$value %>%
          dplyr::mutate(
            value = 1
          ) %>%
          timetk::summarise_by_time(
            .date_var = date_ingested,
            .by = input$agg_choice,
            value = sum(value)
          ) %>%
          timetk::pad_by_time(.date_var = date_ingested, .pad_value = 0) %>%
          dplyr::mutate(
            date_ingested = as.Date(.data$date_ingested)
          )

        plot_title <- glue::glue(
          "<b>Opioid Overdose Cases Agg by {agg_choice}</b>",
          agg_choice = stringr::str_to_title(input$agg_choice)
        )

        plot_subtitle <- stringr::str_c(input$region_select, collapse = ", ")

        highcharter::hchart(
          agg_data, type  = "area", name = "OD Cases",
          highcharter::hcaes(x = date_ingested, y = value)
        ) %>%
          highcharter::hc_title(
            text = plot_title,
            align = "left",
            style = list(useHTML = TRUE)
          ) %>%
          highcharter::hc_subtitle(
            text = plot_subtitle,
            align = "left",
            style = list(useHTML = TRUE)
          ) %>%
          highcharter::hc_yAxis(
            title = FALSE
          ) %>%
          highcharter::hc_xAxis(
            title = FALSE
          ) %>%
          highcharter::hc_navigator(
            enabled = TRUE
          ) %>%
          highcharter::hc_exporting(
            enabled = TRUE,
            filename = "od_agg"
          )
      }

    })

    # ===============================
    # > OD count by agency table ----
    output$od_count_by_agency_table <- reactable::renderReactable({

      od_count_by_agency <-
        tibble::tibble(
          Agency = character(),
          Count = numeric()
        )


      if (!rlang::is_empty(region_od_data$value)) {
        od_count_by_agency <-
          od_count_by_agency %>%
          dplyr::bind_rows(
            region_od_data$value %>%
              dplyr::count(.data$agency, sort = TRUE) %>%
              purrr::set_names(c("Agency", "Count"))
          )
      }

      od_count_by_agency %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          # Selection
          #selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = 5, minRows = 5
        )

    })


    # ========================== #
    # ---- Search Locations ----
    # ========================== #

    full_address <- shiny::reactiveValues(
      value = NULL,
      geocoding_result = NULL
    )

    search_table_data <- shiny::reactiveValues(
      value = tibble::tibble(
        type = character(),
        name = character(),
        contact = character(),
        address = character()
      )
    )

    map_thumbtack <- leaflet::makeAwesomeIcon(
      icon = "thumbtack",
      iconColor = "black",
      markerColor = "green",
      library = "fa"
    )

    map_pin <- leaflet::makeAwesomeIcon(
      icon = "map-pin",
      iconColor = "black",
      markerColor = "red",
      library = "fa"
    )

    # Update Address
    shiny::observeEvent(input$search, {

      # print(input$street_address)
      # print(input$zip)
      # print(input$city)
      # print(input$county)

      if (input$street_address != "") {

        addr <- input$street_address
        if (input$city != "") {
          addr <- stringr::str_c(
            c(addr, input$city), collapse = ", "
          )
        }
        if (input$county != "") {
          addr <- stringr::str_c(
            c(addr, input$county), collapse = ", "
          )
        }
        if (input$zip != "") {
          addr <- stringr::str_c(
            c(addr, input$zip), collapse = ", "
          )
        }

        full_address$value <- addr

      } else {
        full_address$value <- NULL
      }
    })

    # Filter
    shiny::observe({

      if (!rlang::is_empty(full_address$value)) {

        geocoding_result <-
          geocoding(full_address$value)

        if (is.na(geocoding_result$lat)) {
          full_address$geocoding_result <- NULL

          leaflet::leafletProxy("fcph_map") %>%
            leaflet::clearGroup(
              c("Search")
            )

          search_table_data$value <-
            tibble::tibble(
              type = character(),
              name = character(),
              contact = character(),
              address = character()
            )

        } else {

          # Update geocoding result
          full_address$geocoding_result <- geocoding_result

          leaflet::leafletProxy("fcph_map") %>%
            leaflet::clearGroup(
              c("Search")
            ) %>%
            leaflet::addAwesomeMarkers(
              lat = full_address$geocoding_result$lat,
              lng = full_address$geocoding_result$lng,
              icon = map_thumbtack,
              group = "Search"
              # full_address$value
            )


          zip_search <- full_address$geocoding_result$zip_geocode

          search_table <-
            tibble::tibble(
              type = character(),
              name = character(),
              contact = character(),
              address = character()
            )

          # > Food Pantires
          filtered_food_pantries <-
            food_pantries %>%
            dplyr::filter(
              as.character(.data$zip_code) %in% zip_search
            )

          if (nrow(filtered_food_pantries) != 0) {
            search_table <-
              search_table %>%
              dplyr::bind_rows(
                filtered_food_pantries %>%
                  dplyr::transmute(
                    type = "Food Pantries",
                    name = .data$name,
                    contact = .data$website,
                    address = .data$street_address
                  )
              )
          }

          # > HIV Testing Sites
          filtered_hiv_testing_locations <-
            hiv_testing_locations %>%
            dplyr::filter(
              as.character(.data$zip) %in% zip_search
            )

          if (nrow(filtered_hiv_testing_locations) != 0) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                filtered_hiv_testing_locations %>%
                  dplyr::transmute(
                    type = "HIV Testing Locations",
                    name = .data$name,
                    contact = glue::glue(
                      "Phone: {phone}; Website: {website}",
                      phone = .data$telephone,
                      website = .data$website
                    ),
                    address = .data$street_address
                  )
              )
          }
          # > Treatment Providers
          filtered_treatment_providers <-
            treatment_providers_data_all %>%
            dplyr::filter(
              as.character(.data$zip) %in% zip_search
            )

          if (nrow(filtered_treatment_providers) != 0) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                filtered_treatment_providers %>%
                  dplyr::transmute(
                    type = "Treatment Providers",
                    name = .data$name,
                    contact = glue::glue(
                      "Phone: {phone}; Website: {website}",
                      phone = .data$phone,
                      website = .data$website
                    ),
                    address = .data$address
                  )
              )
          }


          # > Hep C Treatment Providers
          filtered_hep_C_treatment_providers <-
            hepc_treatment %>%
            dplyr::filter(
              stringr::str_detect(
                as.character(.data$zip),
                zip_search
              )
            )

          if (nrow(filtered_hep_C_treatment_providers) != 0) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                filtered_hep_C_treatment_providers %>%
                  dplyr::transmute(
                    type = "Hep C Treatment Providers",
                    name = glue::glue(
                      "{lst_nm}, {frst_nm}; Org: {org_nm}",
                      lst_nm = .data$lst_nm,
                      frst_nm = .data$frst_nm,
                      org_nm = .data$org_nm
                    ),
                    contact = glue::glue(
                      "Phone: {phone}",
                      phone = as.character(.data$phn_numbr)
                    ),
                    address = .data$adr_geocode
                  )
              )
          }

          # > COTA Stops
          filtered_COTA_stops <-
            sf_cota_stops %>%
            dplyr::filter(
              as.character(.data$ZIP_CODE) %in% zip_search
            )

          if (nrow(filtered_COTA_stops) != 0) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                tibble::as_tibble(filtered_COTA_stops) %>%
                  dplyr::transmute(
                    type = "COTA Bus Stop",
                    name = glue::glue(
                      "Line: {line}; Stop: {stop}",
                      line = .data$Lines, stop = .data$StopName
                    ),
                    contact = NA,
                    address = .data$StopName
                  )
              )
          }

          # > FCPH Location of Interest
          filtered_FCPH_location_of_interest <-
            opioidDashboard::FCPH_LOCATION_OF_INTEREST %>%
            dplyr::filter(
              as.character(.data$zip_geocode) %in% zip_search
            )

          if (nrow(filtered_FCPH_location_of_interest) != 0) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                tibble::as_tibble(filtered_FCPH_location_of_interest) %>%
                  dplyr::transmute(
                    type = "FCPH Location of Interest",
                    name = .data$name,
                    contact = NA,
                    address = .data$address
                  )
              )
          }

          # > Public Locations
          filtered_public_locations <-
            public_places %>%
            dplyr::filter(
              as.character(.data$zipcode) %in% zip_search
            )

          if (nrow(filtered_public_locations)) {

            search_table <-
              search_table %>%
              dplyr::bind_rows(
                tibble::as_tibble(filtered_public_locations) %>%
                  dplyr::transmute(
                    type = .data$type,
                    name = .data$POI_NAME,
                    contact = NA,
                    address = .data$LSN
                  )
              )
          }

          search_table_data$value <- search_table
        }

      }

      #print(full_address$geocoding_result)
    })

    # Update Table
    output$search_table <- reactable::renderReactable({

      search_table_data$value %>%
        purrr::set_names(
          c("Type", "Name", "Contact", "Address")
        ) %>%
        reactable::reactable(
          # Group
          groupBy = "Type",
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          height = 400,
          # Selection
          #selection = "multiple",
          onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = 5, minRows = 5
        )
    })

    # Download Table
    output$export <- shiny::downloadHandler(
      filename = function() {
        paste("search_result", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        readr::write_csv(search_table_data$value, file)
      }
    )




  })

}

