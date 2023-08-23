get_iso <- function(
  location = c(),
  range = c(),
  range_type = c("distance", "time"),
  type = c("car", "walk", "cycle")
) {

  type <- switch(
    type,
    "car"= "driving-car",
    "walk"= "foot-walking",
    "cycle"= "cycling-regular"
  )

  # Set the request URL and payload data
  url <- glue::glue(
    "https://api.openrouteservice.org/v2/isochrones/{type}",
    type = type
  )
  body <- list(
    locations = list(
      location
    ),
    range = list(
      range
    ),
    range_type = range_type
  )

  # Make the POST request
  resp <-
    httr2::request(url) %>%
    httr2::req_headers(
      Accept = "application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8",
      Authorization = emthub::TOKENS$osm,
      `Content-Type` = "application/json; charset=utf-8"
    ) %>%
    httr2::req_body_json(body) %>%
    httr2::req_perform()

  # if (resp$status_code != 200) {
  #   stop(
  #     glue::glue(
  #       "Failed to get response from API; Status code: {code}",
  #       code = resp$status_code
  #     )
  #   )
  # }

  return(
    list(
      location = location,
      range = range,
      range_type = range_type,
      type = type,
      sf = sf::read_sf(httr2::resp_body_string(resp))
      #geojson = httr2::resp_body_json(resp)$features
    )
  )
}


pull_iso_resource <- function(
  iso_sf,
  index_sf = get_sf_ct(parquet = TRUE),
  index_sf_key = "GEOID",
  resource_tbl,
  resource_tbl_key = "Census_Tract",
  resource_tbl_coords = c("longitude", "latitude")
) {

  # ================================================ #
  # ---- Find Overlap (Census Tract by Default) ----
  # ================================================ #

  overlap_index <-
    sf::st_intersects(
      iso_sf,
      index_sf
    ) %>% unlist()

  overlap_object <-
    index_sf[overlap_index, ][[index_sf_key]]

  # =============================== #
  # ---- Find Covered Resource ----
  # =============================== #

  resource_filtered <-
    resource_tbl %>%
    dplyr::filter(.data[[resource_tbl_key]] %in% overlap_object) %>%
    dplyr::collect() %>%
    sf::st_as_sf(
      coords = resource_tbl_coords
    ) %>%
    sf::st_set_crs(value = 4326)


  resource_index <-
    sf::st_contains(
      iso_sf,
      resource_filtered
    ) %>% unlist()

  return(resource_filtered[resource_index, ])
}

addISO <- function(
  map_id,
  full_address = NULL,
  location = c(),
  range = c(),
  range_type = c("distance", "time"),
  type = c("car", "walk", "cycle"),
  resource_params = NULL,
  label = NULL
) {

  # ================= #
  # ---- Geocode ----
  # ================= #
  # If full address provided
  shiny::showNotification("Generating Isochron ...", duration = 10, type = "message")

  if (!rlang::is_empty(full_address) && full_address != "") {
    # Geocode
    geocoding_result <-
      tidygeocoder::geo_combine(
        queries = list(
          list(method = "osm"),
          list(method = "census"),
          list(method = "iq")
        ),
        global_params = list(address = 'address'),
        address = full_address,
        cascade = TRUE,
        lat = "lat",
        long = "lng"
      )
    if (is.na(geocoding_result$lat)) {
      stop("Failed geocode the address provided")
    }

    location <- c(geocoding_result$lng, geocoding_result$lat)
  }


  # ============= #
  # ---- ISO ----
  # ============= #
  iso_result <-
    get_iso(
      location = location,
      range = range,
      range_type = range_type,
      type = type
    )

  # =========================== #
  # ---- Meta Data for Map ----
  # =========================== #

  if (range_type == "distance") {
    # To KM
    range_label <- round(range/1000, 1)
    range_type_label <- "KM"
  } else {
    # To Sec
    range_label <- round(range/60, 0)
    range_type_label <- "Mins"
  }

  if (rlang::is_empty(label)) {
    popup_label <-
      glue::glue(
        "
    <b>{addr}</b></br>
    <b>Range: </b>{range} {range_type}</br>
    <b>Type: </b> {type}
    ",
        addr = full_address,
        range = range_label,
        range_type = range_type_label,
        type = type
      )
  } else {
    popup_label <-
      glue::glue(
        "
    <b>{label}</b></br>
    <b>{addr}</b></br>
    <b>Range: </b>{range} {range_type}</br>
    <b>Type: </b> {type}
    ",
        label = label,
        addr = full_address,
        range = range_label,
        range_type = range_type_label,
        type = type
      )
  }

  get_center_icon <- function(type) {

    icon <- switch(
      type,
      "car"= "car",
      "walk"= "walking",
      "cycle"= "bicycle"
    )

    leaflet::makeAwesomeIcon(
      text = fontawesome::fa(icon),
      iconColor = 'black',
      markerColor = "purple"
    )
  }

  center_icon <- get_center_icon(type = type)

  # ==================== #
  # ---- Add to Map ----
  # ==================== #

  if (!rlang::is_empty(iso_result$sf) && nrow(iso_result$sf) > 0) {

    iso_resource_vax_provider <-
      pull_iso_resource(
        iso_sf = iso_result$sf,
        index_sf = resource_params$index_sf,
        index_sf_key = resource_params$index_sf_key,
        resource_tbl = resource_params$resource_tbl,
        resource_tbl_key = resource_params$resource_tbl_key,
        resource_tbl_coords = resource_params$resource_tbl_coords
      ) %>% dplyr::distinct(.data$provider_location_guid, .data$popup)

    leaflet::leafletProxy(map_id) %>%
      leaflet.extras2::addSpinner() %>%
      leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%

      # ==================== #
      # ---- Add Center ----
    # ===================== #
    leaflet::addAwesomeMarkers(
      lat = location[2],
      lng = location[1],
      group = "Isochron",
      popup = popup_label,
      label = htmltools::HTML(popup_label),
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-size" = "15px",
          "font-style" = "bold",
          "border-color" = "rgba(0,0,0,0.5)"
        )
      ),
      icon = center_icon
    ) %>%

      # ================= #
      # ---- Add ISO ----
    # ================== #
    leaflet::addPolygons(
      data = iso_result$sf,
      group = "Isochron",
      stroke = TRUE,
      color = "blue",
      weight = 2,
      opacity = 0.8,
      dashArray = "1",
      fillOpacity = 0.2,
      #options = leaflet::pathOptions(pane = "County_districts_polyline"),

      label = htmltools::HTML(popup_label),

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
      # ---- Add Resource ----
    # ====================== #
    leaflet::addAwesomeMarkers(
      data = iso_resource_vax_provider,
      group = "ISO Resource - Vaccine Providers",
      #lng = ~longitude, lat = ~latitude,
      icon = leaflet::makeAwesomeIcon(
        text = fontawesome::fa("house-medical"),
        iconColor = 'black',
        markerColor = "blue"
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

    shiny::showNotification("Isochron Generated!", type = "message")
  } else {
    shiny::showNotification("Failed to generate Isochron. Please try later.", type = "message")
    print(iso_result)
    print(iso_result$sf)
  }

}
