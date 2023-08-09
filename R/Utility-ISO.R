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
      geojson = httr2::resp_body_json(resp)$features
    )
  )
}



addISO <- function(
  map_id,
  full_address = NULL,
  location = c(),
  range = c(),
  range_type = c("distance", "time"),
  type = c("car", "walk", "cycle")
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

  shiny::showNotification("Isochron Generated!", type = "message")

  # =========================== #
  # ---- Meta Data for Map ----
  # =========================== #

  if (range == "distance") {
    # To KM
    range_label <- range/1000
    range_type_label <- "KM"
  } else {
    # To Sec
    range_label <- range/60
    range_type_label <- "Mins"
  }

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
      markerColor = "blue"
    )
  }

  center_icon <- get_center_icon(type = type)

  # ==================== #
  # ---- Add to Map ----
  # ==================== #

  leaflet::leafletProxy(map_id) %>%
    leaflet.extras2::addSpinner() %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 30)) %>%
    # leaflet::addGeoJSON(
    #   data = iso_result$geojson,
    #   stroke = TRUE,
    #   group = "Isochron",
    #   popup = popup_label,
    #   options = leaflet::pathOptions(pane = "layer_top")
    # ) %>%
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
    leaflet.extras::addGeoJSONv2(
      geojson = iso_result$geojson,
      group = "Isochron",
      #popupProperty = "type",
      color = "blue",
      opacity = 0.8,
      weight = 5,
      fillOpacity = 0.2,
      highlight = leaflet::highlightOptions(
        weight = 3,
        fillOpacity = 0.1,
        color = "black",
        dashArray = "",
        opacity = 0.5,
        bringToFront = TRUE,
        sendToBack = TRUE
      ),
      pathOptions = leaflet::pathOptions(pane = "layer_top")
    ) %>%
    leaflet.extras2::stopSpinner()

}
