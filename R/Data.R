

get_acc_data <- function() {

  sf::st_read(
    "/fs/ess/PAS2531/emthub/Accessibility/mahoning_accessibility_ctracts_2010.geojson"
  ) %>%
    dplyr::mutate(
      censustract = as.character(.data$censustract)
    )
}

get_disease_data <- function() {

  readr::read_csv(
    "/fs/ess/PAS2531/emthub/Disease/mahoning_county_data.csv"
  ) %>%
    dplyr::mutate(
      censustract = as.character(.data$censustract)
    )
}


get_sf_ct <- function() {

  sf::st_read("/fs/ess/PAS2531/emthub/Shapefile/sf_census_tract.geojson") %>%
    dplyr::filter(
      as.character(.data$GEOID) %in% as.character(get_disease_data()$censustract)
    )
}

get_sf_zip <- function() {
  sf::st_read("/fs/ess/PAS2531/emthub/Shapefile/tl_2018_us_zcta510_for_Mahoning_County.geojson")
}

get_business_location <- function() {

  readr::read_csv(
    "/fs/ess/PAS2531/emthub/Places/mahoning_business.csv"
  ) %>%
    dplyr::mutate(
      full_addr = .data$name_address,
      coded_addr = urltools::url_encode(.data$full_addr),
      popup = glue::glue(
        "<h6>{name}</h6></hr>
      <b>Type: </b>{type}</br>
      <b>Operational Status: </b>{operational_status}</br>
      <b>Addr: </b>{addr}</br>
      <b>City: </b>{city}</br>
      <b>Zip: </b>{zip}</br>
      <b>Census Tract: </b>{ct}</br>
      <a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>
      ",
        name = .data$Name,
        type = .data$Business_Type,
        operational_status = stringr::str_to_title(.data$operational_status),
        addr = .data$Address,
        city = .data$City,
        zip = .data$`Zip Code`,
        ct = .data$`Census Tract`,
        coded_addr = .data$coded_addr
      )
    ) %>%
    dplyr::filter(
      .data$operational_status != "CLOSED_PERMANENTLY"
    )
}
