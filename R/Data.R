
# ============= #
# ---- Acc ----
# ============= #
get_acc_data <- function() {

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Accessibility",
      "mahoning_accessibility_ctracts_2010.geojson"
    )
  ) %>%
    dplyr::mutate(
      censustract = as.character(.data$censustract)
    )
}

# ====================== #
# ---- Disease Data ----
# ====================== #

get_disease_data <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Disease",
      "mahoning_county_data.csv"
    )
  ) %>%
    dplyr::mutate(
      censustract = as.character(.data$censustract)
    )
}


# ================================ #
# ---- Places Data (Mahoning) ----
# ================================ #

get_business_location <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Places",
      "mahoning_business.csv"
    )
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


# ======================================== #
# ---- Point of Interest (State-wide) ----
# ======================================== #

get_point_of_interest <- function() {

  vroom::vroom(
    fs::path(
      emthub::ROOT,
      "Places",
      "poi_for_ohio.csv"
    )
  )
}


# ============= #
# ---- SVI ----
# ============= #


get_SVI <- function() {
  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Demographic",
      "rescaled_SVI.csv"
    )
  )
}



# ================================================== #
# ---- % of Households Speaking Limited English ----
# ================================================== #

get_pct_household_limited_english <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Demographic",
      "tracts_acs5_s1602_2019_prcnt_limited_english_speaking_households.csv"
    )
  )
}

# ================= #
# ---- Vaccine ----
# ================= #




# =============================== #
# ---- Vaccine: Vax Provider ----
# =============================== #

get_vax_provider <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "cdc_vax_providers.csv"
    )
  )
}

# ===================================== #
# ---- Vaccine: Travel Time by Car ----
# ===================================== #
get_vax_provider_travel_time_by_car <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "travel_time_to_nearest_ped_vacc_provider_by_car_2023-03-02.csv"
    )
  )
}


get_vax_provider_travel_time_by_transit <- function() {

  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.csv"
    )
  )
}





# ===================== #
# ---- Shapefiles -----
# ===================== #


# ================================================ #
# ---- Shapefile: Hub Boundaries (State-wide) ----
# ================================================ #

get_sf_hub <- function() {
  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      "HUB_Counties_07.28.2023.geojson"
    )
  )
}

# ============================================== #
# ---- Shapefile: Census Tract (State-wide) ----
# ============================================== #

get_sf_ct <- function() {

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      "sf_census_tract.geojson"
    )
  )
}

# =================================== #
# ---- Shapefile: Zip (Mahoning) ----
# =================================== #

get_sf_zip <- function() {
  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      "tl_2018_us_zcta510_for_Mahoning_County.geojson"
    )
  )
}

