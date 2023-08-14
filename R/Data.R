
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
    # dplyr::mutate(
    #   popup = glue::glue(
    #     "
    #   <h6>{name}</h6></hr>
    #   <b>Type: </b>{type}</br>
    #   <b>Hub: </b>{hub}</br>
    #   <b>Addr: </b>{street_addr}, {city}, {county}, {zip}
    #   ",
    #     name = .data$Company,
    #     type = .data$Type,
    #     street_addr = .data$`Address Line 1`,
    #     city = .data$City,
    #     county = .data$county,
    #     zip = .data$Zipcode,
    #     hub = .data$hub
    #   )
    # ) %>%
    # readr::write_csv(
    #   fs::path(
    #     emthub::ROOT,
    #     "Places",
    #     "poi_for_ohio.csv"
    #   )
    # )
}


# ============= #
# ---- SVI ----
# ============= #


get_SVI <- function() {
  # readr::read_csv(
  #   fs::path(
  #     emthub::ROOT,
  #     "Demographic",
  #     "rescaled_SVI.csv"
  #   )
  # ) %>%
  #   dplyr::mutate(
  #     censustract = as.character(.data$censustract)
  #   )
  #

  # SF_CT %>%
  #   dplyr::left_join(
  #     svi_data,
  #     by = c("GEOID" = "censustract")
  #   ) %>%
  #   dplyr::select(
  #     GEOID, recalc_RPL_THEMES
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Demographic",
  #       "rescaled_SVI.geojson"
  #     )
  #   )
  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Demographic",
      "rescaled_SVI.geojson"
    )
  )
}



# ================================================== #
# ---- % of Households Speaking Limited English ----
# ================================================== #

get_pct_household_limited_english <- function() {

  # get_sf_ct() %>%
  #   dplyr::left_join(
  #     readr::read_csv(
  #       fs::path(
  #         emthub::ROOT,
  #         "Demographic",
  #         "tracts_acs5_s1602_2019_prcnt_limited_english_speaking_households.csv"
  #       )
  #     ) %>% dplyr::mutate(censustract = as.character(censustract)),
  #     by = c("GEOID" = "censustract")
  #   ) %>%
  #   dplyr::transmute(
  #     GEOID,
  #     prcnt_limited_english_speaking_households = as.numeric(prcnt_limited_english_speaking_households)
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Demographic",
  #       "tracts_acs5_s1602_2019_prcnt_limited_english_speaking_households.geojson"
  #     )
  #   )

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Demographic",
      "tracts_acs5_s1602_2019_prcnt_limited_english_speaking_households.geojson"
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
    ),
    lazy = TRUE
  ) %>%
    dplyr::mutate(
      Census_Tract = as.character(.data$Census_Tract),
      popup = glue::glue(
        "
        <h6><a href='{website}' target='_blank'>{name}</a></h6></hr>
        <b>Type: </b>{type}</br>
        <b>Phone: </b>{phone}</br>
        <b>Addr: </b>{street_addr}, {city}, {county}, {zip} </br>
        <b><a href='{prescreening_site}' target='_blank'>Prescreening</a></b></br>
        ",
        name = .data$Place_Name,
        type = .data$Vaccine_Type,
        phone = .data$Phone,
        website = .data$Website,
        prescreening_site = .data$Prescreening_Website,
        street_addr = .data$Address,
        city = .data$City,
        county = .data$County,
        zip = .data$Zip
      )
    )
}

# ===================================== #
# ---- Vaccine: Travel Time by Car ----
# ===================================== #
get_vax_provider_travel_time_by_car <- function() {

  # get_sf_ct() %>%
  #   dplyr::left_join(
  #     readr::read_csv(
  #       fs::path(
  #         emthub::ROOT,
  #         "Vaccine",
  #         "travel_time_to_nearest_ped_vacc_provider_by_car_2023-03-02.csv"
  #       )
  #     ) %>%
  #       dplyr::transmute(
  #         GEOID = as.character(.data$GEOID),
  #         travel_time_to_nearest_ped_vacc_provider_by_car
  #       ),
  #     by = "GEOID"
  #   ) %>%
  #   dplyr::transmute(
  #     .data$GEOID,
  #     travel_time_to_nearest_ped_vacc_provider_by_car
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Vaccine",
  #       "travel_time_to_nearest_ped_vacc_provider_by_car_2023-03-02.geojson"
  #     )
  #   )

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "travel_time_to_nearest_ped_vacc_provider_by_car_2023-03-02.geojson"
    )
  )
}


get_vax_provider_travel_time_by_transit <- function() {

  # get_sf_ct() %>%
  #   dplyr::left_join(
  #     readr::read_csv(
  #       fs::path(
  #         emthub::ROOT,
  #         "Vaccine",
  #         "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.csv"
  #       )
  #     ) %>%
  #       dplyr::transmute(
  #         GEOID = as.character(.data$GEOID),
  #         travel_time_to_nearest_ped_vacc_provider_by_transit
  #       ),
  #     by = "GEOID"
  #   ) %>%
  #   dplyr::transmute(
  #     .data$GEOID,
  #     travel_time_to_nearest_ped_vacc_provider_by_transit
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Vaccine",
  #       "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.geojson"
  #     )
  #   )

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.geojson"
    )
  )
}


# ======================================================== #
# ---- COVID: County Case Rate & Booster (State-wide) ----
# ======================================================== #

get_covid_data_county<- function() {

  # get_sf_county() %>%
  # dplyr::left_join(
  #   readr::read_csv(
  #     fs::path(
  #       emthub::ROOT,
  #       "Vaccine",
  #       "Ohio_COVID_case_and_vacc_rates.csv"
  #     )
  #   ),
  #   by = c("NAME" = "County")
  # ) %>%
  #   dplyr::select(
  #     county = COUNTY,
  #     caserate_last3weeks,
  #     bivalent_booster_percentage
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Vaccine",
  #       "Ohio_COVID_case_and_vacc_rates.geojson"
  #     )
  #   )

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Vaccine",
      "Ohio_COVID_case_and_vacc_rates.geojson"
    ),
    quiet=TRUE
  )
}

# ======================== #
# ---- Birth Outcomes ----
# ======================== #

get_birth_outcomes <- function() {
  readr::read_csv(
    #"/fs/ess/PAS2531/emthub/Demographic/birth_outcomes.csv"
    fs::path(
      emthub::ROOT,
      "Demographic",
      "birth_outcomes.csv"
    )
  ) %>%
    purrr::set_names(
      c("census_tract", "ocoi_quantile", "county", "infant_health_score_quantile", "infant_health_score", "ocoi")
    ) %>%
    dplyr::mutate(
      census_tract = as.character(.data$census_tract)
    )
}


# ====================
# > Census Tratc Level

# - Disease Outcomes
# - Recalc SVI
# - Percent Limited English Speaking Household
# x Percent Hispanic Or Latino
# - Low Income & Food Access
# - Poverty Rate
# - Infant Health Score
# - Min Travel Time to Vax Provider

get_ct_level_data <- function() {

  # ct_level_data <-
  #   readr::read_csv(
  #     fs::path(
  #       emthub::ROOT,
  #       "Demographic",
  #       "emt_oh_ctracts_dataset.csv"
  #     )
  #   ) %>%
  #   dplyr::mutate(
  #     censustract = as.character(.data$censustract)
  #   ) %>%
  #   dplyr::rename(
  #     scaled_rank_sum = .data$`composite chronic disease burden score`
  #   )
  #
  #
  # birth_outcomes <-
  #   get_birth_outcomes() %>%
  #   dplyr::select(
  #     censustract = census_tract,
  #     infant_health_score_quantile,
  #     infant_health_score
  #   )
  #
  #
  # vax_provider_travel_time_by_car <-
  #   get_vax_provider_travel_time_by_car() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::select(
  #     .data$GEOID, .data$travel_time_to_nearest_ped_vacc_provider_by_car
  #   )
  #
  # vax_provider_travel_time_by_transit <-
  #   get_vax_provider_travel_time_by_transit() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::select(
  #     .data$GEOID, .data$travel_time_to_nearest_ped_vacc_provider_by_transit
  #   )
  #
  #
  # get_sf_ct() %>%
  #   dplyr::select(.data$GEOID) %>%
  #   dplyr::left_join(
  #     ct_level_data,
  #     by = c("GEOID" = "censustract")
  #   ) %>%
  #   dplyr::left_join(
  #     birth_outcomes,
  #     by = c("GEOID" = "censustract")
  #   ) %>%
  #   dplyr::left_join(
  #     vax_provider_travel_time_by_car,
  #     by = "GEOID"
  #   ) %>%
  #   dplyr::left_join(
  #     vax_provider_travel_time_by_transit,
  #     by = "GEOID"
  #   ) %>%
  #   sf::st_write(
  #     fs::path(
  #       emthub::ROOT,
  #       "Demographic",
  #       "emt_oh_ctracts_dataset.geojson"
  #     )
  #   )

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Demographic",
      "emt_oh_ctracts_dataset.geojson"
    ),
    quiet=TRUE
  )
}








# ===================== #
# ---- Shapefiles -----
# ===================== #

get_sf_county <- function() {
  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      "sf_county_hub.geojson"
    ),
    quiet=TRUE
  )
}


# ================================================ #
# ---- Shapefile: Hub Boundaries (State-wide) ----
# ================================================ #

get_sf_hub <- function() {
  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      #"HUB_Counties_07.28.2023.geojson"
      "sf_hub.geojson"
    ),
    quiet=TRUE
  )
}


# SF_HUB <-
#   SF_HUB %>%
#   dplyr::select(
#     .data$HUB_Name
#   ) %>%
#   dplyr::group_by(.data$HUB_Name) %>%
#   dplyr::arrange(.data$HUB_Name)
#
# SF_HUB %>%
#   dplyr::group_by(.data$HUB_Name) %>%
#   dplyr::group_map(.f = ~ sf::st_combine(.x)) %>%
#   purrr::set_names(unique(SF_HUB$HUB_Name)) %>%
#   dplyr::bind_rows() %>%
#   tidyr::pivot_longer(
#     dplyr::everything(), names_to = "HUB_Name", values_to = "geometry"
#   ) %>%
#   sf::st_as_sf() %>%
#   sf::st_write(
#     fs::path(
#       emthub::ROOT,
#       "Shapefile",
#       #"HUB_Counties_07.28.2023.geojson"
#       "sf_hub.geojson"
#     )
#   )



# ============================================== #
# ---- Shapefile: Census Tract (State-wide) ----
# ============================================== #

get_sf_ct <- function() {

  sf::st_read(
    fs::path(
      emthub::ROOT,
      "Shapefile",
      "sf_census_tract.geojson"
    ),
    quiet=TRUE
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
    ),
    quiet=TRUE
  )
}

