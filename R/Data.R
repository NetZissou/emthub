
# ================================ #
# ---- Places Data (Mahoning) ----
# ================================ #

get_business_location <- function(parquet = FALSE) {

  # readr::read_csv(
  #   fs::path(
  #     emthub::ROOT,
  #     "Places",
  #     "mahoning_business.csv"
  #   )
  # ) %>%
  #   dplyr::mutate(
  #     full_addr = .data$name_address,
  #     coded_addr = urltools::url_encode(.data$full_addr),
  #     popup = glue::glue(
  #       "<h6>{name}</h6></hr>
  #     <b>Type: </b>{type}</br>
  #     <b>Operational Status: </b>{operational_status}</br>
  #     <b>Addr: </b>{addr}</br>
  #     <b>City: </b>{city}</br>
  #     <b>Zip: </b>{zip}</br>
  #     <b>Census Tract: </b>{ct}</br>
  #     <a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>
  #     ",
  #       name = .data$Name,
  #       type = .data$Business_Type,
  #       operational_status = stringr::str_to_title(.data$operational_status),
  #       addr = .data$Address,
  #       city = .data$City,
  #       zip = .data$`Zip Code`,
  #       ct = .data$`Census Tract`,
  #       coded_addr = .data$coded_addr
  #     )
  #   ) %>%
  #   dplyr::filter(
  #     .data$operational_status != "CLOSED_PERMANENTLY"
  #   ) %>%
  #   readr::write_csv(
  #     fs::path(
  #       emthub::ROOT,
  #       "Places",
  #       "mahoning_business_modified.csv"
  #     )
  #   )
  if (parquet) {
    arrow::read_parquet(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "mahoning_business_modified.parquet"
      ),
      as_data_frame = FALSE
    )
  } else {
    readr::read_csv(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "mahoning_business_modified.csv"
      ),
      lazy = TRUE
    )
  }
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
      "Regional Rates Data",
      "raw", "ct_level",
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
      "Regional Rates Data",
      "raw", "ct_level",
      "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.geojson"
    )
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

get_ct_level_data <- function(parquet = FALSE) {

  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "emt_oh_ctracts_dataset.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "emt_oh_ctracts_dataset.geojson"
      ),
      quiet=TRUE
    )
  }
}


update_ct_level_data <- function() {

  # =============================== #
  # ---- Archive Previous Data ----
  # =============================== #

  file_name <- "emt_oh_ctracts_dataset"
  ct_level_data_path_geojson <-
    fs::path(
      emthub::ROOT,
      "Regional Rates Data",
      paste0(file_name, ".geojson")
    )

  ct_level_data_path_parquet <-
    fs::path(
      emthub::ROOT,
      "Regional Rates Data",
      paste0(file_name, ".parquet")
    )

  if (fs::file_exists(ct_level_data_path_geojson)) {

    archive_dir <-
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "archive",
        paste0(
          "ct_level_data_", format(Sys.time(), "%Y_%m_%d_%H%M%S")
        )
      )

    if (!fs::dir_exists(archive_dir)) {
      fs::dir_create(
        archive_dir
      )
    }

    fs::file_move(
      ct_level_data_path_geojson,
      fs::path(
        archive_dir, paste0(file_name, ".geojson")
      )
    )

    fs::file_move(
      ct_level_data_path_parquet,
      fs::path(
        archive_dir, paste0(file_name, ".parquet")
      )
    )
  }

  # =================== #
  # ---- Load Data ----
  # =================== #

  # > Shapefile
  sf_ct <- get_sf_ct(T)

  # > ct level data
  ct_level_data <-
    readr::read_csv(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data", "raw", "ct_level",
        "emt_oh_ctracts_dataset.csv"
      )
    ) %>%
    dplyr::mutate(
      censustract = as.character(.data$censustract)
    ) %>%
    dplyr::rename(
      scaled_rank_sum = .data$`composite chronic disease burden score`
    )


  # > Birth Outcomes
  birth_outcomes <-
    readr::read_csv(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data", "raw", "ct_level",
        "birth_outcomes.csv"
      )
    ) %>%
    purrr::set_names(
      c("census_tract", "ocoi_quantile", "county", "infant_health_score_quantile", "infant_health_score", "ocoi")
    ) %>%
    dplyr::select(
      census_tract = .data$census_tract,
      .data$infant_health_score_quantile,
      .data$infant_health_score
    ) %>%
    dplyr::mutate(
      census_tract = as.character(.data$census_tract),
      infant_health_score_quantile = dplyr::case_when(
        infant_health_score_quantile == "Q1" ~ "Q1 (worst)",
        infant_health_score_quantile == "Q7" ~ "Q7 (best)",
        TRUE ~ infant_health_score_quantile
      )
    )

  # > Vax Provider Tavel Time
  vax_provider_travel_time_by_car <-
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data", "raw", "ct_level",
        "travel_time_to_nearest_ped_vacc_provider_by_car_2023-03-02.geojson"
      ),
      quiet = TRUE
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::select(
      .data$GEOID, .data$travel_time_to_nearest_ped_vacc_provider_by_car
    )

  vax_provider_travel_time_by_transit <-
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data", "raw", "ct_level",
        "travel_time_to_nearest_ped_vacc_provider_by_transit_2023-03-23.geojson"
      ),
      quiet = TRUE
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::select(
      .data$GEOID, .data$travel_time_to_nearest_ped_vacc_provider_by_transit
    )


  # =============== #
  # ---- Merge ----
  # =============== #
  ct_level_data <-
    sf_ct %>%
    dplyr::select(.data$GEOID) %>%
    dplyr::left_join(
      ct_level_data,
      by = c("GEOID" = "censustract")
    ) %>%
    dplyr::left_join(
      birth_outcomes,
      by = c("GEOID" = "census_tract")
    ) %>%
    dplyr::left_join(
      vax_provider_travel_time_by_car,
      by = "GEOID"
    ) %>%
    dplyr::left_join(
      vax_provider_travel_time_by_transit,
      by = "GEOID"
    )


  # =============== #
  # ---- Write ----
  # =============== #

  sf::st_write(
    ct_level_data,
    ct_level_data_path_geojson
  )

  sfarrow::st_write_parquet(
    ct_level_data,
    ct_level_data_path_parquet
  )


}


# ======================================================== #
# ---- COVID: County Case Rate & Booster (State-wide) ----
# ======================================================== #

get_covid_data_county<- function(parquet = FALSE) {

  if (parquet) {

    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "Ohio_COVID_case_and_vacc_rates.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "Ohio_COVID_case_and_vacc_rates.geojson"
      ),
      quiet=TRUE
    )
  }
}

update_covid_data_county <- function() {

  # =============================== #
  # ---- Archive Previous Data ----
  # =============================== #
  file_name <- "Ohio_COVID_case_and_vacc_rates"
  data_path_geojson <-
    fs::path(
      emthub::ROOT,
      "Regional Rates Data",
      paste0(file_name, ".geojson")
    )

  data_path_parquet <-
    fs::path(
      emthub::ROOT,
      "Regional Rates Data",
      paste0(file_name, ".parquet")
    )

  if (fs::file_exists(data_path_geojson)) {

    archive_dir <-
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "archive",
        paste0(
          "census_level_covid_data_", format(Sys.time(), "%Y_%m_%d_%H%M%S")
        )
      )

    if (!fs::dir_exists(archive_dir)) {
      fs::dir_create(
        archive_dir
      )
    }

    fs::file_move(
      data_path_geojson,
      fs::path(
        archive_dir, paste0(file_name, ".geojson")
      )
    )

    fs::file_move(
      data_path_parquet,
      fs::path(
        archive_dir, paste0(file_name, ".parquet")
      )
    )
  }


  # =================== #
  # ---- Load Data ----
  # =================== #
  sf_county <- get_sf_county(T)

  county_level_data <-
    readr::read_csv(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data", "raw", "county_level",
        "Ohio_COVID_case_and_vacc_rates.csv"
      )
    )

  # =============== #
  # ---- Merge ----
  # =============== #
  county_level_data <-
    sf_county %>%
    dplyr::left_join(
      county_level_data,
      by = c("NAME" = "County")
    ) %>%
    dplyr::select(
      county = .data$COUNTY,
      .data$caserate_last3weeks,
      .data$bivalent_booster_percentage
    )

  # =============== #
  # ---- Write ----
  # =============== #

  sf::st_write(
    county_level_data,
    data_path_geojson
  )

  sfarrow::st_write_parquet(
    county_level_data,
    data_path_parquet
  )
}



# ====================== #
# ---- Acc Mahoning ----
# ====================== #
get_acc_data <- function(parquet = FALSE) {

  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "mahoning_accessibility_ctracts_2010.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Regional Rates Data",
        "mahoning_accessibility_ctracts_2010.geojson"
      )
    ) %>%
      dplyr::mutate(
        censustract = as.character(.data$censustract)
      )
  }
}

# ========================== #
# ---- Point Level: POI ----
# ========================== #

get_point_of_interest <- function(parquet = FALSE) {

  if (parquet) {
    arrow::read_parquet(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "poi_for_ohio.parquet"
      ),
      as_data_frame = FALSE
    )
  } else {
    vroom::vroom(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "poi_for_ohio.csv"
      )
    )
  }

  # dplyr::mutate(
  #   full_addr = glue::glue(
  #     "{name}, {street}, {city}, {zip}",
  #     name = .data$Company,
  #     street = .data$`Address Line 1`,
  #     city = .data$City,
  #     zip = .data$Zipcode
  #   ),
  #   full_addr = stringr::str_remove(.data$full_addr, "^NA, "),
  #   coded_addr = urltools::url_encode(.data$full_addr),
  #   popup = glue::glue(
  #     "
  #   <h6>{name}</h6></hr>
  #   <b>Type: </b>{type}</br>
  #   <b>Hub: </b>{hub}</br>
  #   <b>Addr: </b>{street_addr}, {city}, {county}, {zip}</br>
  #   <a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>
  #   ",
  #     name = .data$Company,
  #     type = .data$Type,
  #     street_addr = .data$`Address Line 1`,
  #     city = .data$City,
  #     county = .data$county,
  #     zip = .data$Zipcode,
  #     hub = .data$hub,
  #     coded_addr = .data$coded_addr
  #   )
  # ) %>%
  #   dplyr::select(
  #     -full_addr,
  #     -coded_addr
  #   ) %>%
  # readr::write_csv(
  #   fs::path(
  #     emthub::ROOT,
  #     "Places",
  #     "poi_for_ohio.csv"
  #   )
  # )
}

# =================================== #
# ---- Point Level: Vax Provider ----
# =================================== #

update_vax_provider <- function() {

  # =================== #
  # ---- Load Data ----
  # =================== #

  vax_provider <-
    readr::read_csv(
      fs::path(
        "/fs/ess/PDE0003/vaccineaccess/cdc_vax_providers.csv"
      ),
      lazy = TRUE
    )

  sf_county_hub <-
    get_sf_county(T) %>%
    dplyr::as_tibble() %>%
    dplyr::select(County = .data$COUNTY, Hub = .data$HUB_Name)

  # =============== #
  # ---- Merge ----
  # =============== #

  vax_provider_type <-
    vax_provider %>%
    dplyr::collect() %>%
    tidyr::pivot_wider(
      names_from  = .data$Vaccine_Type,
      values_from = .data$Vaccine_Type
    ) %>%
    dplyr::transmute(
      .data$provider_location_guid,
      vax_type_full = purrr::pmap_chr(
        .l = list(
          .data$`COVID-19 Bivalent Booster (Pfizer-BioNTech)- Age 12+ years`,
          .data$`COVID-19 Bivalent Booster (Pfizer-BioNTech)- Age 5 to 11 years`,
          .data$`COVID-19 Bivalent Booster Provider (Moderna) - Age 18+`
        ),
        .f = function(type_1, type_2, type_3) {
          types <- c(type_1, type_2, type_3)
          types <- types[!is.na(types)]

          stringr::str_c(types, collapse = "; ")
        }
      )
    )

  vax_provider <-
    vax_provider %>%
    dplyr::left_join(
      vax_provider_type,
      by = "provider_location_guid"
    ) %>%
    dplyr::left_join(
      sf_county_hub,
      by = "County"
    ) %>%
    dplyr::mutate(
      Census_Tract = as.character(.data$Census_Tract),
      full_addr = glue::glue(
        "{name}, {street}, {city}, {zip}",
        name = .data$Place_Name,
        street = .data$Address,
        city = .data$City,
        zip = .data$Zip
      ),
      full_addr = stringr::str_remove(.data$full_addr, "^NA, "),
      coded_addr = urltools::url_encode(.data$full_addr),
      popup = glue::glue(
        "
        <h6><a href='{website}' target='_blank'>{name}</a></h6></hr>
        <b>Type: </b>{type}</br>
        <b>Phone: </b>{phone}</br>
        <b>Addr: </b>{street_addr}, {city}, {county}, {zip} </br>
        <b>Hub: </b>{hub}</br>
        <b><a href='{prescreening_site}' target='_blank'>Prescreening</a></b></br>
        <b><a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a><b>
        ",
        name = .data$Place_Name,
        type = .data$vax_type_full,
        phone = .data$Phone,
        website = .data$Website,
        prescreening_site = .data$Prescreening_Website,
        street_addr = .data$Address,
        city = .data$City,
        county = .data$County,
        zip = .data$Zip,
        hub = .data$Hub,
        coded_addr =.data$coded_addr
      )
    ) %>%
    dplyr::select(-.data$vax_type_full, -.data$full_addr, -.data$coded_addr)

  # =============== #
  # ---- Write ----
  # =============== #
  vax_provider %>%
    readr::write_csv(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "cdc_vax_providers.csv"
      )
    )

  vax_provider %>%
    arrow::write_parquet(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "cdc_vax_providers.parquet"
      )
    )

}

get_vax_provider <- function(parquet = F) {

  if (parquet) {

    arrow::read_parquet(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "cdc_vax_providers.parquet"
      ),
      as_data_frame = FALSE
    )
  } else {

    readr::read_csv(
      fs::path(
        emthub::ROOT,
        "Point Level Data",
        "cdc_vax_providers.csv"
      ),
      lazy = TRUE
    )

  }
}

# ===================== #
# ---- Shapefiles -----
# ===================== #

get_sf_county <- function(parquet = F) {
  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_county.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_county_hub.geojson"
      ),
      quiet=TRUE
    )
  }
}


# ================================================ #
# ---- Shapefile: Hub Boundaries (State-wide) ----
# ================================================ #

get_sf_hub <- function(parquet = F) {

  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_hub.parquet"
      )
    )
  } else {
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
}


# ============================================== #
# ---- Shapefile: Census Tract (State-wide) ----
# ============================================== #

get_sf_ct <- function(parquet = F) {

  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_census_tract.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_census_tract.geojson"
      ),
      quiet=TRUE
    )
  }
}

# =================================== #
# ---- Shapefile: Zip (Mahoning) ----
# =================================== #

get_sf_zip <- function(parquet = F) {
  if (parquet) {
    sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_zip.parquet"
      )
    )
  } else {
    sf::st_read(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "tl_2020_us_zcta520_clip_to_oh.geojsonn"
      ),
      quiet=TRUE
    )
  }
}


get_sf_zip_by_county <- function(county_name) {

  if (!all(stringr::str_detect(county_name, "County"))) {
    county_name <- stringr::str_to_title(paste0(county_name, " County"))
  }

  purrr::map_df(
    .x = county_name,
    .f = ~sfarrow::st_read_parquet(
      fs::path(
        emthub::ROOT,
        "Shapefile",
        "sf_zip_by_county",
        glue::glue(
          "sf_zip_{county}.parquet", county = .x
        )
      )
    )
  )
}

