library(tidyverse)
library(furrr)
library(future)
cores <- availableCores() - 1
future::plan(multisession, workers = cores)

# ================ #
# ---- Relink ----
# ================ #

data_relink <-
  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "Ohio_public_health_resources.csv"
    )
  )

# > Geocode HUB
SF_HUB <- get_sf_hub(T)

geocoding_result <-
  furrr::future_map2_chr(
    .x = data_relink$Latitude,
    .y = data_relink$Longitude,
    .f = geocoder::geocoding_sf,
    sf = SF_HUB,
    id = "HUB_Name",
    .progress = TRUE
  )

data_relink_tidy <-
  data_relink %>%
  dplyr::transmute(
    name = .data$Name,
    street_addr = .data$Address,
    city = .data$City,
    county = .data$County,
    state = .data$State,
    zip = .data$`Zip Code`,
    type = .data$Business_type_condensed,
    census_tract = .data$`Census Tract`,
    lng = .data$Longitude,
    lat = .data$Latitude,
    hub = geocoding_result,
    coded_addr = urltools::url_encode(.data$name_address),
    popup = glue::glue(
      "
    <h6>{name}</h6></hr>
    <b>Type: </b>{type}</br>
    <b>Hub: </b>{hub}</br>
    <b>Addr: </b>{street_addr}, {city}, {county}, {zip}</br>
    <a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>
    ",
      name = .data$name,
      type = .data$type,
      street_addr = .data$street_addr,
      city = .data$city,
      county = .data$county,
      zip = .data$zip,
      hub = .data$hub,
      coded_addr = .data$coded_addr
    ),
    source = "Relink"
  ) %>%
  dplyr::select(-.data$coded_addr)

data_relink_tidy %>%
  dplyr::mutate(
    source = "Relink"
  ) %>%
  readr::write_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "poi_relink.csv"
    )
  )


# ======================= #
# ---- Food Pantries ----
# ======================= #

data_food_pantries <-
  rjson::fromJSON(
    file = fs::path(
      FOCALPipe::ROOT_PATH,
      "other",
      "Food Pantries",
      "food_pantries.json"
    )
  )



parse_service <- function(service) {

  service_social_media <- service$social_media_links

  service <- service[-1]

  service$website <- ifelse(
    is.null(service_social_media$Website),
    NA_character_,
    service_social_media$Website
  )
  service$facebook <- ifelse(
    is.null(service_social_media$Facebook),
    NA_character_,
    service_social_media$Facebook
  )
  service$instagram <- ifelse(
    is.null(service_social_media[["Instagram URL"]]),
    NA_character_,
    service_social_media[["Instagram URL"]]
  )
  service$twitter <- ifelse(
    is.null(service_social_media[["Twitter Address"]]),
    NA_character_,
    service_social_media[["Twitter Address"]]
  )

  service %>%
    purrr::modify(
      ~ ifelse(is.null(.), NA_character_, .)
    ) %>%
    tibble::as_tibble()

  return(service)
}


parse_city <- function(city) {

  purrr::map_df(
    .x = city$services,
    .f = parse_service
  )
}

data_food_pantries_tbl <-
  purrr::map_df(
    data_food_pantries,
    parse_city
  ) %>%
  dplyr::mutate(
    full_address = paste(.data$street_address, .data$city, .data$state, "United States", .data$zip_code, sep = ", ")
  )

geocoding_result <-
  tidygeocoder::geo(
    address = data_food_pantries_tbl$full_address,
    method = "geocodio"
  )

data_food_pantries_tbl <-
  data_food_pantries_tbl %>%
  dplyr::mutate(
    lat = geocoding_result$lat,
    lng = geocoding_result$long
  ) %>%
  dplyr::filter(!is.na(.data$lat))


# > Hub
geocoding_result <-
  furrr::future_map2_chr(
    .x = data_food_pantries_tbl$lat,
    .y = data_food_pantries_tbl$lng,
    .f = geocoder::geocoding_sf,
    sf = SF_HUB,
    id = "HUB_Name",
    .progress = TRUE
  )

data_food_pantries_tbl$hub <- geocoding_result

# > County
SF_COUNTY <- get_sf_county()
geocoding_result <-
  furrr::future_map2_chr(
    .x = data_food_pantries_tbl$lat,
    .y = data_food_pantries_tbl$lng,
    .f = geocoder::geocoding_sf,
    sf = SF_COUNTY,
    id = "COUNTY",
    .progress = TRUE
  )

data_food_pantries_tbl$county <- geocoding_result

#> Census Tract
SF_CT <- get_sf_ct()
geocoding_result <-
  furrr::future_map2_chr(
    .x = data_food_pantries_tbl$lat,
    .y = data_food_pantries_tbl$lng,
    .f = geocoder::geocoding_sf,
    sf = SF_CT,
    id = "GEOID",
    .progress = TRUE
  )

data_food_pantries_tbl$census_tract <- geocoding_result



data_food_pantries_tidy <-
  data_food_pantries_tbl %>%
  dplyr::transmute(
    name = .data$name,
    street_addr = .data$street_address,
    city = .data$city,
    county = .data$county,
    state = .data$state,
    zip = .data$zip_code,
    type = "Food Pantry",
    census_tract = .data$census_tract,
    lng = .data$lng,
    lat = .data$lat,
    hub = .data$hub,
    coded_addr = urltools::url_encode(.data$full_address),
    popup = paste(sep = "",
                  glue::glue(
                    "
    <h6>{name}</h6></hr>
    <b>Type: </b>{type}</br>
    <b>Hub: </b>{hub}</br>
    <b>Addr: </b>{street_addr}, {city}, {county}, {zip}</br>
    ",
                    name = .data$name,
                    type = .data$type,
                    street_addr = .data$street_addr,
                    city = .data$city,
                    county = .data$county,
                    zip = .data$zip,
                    hub = .data$hub,
                    #coded_addr = .data$coded_addr
                  ),
                  "<br/>",
                  # > Phone
                  ifelse(
                    is.na(.data$telephone),
                    "",
                    glue::glue(
                      "<b>Contact: {telephone}</b><br/>",
                      telephone = .data$telephone
                    )
                  ),
                  # > Social Media: Facebook
                  ifelse(
                    is.na(.data$facebook),
                    "",
                    glue::glue(
                      "<a href = {facebook_link} target='_blank'>Facebook</a><br/>",
                      facebook_link = .data$facebook
                    )
                  ),
                  # > Social Media: Instagram
                  ifelse(
                    is.na(.data$instagram),
                    "",
                    glue::glue(
                      "<a href = {instagram_link} target='_blank'>Instagram</a><br/>",
                      instagram_link = .data$instagram
                    )
                  ),
                  # > Social Media: Twitter
                  ifelse(
                    is.na(.data$twitter),
                    "",
                    glue::glue(
                      "<a href = {twitter_link} target='_blank'>Twitter</a><br/>",
                      twitter_link = .data$twitter
                    )
                  ),
                  # > Address
                  stringr::str_remove(
                    .data$full_address,
                    "United States, "
                  ),
                  "<br/>",
                  # Google map
                  glue::glue(
                    "<a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>",
                    coded_addr = .data$coded_addr
                  ),
                  "<br/>",
                  # Additional Description
                  ifelse(
                    is.na(.data$description),
                    "",
                    .data$description
                  )


    ),
    source = "foodpantries.org"
  ) %>%
  dplyr::select(-.data$coded_addr)

data_food_pantries_tidy %>%
  dplyr::mutate(
    source = "foodpantries.org"
  ) %>%
  readr::write_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "poi_food_pantries.csv"
    )
  )



# ========================== #
# ---- Ohio POI Dataset ----
# ========================== #

poi <-
  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "poi_for_ohio.csv"
    )
  )

poi_relink <-
  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "poi_relink.csv"
    )
  )

poi_food_pantries <-
  readr::read_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data", "raw",
      "poi_food_pantries.csv"
    )
  )

poi_agg <-
  poi %>%
  dplyr::transmute(
    name = .data$Company,
    street_addr = .data$`Address Line 1`,
    city = .data$City,
    county = .data$county,
    state = .data$State,
    zip = .data$Zipcode,
    type = .data$Type,
    census_tract = .data$census_tract,
    lng = .data$Longitude,
    lat = .data$Latitude,
    hub = hub,
    popup = popup,
    source = "POI"
  ) %>%
  dplyr::bind_rows(
    poi_relink,
    poi_food_pantries
  ) %>%
  dplyr::select(-.data$state) %>%
  dplyr::mutate(
    type = stringr::str_to_title(.data$type),
    city = stringr::str_to_title(.data$city)
  )



poi_agg %>%
  readr::write_csv(
    fs::path(
      emthub::ROOT,
      "Point Level Data",
      "poi_agg.csv"
    )
  )

poi_agg %>%
  arrow::write_parquet(
    fs::path(
      emthub::ROOT,
      "Point Level Data",
      "poi_agg.parquet"
    )
  )




