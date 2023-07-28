## code to prepare `BUSINESS_LOCATION_DATA` dataset goes here


# geocoding_result <-
#   tidygeocoder::reverse_geo(
#     lat = BUSINESS_LOCATION_DATA$Latitude,
#     long = BUSINESS_LOCATION_DATA$Longitude,
#     method = "osm"
#   )
# get_full_addr <-
#   function(name, addr, city, county, zip, state = "OH") {
#
#     full_addr <- ""
#
#     if (!is.na(name)) {
#       full_addr <- name
#     }
#
#     if (!is.na(addr)) {
#
#       if (full_addr == "") {
#         full_addr <-  addr
#       } else {
#         full_addr <-  paste(full_addr, addr, sep = ", ")
#       }
#
#     }
#
#     if (!is.na(city)) {
#       full_addr <-  paste(full_addr, city, sep = ", ")
#     }
#
#     if (!is.na(county)) {
#       full_addr <-  paste(full_addr, county, sep = ", ")
#     }
#
#     if (!is.na(state)) {
#       full_addr <-  paste(full_addr, state, sep = ", ")
#     }
#
#     if (!is.na(zip)) {
#       full_addr <-  paste(full_addr, zip, sep = ", ")
#     }
#
#     return(full_addr)
#   }
#
#
# full_address <- purrr::pmap_chr(
#   .l = list(BUSINESS_LOCATION_DATA$Name, BUSINESS_LOCATION_DATA$Address, BUSINESS_LOCATION_DATA$City, BUSINESS_LOCATION_DATA$County, BUSINESS_LOCATION_DATA$`Zip Code`, BUSINESS_LOCATION_DATA$State),
#   .f = get_full_addr
# )

BUSINESS_LOCATION_DATA <- get_business_location()
  # readr::read_csv(
  #   "/fs/ess/PDE0003/Mahoning Valley Pilot/Business Dataset/mahoning_business.csv"
  # ) %>%
  # dplyr::mutate(
  #   full_addr = name_address,
  #   coded_addr = urltools::url_encode(.data$full_addr),
  #   popup = glue::glue(
  #     "<h6>{name}</h6></hr>
  #     <b>Type: </b>{type}</br>
  #     <b>Operational Status: </b>{operational_status}</br>
  #     <b>Addr: </b>{addr}</br>
  #     <b>City: </b>{city}</br>
  #     <b>Zip: </b>{zip}</br>
  #     <b>Census Tract: </b>{ct}</br>
  #     <a href='https://www.google.com/maps/search/?api=1&query={coded_addr}' target='_blank'>Open in Google Maps</a>
  #     ",
  #     name = .data$Name,
  #     type = .data$Business_Type,
  #     operational_status = stringr::str_to_title(.data$operational_status),
  #     addr = .data$Address,
  #     city = .data$City,
  #     zip = .data$`Zip Code`,
  #     ct = .data$`Census Tract`,
  #     coded_addr = .data$coded_addr
  #   )
  # ) %>%
  # dplyr::filter(
  #   .data$operational_status != "CLOSED_PERMANENTLY"
  # )

## code to prepare `SELECT_INPUT` dataset goes here
FILTER_CT_CHOICES <-
  as.character(unique(BUSINESS_LOCATION_DATA$`Census Tract`))

FILTER_ZIP_CHOICES <-
  as.character(unique(BUSINESS_LOCATION_DATA$`Zip Code`))

FILTER_CITY_CHOICES <-
  as.character(unique(BUSINESS_LOCATION_DATA$City))

FILTER_TYPE_CHOICES <-
  as.character(unique(BUSINESS_LOCATION_DATA$Business_Type))


usethis::use_data(FILTER_CT_CHOICES, overwrite = TRUE)
usethis::use_data(FILTER_ZIP_CHOICES, overwrite = TRUE)
usethis::use_data(FILTER_CITY_CHOICES, overwrite = TRUE)
usethis::use_data(FILTER_TYPE_CHOICES, overwrite = TRUE)


# usethis::use_data(BUSINESS_LOCATION_DATA, overwrite = TRUE)
