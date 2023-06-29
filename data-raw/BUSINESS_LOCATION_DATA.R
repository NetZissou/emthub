## code to prepare `BUSINESS_LOCATION_DATA` dataset goes here
BUSINESS_LOCATION_DATA <-
  readr::read_csv(
    "/fs/ess/PDE0003/Mahoning Valley Pilot/mahoning_business_locations.csv"
  ) %>%
  dplyr::mutate(
    popup = glue::glue(
      "<h6>{name}</h6></hr>
      <b>Type: </b>{type}</br>
      <b>Addr: </b>{addr}</br>
      <b>City: </b>{city}</br>
      <b>Zip: </b>{zip}</br>
      <b>Census Tract: </b>{ct}</br>
      ",
      name = .data$Name,
      type = .data$Business_Type,
      addr = .data$Address,
      city = .data$City,
      zip = .data$`Zip Code`,
      ct = .data$`Census Tract`
    )
  )

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


usethis::use_data(BUSINESS_LOCATION_DATA, overwrite = TRUE)
