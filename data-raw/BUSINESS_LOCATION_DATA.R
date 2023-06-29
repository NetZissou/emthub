## code to prepare `BUSINESS_LOCATION_DATA` dataset goes here
BUSINESS_LOCATION_DATA <-
  readr::read_csv(
    "/fs/ess/PDE0003/Mahoning Valley Pilot/mahoning_business_locations.csv"
  )


usethis::use_data(BUSINESS_LOCATION_DATA, overwrite = TRUE)
