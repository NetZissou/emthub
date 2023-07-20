## code to prepare `ACCESSIBILITY_DATA` dataset goes here
ACCESSIBILITY_DATA <-
  sf::st_read(
    "/fs/ess/PDE0003/Mahoning Valley Pilot/mahoning_accessibility_ctracts_2010.geojson"
  ) %>%
  dplyr::mutate(
    censustract = as.character(.data$censustract)
  )

ACC_BUSINESS_TYPE <- unique(ACCESSIBILITY_DATA$Business_type)
ACC_TRANSPORTATION_METHODS <- c("car", "transit", "walk")

usethis::use_data(ACCESSIBILITY_DATA, overwrite = TRUE)
usethis::use_data(ACC_BUSINESS_TYPE, overwrite = TRUE)
usethis::use_data(ACC_TRANSPORTATION_METHODS, overwrite = TRUE)

# sf_ct <-
#   sf::st_read(
#     "/fs/ess/PDE0001/focal_data_ingestion/other/Shapefile/tl_2019_us_county"
#   )
#
# sf_ct %>% dplyr::filter(
#   GEOID %in% x
# )
#
# SF_CENSUS_TRACT %>% dplyr::filter(
#   GEOID %in% x
# )






