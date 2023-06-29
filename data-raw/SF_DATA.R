## code to prepare `SF_CENSUS_TRACT` dataset goes here

SF_CENSUS_TRACT <-
  sf::st_read("/fs/ess/PDE0001/focal_data_ingestion/other/Shapefile/sf_census_tract.geojson") %>%
  dplyr::filter(
    as.character(.data$GEOID) %in% as.character(DISEASE_DATA$LocationName)
  )
usethis::use_data(SF_CENSUS_TRACT, overwrite = TRUE)
