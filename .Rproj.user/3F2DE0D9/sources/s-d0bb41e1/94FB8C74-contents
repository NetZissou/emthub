## code to prepare `DISEASE_DATA` dataset goes here
DISEASE_DATA <-
  readr::read_csv(
    "/fs/ess/PDE0003/Mahoning Valley Pilot/mahoning_county_data.csv"
  ) %>%
  dplyr::mutate(
    censustract = as.character(censustract)
  )

DISEASE_OUTCOMES <-
  DISEASE_DATA %>%
  dplyr::select(4:15) %>%
  names() %>%
  sort()

usethis::use_data(DISEASE_DATA, overwrite = TRUE)
usethis::use_data(DISEASE_OUTCOMES, overwrite = TRUE)
