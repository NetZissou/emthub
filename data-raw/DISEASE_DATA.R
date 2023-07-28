## code to prepare `DISEASE_DATA` dataset goes here
DISEASE_DATA <- get_disease_data()

DISEASE_OUTCOMES <-
  DISEASE_DATA %>%
  dplyr::select(4:15) %>%
  names() %>%
  sort()

#usethis::use_data(DISEASE_DATA, overwrite = TRUE)
usethis::use_data(DISEASE_OUTCOMES, overwrite = TRUE)
