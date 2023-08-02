get_birth_outcomes <- function() {
  readr::read_csv(
    "/fs/ess/PAS2531/emthub/Demographic/birth_outcomes.csv"
  ) %>%
    purrr::set_names(
      c("census_tract", "ocoi_quantile", "county", "infant_health_score_quantile", "infant_health_score", "ocoi")
    ) %>%
    dplyr::mutate(
      census_tract = as.character(.data$census_tract)
    )
}
