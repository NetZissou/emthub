library(tidyverse)
library(furrr)
library(future)
cores <- availableCores() - 1
future::plan(multisession, workers = cores)

SF_HUB <- get_sf_hub()

# ====================== #
# ---- Vax Provider ----
# ====================== #

vax_provider <- get_vax_provider()

furrr_result_vax <-
  furrr::future_map2_chr(
    .x = vax_provider$latitude,
    .y = vax_provider$longitude,
    .f = geocoder::geocoding_sf,
    sf = SF_HUB,
    id = "GEOID",
    .progress = TRUE
  )
# =========================== #
# ---- Point of Interest ----
# =========================== #

point_of_interest <-
  get_point_of_interest() %>%
  dplyr::mutate(
    partition_id = ceiling(dplyr::row_number() / 9999)
  )

partitions <-
  point_of_interest %>%
  dplyr::group_split(partition_id)


for (i in 1:length(partitions)) {
  message("Processing split ", i, "/", length(partitions))
  partition <- partitions[[i]]

  furrr_result_poi <-
    furrr::future_map2_chr(
      .x = partition$Latitude,
      .y = partition$Longitude,
      .f = geocoder::geocoding_sf,
      sf = SF_HUB,
      id = "GEOID",
      .progress = TRUE
    )


  file_path <-
    glue::glue(
      "/fs/ess/PAS2531/emthub/Geocoding/poi_result_{id}.csv",
      id = i
    )

  geocoding_result_census %>%
    readr::write_csv(
      file_path
    )
}


purrr::map_df(
  .x = fs::dir_info("/fs/ess/PAS2531/emthub/Geocoding")$path,
  .f = vroom::vroom
) -> poi

poi %>%
  dplyr::select(-.data$partition_id) %>%
  readr::write_csv(fs::path(
    emthub::ROOT,
    "Places",
    "poi_for_ohio.csv"
  ))

