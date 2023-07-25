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



ACC_CAR_PAL_COLOR <- c("#deebf7", "#9ecae1", "#3182bd")
ACC_TRANSIT_PAL_COLOR <- c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5")
ACC_WALK_PAL_COLOR <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

ACC_CAR_LEVEL <- c("<15 mins", "15-29 mins", ">30 mins")
ACC_TRANSIT_LEVEL <- c("<30 mins", "30-44 mins", "45-89 mins", ">90 mins")
ACC_WALK_LEVEL <- c("<5 mins", "5-9 mins", "10-19 mins", "20-29 mins", ">30 mins")

ACC_PARAM_LIST <- list(
  legend_title = list(
    car = "Travel by car",
    transit = "Travel by public transit",
    walk = "Walking"
  ),
  case = list(
    car = function(time) {

      # dplyr::case_when(
      #   time < 15 ~ "<15 mins",
      #   time < 30 ~ "15-29 mins",
      #   time >= 30 ~ ">30 mins",
      #   TRUE ~ NA_character_
      # )
      dplyr::case_when(
        time < 15 ~ "1",
        time < 30 ~ "2",
        time >= 30 ~ "3",
        TRUE ~ NA_character_
      )
    },

    transit = function(time) {

      # dplyr::case_when(
      #   time < 30 ~ "<30 mins",
      #   time < 45 ~ "30-44 mins",
      #   time < 90 ~ "45-89 mins",
      #   time >= 90 ~ ">90 mins",
      #   TRUE ~ NA_character_
      # )

      dplyr::case_when(
        time < 30 ~ "1",
        time < 45 ~ "2",
        time < 90 ~ "3",
        time >= 90 ~ "4",
        TRUE ~ NA_character_
      )
    },

    walk = function(time) {

      # dplyr::case_when(
      #   time < 5 ~ "<5 mins",
      #   time < 10 ~ "5-9 mins",
      #   time < 20 ~ "10-19 mins",
      #   time < 30  ~ "20-29 mins",
      #   time >= 30 ~ ">30 mins",
      #   TRUE ~ NA_character_
      # )

      dplyr::case_when(
        time < 5 ~ "1",
        time < 10 ~ "2",
        time < 20 ~ "3",
        time < 30  ~ "4",
        time >= 30 ~ "5",
        TRUE ~ NA_character_
      )
    }
  ),
  color = list(
    car = ACC_CAR_PAL_COLOR,
    transit = ACC_TRANSIT_PAL_COLOR,
    walk = ACC_WALK_PAL_COLOR
  ),
  level = list(
    car = ACC_CAR_LEVEL,
    transit = ACC_TRANSIT_LEVEL,
    walk = ACC_WALK_LEVEL
  ),

  level_n = list(
    car = as.character(c(1:3)),
    transit = as.character(1:4),
    walk = as.character(1:5)
  )
)



usethis::use_data(ACCESSIBILITY_DATA, overwrite = TRUE)
usethis::use_data(ACC_BUSINESS_TYPE, overwrite = TRUE)
usethis::use_data(ACC_TRANSPORTATION_METHODS, overwrite = TRUE)
usethis::use_data(ACC_PARAM_LIST, overwrite = TRUE)


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






