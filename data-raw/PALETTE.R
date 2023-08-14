## code to prepare `BIRTH_OUTCOMES_PAL` dataset goes here
BIRTH_OUTCOMES_PAL <-
  c(
    '#762a83','#af8dc3','#e7d4e8','#f7f7f7','#d9f0d3','#7fbf7b','#1b7837'
  )


covid_data <- get_covid_data_county()
ct_level_data <- get_ct_level_data()


pal_svi <-
  leaflet::colorNumeric(
    palette = "Reds",
    domain = ct_level_data$recalc_svi_2018
  )

pal_household_english <-
  leaflet::colorNumeric(
    palette = "Purples",
    domain = ct_level_data$prcnt_limited_english_speaking_households
  )

pal_covid_case_rate <-
  leaflet::colorNumeric(
    palette = "OrRd",
    domain = covid_data$caserate_last3weeks
  )

pal_booster_rate <-
  leaflet::colorNumeric(
    palette = "YlGn",
    domain = covid_data$bivalent_booster_percentage
  )

pal_vax_provider_travel_time_by_car <-
  leaflet::colorFactor(
    palette = c("#fdae6b", "#e6550d"),
    domain = unique(ct_level_data$travel_time_to_nearest_ped_vacc_provider_by_car),
    levels = unique(ct_level_data$travel_time_to_nearest_ped_vacc_provider_by_car),
    ordered = TRUE
  )

pal_vax_provider_travel_time_by_transit <-
  leaflet::colorFactor(
    palette = c("#feedde", "#d94701", "#fdbe85", "#fd8d3c"),
    domain = unique(ct_level_data$travel_time_to_nearest_ped_vacc_provider_by_transit),
    levels = unique(ct_level_data$travel_time_to_nearest_ped_vacc_provider_by_transit),
    ordered = TRUE
  )

pal_scaled_sum_rank <- leaflet::colorNumeric(
  palette = "Reds",
  domain = ct_level_data$scaled_rank_sum,
  na.color = "#808080"
)

pal_poverty_rate <- leaflet::colorNumeric(
  palette = "RdPu",
  domain = ct_level_data$PovertyRate
)

pal_birth_outcomes <- leaflet::colorFactor(
  palette = BIRTH_OUTCOMES_PAL,
  domain = unique(ct_level_data$infant_health_score_quantile),
  reverse = FALSE
)


PAL <- list(
  pal_svi = pal_svi,
  pal_household_english = pal_household_english,
  pal_covid_case_rate = pal_covid_case_rate,
  pal_booster_rate = pal_booster_rate,
  pal_vax_provider_travel_time_by_car = pal_vax_provider_travel_time_by_car,
  pal_vax_provider_travel_time_by_transit = pal_vax_provider_travel_time_by_transit,
  pal_scaled_sum_rank = pal_scaled_sum_rank,
  pal_poverty_rate = pal_poverty_rate,
  pal_birth_outcomes = pal_birth_outcomes
)

usethis::use_data(PAL, overwrite = TRUE)
usethis::use_data(BIRTH_OUTCOMES_PAL, overwrite = TRUE)
