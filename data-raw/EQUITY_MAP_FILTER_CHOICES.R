## code to prepare `EQUITY_MAP_FILTER_CHOICES` dataset goes here

SF_HUB <-
  get_sf_hub()

CHOICES_HUB <- sort(unique(SF_HUB$HUB_Name))

CHOICES_COUNTY <- sort(unique(SF_HUB$COUNTY))

CHOICES_VAX_TYPE <- sort(unique(get_vax_provider()$Vaccine_Type))

CHOICES_POINT_OF_INTEREST <-
  sort(unique(get_point_of_interest()$Type))

CHOICES_NEAREST_VAX_BY_CAR <-
  sort(unique(get_vax_provider_travel_time_by_car()$travel_time_to_nearest_ped_vacc_provider_by_car))

CHOICES_NEAREST_VAX_BY_TRANSIT <-
  sort(unique(get_vax_provider_travel_time_by_transit()$travel_time_to_nearest_ped_vacc_provider_by_transit))

EQUITY_MAP_FILTER_CHOICES <-
  list(
    hub = CHOICES_HUB,
    county = CHOICES_COUNTY,
    vax_type = CHOICES_VAX_TYPE,
    point_of_interest = CHOICES_POINT_OF_INTEREST,
    nearest_vax_by_car = CHOICES_NEAREST_VAX_BY_CAR,
    nearest_vax_by_transit = CHOICES_NEAREST_VAX_BY_TRANSIT
  )

usethis::use_data(EQUITY_MAP_FILTER_CHOICES, overwrite = TRUE)
