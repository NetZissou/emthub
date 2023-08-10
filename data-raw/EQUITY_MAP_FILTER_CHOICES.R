## code to prepare `EQUITY_MAP_FILTER_CHOICES` dataset goes here

SF_HUB <- get_sf_hub()
SF_ZIP <- get_sf_zip()
SF_CT <- get_sf_ct()
SF_COUNTY <- get_sf_county()

vax_provider <- get_vax_provider()

CHOICES_HUB <- sort(unique(SF_HUB$HUB_Name))
CHOICES_CT <- sort(unique(SF_CT$GEOID))
CHOICES_COUNTY <- sort(unique(SF_COUNTY$COUNTY))

CHOICES_ZIP <- sort(as.character(unique(SF_ZIP$GEOID10)))

CHOICES_VAX_TYPE <- sort(unique(vax_provider$Vaccine_Type))
names(CHOICES_VAX_TYPE) <- c(
  "Pfizer-BioNTech - Age 12+ years",
  "Pfizer-BioNTech -  Age 5 to 11 years",
  "Moderna - Age 18+"
)
CHOICES_VAX_CITY <- sort(unique(vax_provider$City))

CHOICES_POINT_OF_INTEREST <-
  sort(unique(get_point_of_interest()$Type))

CHOICES_NEAREST_VAX_BY_CAR <-
  sort(unique(get_vax_provider_travel_time_by_car()$travel_time_to_nearest_ped_vacc_provider_by_car))

CHOICES_NEAREST_VAX_BY_TRANSIT <-
  sort(unique(get_vax_provider_travel_time_by_transit()$travel_time_to_nearest_ped_vacc_provider_by_transit))

EQUITY_MAP_FILTER_CHOICES <-
  list(
    hub = CHOICES_HUB,
    ct = CHOICES_CT,
    county = CHOICES_COUNTY,
    zip = CHOICES_ZIP,
    vax_type = c(
      CHOICES_VAX_TYPE
    ),
    vax_city = CHOICES_VAX_CITY,
    point_of_interest = CHOICES_POINT_OF_INTEREST,
    nearest_vax_by_car = CHOICES_NEAREST_VAX_BY_CAR,
    nearest_vax_by_transit = CHOICES_NEAREST_VAX_BY_TRANSIT
  )

usethis::use_data(EQUITY_MAP_FILTER_CHOICES, overwrite = TRUE)
