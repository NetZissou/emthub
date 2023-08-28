## code to prepare `DATA_SOURCE_DOC` dataset goes here
DATA_SOURCE_DOC_Mahoning <- list(
  list(
    name = "Polling location data",
    source = "BOE websites",
    last_accessed = "5/12/2023",
    last_updated = "5/1/2023",
    notes = "Data downloaded from different sources and merged together. Geocoded on R and operational status verified using the Google Places API"
  ),
  list(
    name = "Accessibility",
    source = "",
    last_accessed = "7/1/2023",
    last_updated = "7/13/2023",
    notes = ""
  ),
  list(
    name = "Businesses",
    source = 'Data Axle',
    last_accessed = "see notes",
    last_updated = "see notes",
    notes = "Business listings are from the 2020 Historical Business dataset from Data Axle (licensed to Ohio State University). Operational status was verified using Google Places API (Application Programming Interface) in July 2023."
  ),
  list(
    name = "Providers and other resources",
    source = '<a href="https://relink.org/" target="_blank">relink.org</a>',
    last_accessed = "7/24/2023",
    last_updated = "7/24/2023",
    notes = "Relink data are filtered for selected county. Operational status was verified using Google Places API (Application Programming Interface) in July 2023."
  ),
  list(
    name = "CDC Places - Chronic Diseases",
    source = '<a href="https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh" target="_blank">https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh</a>',
    last_accessed = "see notes",
    last_updated = "see notes",
    notes = 'The model-based estimates were generated using BRFSS 2021 or 2020, Census 2010 population counts or census county population estimates of 2021 or 2020, and ACS 2015-2019 or ACS 2016-2020, ACS 2017-2021. Estimates are not available for areas shaded in gray. For more information visit <a href="https://www.cdc.gov/places" target="_blank">https://www.cdc.gov/places</a>. Credit: Centers for Disease Control and Prevention, National Center for Chronic Disease and Health Promotion, Division of Population Health, Atlanta, GA. '
  ),
  list(
    name = "Mahoning food access",
    source = '<a href="https://www.ers.usda.gov/webdocs/DataFiles/80591/FoodAccessResearchAtlasData2019.xlsx?v=8897.6" target="_blank">https://www.ers.usda.gov/webdocs/DataFiles/80591/FoodAccessResearchAtlasData2019.xlsx?v=8897.6</a>',
    last_accessed = "5/22/2023",
    last_updated = "see notes",
    notes = "Data are from the 2019 Food  Access Research Atlas. See url in data source column for more information."
  ),
  list(
    name = "American Community Survey 5 year population estimate (2015-2019)",
    source = '<a href="https://www.census.gov/data/developers/data-sets/acs-5year.2021.html#list-tab-1036221584" target="_blank">https://www.census.gov/data/developers/data-sets/acs-5year.2021.html#list-tab-1036221584</a>',
    last_accessed = "5/23/2023",
    last_updated = "5/1/2023",
    notes = ""
  ),
  list(
    name = "Naloxone administration by EMS",
    source = '<a href="https://ems.ohio.gov/ems-trauma-data/naloxone-watch" target="_blank">https://ems.ohio.gov/ems-trauma-data/naloxone-watch</a>',
    last_accessed = "6/22/2023",
    last_updated = "5/1/2023",
    notes = ""
  ),
  list(
    name = "Ohio Childhood Opportunity Index - Infant Health Score",
    source = '<a href="https://hpdw.osu.edu/t/COI/views/2020-07-23-4PME_16111908392390/OCOI?%3Aembed_code_version=3&amp;%3Aembed=y&amp;%3AloadOrderID=0&amp;%3Adisplay_spinner=no&amp;%3AshowAppBanner=false&amp;%3Adisplay_count=n&amp;%3AshowVizHome=n&amp;%3Aorigin=viz_share_link" target="_blank">https://hpdw.osu.edu/t/COI/views/2020-07-23-4PME_16111908392390/OCOI?%3Aembed_code_version=3&amp;%3Aembed=y&amp;%3AloadOrderID=0&amp;%3Adisplay_spinner=no&amp;%3AshowAppBanner=false&amp;%3Adisplay_count=n&amp;%3AshowVizHome=n&amp;%3Aorigin=viz_share_link</a>',
    last_accessed = "7/27/2023",
    last_updated = "see notes",
    notes = 'Based on data from2013-2017. Scores based on more recent data will be updated when available from the data source. Infant health score is one component of the Ohio Childhood Opportunity Index. It is determined based on census-tract level data multiple infant health variables (infant injury, mortality, NAS, NICU, preterm birth, severe maternal morbidity, well child visits). For more details see: Ohio Opportunity Index Consortium. (2021). The Ohio Opportunity Indices and Dashboard Tools. The Ohio College of Medicine Government Resource Center Website. <a href="https://grc.osu.edu/Projects/OhioOpportunityIndex" target="_blank">https://grc.osu.edu/Projects/OhioOpportunityIndex</a>'
  )
) %>%
  dplyr::bind_rows()
usethis::use_data(DATA_SOURCE_DOC_Mahoning, overwrite = TRUE)
