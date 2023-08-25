## code to prepare `DATA_SOURCE_DOC` dataset goes here
DATA_SOURCE_DOC_Mahoning <- list(
  list(
    name = "Polling location data",
    source = "BOE websites",
    last_updated = "5/12/2023",
    notes = "Data downloaded from different sources and merged together. Geocoded on R and operational status verified using the Google Places API"
  ),
  list(
    name = "Mahoning accessibility",
    source = "",
    last_updated = "7/13/2023",
    notes = ""
  ),
  list(
    name = "Mahoning businesses",
    source = 'OSC business listings and <a href="https://relink.org/" target="_blank">relink.org</a>',
    last_updated = "7/24/2023",
    notes = "Business listings and relink data filtered and merged in R. Operational status verified using Google Places API"
  ),
  list(
    name = "CDC Places chronic diseases",
    source = '<a href="https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh" target="_blank">https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh</a>',
    last_updated = "5/23/2023",
    notes = ""
  ),
  list(
    name = "Mahoning food access",
    source = '<a href="https://www.ers.usda.gov/webdocs/DataFiles/80591/FoodAccessResearchAtlasData2019.xlsx?v=8897.6" target="_blank">https://www.ers.usda.gov/webdocs/DataFiles/80591/FoodAccessResearchAtlasData2019.xlsx?v=8897.6</a>',
    last_updated = "5/22/2023",
    notes = ""
  ),
  list(
    name = "ACS 5 year population estimate (2015-2019)",
    source = '<a href="https://www.census.gov/data/developers/data-sets/acs-5year.2021.html#list-tab-1036221584" target="_blank">https://www.census.gov/data/developers/data-sets/acs-5year.2021.html#list-tab-1036221584</a>',
    last_updated = "5/23/2023",
    notes = ""
  ),
  list(
    name = "Mahoning naloxone administration",
    source = '<a href="https://ems.ohio.gov/ems-trauma-data/naloxone-watch" target="_blank">https://ems.ohio.gov/ems-trauma-data/naloxone-watch</a>',
    last_updated = "6/22/2023",
    notes = ""
  ),
  list(
    name = "OCOI infant birth outcomes",
    source = '<a href="https://hpdw.osu.edu/t/COI/views/2020-07-23-4PME_16111908392390/OCOI?%3Aembed_code_version=3&amp;%3Aembed=y&amp;%3AloadOrderID=0&amp;%3Adisplay_spinner=no&amp;%3AshowAppBanner=false&amp;%3Adisplay_count=n&amp;%3AshowVizHome=n&amp;%3Aorigin=viz_share_link" target="_blank">https://hpdw.osu.edu/t/COI/views/2020-07-23-4PME_16111908392390/OCOI?%3Aembed_code_version=3&amp;%3Aembed=y&amp;%3AloadOrderID=0&amp;%3Adisplay_spinner=no&amp;%3AshowAppBanner=false&amp;%3Adisplay_count=n&amp;%3AshowVizHome=n&amp;%3Aorigin=viz_share_link</a>',
    last_updated = "7/27/2023",
    notes = "Census tracts converted from partial to full in R. Infant health score determined by distribution scores for infant health variables (infant injury, mortality, NAS, NICU, preterm birth, severe maternal morbidity, well child visits)"
  )
) %>%
  dplyr::bind_rows()
usethis::use_data(DATA_SOURCE_DOC_Mahoning, overwrite = TRUE)
