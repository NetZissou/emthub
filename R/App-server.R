#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  # ====================== #
  # ---- Hyper Params ----
  # ====================== #
  app_county <- shiny::reactiveValues(
    value = "Franklin"
  )
  shiny::observe(priority = 999, {
    query <- shiny::parseQueryString(session$clientData$url_search)

    if (!rlang::is_empty(query$county)) {
      app_county$value <- stringr::str_to_title(query$county)
    }

    if (!rlang::is_empty(query$county) && stringr::str_to_title(query$county) == "Mahoning") {
      shiny::hideTab(
        inputId = "emt",
        target = "Equity Map"
      )

      shiny::updateTabsetPanel(
        session, "emt",
        selected = "Disease Outcomes Map"
      )
    }
  })

  # ============== #
  # ---- Data ----
  # ============== #
  ct_level_data_all <- get_ct_level_data(parquet = TRUE)

  shapefile_list <-
    list(
      SF_HUB = get_sf_hub(parquet = T),
      SF_CT = get_sf_ct(parquet = T),
      SF_COUNTY = get_sf_county(parquet = T)
      #SF_ZIP = get_sf_zip(parquet = T)
    )


  # ================= #
  # ---- Servers ----
  # ================= #

  diseaseOutcomesServer(
    "disease_outcomes",
    ct_level_data_all,
    app_county,
    shapefile_list
  )

  equityMapServer("equity_map", ct_level_data_all, shapefile_list)

  dataDocServer("data_doc")
}
