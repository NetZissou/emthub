#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  app_county <- shiny::reactiveValues(
    value = NULL
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
    }
  })

  #ct_level_data <- get_ct_level_data()



  ct_level_data <- shiny::reactive({

    if (rlang::is_empty(app_county$value)) {
      return(get_ct_level_data())
    } else {
      return(
        get_ct_level_data() %>%
          dplyr::filter(
            stringr::str_to_lower(.data$CountyName) %in% stringr::str_to_lower(app_county$value)
          )
      )
    }
  })


  diseaseOutcomesServer(
    "disease_outcomes"
    #ct_level_data
  )

  equityMapServer("equity_map", ct_level_data = ct_level_data)

}
