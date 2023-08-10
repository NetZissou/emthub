#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  shiny::observe(priority = 999, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!rlang::is_empty(query$county) && stringr::str_to_title(query$county) == "Mahoning") {
      shiny::hideTab(
        inputId = "emt",
        target = "Equity Map"
      )
    }
  })

  diseaseOutcomesServer("disease_outcomes")

  equityMapServer("equity_map")

}
