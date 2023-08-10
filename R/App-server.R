#' Shiny server
#'
#' @param input shiny input
#' @param output shiny outptu
#' @param session shiny session
#'
#' @export
server <- function(input, output, session) {

  diseaseOutcomesServer("disease_outcomes")

  equityMapServer("equity_map")

}
