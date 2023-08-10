


#' User Interface for EMT application
#'
#' @return shiny ui
#' @export
ui <- function() {

  bslib::page_navbar(
    title = "EMT Hub Dashboard",

    theme = bslib::bs_theme(
      font_scale = NULL,
      `enable-rounded` = TRUE,
      bootswatch = "pulse"
    ),

    sidebar = NULL,

    selected = "Disease Outcomes Map",

    shiny::tags$head(
      # Note the wrapping of the string in HTML()
      shiny::tags$style(shiny::HTML("
    meter::-webkit-meter-optimum-value {
    background: red; /* Green */
    }"))
    ),

    bslib::nav_panel(
      "Disease Outcomes Map",
      diseaseOutcomesUI("disease_outcomes")
    ),
    bslib::nav_panel("Data",
                     shiny::h3("Data Source Documentation")
    ),

    bslib::nav_panel(
      "Equity Map",
      waiter::autoWaiter(),
      equityMapUI("equity_map")

    )


  )
}

