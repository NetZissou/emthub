


#' User Interface for EMT application
#'
#' @return shiny ui
#' @export
ui <- function() {

  bslib::page_navbar(
    id = "emt",
    title = "EMT Hub Dashboard",

    theme = bslib::bs_theme(
      font_scale = NULL,
      `enable-rounded` = TRUE,
      bootswatch = "pulse"
    ),

    sidebar = NULL,

    #selected = "Equity Map",

    shiny::tags$head(
      # Note the wrapping of the string in HTML()
      shiny::tags$style(shiny::HTML("
    meter::-webkit-meter-optimum-value {
    background: red; /* Green */
    }"))
    ),

    bslib::nav_panel(
      "Equity Map",
      waiter::autoWaiter(),
      equityMapUI("equity_map")

    ),

    bslib::nav_panel(
      "Disease Outcomes Map",
      waiter::waiterPreloader(
        html = shiny::tagList(
          waiter::spin_flower(),
          shiny::h4("Launching EMT ...")
        )
      ),
      waiter::autoWaiter(),
      shinyjs::useShinyjs(),
      diseaseOutcomesUI("disease_outcomes")
    ),

    bslib::nav_panel(
      "Data",
      shiny::h3("Data Source Documentation"),
      dataDocUI("data_doc")
    )
  )
}

