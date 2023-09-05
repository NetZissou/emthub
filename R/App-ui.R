


#' User Interface for EMT application
#'
#' @return shiny ui
#' @export
ui <- function() {

  bslib::page_navbar(
    id = "emt",
    title = "Equity Mapping Tool Dashboard",

    theme = bslib::bs_theme(
      font_scale = NULL,
      `enable-rounded` = TRUE,
      bootswatch = "pulse"
    ),

    sidebar = NULL,

    selected = "Main Map",

    shiny::tags$head(
      # Note the wrapping of the string in HTML()
      shiny::tags$style(shiny::HTML("
    meter::-webkit-meter-optimum-value {
    background: red; /* Green */
    }"))
    ),

    bslib::nav_panel(
      "Introduction"
      # TODO: Intro Page Module
    ),

    bslib::nav_panel(
      "Main Map",
      waiter::autoWaiter(),
      equityMapUI("equity_map")

    ),

    bslib::nav_panel(
      "Chronic Diseases Mapping Tool",
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
      "Data Sources",
      shiny::h3("Data Source Documentation"),
      dataDocUI("data_doc")
    )
  )
}

