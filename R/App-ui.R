


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

    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          '
          .intro h1, .intro h2, .intro h3 {
            font-family: serif;
          }

          .intro h1 {
            font-size: 45px;
          }

          .intro h2 {
            font-size: 35px;
          }

          .intro body {
            font-family: serif;
            font-size: 30px;
          }
          meter::-webkit-meter-optimum-value {
            background: red; /* Green */
          }
          '
        )
      )
    ),

    sidebar = NULL,

    selected = "Main Map",

    bslib::nav_panel(
      "Introduction",
      introUI("intro")
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

