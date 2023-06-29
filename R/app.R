#' Opioid app activation function
#'
#' @export
app <- function() {
  shiny::shinyApp(
    emthub::ui,
    emthub::server
  )
}
