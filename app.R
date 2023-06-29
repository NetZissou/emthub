library(devtools)
load_all()

shiny::shinyApp(
  emthub::ui,
  emthub::server
)
