dataDocUI <- function(id) {
  bslib::page_fillable(
    #reactable_searchBar(shiny::NS(id, "data_doc_table"), placeholder = "Search for Data  ..."),
    #reactable_csvDownloadButton(shiny::NS(id, "data_doc_table"), filename = "data_source_documentation.csv"),
    reactable::reactableOutput(shiny::NS(id, "data_doc_table"))
  )
}



dataDocServer <- function(id, app_county) {

  shiny::moduleServer(id, function(input, output, session){

    output$data_doc_table <- reactable::renderReactable({
      #shiny::req(app_county$value == "Mahoning")

      emthub::DATA_SOURCE_DOC_Mahoning %>%
        purrr::set_names(c("Name", "Source", "Last Updated", "Notes")) %>%
        reactable::reactable(
          # Table Format
          filterable = TRUE,
          outlined = TRUE,
          # Selection
          #selection = "multiple", onClick = "select",
          highlight = TRUE,
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          # Table Size
          defaultPageSize = 10, minRows = 10,
          # Cols
          columns = list(
            Name = reactable::colDef(minWidth = 100),
            Source = reactable::colDef(html = TRUE, minWidth = 100),
            `Last Updated` = reactable::colDef(minWidth = 50, align = "center"),
            Notes = reactable::colDef(minWidth = 150)
          )
        )
    })
  })
}
