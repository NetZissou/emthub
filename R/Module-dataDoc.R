dataDocUI <- function(id) {
  bslib::page_fillable(
    #reactable_searchBar(shiny::NS(id, "data_doc_table"), placeholder = "Search for Data  ..."),
    #reactable_csvDownloadButton(shiny::NS(id, "data_doc_table"), filename = "data_source_documentation.csv"),
    # shiny::a("For ... information, please use the"),
    # shiny::actionLink(shiny::NS(id, "go_to_tab_equity_map"), "Equity Map Tab."),
    reactable::reactableOutput(shiny::NS(id, "data_doc_table"))
  )
}



dataDocServer <- function(id, app_county) {

  shiny::moduleServer(id, function(input, output, session){

    shiny::observeEvent(input$go_to_tab_equity_map, {

      shiny::updateTabsetPanel(
        session = session,
        inputId = "emt",
        selected = "Equity Map"
      )
    })

    output$data_doc_table <- reactable::renderReactable({
      #shiny::req(app_county$value == "Mahoning")

      emthub::DATA_SOURCE_DOC_Mahoning %>%
        purrr::set_names(c("Name", "Source", "Last Accessed", "Last Updated", "Notes")) %>%
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
            Source = reactable::colDef(html = TRUE, minWidth = 100, filterable = FALSE, sortable = FALSE),
            `Last Accessed` = reactable::colDef(minWidth = 50, align = "center", filterable = FALSE, sortable = FALSE),
            `Last Updated` = reactable::colDef(minWidth = 50, align = "center", filterable = FALSE, sortable = FALSE),
            Notes = reactable::colDef(html = TRUE, minWidth = 150, filterable = FALSE, sortable = FALSE)
          )
        )
    })
  })
}
