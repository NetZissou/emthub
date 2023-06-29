
shiny_gauge <- function(value, min = 0, max = 1) {
  shiny::tagList(
    # tags$label(
    #   `for` = id,
    #   paste0(label, ": ", value)
    # ),
    shiny::tags$meter(
      #id = id,
      value = value,
      min = min,
      max = max
    )
  )
}

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

    shiny::tags$head(
      # Note the wrapping of the string in HTML()
      shiny::tags$style(shiny::HTML("
    meter::-webkit-meter-optimum-value {
    background: red; /* Green */
    }"))
    ),

    bslib::nav_panel(
      "Disease Outcomes Map",

      bslib::navset_card_tab(
        title = NULL,
        bslib::nav_panel(
          "Map",
          #card_title("Map"),
          bslib::layout_columns(
            bslib::card(

              bslib::card_body(
                class = "p-0",
                leaflet::leafletOutput("index_map")
              ),
              full_screen = TRUE
            ),
            bslib::card(
              bslib::navset_pill(
                bslib::nav_panel(
                  'Info',
                  shiny::htmlOutput("index_map_info")
                ),
                bslib::nav_panel(
                  'Business',

                  bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::selectInput(
                      inputId = "filter_ct",
                      label = "Census Tract",
                      choices = emthub::FILTER_CT_CHOICES,
                      multiple = TRUE
                    ),

                    shiny::selectInput(
                      inputId = "filter_zip",
                      label = "Zip",
                      choices = emthub::FILTER_ZIP_CHOICES,
                      multiple = TRUE
                    ),
                    shiny::selectInput(
                      inputId = "filter_city",
                      label = "City",
                      choices = emthub::FILTER_CITY_CHOICES,
                      multiple = TRUE
                    ),

                    shiny::selectInput(
                      inputId = "filter_type",
                      label = "Type",
                      choices = emthub::ILTER_TYPE_CHOICES,
                      multiple = TRUE
                    ),
                  ),

                  shiny::actionButton(
                    inputId = "apply_filter_business",
                    label = "Apply Filters"
                  ),

                  shiny::tags$hr(),
                  shiny::helpText("Toggle to add business to the map"),
                  reactable::reactableOutput("business_table")
                )
              )
            ),
            col_widths = c(8,4)

          )
        ),

        bslib::nav_panel(
          "Controls",
          sortable::bucket_list(
            header = "Disease Outcomes of Interest",
            group_name = "bucket_list_group",
            orientation = "horizontal",

            sortable::add_rank_list(
              text = "Outcomes",
              labels = emthub::DISEASE_OUTCOMES,
              input_id = "outcomes"
            ),
            sortable::add_rank_list(
              text = "Tier 1",
              labels = NULL,
              input_id = "rank_list_1"
            ),
            sortable::add_rank_list(
              text = "Tier 2",
              labels = NULL,
              input_id = "rank_list_2"
            ),
            sortable::add_rank_list(
              text = "Tier 3",
              labels = NULL,
              input_id = "rank_list_3"
            ),
            options = sortable::sortable_options(multiDrag = TRUE)
          )
        )


      )

    )
    # bslib::nav_panel("Tab 2",
    #                  "XXX"
    # )

  )
}

