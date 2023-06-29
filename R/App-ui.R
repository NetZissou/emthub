
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
              leaflet::leafletOutput("index_map"),
              full_screen = TRUE
            ),
            bslib::card(
              shiny::htmlOutput("index_map_info")
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
# ui <- bslib::page_navbar(
#   title = "EMT Hub Dashboard",
#
#   sidebar = NULL,
#
#   shiny::tags$head(
#     # Note the wrapping of the string in HTML()
#     shiny::tags$style(shiny::HTML("
#     meter::-webkit-meter-optimum-value {
#     background: red; /* Green */
#     }"))
#   ),
#
#   bslib::nav_panel(
#     "Tab 1",
#
#     bslib::accordion(
#
#       open = c("Result"),
#       bslib::accordion_panel(
#         "Controls",
#         sortable::bucket_list(
#           header = "Disease Outcomes of Interest",
#           group_name = "bucket_list_group",
#           orientation = "horizontal",
#
#           sortable::add_rank_list(
#             text = "Outcomes",
#             labels = emthub::DISEASE_OUTCOMES,
#             input_id = "outcomes"
#           ),
#           sortable::add_rank_list(
#             text = "Tier 1",
#             labels = NULL,
#             input_id = "rank_list_1"
#           ),
#           sortable::add_rank_list(
#             text = "Tier 2",
#             labels = NULL,
#             input_id = "rank_list_2"
#           ),
#           sortable::add_rank_list(
#             text = "Tier 3",
#             labels = NULL,
#             input_id = "rank_list_3"
#           ),
#           options = sortable::sortable_options(multiDrag = TRUE)
#         )
#       ),
#       bslib::accordion_panel(
#         "Result",
#         bslib::navset_card_tab(
#           full_screen = TRUE,
#           title = NULL,
#           bslib::nav_panel(
#             "Map",
#             #card_title("Map"),
#             bslib::layout_columns(
#               bslib::card(
#                 leaflet::leafletOutput("index_map"),
#                 full_screen = TRUE
#               ),
#               bslib::card(
#                 shiny::htmlOutput("index_map_info")
#               ),
#               col_widths = c(8,4)
#
#             )
#           ),
#
#           bslib::nav_panel(
#             shiny::icon("circle-info"),
#             reactable::reactableOutput("index_table")
#           )
#         )
#       )
#     )
#   )
#   # bslib::nav_panel("Tab 2",
#   #                  "XXX"
#   # )
#
# )
