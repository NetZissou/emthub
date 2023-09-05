get_intro_content <- function(popup = FALSE) {

  if (popup) {
    header <- shiny::tags$line()
  } else {
    header <- shiny::tags$h1(
      style="text-align: left; font-weight: bold;",
      shiny::tags$b(
        "Equity Mapping Tool"
      )
    )
  }

  shiny::tagList(
    shiny::tags$div(
      class = "intro",
      style = "text-align: left;",

      header,

      shiny::tags$h2(
        style="text-align: left; font-weight: bold;",
        shiny::tags$b( "Empowering community health workers with local data for local decisions to advance the health of all Ohioans.")
      ),

      shiny::tags$br(),

      shiny::tags$p(
        "This web application provides maps and data to support the work of community health workers. Specifically, this tool supports COVID-19 public health response efforts, including building community resiliency by addressing social determinants of health. This web application is one of several tools provided by the Equity Mapping Tool project."
      ),

      shiny::tags$br(),

      shiny::tags$p(
        style = "font-weight: bold;",
        'If this is your first time on this site, then please watch the training video (click here) and review the "Data Sources" tab prior to using the information on this website. If you are part of or connected to a Pathways HUB in Ohio, then please enroll in the EMT Training Program and complete at least one of the courses in the training program before using this tool in your daily work as a CHW, CHW supervisor, of HUB staff.'
      ),

      shiny::tags$br(),

      shiny::HTML('<p style="color: orange;"><u>Please note: This website is provided for informational purposes only. Maps may load slowly during your first use.</u></p>'),

      shiny::tags$br(),

      shiny::tags$p(
        "This application works best when using the Chrome web browser and viewed using a laptop or desktop."
      ),

      shiny::tags$br(),

      shiny::tags$p(
        shiny::tags$i(
          "Funding provided by: Community Health Workers for COVID Response and Resilient Communities (CCR): A Component C Innovative Demonstration Project"
        )
      ),

      shiny::tags$p(
        shiny::tags$i(
          "Developed and maintained by: The Computational Epidemiology Lab (PI: Dr. Ayaz Hyder) at the College of Public Health and Translational Data Analytics Institute, The Ohio State University. This website does not reflect the views of Ohio Department of Health or Health Impact Ohio."
        )
      ),

      shiny::tags$p(
        shiny::tags$i(
          shiny::HTML(
            'For more information please contact Dr. Ayaz Hyder at <a href="mailto:hyder.22@osu.edu">hyder.22@osu.edu</a>.'
          )
        )
      )


    )
  )
}

introUI <- function(id) {
  bslib::page_fillable(
    bslib::layout_columns(
      col_widths = c(6,6),
      get_intro_content()
    )
  )
}





