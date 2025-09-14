#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("Vivino betyg för Systembolaget"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("ratings", "Snittbetyg:", min = 0, max = 5, step = 0.01, value = c(0, 5)),
          selectInput("countries", "Land:", multiple = TRUE, choices = country_list()),
          fluidRow(
            column(6, numericInput("min_price", "Pris:", value = 0, min = 0, max = max_price())),
            column(6, numericInput("max_price", "", value = max_price(), min = 0, max = max_price()))
          ),
          selectInput("store", "Butik:", choices = store_list()),
          selectInput(
            "taste",
            label = tags$span(
              "Passar till:",
              tags$i(
                class = "glyphicon glyphicon-info-sign",
                style = "color:#0072B2;",
                title = "De valda alternativen kombineras genom 'eller'"
              )
            ),
            choices = get_taste_symbol_list(),
            multiple = TRUE
          ),
          selectInput("category", "Kategori:", choices = category_list(), multiple = TRUE)
        ),

        mainPanel(
          DT::dataTableOutput("table")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bolagetratings"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
