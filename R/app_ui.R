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
      titlePanel("Vivino ratings for Systembolaget"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("ratings", "Average rating:", min = 0, max = 5, step = 0.01, value = c(0, 5)),
          selectInput("countries", "Countries:", multiple = TRUE, choices = country_list()),
          sliderInput("price", "Price:", min = 0, max = max_price(), step = 1, value = c(0, max_price())),
          selectInput("store", "Store:", choices = store_list()),
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
          )
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bolagetratings"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
