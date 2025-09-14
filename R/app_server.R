#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  output$table <- DT::renderDataTable(
    {
      merged %>%
        filter_store() %>%
        filter_taste() %>%
        filter_ratings() %>%
        filter_country() %>%
        filter_price() %>%
        filter_category() %>%
        dplyr::mutate(
          rating_per_price = round(.data$rating_per_price, 2),
          join_score = round(100 * .data$join_score, 2)
        ) %>%
        dplyr::select(
          Namn = "name",
          Pris = "price",
          Snittbetyg = "ratings_average",
          "Antal betyg" = "ratings_count",
          Volym = "volume",
          Land = "country",
          "Betyg per krong" = "rating_per_price",
          Matchningsbetyg = "join_score"
        )
    },
    rownames = FALSE
  )

  filter_ratings <- function(df) {
    df %>%
      dplyr::filter(
        dplyr::between(.data$ratings_average, input$ratings[1], input$ratings[2])
      )
  }

  filter_country <- function(df) {
    if (is.null(input$countries)) {
      return(df)
    }
    df %>%
      dplyr::filter(.data$country %in% input$countries)
  }

  filter_price <- function(df) {
    df %>%
      dplyr::filter(
        dplyr::between(.data$price, input$min_price, input$max_price)
      )
  }

  filter_store <- function(df) {
    if (input$store != 0) {
      df <- df %>%
        dplyr::semi_join(
          dplyr::filter(store_products, .data$storeId == input$store)
        )
    }
    return(df)
  }

  filter_taste <- function(df) {
    if (is.null(input$taste)) {
      return(df)
    }
    df %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(input$taste), as.logical)
      )
  }

  filter_category <- function(df) {
    if (is.null(input$category)) {
      return(df)
    }
    df %>%
      dplyr::filter(
        .data$categoryLevel2 %in% input$category
      )
  }

}
