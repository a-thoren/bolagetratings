fetch_vivino_rating <- function(
    query,
    productId,
    user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"
) {

  url <- paste0(
    "https://www.vivino.com/search/wines?q=",
    utils::URLencode(query, reserved = TRUE)
  )

  resp <- httr2::request(url) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Accept" = "text/html"
    ) |>
    httr2::req_perform()

  html <- httr2::resp_body_html(resp)

  preload_node <- rvest::html_element(html, "#search-page")
  preload_json <- rvest::html_attr(preload_node, "data-preloaded-state")

  if (is.na(preload_json)) {
    return(NULL)
  }

  data <- jsonlite::fromJSON(preload_json)

  if (is.null(data$search_results$matches) || length(data$search_results$matches) == 0) {
    return(NA)
  } else {

    qs::qsave(
      data$search_results$matches,
      sprintf("./data/vivino/%s.qs", productId)
    )

  }

  return(NA)
}


cache_systembolaget_wines <- function(systembolaget_products) {

  systembolaget_wines <- systembolaget_products %>%
    dplyr::filter(
      .data$categoryLevel1 == "Vin",
      grepl("flaska", .data$packagingLevel1, ignore.case = TRUE)
    ) %>%
    dplyr::mutate(name = paste(.data$productNameBold, .data$productNameThin))

  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0 Safari/537.36",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Mobile/15E148 Safari/604.1"
  )

  i <- 1

  done <- list.files("./data/vivino/") %>%
    stringr::str_extract("\\d+")

  while (i <= nrow(systembolaget_wines)) {

    name <- systembolaget_wines[i, "name"]
    productId <- systembolaget_wines[i, "productId"]

    if (productId %in% done) {
      i <- i + 1
      next
    }

    ua <- user_agents[((i - 1) %/% 10) %% length(user_agents) + 1]
    error_message <- NULL
    res <- tryCatch({
      fetch_vivino_rating(name, user_agent = ua, productId)
    }, error = function(e) {
      error_message <- e$message
      message(sprintf("%s: [%d/%d] Failed on wine %s: %s", format(Sys.time()), i, nrow(systembolaget_wines), name, e$message))
      return(NULL)
    })

    if (is.null(res)) {

      if (grepl("429", error_message)) {
        message(sprintf("%s: Too many requests made, sleeping for 30 seconds then retrying", format(Sys.time())))
      } else {
        message(sprintf("%s: Request failed, sleeping for 30 seconds then retrying", format(Sys.time())))
      }
      Sys.sleep(30)
      next

    }

    message(sprintf("%s: [%d/%d] Saved wine %s [%s]", format(Sys.time()), i, nrow(systembolaget_wines), name, productId))
    i <- i + 1

  }


}

extract_vivino_products <- function() {

  files <- list.files("./data/vivino/", full.names = TRUE)

  price_or_na <- function(data) {
    if (all(is.na(data$price))) {
      NA
    } else {
      data$price$amount
    }
  }

  vivino <- purrr::map(
    as.list(files),
    \(x) {
      data <- qs::qread(x)
      tibble::tibble(
        productId = stringr::str_extract(x, "\\d+"),
        price = price_or_na(data),
        name = data$vintage$name,
        ratings_count = data$vintage$statistics$ratings_count,
        ratings_average = data$vintage$statistics$ratings_average,
        country = data$vintage$wine$region$country$name
      )
    },
    .progress = TRUE
  ) %>%
    dplyr::bind_rows()

  return(vivino)

}
