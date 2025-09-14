#' Gets Systembolagets stores and relevant information
#'
#' @return data.frame
#' @export
get_stores <- function() {

  res <- httr::GET(
    url = "https://api-extern.systembolaget.se/sb-api-ecommerce/v1/sitesearch/site",
    httr::add_headers("Ocp-Apim-Subscription-Key" = "cfc702aed3094c86b92d6d4ff7a54c84")
  ) |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  stores <- res[[1]] |>
    tidyr::unnest_wider(.data$position)

  return(stores)

}

#' Gets a single page of Systembolaget products
#'
#' If a `storeId` is provided, only `productId` and `storeId` are included in
#' the result.
#'
#' @param page the page to get
#' @param storeId an optional store id
#'
#' @return data.frame
#' @export
get_produts_page <- function(page = 1, storeId = NULL) {

  message(sprintf("Getting page %d...", page))

  url <- sprintf(
    "https://api-extern.systembolaget.se/sb-api-ecommerce/v1/productsearch/search?page=%d",
    page
  )

  if (!is.null(storeId)) {
    url <- sprintf("%s&storeId=%s&isInStoreAssortmentSearch=true", url, storeId)
  }

  httr::GET(
    url = url,
    httr::add_headers("Ocp-Apim-Subscription-Key" = "cfc702aed3094c86b92d6d4ff7a54c84")
  ) |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

}

#' Gets all Systembolaget products
#'
#' If a `storeId` is provided, only that stores products are returned
#'
#' @param storeId an optional store id
#'
#' @return data.frame
#' @export
get_products <- function(storeId = NULL) {

  message(sprintf("Getting products for store %s...", storeId))

  res <- get_produts_page(storeId = storeId)

  metadata <- res$metadata
  products <- res$products

  while (metadata$nextPage != -1) {

    res <- get_produts_page(page = metadata$nextPage, storeId = storeId)
    metadata <- res$metadata

    products <- dplyr::bind_rows(products, res$products)

  }

  if (!is.null(storeId)) {
    if (length(products) == 0) return(NULL)
    products <- products %>%
      dplyr::select("productId") %>%
      dplyr::mutate(storeId = .env$storeId)
  }


  return(products)

}

get_store_products <- function(stores) {
  store_produts <- stores %>%
    dplyr::filter(tolower(.data$city) %in% c("göteborg", "västra frölunda")) %>%
    dplyr::pull("siteId") %>%
    purrr::map(get_products) %>%
    dplyr::bind_rows()
}
