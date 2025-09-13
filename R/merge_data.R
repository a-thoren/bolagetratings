#' Merge Systembolaget with Vivino ratings
#'
#' @param systembolaget Systembolaget data, result of [get_products()]
#' @param vivino Vivino data, result of [cache_systembolaget_wines()] and
#' [extract_vivino_products()]
#'
#' @return merged data.frame
#' @export
merge_data <- function(systembolaget, vivino) {

  systembolaget <- systembolaget %>%
    dplyr::mutate(name = paste(.data$productNameBold, .data$productNameThin))

  merged <- systembolaget %>%
    dplyr::select(
      "productId",
      "name",
      "price",
      "country",
      "volume",
      "categoryLevel1",
      "categoryLevel2",
      "categoryLevel3",
      "originLevel1",
      "tasteSymbols"
    ) %>%
    dplyr::right_join(
      vivino,
      by = c("productId", "country"),
      suffix = c("", "_vivino"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      name_similarity = stringdist::stringsim(.data$name, .data$name_vivino, method = "jaccard"),
      price_similarity = 1 / (1 + abs(.data$price - .data$price_vivino)),
      join_score = dplyr::case_when(
        is.na(.data$price_vivino) ~ .data$name_similarity,
        .default = 0.7 * .data$name_similarity + 0.3 * .data$price_similarity
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$join_score)) %>%
    dplyr::distinct(.data$productId, .keep_all = TRUE) %>%
    dplyr::mutate(
      price_per_750 = 750 * .data$price / .data$volume,
      rating_per_price = .data$ratings_average / .data$price_per_750
    ) %>%
    dplyr::relocate("rating_per_price", .before = 1) %>%
    tidyr::unnest(.data$tasteSymbols, keep_empty = TRUE) %>%
    dplyr::mutate(flag = 1) %>%
    tidyr::pivot_wider(
      names_from = "tasteSymbols",
      values_from = "flag",
      values_fill = 0
    ) %>%
    dplyr::select(-"NA")

  return(merged)
}
