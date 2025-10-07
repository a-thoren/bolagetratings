country_list <- function() {
  merged %>%
    dplyr::count(.data$country) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::distinct(.data$country) %>%
    dplyr::pull("country")
}

max_price <- function() {
  merged %>%
    dplyr::summarise(max = max(.data$price, na.rm = TRUE)) %>%
    dplyr::pull("max")
}

store_list <- function() {

  tmp <- stores %>%
    dplyr::semi_join(store_products, by = dplyr::join_by("siteId" == "storeId")) %>%
    dplyr::mutate(
      name = sprintf(
        "%s, %s",
        stringr::str_to_title(.data$city),
        stringr::str_to_title(.data$displayName)
        )
    ) %>%
    dplyr::select("siteId", "name")

  c(
    "Alla" = "0",
    stats::setNames(
      dplyr::pull(tmp, "siteId"),
      dplyr::pull(tmp, "name")
    )
  )

}

get_taste_symbol_list <- function() {

  c(
    "Aperitif",
    "Fisk",
    "Grönsaker",
    "Skaldjur",
    "Fläsk",
    "Fågel",
    "Sällskapsdryck",
    "Buffémat",
    "Lamm",
    "Nöt",
    "Vilt",
    "Dessert",
    "Ost",
    "Asiatiskt",
    "Kryddstarkt"
  )

}

category_list <- function() {
  merged %>%
    dplyr::distinct(.data$categoryLevel2) %>%
    dplyr::pull("categoryLevel2")
}
