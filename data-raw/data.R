# Systembolaget
stores <- get_stores()
products <- get_products()
store_products <- get_store_products(stores)

# Vivino
cache_systembolaget_wines(products)
vivino <- extract_vivino_products()

merged <- merge_data(products, vivino)

usethis::use_data(
  merged,
  products,
  vivino,
  store_products,
  stores,
  overwrite = TRUE,
  internal = TRUE
)
