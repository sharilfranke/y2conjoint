sample_crosswalk <- function() {
  tibble::tibble(
    old_name = c("A1B1", "A1B2", "A2B1", "A2B2"),
    user_name = c("Northwind", "Cascade", "$249", "$299"),
    collection_name = c("Brand", "Brand", "Price", "Price"),
    collection_order = c(NA, NA, 1, 2)
  )
}

sample_data <- function() {
  tibble::tibble(
    ID = 1:3,
    A1B1 = c(0.2, -0.1, 0.5),
    A1B2 = c(-0.3, 0.4, 0.1),
    A2B1 = c(0.1, 0.2, -0.2),
    A2B2 = c(0.0, -0.1, 0.4),
    NONE = c(0, 0.1, -0.2),
    age = c(30, 40, 25)
  )
}

sample_conjoint <- function() {
  conjoint_df(sample_data(), sample_crosswalk())
}
