test_that("utility_dist_chart returns a ggplot built on the selected collections", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_s3_class(p, "ggplot")
  expect_setequal(levels(p$data$label), c("Northwind", "Cascade", "Meridian"))
})

test_that("utility_dist_chart defaults to every collection", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(cjt, color = "#F28E2B")

  all_levels <- purrr::list_c(purrr::map(
    get_collections(cjt),
    collection_levels
  ))
  expect_setequal(levels(p$data$label), all_levels)
})

test_that("utility_dist_chart orders a plain collection's levels by ascending mean utility", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(cjt, collection = "Brand", color = "#F28E2B")

  avgs <- p$data |>
    dplyr::summarize(avg = mean(utility_score), .by = "label")
  expected_order <- avgs$label[order(avgs$avg)] |> as.character()
  expect_equal(levels(p$data$label), expected_order)
})

test_that("utility_dist_chart keeps an ordered_collection's defined level order", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(cjt, collection = "Price", color = "#F28E2B")

  price <- purrr::keep(get_collections(cjt), \(cl) cl@name == "Price")[[1]]
  expect_equal(levels(p$data$label), price@order)
})

test_that("utility_dist_chart does not facet a single collection", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_s3_class(p$facet, "FacetNull")
})

test_that("utility_dist_chart facets by collection when more than one is selected", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(
    cjt,
    collection = c("Battery", "Storage"),
    color = "#4E79A7"
  )

  expect_s3_class(p$facet, "FacetWrap")
})

test_that("utility_dist_chart accepts a different color per collection", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(
    cjt,
    collection = c("Brand", "Price"),
    color = c(Brand = "#F28E2B", Price = "#4E79A7")
  )

  built <- ggplot2::ggplot_build(p)
  by_collection <- split(built$data[[1]]$fill, built$data[[1]]$PANEL)
  brand_darkest <- grDevices::colorRampPalette(c("white", "#F28E2B"))(4)[[4]]
  price_darkest <- grDevices::colorRampPalette(c("white", "#4E79A7"))(4)[[4]]
  expect_true(brand_darkest %in% unlist(by_collection))
  expect_true(price_darkest %in% unlist(by_collection))
})

test_that("utility_dist_chart gives every level a matching fill and outline color", {
  cjt <- sample_conjoint()
  p <- utility_dist_chart(
    cjt,
    collection = c("Brand", "Price", "Battery"),
    color = c(Brand = "#F28E2B", Price = "#4E79A7", Battery = "#59A14F")
  )

  built <- ggplot2::ggplot_build(p)
  fill_hex <- toupper(built$data[[1]]$fill)
  outline_hex <- toupper(substr(built$data[[1]]$colour, 1, 7))
  expect_equal(outline_hex, fill_hex)
})

test_that("utility_dist_chart errors when color names don't match the selected collections", {
  cjt <- sample_conjoint()
  expect_error(
    utility_dist_chart(
      cjt,
      collection = c("Brand", "Price"),
      color = c(Foo = "#F28E2B", Bar = "#4E79A7")
    ),
    class = "rlang_error"
  )
})

test_that("utility_dist_chart errors on an unknown collection", {
  cjt <- sample_conjoint()
  expect_error(
    utility_dist_chart(cjt, collection = "typo", color = "#F28E2B"),
    class = "rlang_error"
  )
})
