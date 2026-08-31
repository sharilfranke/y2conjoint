test_that("partworth_chart returns a ggplot built on the selected collections", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_s3_class(p, "ggplot")
  expect_setequal(levels(p$data$label), c("Northwind", "Cascade", "Meridian"))
})

test_that("partworth_chart defaults to every collection", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, color = "#F28E2B")

  all_levels <- purrr::list_c(purrr::map(
    get_collections(cjt),
    collection_levels
  ))
  expect_setequal(levels(p$data$label), all_levels)
})

test_that("partworth_chart centers each level on 0", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_equal(mean(p$data$partworth), 0)
})

test_that("partworth_chart orders a plain collection's levels ascending by part-worth", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Brand", color = "#F28E2B")

  expected_order <- p$data$label[order(p$data$partworth)] |> as.character()
  expect_equal(levels(p$data$label), expected_order)
})

test_that("partworth_chart keeps an ordered_collection's relative level order", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Battery", color = "#F28E2B")

  battery <- purrr::keep(get_collections(cjt), \(cl) cl@name == "Battery")[[1]]
  expect_equal(levels(p$data$label), battery@order)
})

test_that("partworth_chart flips an ordered_collection whose defined order trends toward lower part-worths", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Price", color = "#F28E2B")

  # $199 has the highest mean utility of the three Price levels and $399 the
  # lowest, so ascending price ($199, $299, $399) trends toward lower
  # part-worths; the chart should flip it to ($399, $299, $199) so the top of
  # the panel (last factor level) is the most positive, not the most negative.
  price <- purrr::keep(get_collections(cjt), \(cl) cl@name == "Price")[[1]]
  expect_equal(levels(p$data$label), rev(price@order))
})

test_that("partworth_chart draws a plain collection's absence level first, ahead of utility", {
  cw <- sample_crosswalk()
  # Northwind has the highest mean utility of the three Brand levels, so it
  # would normally sort last (ascending); flagging it as absence should move
  # it to the front instead.
  cw$absence <- cw$user_name == "Northwind"
  cjt <- conjoint_df(sample_data(), cw)

  p <- partworth_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_equal(levels(p$data$label)[[1]], "Northwind")
})

test_that("partworth_chart draws an ordered_collection's absence level first, ahead of @order", {
  cw <- sample_crosswalk()
  # 512 GB is both last in Storage's defined order and its highest-utility
  # level; flagging it as absence should still move it to the front.
  cw$absence <- cw$user_name == "512 GB"
  cjt <- conjoint_df(sample_data(), cw)

  p <- partworth_chart(cjt, collection = "Storage", color = "#F28E2B")

  expect_equal(levels(p$data$label)[[1]], "512 GB")
  expect_setequal(levels(p$data$label)[-1], c("128 GB", "256 GB"))
})

test_that("partworth_chart does not facet a single collection", {
  cjt <- sample_conjoint()
  p <- partworth_chart(cjt, collection = "Brand", color = "#F28E2B")

  expect_s3_class(p$facet, "FacetNull")
})

test_that("partworth_chart facets by collection when more than one is selected", {
  cjt <- sample_conjoint()
  p <- partworth_chart(
    cjt,
    collection = c("Battery", "Storage"),
    color = "#4E79A7"
  )

  expect_s3_class(p$facet, "FacetWrap")
})

test_that("partworth_chart applies a single color to every collection", {
  cjt <- sample_conjoint()
  p <- partworth_chart(
    cjt,
    collection = c("Brand", "Price"),
    color = "#F28E2B"
  )

  built <- ggplot2::ggplot_build(p)
  expect_true(all(toupper(built$data[[1]]$fill) == "#F28E2B"))
})

test_that("partworth_chart accepts a different color per collection", {
  cjt <- sample_conjoint()
  p <- partworth_chart(
    cjt,
    collection = c("Brand", "Price"),
    color = c(Brand = "#F28E2B", Price = "#4E79A7")
  )

  built <- ggplot2::ggplot_build(p)
  by_collection <- split(
    toupper(built$data[[1]]$fill),
    built$data[[1]]$PANEL
  )
  expect_true(all(unlist(by_collection[[1]]) %in% c("#F28E2B", "#4E79A7")))
  fills <- toupper(built$data[[1]]$fill)
  expect_setequal(fills, c("#F28E2B", "#4E79A7"))
})

test_that("partworth_chart errors when color names don't match the selected collections", {
  cjt <- sample_conjoint()
  expect_error(
    partworth_chart(
      cjt,
      collection = c("Brand", "Price"),
      color = c(Foo = "#F28E2B", Bar = "#4E79A7")
    ),
    class = "rlang_error"
  )
})

test_that("partworth_chart errors on an unknown collection", {
  cjt <- sample_conjoint()
  expect_error(
    partworth_chart(cjt, collection = "typo", color = "#F28E2B"),
    class = "rlang_error"
  )
})
