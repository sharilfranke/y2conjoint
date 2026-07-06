test_that("competitive_set holds its specs", {
  cjt <- sample_conjoint()
  cs <- competitive_set(list(
    spec(cjt, c("Northwind", "$249"), name = "A"),
    spec(cjt, c("Cascade", "$299"), name = "B")
  ))
  expect_length(cs@specs, 2)
})

test_that("competitive_set rejects non-spec elements", {
  expect_snapshot(error = TRUE, competitive_set(list(1, 2)))
})

test_that("competitive_set rejects specs over different collections", {
  brand_only <- list(collection(
    name = "Brand",
    levels = c("Northwind", "Cascade")
  ))
  price_only <- list(collection(name = "Price", levels = c("$249", "$299")))
  expect_snapshot(
    error = TRUE,
    competitive_set(list(
      spec(brand_only, "Northwind"),
      spec(price_only, "$249")
    ))
  )
})
