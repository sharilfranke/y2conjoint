test_that("competitive_set holds its specs", {
  cjt <- sample_conjoint()
  cs <- competitive_set(
    spec(
      cjt,
      c("Northwind", "$199", "20 hours", "256 GB", "Black"),
      name = "A"
    ),
    spec(cjt, c("Cascade", "$299", "10 hours", "128 GB", "Blue"), name = "B")
  )
  expect_length(cs@specs, 2)
})

test_that("competitive_set rejects non-spec elements", {
  expect_snapshot(error = TRUE, competitive_set(1, 2))
})

test_that("competitive_set prints named and unnamed specs", {
  cjt <- sample_conjoint()
  cs <- competitive_set(
    spec(
      cjt,
      c("Northwind", "$199", "20 hours", "256 GB", "Black"),
      name = "A"
    ),
    spec(cjt, c("Cascade", "$299", "10 hours", "128 GB", "Blue"), name = "B"),
    spec(cjt, c("Meridian", "$399", "30 hours", "512 GB", "Silver")),
    name = "Launch"
  )
  expect_snapshot(print(cs))
})

test_that("competitive_set rejects specs over different collections", {
  brand_only <- list(collection(
    name = "Brand",
    levels = c("Northwind", "Cascade")
  ))
  price_only <- list(collection(name = "Price", levels = c("$249", "$299")))
  expect_snapshot(
    error = TRUE,
    competitive_set(
      spec(brand_only, "Northwind"),
      spec(price_only, "$249")
    )
  )
})
