test_that("spec groups levels by collection", {
  cjt <- sample_conjoint()
  s <- spec(
    cjt,
    c("Northwind", "$199", "20 hours", "256 GB", "Black"),
    name = "Cheap"
  )
  expect_equal(s@name, "Cheap")
  expect_equal(s@selections$Brand, "Northwind")
  expect_equal(s@selections$Price, "$199")
})

test_that("spec allows multiple levels within one collection", {
  cjt <- sample_conjoint()
  s <- spec(
    cjt,
    c("Northwind", "Cascade", "$199", "20 hours", "256 GB", "Black")
  )
  expect_setequal(s@selections$Brand, c("Northwind", "Cascade"))
})

test_that("spec warns when a collection has no selected levels", {
  cjt <- sample_conjoint()
  expect_snapshot(s <- spec(cjt, "Northwind"))
  expect_equal(s@selections$Price, character())
})

test_that("spec errors on an unknown level", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, spec(cjt, "Nokia"))
})
