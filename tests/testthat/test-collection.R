test_that("collection stores name and levels", {
  cl <- collection(name = "Brand", levels = c("Northwind", "Cascade"))
  expect_equal(cl@name, "Brand")
  expect_equal(cl@levels, c("Northwind", "Cascade"))
  expect_false(is_ordered(cl))
})

test_that("ordered_collection is ordered and keeps level order", {
  cl <- ordered_collection(name = "Price", levels = c("$249", "$299"))
  expect_true(is_ordered(cl))
  expect_equal(cl@levels, c("$249", "$299"))
})

test_that("ordered_collection order defaults to unique(levels)", {
  cl <- ordered_collection(name = "Price", levels = c("$249", "$299"))
  expect_equal(cl@order, c("$249", "$299"))
})

test_that("ordered_collection stores an explicit order", {
  cl <- ordered_collection(
    name = "Price",
    levels = c("$299", "$249"),
    order = c("$249", "$299")
  )
  expect_equal(cl@order, c("$249", "$299"))
})

test_that("ordered_collection rejects an order that mismatches levels", {
  expect_snapshot(
    error = TRUE,
    ordered_collection(
      name = "Price",
      levels = c("$249", "$299"),
      order = c("$249", "$399")
    )
  )
})

test_that("collection rejects empty levels", {
  expect_snapshot(
    error = TRUE,
    collection(name = "Brand", levels = character())
  )
})

test_that("collection rejects duplicate levels", {
  expect_snapshot(
    error = TRUE,
    collection(name = "Brand", levels = c("A", "A"))
  )
})

test_that("collection rejects an empty name", {
  expect_snapshot(error = TRUE, collection(name = "", levels = "A"))
})
