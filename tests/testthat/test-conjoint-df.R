test_that("conjoint_df renames model columns to user names", {
  cjt <- sample_conjoint()
  expect_contains(names(cjt), c("Northwind", "Cascade", "$199", "$299"))
  expect_disjoint(names(cjt), c("A1B1", "A2B1"))
})

test_that("conjoint_df keeps extra columns", {
  cjt <- sample_conjoint()
  expect_contains(names(cjt), c("respondent_id", "age"))
})

test_that("conjoint_df builds ordered and unordered collections", {
  cjt <- sample_conjoint()
  collections <- conjoint_collections(cjt)
  names(collections) <- vapply(collections, function(cl) cl@name, character(1))
  expect_false(is_ordered(collections$Brand))
  expect_true(is_ordered(collections$Price))
  expect_equal(collections$Price@levels, c("$199", "$299", "$399"))
})

test_that("protected_cols covers levels and NONE", {
  cjt <- sample_conjoint()
  expect_setequal(
    protected_cols(cjt),
    c(
      "Northwind",
      "Cascade",
      "Meridian",
      "$199",
      "$299",
      "$399",
      "10 hours",
      "20 hours",
      "30 hours",
      "128 GB",
      "256 GB",
      "512 GB",
      "Black",
      "Silver",
      "Blue",
      "NONE"
    )
  )
})

test_that("conjoint_df errors when a crosswalk level is missing from data", {
  crosswalk <- sample_crosswalk()
  crosswalk$old_name[1] <- "A9B9"
  expect_snapshot(error = TRUE, conjoint_df(sample_data(), crosswalk))
})

test_that("conjoint_df errors on duplicate user names", {
  crosswalk <- sample_crosswalk()
  crosswalk$user_name[2] <- "Northwind"
  expect_snapshot(error = TRUE, conjoint_df(sample_data(), crosswalk))
})

test_that("conjoint_df errors when one old_name maps to multiple collections", {
  crosswalk <- sample_crosswalk()
  extra <- crosswalk[1, ]
  extra$collection_name <- "Other"
  extra$user_name <- "Northwind2"
  crosswalk <- rbind(crosswalk, extra)
  expect_snapshot(error = TRUE, conjoint_df(sample_data(), crosswalk))
})

test_that("conjoint_df errors when the NONE column is absent", {
  data <- sample_data()
  data$NONE <- NULL
  expect_snapshot(error = TRUE, conjoint_df(data, sample_crosswalk()))
})

test_that("conjoint_df errors on a partial collection order", {
  crosswalk <- sample_crosswalk()
  # Drop one Price rank so that collection is partially ordered.
  crosswalk$collection_order[5] <- NA
  expect_snapshot(error = TRUE, conjoint_df(sample_data(), crosswalk))
})

test_that("conjoint_df prints a structure header", {
  expect_snapshot(print(sample_conjoint()))
})
