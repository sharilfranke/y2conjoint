test_that("filter preserves the conjoint_df class and metadata", {
  cjt <- sample_conjoint()
  out <- dplyr::filter(cjt, age > 28)
  expect_s3_class(out, "conjoint_df")
  expect_equal(nrow(out), sum(cjt$age > 28))
  expect_setequal(protected_cols(out), protected_cols(cjt))
})

test_that("mutating a demographic column is allowed", {
  cjt <- sample_conjoint()
  out <- dplyr::mutate(cjt, age2 = age * 2)
  expect_s3_class(out, "conjoint_df")
  expect_equal(out$age2, cjt$age * 2)
})

test_that("selecting demographics alongside all protected columns is allowed", {
  cjt <- sample_conjoint()
  out <- dplyr::select(cjt, dplyr::all_of(protected_cols(cjt)), age)
  expect_s3_class(out, "conjoint_df")
})

test_that("renaming a demographic column is allowed", {
  cjt <- sample_conjoint()
  out <- dplyr::rename(cjt, respondent = respondent_id)
  expect_s3_class(out, "conjoint_df")
  expect_contains(names(out), "respondent")
})

test_that("overwriting a protected column errors", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, dplyr::mutate(cjt, Northwind = 0))
})

test_that("dropping a protected column via select errors", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, dplyr::select(cjt, age))
})

test_that("renaming a protected column errors", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, dplyr::rename(cjt, brand = Northwind))
})

test_that("errors are attributed to the dplyr verb, not internal helpers", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, dplyr::mutate(cjt, NONE = 0))
  expect_snapshot(error = TRUE, dplyr::rename_with(cjt, toupper))
})

test_that("transmute is unsupported and suggests alternatives", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, dplyr::transmute(cjt, age))
})

test_that("base assignment guards protected columns", {
  cjt <- sample_conjoint()
  expect_snapshot(error = TRUE, cjt$Northwind <- 0)
  expect_snapshot(error = TRUE, cjt[["NONE"]] <- 1)
})
