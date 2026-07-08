test_that("compute_product_utility combines within and sums across collections", {
  cjt <- sample_conjoint()
  s <- spec(cjt, c("Northwind", "Cascade", "$249"))
  expected <- pmax(cjt$Northwind, cjt[["Cascade"]]) + cjt[["$249"]]
  expect_equal(compute_product_utility(cjt, s), expected)
})

test_that("compute_product_utility accepts a per-collection combine_fn", {
  cjt <- sample_conjoint()
  s <- spec(cjt, c("Northwind", "Cascade", "$249"))
  # Sum brand levels, leave price to the pmax fallback.
  expected <- (cjt$Northwind + cjt[["Cascade"]]) + cjt[["$249"]]
  actual <- compute_product_utility(cjt, s, combine_fn = list(Brand = `+`))
  expect_equal(actual, expected)
})

test_that("run_scenario rejects combine_fn naming an unknown collection", {
  cjt <- sample_conjoint()
  cs <- competitive_set(spec(cjt, c("Northwind", "$249"), name = "A"))
  expect_snapshot(
    error = TRUE,
    run_scenario(cjt, cs, combine_fn = list(Colour = pmax))
  )
})

test_that("run_scenario returns named shares that sum to total_share", {
  cjt <- sample_conjoint()
  cs <- competitive_set(
    spec(cjt, c("Northwind", "$249"), name = "A"),
    spec(cjt, c("Cascade", "$299"), name = "B")
  )
  out <- run_scenario(cjt, cs)
  expect_named(out, c("share_A", "share_B", "total_share"))
  expect_equal(out$total_share, out$share_A + out$share_B)
})

test_that("run_scenario names unnamed specs sequentially", {
  cjt <- sample_conjoint()
  cs <- competitive_set(
    spec(cjt, c("Northwind", "$249")),
    spec(cjt, c("Cascade", "$299"))
  )
  out <- run_scenario(cjt, cs)
  expect_named(out, c("share_product_1", "share_product_2", "total_share"))
})

test_that("run_scenario matches a hand-computed softmax", {
  cjt <- sample_conjoint()
  cs <- competitive_set(spec(cjt, c("Northwind", "$249"), name = "A"))
  util_a <- cjt$Northwind + cjt[["$249"]]
  util_none <- cjt$NONE
  row_max <- pmax(util_a, util_none)
  exp_a <- exp(util_a - row_max)
  exp_none <- exp(util_none - row_max)
  share_a <- round(mean(exp_a / (exp_a + exp_none)), 3)
  expect_equal(run_scenario(cjt, cs)$share_A, share_a)
})
