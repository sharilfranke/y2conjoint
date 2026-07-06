# Generates the example datasets shipped with y2conjoint:
#   example_utilities - individual-level part-worth utilities in model format
#   example_crosswalk - maps model-format columns to collections and user names
# Run with: source("data-raw/example.R")

library(tibble)

set.seed(4127)

n <- 25

# Part-worth utilities. Brand levels differ in appeal; higher prices carry lower
# utility; longer battery life carries higher utility.
example_utilities <- tibble(
  respondent_id = seq_len(n),
  A1B1 = round(rnorm(n, 0.4, 0.5), 3), # Brand: Northwind
  A1B2 = round(rnorm(n, 0.1, 0.5), 3), # Brand: Cascade
  A1B3 = round(rnorm(n, -0.2, 0.5), 3), # Brand: Meridian
  A2B1 = round(rnorm(n, 0.5, 0.4), 3), # Price: $199
  A2B2 = round(rnorm(n, 0.0, 0.4), 3), # Price: $299
  A2B3 = round(rnorm(n, -0.5, 0.4), 3), # Price: $399
  A3B1 = round(rnorm(n, -0.2, 0.3), 3), # Battery: 10 hours
  A3B2 = round(rnorm(n, 0.3, 0.3), 3), # Battery: 20 hours
  NONE = round(rnorm(n, -0.3, 0.5), 3),
  age = sample(22:70, n, replace = TRUE),
  gender = sample(
    c("Female", "Male", "Nonbinary"),
    n,
    replace = TRUE,
    prob = c(0.48, 0.48, 0.04)
  ),
  region = sample(
    c("Northeast", "Midwest", "South", "West"),
    n,
    replace = TRUE
  ),
  income = round(rnorm(n, 65000, 20000) / 1000) * 1000
)

example_crosswalk <- tibble(
  old_name = c(
    "A1B1",
    "A1B2",
    "A1B3",
    "A2B1",
    "A2B2",
    "A2B3",
    "A3B1",
    "A3B2"
  ),
  user_name = c(
    "Northwind",
    "Cascade",
    "Meridian",
    "$199",
    "$299",
    "$399",
    "10 hours",
    "20 hours"
  ),
  collection_name = c(
    "Brand",
    "Brand",
    "Brand",
    "Price",
    "Price",
    "Price",
    "Battery",
    "Battery"
  ),
  collection_order = c(NA, NA, NA, 1, 2, 3, 1, 2)
)

usethis::use_data(example_utilities, example_crosswalk, overwrite = TRUE)
