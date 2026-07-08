#' Example conjoint utilities
#'
#' A small, simulated set of individual-level part-worth utilities in the model
#' format (`A[NUM]B[NUM]`), used throughout the examples and the getting-started
#' article. Pass it together with [example_crosswalk] to [conjoint_df()].
#'
#' @format A tibble with 200 rows and 21 columns:
#' \describe{
#'   \item{respondent_id}{Respondent identifier.}
#'   \item{A1B1, A1B2, A1B3}{Utilities for the three Brand levels.}
#'   \item{A2B1, A2B2, A2B3}{Utilities for the three Price levels.}
#'   \item{A3B1, A3B2, A3B3}{Utilities for the three Battery levels.}
#'   \item{A4B1, A4B2, A4B3}{Utilities for the three Storage levels.}
#'   \item{A5B1, A5B2, A5B3}{Utilities for the three Color levels.}
#'   \item{NONE}{Utility of the outside good (choosing nothing).}
#'   \item{age}{Respondent age in years.}
#'   \item{gender}{Respondent gender.}
#'   \item{region}{U.S. census region.}
#'   \item{income}{Respondent annual income in dollars.}
#' }
#' @seealso [example_crosswalk]
"example_utilities"

#' Example conjoint crosswalk
#'
#' The crosswalk that maps the model-format columns of [example_utilities] to
#' their collections and user-facing level names.
#'
#' @format A tibble with 15 rows and 4 columns:
#' \describe{
#'   \item{old_name}{Model-format column name (`A[NUM]B[NUM]`).}
#'   \item{user_name}{User-facing level name.}
#'   \item{collection_name}{Collection (attribute) the level belongs to.}
#'   \item{collection_order}{Integer rank for ordered collections, `NA`
#'     otherwise.}
#' }
#' @seealso [example_utilities]
"example_crosswalk"
