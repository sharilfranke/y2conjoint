#' Compute the total utility of a product
#'
#' A product is described by a [spec], which selects one or more levels from each
#' collection (attribute). This function turns that description into a single
#' utility per respondent by:
#' 1. combining the selected levels *within* each collection into one value
#'    (e.g. [pmax()] takes the best available level, which is how co-branded
#'    products that list two brands are handled), and
#' 2. summing those per-collection values *across* collections.
#'
#' @param x A [conjoint_df].
#' @param spec A [spec].
#' @param combine_fn How to combine multiple selected levels within a collection.
#'   Either a single function applied to every collection (the default,
#'   [pmax()]), or a named list mapping collection names to functions. Any
#'   collection absent from the list falls back to [pmax()].
#'
#' @return A numeric vector, one utility per row of `x`.
#' @export
compute_product_utility <- function(x, spec, combine_fn = pmax) {
  # Keep only the collections this product actually selects a level from.
  selections <- purrr::keep(spec@selections, \(levels) length(levels) > 0)

  # A product that selects nothing has zero utility for everyone.
  if (length(selections) == 0) {
    return(rep(0, nrow(x)))
  }

  # For each collection, combine its selected level columns into one utility
  # vector, then sum those vectors to get the product's total utility.
  per_collection <- purrr::imap(selections, function(levels, collection) {
    combine <- resolve_combine_fn(combine_fn, collection)
    level_utilities <- purrr::map(levels, \(level) x[[level]])
    rlang::inject(combine(!!!level_utilities))
  })
  purrr::reduce(per_collection, `+`)
}

#' Estimate preference shares for a competitive set
#'
#' Applies a logit (softmax) choice model: each respondent's utility for every
#' product is turned into a probability of choosing it, and those probabilities
#' are averaged across respondents to give each product's mean preference share.
#' The NONE outside good competes for share as a "choose nothing" option.
#'
#' @param x A [conjoint_df] of individual-level utilities.
#' @param competitive_set A [competitive_set].
#' @param combine_fn How to combine multiple selected levels within a collection.
#'   Either a single function applied to every collection (the default,
#'   [pmax()]), or a named list mapping collection names to functions. Any
#'   collection absent from the list falls back to [pmax()].
#' @param scaling_factor A numeric multiplier applied to all utilities before the
#'   softmax. Values above 1 sharpen the choice probabilities toward the highest
#'   utility; values below 1 flatten them. Defaults to `1`.
#'
#' @return A one-row tibble of `share_*` columns plus `total_share`.
#' @export
run_scenario <- function(
  x,
  competitive_set,
  combine_fn = pmax,
  scaling_factor = 1
) {
  specs <- competitive_set@specs
  check_specs_columns(x, specs)
  check_combine_fn(combine_fn, x)
  # From here the inputs are validated, so the rest is pure computation.

  # One utility column per product (rows = respondents).
  product_utilities <- purrr::map(
    specs,
    \(spec) compute_product_utility(x, spec, combine_fn) * scaling_factor
  )
  names(product_utilities) <- spec_output_names(specs)

  # Add NONE as one more column so the "choose nothing" option competes with
  # the products for share.
  product_utilities[["none"]] <- x[[conjoint_none(x)]] * scaling_factor

  # Convert utilities to per-respondent choice probabilities, then average
  # across respondents to get each option's mean share.
  utilities <- do.call(cbind, product_utilities)
  shares <- colMeans(softmax_rows(utilities))

  format_shares(shares)
}

# Softmax: convert a matrix of utilities into row-wise choice probabilities via
# the logit rule P(i) = exp(u_i) / sum_j exp(u_j).
#' @keywords internal
softmax_rows <- function(m) {
  # Subtract each row's largest utility before exponentiating. Large utilities
  # would overflow exp() to Inf; subtracting a per-row constant prevents that
  # and cancels in the ratio, so the probabilities are unchanged.
  row_max <- as.numeric(purrr::reduce(asplit(m, 2), pmax))
  weights <- exp(m - row_max)
  weights / rowSums(weights)
}

# Drop the outside good and return a one-row tibble of rounded product shares
# plus their total (the share of the market the products capture together).
#' @keywords internal
format_shares <- function(shares) {
  shares <- round(shares, 3)
  products <- shares[names(shares) != "none"]

  out <- tibble::as_tibble(as.list(products))
  names(out) <- paste0("share_", names(products))
  out$total_share <- sum(products)
  out
}

# Resolve the combine function for a single collection. `combine_fn` is either
# one function for every collection, or a named list keyed by collection name.
#' @keywords internal
resolve_combine_fn <- function(combine_fn, collection) {
  if (is.function(combine_fn)) {
    return(combine_fn)
  }
  fn <- combine_fn[[collection]]
  if (is.null(fn)) pmax else fn
}

# Name each product's output column after its spec, falling back to a positional
# name (product_1, product_2, ...) for unnamed specs.
#' @keywords internal
spec_output_names <- function(specs) {
  purrr::imap_chr(specs, function(spec, i) {
    nm <- spec@name
    if (length(nm) == 1 && nzchar(nm)) nm else paste0("product_", i)
  })
}

# Fail early if any spec references a level that is not a column of `x`.
#' @keywords internal
check_specs_columns <- function(x, specs, call = rlang::caller_env()) {
  used <- unique(purrr::list_c(purrr::map(specs, \(spec) {
    purrr::list_c(spec@selections)
  })))
  missing <- setdiff(used, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "x" = "The competitive set references {cli::qty(missing)}level{?s} that {?is/are} not in {.arg x}.",
        "!" = "Missing from {.arg x}: {.field {missing}}."
      ),
      call = call
    )
  }
}

# Validate the combine_fn argument: either a function, or a named list of
# functions whose names are all real collections of `x` (catches typos).
#' @keywords internal
check_combine_fn <- function(combine_fn, x, call = rlang::caller_env()) {
  if (is.function(combine_fn)) {
    return(invisible())
  }
  if (!is.list(combine_fn) || !is_fully_named(combine_fn)) {
    cli::cli_abort(
      c(
        "x" = "{.arg combine_fn} must be a function or a named list of functions.",
        "i" = "Name each element after the collection it combines."
      ),
      call = call
    )
  }
  not_function <- names(combine_fn)[!purrr::map_lgl(combine_fn, is.function)]
  if (length(not_function) > 0) {
    cli::cli_abort(
      c(
        "x" = "Every element of {.arg combine_fn} must be a function.",
        "!" = "Not {cli::qty(not_function)}{?a function/functions}: {.field {not_function}}."
      ),
      call = call
    )
  }
  known <- purrr::map_chr(conjoint_collections(x), \(collection) {
    collection@name
  })
  unknown <- setdiff(names(combine_fn), known)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg combine_fn} names {cli::qty(unknown)}collection{?s} not found in {.arg x}.",
        "!" = "Unknown: {.field {unknown}}."
      ),
      call = call
    )
  }
  invisible()
}

#' @keywords internal
is_fully_named <- function(x) {
  !is.null(names(x)) && all(nzchar(names(x)))
}
