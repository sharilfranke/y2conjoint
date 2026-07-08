#' Create a conjoint data frame
#'
#' `conjoint_df()` builds a tibble subclass that carries its conjoint structure
#' as metadata. Utility columns arrive in the model format (`A[NUM]B[NUM]`) and
#' are renamed to their user-facing names; they are grouped into [collection]s
#' via the supplied `crosswalk`. Any columns not described by the crosswalk
#' (e.g. demographics or IDs) are kept as-is.
#'
#' The level columns and the `none` column are *protected*: dplyr and base
#' operations may freely edit other columns, but attempts to drop, rename, or
#' overwrite a protected column error. Convert with [tibble::as_tibble()] first
#' to escape those guards.
#'
#' @param data A data frame of individual-level utilities. Level columns are
#'   named in the model format `A[NUM]B[NUM]`; extra columns are allowed. It is
#'   coerced to a tibble internally.
#' @param crosswalk A data frame mapping levels to collections, with columns
#'   `old_name` (the model-format column), `user_name` (the renamed column),
#'   `collection_name`, and `collection_order` (an integer rank, or `NA` for
#'   unordered collections). It is coerced to a tibble internally, which gives
#'   stricter column access (no partial matching) than a base data frame.
#' @param none The name of the outside-good column. Defaults to `"NONE"`.
#'
#' @return A `conjoint_df`.
#' @examples
#' conjoint_df(example_utilities, example_crosswalk)
#' @export
conjoint_df <- function(data, crosswalk, none = "NONE") {
  data <- tibble::as_tibble(data)
  crosswalk <- tibble::as_tibble(crosswalk)

  validate_conjoint_input(data, crosswalk, none)

  data <- rename_to_user_names(data, crosswalk)
  collections <- build_collections(crosswalk)
  new_conjoint_df(data, collections = collections, none = none)
}

#' @keywords internal
new_conjoint_df <- function(data, collections, none) {
  tibble::new_tibble(
    data,
    collections = collections,
    none = none,
    nrow = nrow(data),
    class = "conjoint_df"
  )
}

#' @keywords internal
crosswalk_columns <- function() {
  c("old_name", "user_name", "collection_name", "collection_order")
}

#' @keywords internal
validate_conjoint_input <- function(
  data,
  crosswalk,
  none,
  call = rlang::caller_env()
) {
  missing_cols <- setdiff(crosswalk_columns(), names(crosswalk))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg crosswalk} is missing required column{?s} {.field {missing_cols}}.",
        "i" = "A crosswalk needs {.field {crosswalk_columns()}}."
      ),
      call = call
    )
  }
  validate_crosswalk_types(crosswalk, call = call)
  validate_collection_orders(crosswalk, call = call)
  missing_levels <- setdiff(crosswalk$old_name, names(data))
  if (length(missing_levels) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg crosswalk} maps {cli::qty(missing_levels)}column{?s} {.field {missing_levels}} that {?is/are} not in {.arg data}.",
        "!" = "Every {.field old_name} must name a column of {.arg data}."
      ),
      call = call
    )
  }
  duplicates <- unique(crosswalk$user_name[duplicated(crosswalk$user_name)])
  if (length(duplicates) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.field user_name} values in {.arg crosswalk} must be unique.",
        "!" = "Duplicated: {.field {duplicates}}."
      ),
      call = call
    )
  }
  if (!none %in% names(data)) {
    cli::cli_abort(
      c(
        "x" = "{.arg none} column {.field {none}} is not in {.arg data}.",
        "i" = "Set {.arg none} to the name of your outside-good column."
      ),
      call = call
    )
  }
  invisible()
}

#' @keywords internal
validate_crosswalk_types <- function(crosswalk, call = rlang::caller_env()) {
  text_cols <- c("old_name", "user_name", "collection_name")
  is_text <- purrr::map_lgl(crosswalk[text_cols], is.character)
  if (!all(is_text)) {
    cli::cli_abort(
      c(
        "x" = "Column{?s} {.field {text_cols[!is_text]}} in {.arg crosswalk} must be character."
      ),
      call = call
    )
  }
  order <- crosswalk$collection_order
  if (!is.numeric(order) && !all(is.na(order))) {
    cli::cli_abort(
      c(
        "x" = "{.field collection_order} in {.arg crosswalk} must be numeric or {.val {NA}}.",
        "i" = "Use integer ranks for ordered collections and {.val {NA}} otherwise."
      ),
      call = call
    )
  }
}

#' @keywords internal
validate_collection_orders <- function(crosswalk, call = rlang::caller_env()) {
  orders <- split(crosswalk$collection_order, crosswalk$collection_name)
  # A collection is either fully unordered (all NA) or fully ranked. A mix means
  # some levels were left without a rank.
  is_partial <- purrr::map_lgl(orders, \(order) {
    anyNA(order) && !all(is.na(order))
  })
  partial <- names(orders)[is_partial]
  if (length(partial) > 0) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(partial)}Collection{?s} {.field {partial}} {?has/have} a partial {.field collection_order}.",
        "!" = "Ordered collections need a rank for every level.",
        "i" = "Give every level an order, or set them all to {.val {NA}}."
      ),
      call = call
    )
  }
}

#' @keywords internal
rename_to_user_names <- function(data, crosswalk) {
  match_idx <- match(crosswalk$old_name, names(data))
  names(data)[match_idx] <- crosswalk$user_name
  data
}

#' @keywords internal
build_collections <- function(crosswalk) {
  collection_names <- unique(crosswalk$collection_name)
  purrr::map(collection_names, \(nm) {
    build_one_collection(crosswalk[crosswalk$collection_name == nm, ])
  })
}

# Assumes validate_collection_orders() has already run, so the order is either
# all NA (unordered) or fully specified (ordered).
#' @keywords internal
build_one_collection <- function(rows) {
  name <- rows$collection_name[[1]]
  order <- rows$collection_order
  if (all(is.na(order))) {
    return(collection(name = name, levels = rows$user_name))
  }
  ordered_collection(name = name, levels = rows$user_name[order(order)])
}

#' @keywords internal
conjoint_collections <- function(x) {
  attr(x, "collections")
}

#' @keywords internal
conjoint_none <- function(x) {
  attr(x, "none")
}

#' Protected columns of a conjoint data frame
#'
#' The level columns of every collection plus the `none` column. These columns
#' cannot be dropped, renamed, or overwritten in place.
#'
#' @param x A [conjoint_df].
#' @return A character vector of column names.
#' @export
protected_cols <- function(x) {
  levels <- purrr::list_c(purrr::map(
    conjoint_collections(x),
    collection_levels
  ))
  unique(c(levels, conjoint_none(x)))
}

# Registered as the print method for conjoint_df in .onLoad(). We register
# manually (rather than via @export) because S7::methods_register() drops plain
# S3 methods on the print generic once S7 owns other print methods.
print_conjoint_df <- function(x, ...) {
  collections <- conjoint_collections(x)
  names <- purrr::map_chr(collections, \(cl) cl@name)
  ordered <- purrr::map_lgl(collections, is_ordered)
  labels <- names
  labels[ordered] <- paste0(names[ordered], "*")
  n_extra <- ncol(x) - length(protected_cols(x))

  cat(
    cli::cli_fmt({
      cli::cli_text(
        "{.cls conjoint_df}: {length(collections)} collection{?s}"
      )
      cli::cli_text("Collections: {.field {labels}}")
      cli::cli_text(
        "(* = ordered), NONE = {.field {conjoint_none(x)}}, {n_extra} extra column{?s}"
      )
    }),
    sep = "\n"
  )
  print(unclass_conjoint(x), ...)
  invisible(x)
}

#' @keywords internal
unclass_conjoint <- function(x) {
  class(x) <- setdiff(class(x), "conjoint_df")
  x
}
