#' Define a product specification
#'
#' A `spec` records the levels that make up a single product, grouped by
#' collection. It is built from a flat vector of user-facing level names (any
#' subset is allowed, including selecting several levels from one collection);
#' the levels are validated against, and grouped by, the collections of `x`.
#'
#' @param x A [conjoint_df] or a list of [collection]s to validate against.
#' @param levels A character vector of user-facing level names.
#' @param name An optional single string naming the product.
#'
#' @return A `spec` object.
#' @examples
#' cjt <- conjoint_df(example_utilities, example_crosswalk)
#'
#' # Select one level per collection.
#' spec(cjt, c("Northwind", "$299", "20 hours", "256 GB", "Black"), name = "Flagship")
#'
#' # Selecting several brands models a co-branded product.
#' spec(
#'   cjt,
#'   c("Northwind", "Cascade", "$199", "10 hours", "128 GB", "Blue"),
#'   name = "Co-brand"
#' )
#'
#' # Any subset is allowed; omitted collections warn and contribute no utility.
#' spec(cjt, c("Meridian", "$399"), name = "Sparse")
#' @export
spec <- S7::new_class(
  "spec",
  properties = list(
    name = S7::class_character,
    selections = S7::class_list
  ),
  constructor = function(x, levels, name = character()) {
    S7::new_object(
      S7::S7_object(),
      name = name,
      selections = group_levels(x, levels)
    )
  },
  validator = function(self) {
    validate_spec_fields(self@name, self@selections)
  }
)

#' @keywords internal
validate_spec_fields <- function(name, selections) {
  if (length(name) > 1) {
    return("@name must be a single string or empty")
  }
  if (is.null(names(selections)) || !all(nzchar(names(selections)))) {
    return("@selections must be a named list")
  }
  is_chr <- purrr::map_lgl(selections, is.character)
  if (!all(is_chr)) {
    return("@selections must contain only character vectors")
  }
  NULL
}

#' @keywords internal
as_collections <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "conjoint_df")) {
    return(conjoint_collections(x))
  }
  is_collection <- function(e) S7::S7_inherits(e, collection)
  if (is.list(x) && length(x) > 0 && all(purrr::map_lgl(x, is_collection))) {
    return(x)
  }
  cli::cli_abort(
    c(
      "x" = "{.arg x} must be a {.cls conjoint_df} or a list of collections.",
      "i" = "You supplied {.obj_type_friendly {x}}."
    ),
    call = call
  )
}

#' @keywords internal
group_levels <- function(x, levels, call = rlang::caller_env()) {
  collections <- as_collections(x, call = call)
  collection_names <- purrr::map_chr(collections, \(cl) cl@name)
  level_sets <- purrr::map(collections, collection_levels)

  unknown <- setdiff(levels, purrr::list_c(level_sets))
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "x" = "Unknown level{?s} {.val {unknown}}.",
        "!" = "Every level must belong to a collection of {.arg x}.",
        "i" = "Available levels: {.val {purrr::list_c(level_sets)}}."
      ),
      call = call
    )
  }

  selections <- purrr::map(level_sets, \(lv) levels[levels %in% lv])
  names(selections) <- collection_names

  empty <- collection_names[lengths(selections) == 0]
  if (length(empty) > 0) {
    cli::cli_warn("No levels selected for collection{?s} {.field {empty}}.")
  }
  selections
}

S7::method(print, spec) <- function(x, ...) {
  label <- if (length(x@name) == 1) x@name else "(unnamed)"
  cat(
    cli::cli_fmt({
      cli::cli_text("{.cls spec} {.strong {label}}")
      for (nm in names(x@selections)) {
        chosen <- x@selections[[nm]]
        value <- if (length(chosen) == 0) "\u2014" else chosen
        cli::cli_li("{.field {nm}}: {value}")
      }
    }),
    sep = "\n"
  )
  invisible(x)
}
