#' Collections of conjoint levels
#'
#' A collection groups the columns of a [conjoint_df] that belong to a single
#' conjoint attribute (e.g. all `Brand` levels). An `ordered_collection` is a
#' collection whose levels have a meaningful order; the order is recorded by the
#' order of `levels` (there is no enforcement of that order elsewhere yet).
#'
#' @param name A single string naming the collection.
#' @param levels A character vector of the level (column) names that make up the
#'   collection.
#' @param order For `ordered_collection`, a character vector giving the levels
#'   in their intended order. It must contain exactly the same levels as
#'   `levels`. Defaults to `unique(levels)`, i.e. the order in which `levels`
#'   were supplied.
#'
#' @return A `collection` or `ordered_collection` object.
#' @examples
#' collection(name = "Brand", levels = c("Northwind", "Cascade"))
#'
#' # By default the level order is taken from `levels`.
#' ordered_collection(name = "Price", levels = c("$199", "$299", "$399"))
#'
#' # Supply `order` to rank levels independently of how they were listed.
#' ordered_collection(
#'   name = "Price",
#'   levels = c("$399", "$199", "$299"),
#'   order = c("$199", "$299", "$399")
#' )
#' @export
collection <- S7::new_class(
  "collection",
  properties = list(
    name = S7::class_character,
    levels = S7::class_character
  ),
  validator = function(self) {
    validate_collection_fields(self@name, self@levels)
  }
)

#' @rdname collection
#' @export
ordered_collection <- S7::new_class(
  "ordered_collection",
  parent = collection,
  properties = list(
    order = S7::class_character
  ),
  constructor = function(name, levels, order = unique(levels)) {
    S7::new_object(
      collection(name = name, levels = levels),
      order = order
    )
  },
  validator = function(self) {
    validate_order(self@order, self@levels)
  }
)

#' @keywords internal
validate_order <- function(order, levels) {
  if (length(order) != length(levels) || !setequal(order, levels)) {
    return("@order must contain exactly the same levels as @levels")
  }
  NULL
}

#' @keywords internal
validate_collection_fields <- function(name, levels) {
  if (length(name) != 1 || is.na(name) || !nzchar(name)) {
    return("@name must be a single non-empty string")
  }
  if (length(levels) == 0) {
    return("@levels must contain at least one level")
  }
  if (anyNA(levels) || !all(nzchar(levels))) {
    return("@levels must not contain missing or empty strings")
  }
  if (anyDuplicated(levels)) {
    return("@levels must be unique")
  }
  NULL
}

#' Is a collection ordered?
#'
#' @param x A [collection] object.
#' @return A single logical.
#' @export
is_ordered <- function(x) {
  S7::S7_inherits(x, ordered_collection)
}

#' @keywords internal
collection_levels <- function(x) {
  x@levels
}

S7::method(print, collection) <- function(x, ...) {
  ordered <- if (is_ordered(x)) " (ordered)" else ""
  levels <- if (is_ordered(x)) x@order else x@levels
  cat(
    cli::cli_fmt({
      cli::cli_text("{.cls collection} {.strong {x@name}}{ordered}")
      cli::cli_ul(levels)
    }),
    sep = "\n"
  )
  invisible(x)
}
