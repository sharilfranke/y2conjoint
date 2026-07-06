#' Bundle specifications into a competitive set
#'
#' A `competitive_set` is an ordered list of [spec]s that compete for share in a
#' scenario. All specs must reference the same set of collections.
#'
#' @param specs A list of [spec] objects.
#' @param name An optional single string naming the set.
#'
#' @return A `competitive_set` object.
#' @examples
#' cjt <- conjoint_df(example_utilities, example_crosswalk)
#' competitive_set(list(
#'   spec(cjt, c("Northwind", "$299", "20 hours"), name = "Flagship"),
#'   spec(cjt, c("Cascade", "$199", "10 hours"), name = "Budget")
#' ))
#' @export
competitive_set <- S7::new_class(
  "competitive_set",
  properties = list(
    name = S7::class_character,
    specs = S7::class_list
  ),
  constructor = function(specs, name = character()) {
    S7::new_object(S7::S7_object(), name = name, specs = specs)
  },
  validator = function(self) {
    validate_competitive_set(self@name, self@specs)
  }
)

#' @keywords internal
validate_competitive_set <- function(name, specs) {
  if (length(name) > 1) {
    return("@name must be a single string or empty")
  }
  if (length(specs) == 0) {
    return("@specs must contain at least one spec")
  }
  is_spec <- purrr::map_lgl(specs, \(s) S7::S7_inherits(s, spec))
  if (!all(is_spec)) {
    return("@specs must contain only spec objects")
  }
  key_sets <- purrr::map(specs, \(s) sort(names(s@selections)))
  if (length(unique(key_sets)) > 1) {
    return("all specs must reference the same collections")
  }
  NULL
}

S7::method(print, competitive_set) <- function(x, ...) {
  label <- if (length(x@name) == 1) x@name else "(unnamed)"
  cat(
    cli::cli_fmt({
      cli::cli_text(
        "{.cls competitive_set} {.strong {label}}: {length(x@specs)} spec{?s}"
      )
      for (s in x@specs) {
        nm <- if (length(s@name) == 1) s@name else "(unnamed)"
        cli::cli_li("{nm}")
      }
    }),
    sep = "\n"
  )
  invisible(x)
}
