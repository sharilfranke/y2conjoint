#' Plot the distribution of utility scores for one or more collections
#'
#' `utility_dist_chart()` reshapes the level columns of the selected
#' [collection]s into long form and draws each level's per-respondent utility
#' scores as a density ridge. Within an [ordered_collection] (e.g. `Price`),
#' levels keep their defined order; within a plain [collection], levels are
#' ordered ascending by mean utility. Each collection is shaded along its own
#' gradient built from a base `color`, running from a light tint (first level)
#' to the full `color` (last level). When more than one collection is
#' selected, they are drawn as separate facets (one panel per collection).
#'
#' @param cjt A [conjoint_df] (or other [collected_df]) of individual-level
#'   utilities.
#' @param collection A character vector of collection names to include.
#'   Defaults to every collection in `cjt`. You can add multiple collections
#'   and it will make a faceted chart by collection.
#' @param color A base color (anything [grDevices::col2rgb()] accepts), or a
#'   named character vector of one base color per collection (names matching
#'   `collection`) to give each collection its own gradient. Pass a list of colors to give each collection its own color. You must specify which collection gets which color.
#' @param scale The `scale` argument passed to
#'   [ggridges::geom_density_ridges()]; controls how much adjacent ridges
#'   overlap. Defaults to `1.5`.
#'
#' @return A `ggplot` object. Add further layers (e.g. `labs()`, `theme()`) to
#'   customize it further.
#' @examples
#' cjt <- conjoint_df(example_utilities, example_crosswalk)
#' utility_dist_chart(cjt, collection = "Brand", color = "#F28E2B")
#'
#' # One color per collection.
#' utility_dist_chart(
#'   cjt,
#'   collection = c("Brand", "Price"),
#'   color = c(Brand = "#F28E2B", Price = "#4E79A7")
#' )
#' @export
utility_dist_chart <- function(
  cjt,
  collection = NULL,
  color,
  scale = 1.5
) {
  rlang::check_installed(c("ggplot2", "ggridges"))

  collections <- get_collections(cjt)
  collection_names <- purrr::map_chr(collections, \(cl) cl@name)
  collection <- collection %||% collection_names
  check_utility_collection(collection, collection_names)
  check_utility_color(color, collection)

  distributions <- utility_distributions(cjt, collections, collection)
  fill_colors <- attribute_gradient_colors(distributions, color)
  # adjustcolor() drops names, but scale_color_manual() needs them to match
  # scale_fill_manual()'s by-name mapping (an unnamed vector is matched
  # positionally instead, against a training order that need not agree with
  # fill_colors' order once facets are involved).
  line_colors <- stats::setNames(
    grDevices::adjustcolor(fill_colors, alpha.f = 0.4),
    names(fill_colors)
  )

  chart <- ggplot2::ggplot(
    distributions,
    ggplot2::aes(x = .data$utility_score, y = .data$label)
  ) +
    ggridges::geom_density_ridges(
      ggplot2::aes(fill = .data$label, color = .data$label),
      scale = scale,
      alpha = 0.45,
      rel_min_height = 0.005,
      linewidth = 0.7
    ) +
    ggplot2::labs(x = "Utility Score", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::scale_y_discrete(limits = rev)

  if (dplyr::n_distinct(distributions$attribute) > 1) {
    chart <- chart +
      ggplot2::facet_wrap(ggplot2::vars(.data$attribute), scales = "free_y")
  }
  chart
}

# One row per respondent per level, restricted to the selected collections.
# Collections are grouped together (descending by name); within a group, an
# ordered_collection keeps its defined @order, while a plain collection is
# ordered ascending by mean utility.
#' @keywords internal
#' @importFrom rlang .data .env
utility_distributions <- function(cjt, collections, collection) {
  selected <- purrr::keep(collections, \(cl) cl@name %in% collection)
  selected <- selected[order(
    purrr::map_chr(selected, \(cl) cl@name),
    decreasing = TRUE
  )]
  # select() on a conjoint_df errors when it would drop a protected (level or
  # NONE) column; escape that guard since we deliberately keep one attribute's
  # levels at a time.
  cjt <- tibble::as_tibble(cjt)

  by_collection <- purrr::map(selected, \(cl) {
    cjt |>
      dplyr::select(dplyr::all_of(collection_levels(cl))) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "label",
        values_to = "utility_score"
      ) |>
      dplyr::mutate(attribute = cl@name)
  })

  level_order <- purrr::list_c(purrr::map2(
    selected,
    by_collection,
    level_order
  ))

  dplyr::bind_rows(by_collection) |>
    dplyr::mutate(label = factor(.data$label, levels = level_order))
}

# The display order for one collection's levels: its own @order when it is an
# ordered_collection, otherwise ascending by mean utility.
#' @keywords internal
level_order <- function(cl, data) {
  if (is_ordered(cl)) {
    return(cl@order)
  }
  data |>
    dplyr::summarize(
      avg = mean(.data$utility_score, na.rm = TRUE),
      .by = "label"
    ) |>
    dplyr::arrange(.data$avg) |>
    dplyr::pull(.data$label)
}

# A gradient of `length(labels)` colors running from a light tint of `color`
# (first label) to `color` itself (last label), named by `labels` for use with
# scale_*_manual().
#' @keywords internal
gradient_colors <- function(labels, color) {
  ramp <- grDevices::colorRampPalette(c("white", color))(length(labels) + 1)
  stats::setNames(ramp[-1], labels)
}

# One independent gradient_colors() ramp per attribute, combined into a single
# named vector keyed by label (for scale_*_manual()), so each collection shades
# from a light tint to its own base color rather than sharing one ramp across
# every level in `distributions`. `color` is either a single color (used for
# every attribute) or a vector named by attribute.
#' @keywords internal
attribute_gradient_colors <- function(distributions, color) {
  level_attr <- distributions |>
    dplyr::distinct(.data$label, .data$attribute) |>
    dplyr::arrange(as.integer(.data$label))

  split(level_attr$label, level_attr$attribute) |>
    purrr::imap(\(labels, attribute) {
      attribute_color <- if (length(color) == 1) color else color[[attribute]]
      gradient_colors(as.character(labels), attribute_color)
    }) |>
    purrr::list_c()
}

# Fail early if a name in `collection` is not a real collection of `cjt`.
#' @keywords internal
check_utility_collection <- function(
  collection,
  collection_names,
  call = rlang::caller_env()
) {
  if (!is.character(collection)) {
    cli::cli_abort(
      c(
        "x" = "{.arg collection} must be a character vector of collection names.",
        "i" = "You supplied {.obj_type_friendly {collection}}."
      ),
      call = call
    )
  }
  unknown <- setdiff(collection, collection_names)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg collection} names {cli::qty(unknown)}collection{?s} not found in {.arg cjt}.",
        "!" = "Unknown: {.field {unknown}}.",
        "i" = "Available collections: {.field {collection_names}}."
      ),
      call = call
    )
  }
  invisible()
}

# `color` must be a single (unnamed) color, or carry exactly one name per
# selected collection.
#' @keywords internal
check_utility_color <- function(color, collection, call = rlang::caller_env()) {
  if (!is.character(color) || length(color) == 0) {
    cli::cli_abort(
      c(
        "x" = "{.arg color} must be a single color or a named color per collection.",
        "i" = "You supplied {.obj_type_friendly {color}}."
      ),
      call = call
    )
  }
  if (length(color) == 1) {
    return(invisible())
  }
  if (is.null(names(color)) || !setequal(names(color), collection)) {
    cli::cli_abort(
      c(
        "x" = "{.arg color} must be a single color or have one named color per selected collection.",
        "!" = "Named: {.field {names(color) %||% '(none)'}}.",
        "i" = "Selected collections: {.field {collection}}."
      ),
      call = call
    )
  }
  invisible()
}
