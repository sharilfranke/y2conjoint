#' Plot part-worth utilities for one or more collections
#'
#' `partworth_chart()` reshapes the level columns of the selected [collection]s
#' into long form, averages each level's utility across respondents, and
#' centers it on its attribute's average to get the overall level utility. Levels are drawn as a diverging bar chart, one bar per level, shaded
#' by collection. Within an [ordered_collection] (e.g. `Price`), levels keep
#' their defined order; within a plain [collection], levels are ordered
#' ascending by part-worth utility. In every collection, the `absence` level
#' (if any, e.g. `"No camera"`) is always drawn first (at the bottom, once
#' flipped), regardless of its utility. When more than one collection is
#' selected, they are drawn as separate facets (one panel per collection).
#'
#' @param cjt A [conjoint_df] (or other [collected_df]) of individual-level
#'   utilities.
#' @param collection A character vector of collection names to include.
#'   Defaults to every collection in `cjt`. Add multiple collections to make a
#'   faceted chart, one panel per collection.
#' @param color A single color (anything [grDevices::col2rgb()] accepts), or a
#'   named character vector of one color per collection (names matching
#'   `collection`) to give each collection its own color.
#' @param digits Number of digits to round the value label on each bar to.
#'   Defaults to `2`.
#'
#' @return A `ggplot` object. Add further layers (e.g. `labs()`, `theme()`) to
#'   customize it further.
#' @examples
#' cjt <- conjoint_df(example_utilities, example_crosswalk)
#' partworth_chart(cjt, collection = "Brand", color = "#F28E2B")
#'
#' # One color per collection.
#' partworth_chart(
#'   cjt,
#'   collection = c("Battery", "Price", "Brand"),
#'   color = c(Battery = "#F28E2B", Price = "#4E79A7", Brand = "grey")
#' )
#' @export
partworth_chart <- function(cjt, collection = NULL, color, digits = 2) {
  collections <- get_collections(cjt)
  collection_names <- purrr::map_chr(collections, \(cl) cl@name)
  collection <- collection %||% collection_names
  check_utility_collection(collection, collection_names)
  check_utility_color(color, collection)

  partworths <- partworth_utilities(cjt, collections, collection)
  bar_colors <- attribute_colors(collection, color)
  above_zero <- dplyr::filter(partworths, .data$partworth >= 0)
  below_zero <- dplyr::filter(partworths, .data$partworth < 0)
  nudge <- diff(range(partworths$partworth)) * 0.02

  chart <- ggplot2::ggplot(
    partworths,
    ggplot2::aes(x = .data$label, y = .data$partworth)
  ) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$attribute), width = 0.8) +
    ggplot2::geom_text(
      data = above_zero,
      ggplot2::aes(
        label = round(.data$partworth, digits),
        color = .data$attribute
      ),
      hjust = 0,
      nudge_y = nudge
    ) +
    ggplot2::geom_text(
      data = below_zero,
      ggplot2::aes(
        label = round(.data$partworth, digits),
        color = .data$attribute
      ),
      hjust = 1,
      nudge_y = -nudge
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::scale_fill_manual(values = bar_colors) +
    ggplot2::scale_color_manual(values = bar_colors) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::coord_flip()

  if (dplyr::n_distinct(partworths$attribute) > 1) {
    chart <- chart +
      ggplot2::facet_wrap(ggplot2::vars(.data$attribute), scales = "free_y")
  }
  chart
}

# One row per level per selected collection, with each level's utility
# averaged across respondents and centered on its attribute's average. See
# utility_distributions() in utility-dist.R for the sibling implementation
# this mirrors: collections are grouped together (descending by name); within
# a group, an ordered_collection keeps its defined @order, while a plain
# collection is ordered ascending by part-worth utility.
#' @keywords internal
#' @importFrom rlang .data .env
partworth_utilities <- function(cjt, collections, collection) {
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
      dplyr::summarize(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE))
      ) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "label",
        values_to = "mean_utility"
      ) |>
      dplyr::mutate(
        partworth = .data$mean_utility - mean(.data$mean_utility),
        attribute = cl@name
      )
  })

  level_order <- purrr::list_c(purrr::map2(
    selected,
    by_collection,
    partworth_level_order
  ))

  dplyr::bind_rows(by_collection) |>
    dplyr::mutate(label = factor(.data$label, levels = level_order))
}

# The display order for one collection's levels: its own @order when it is an
# ordered_collection (oriented so it runs from most negative to most positive
# part-worth, without resorting individual levels - see orient_order()),
# otherwise ascending by part-worth utility. Either way, an absence level (see
# collection()) is always moved to the front so it draws first (at the
# bottom, once coord_flip()'d), regardless of its utility or its position in
# @order.
#' @keywords internal
partworth_level_order <- function(cl, data) {
  order <- if (is_ordered(cl)) {
    orient_order(cl@order, data)
  } else {
    data |>
      dplyr::arrange(.data$partworth) |>
      dplyr::pull(.data$label)
  }
  absence <- collection_absence(cl)
  if (length(absence) == 1) {
    order <- c(absence, setdiff(order, absence))
  }
  order
}

# Orient an ordered_collection's defined `order` so it runs from most negative
# to most positive part-worth, bottom to top once coord_flip()'d - flipping
# the whole sequence end-to-end when its levels trend the other way (e.g.
# ascending Price usually trends toward lower part-worths), but never
# resorting individual levels out of their defined relative order. Falls back
# to `order` as given when the trend is flat or indeterminate (e.g. a single
# level).
#' @keywords internal
orient_order <- function(order, data) {
  utility <- data$partworth[match(order, data$label)]
  trend <- stats::cor(seq_along(order), utility)
  if (is.na(trend) || trend >= 0) order else rev(order)
}

# `color` expanded to one value per name in `collection`, for
# scale_fill_manual()/scale_color_manual(): a single color is repeated for
# every collection, and a named vector is passed through as-is (
# check_utility_color() has already confirmed it names exactly `collection`).
#' @keywords internal
attribute_colors <- function(collection, color) {
  if (length(color) == 1) {
    return(stats::setNames(rep(color, length(collection)), collection))
  }
  color
}
