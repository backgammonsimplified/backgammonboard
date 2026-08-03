#' Configure movement-overlay appearance
#'
#' Creates an explicit movement-overlay style object for passing to [ggboard()].
#' `ghost_fill = NA` leaves the destination marker interior unfilled.
#'
#' @param ghost_fill Ghost interior colour, or `NA` for no fill.
#' @param ghost_fill_alpha Ghost fill opacity from 0 to 1.
#' @param ghost_outline Ghost outer-ring colour.
#' @param ghost_outline_width Ghost outer-ring line width.
#' @param ghost_dot_colour Ghost grid-dot colour.
#' @param ghost_dot_alpha Ghost grid-dot opacity from 0 to 1.
#' @param ghost_grid_rows,ghost_grid_cols Number of grid rows and columns.
#' @param ghost_dot_size Grid-dot size.
#' @param ghost_grid_inset Distance between the checker edge and grid extent.
#' @param arrow_colour Main arrow colour.
#' @param arrow_alpha Arrow opacity from 0 to 1.
#' @param arrow_width Main shaft and arrowhead-wing line width.
#' @param arrow_lineend Line-cap treatment: `"butt"`, `"round"`, or `"square"`.
#' @param arrow_outline_colour Arrow contrast-outline colour.
#' @param arrow_outline_width Extra outline thickness on each side; zero disables it.
#' @param arrowhead_length Arrowhead length in board-coordinate units.
#' @param arrowhead_width Full arrowhead width in board-coordinate units.
#' @param arrow_checker_gap Signed gap between the arrow tip and destination-marker
#'   edge. Positive values stop outside the marker; negative values move inside it.
#' @param arrow_curve_enabled Whether adaptive conflict-driven curvature is enabled.
#' @param arrow_curve_offset First bend magnitude, as a multiple of checker radius.
#' @param arrow_curve_step Additional bend per offset level, in checker radii.
#' @param arrow_curve_max Maximum absolute bend, in checker radii.
#' @param arrow_curve_length_cap Maximum midpoint displacement as a fraction
#'   of the direct source-to-destination length.
#' @param arrow_chain_full_angle,arrow_chain_moderate_angle,
#'   arrow_chain_max_angle Turn-angle thresholds, in degrees, for chained-hop
#'   curvature scaling.
#' @param arrow_chain_full_multiplier,arrow_chain_moderate_multiplier,
#'   arrow_chain_shallow_multiplier Curvature multipliers for the three chain
#'   angle bands.
#' @param arrow_chain_short_length_radii Segment length, in checker radii, at
#'   or below which the additional short-hop curvature cap applies.
#' @param arrow_chain_short_curve_max Maximum short-hop bow in checker radii.
#' @param arrow_collinear_angle_tolerance Direction tolerance in degrees for
#'   approximately collinear paths.
#' @param arrow_overlap_threshold Minimum projected overlap fraction used to
#'   classify substantially overlapping paths.
#' @param arrow_curve_two_path_split Symmetric multiplier used for a two-arrow
#'   conflict group.
#'
#' @return An object of class `backgammon_movement_overlay_style`.
#' @export
movement_overlay_style <- function(
    ghost_fill = NA,
    ghost_fill_alpha = 0.20,
    ghost_outline = "#000000",
    ghost_outline_width = 1.5,
    ghost_dot_colour = "#65707A",
    ghost_dot_alpha = 1,
    ghost_grid_rows = 7L,
    ghost_grid_cols = 7L,
    ghost_dot_size = 1,
    ghost_grid_inset = 0.15,
    arrow_colour = "#D95F32",
    arrow_alpha = 1,
    arrow_width = 2,
    arrow_lineend = "round",
    arrow_outline_colour = "#000000",
    arrow_outline_width = 0,
    arrowhead_length = 0.18,
    arrowhead_width = 0.12,
    arrow_checker_gap = 0.05,
    arrow_curve_enabled = TRUE,
    arrow_curve_offset = 0.22,
    arrow_curve_step = 0.18,
    arrow_curve_max = 0.65,
    arrow_curve_length_cap = 0.08,
    arrow_chain_full_angle = 6,
    arrow_chain_moderate_angle = 12,
    arrow_chain_max_angle = 20,
    arrow_chain_full_multiplier = 1,
    arrow_chain_moderate_multiplier = 0.60,
    arrow_chain_shallow_multiplier = 0.25,
    arrow_chain_short_length_radii = 2.75,
    arrow_chain_short_curve_max = 0.15,
    arrow_collinear_angle_tolerance = 8,
    arrow_overlap_threshold = 0.40,
    arrow_curve_two_path_split = 0.5
) {
  values <- mget(names(formals(movement_overlay_style)), envir = environment())
  validate_movement_overlay_style(values)
  structure(values, class = "backgammon_movement_overlay_style")
}


validate_movement_overlay_style <- function(values) {
  expected <- names(formals(movement_overlay_style))
  if (!is.list(values) || !identical(names(values), expected)) {
    stop("Invalid movement-overlay style fields.", call. = FALSE)
  }
  color_fields <- c(
    "ghost_outline", "ghost_dot_colour", "arrow_colour",
    "arrow_outline_colour"
  )
  for (name in color_fields) {
    value <- values[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be one colour.", name), call. = FALSE)
    }
    tryCatch(
      grDevices::col2rgb(value),
      error = function(error) stop(sprintf("`%s` must be a valid colour.", name), call. = FALSE)
    )
  }
  if (!(length(values$ghost_fill) == 1L &&
        (is.na(values$ghost_fill) || is.character(values$ghost_fill)))) {
    stop("`ghost_fill` must be one colour or NA.", call. = FALSE)
  }
  if (!is.na(values$ghost_fill)) {
    tryCatch(
      grDevices::col2rgb(values$ghost_fill),
      error = function(error) stop("`ghost_fill` must be a valid colour or NA.", call. = FALSE)
    )
  }
  for (name in c("ghost_fill_alpha", "ghost_dot_alpha", "arrow_alpha")) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0 || value > 1) {
      stop(sprintf("`%s` must be between 0 and 1.", name), call. = FALSE)
    }
  }
  for (name in c(
      "ghost_outline_width", "ghost_dot_size", "arrow_width",
      "arrowhead_length", "arrowhead_width", "arrow_curve_offset",
      "arrow_curve_max", "arrow_curve_length_cap",
      "arrow_chain_full_angle", "arrow_chain_moderate_angle",
      "arrow_chain_max_angle", "arrow_chain_short_length_radii",
      "arrow_chain_short_curve_max",
      "arrow_curve_two_path_split")) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(sprintf("`%s` must be greater than zero.", name), call. = FALSE)
    }
  }
  for (name in c(
      "ghost_grid_inset", "arrow_outline_width", "arrow_curve_step"
  )) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0) {
      stop(sprintf("`%s` must be zero or greater.", name), call. = FALSE)
    }
  }
  if (!is.numeric(values$arrow_checker_gap) ||
      length(values$arrow_checker_gap) != 1L ||
      is.na(values$arrow_checker_gap) || !is.finite(values$arrow_checker_gap)) {
    stop("`arrow_checker_gap` must be one finite number.", call. = FALSE)
  }
  if (!is.logical(values$arrow_curve_enabled) ||
      length(values$arrow_curve_enabled) != 1L ||
      is.na(values$arrow_curve_enabled)) {
    stop("`arrow_curve_enabled` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(values$arrow_collinear_angle_tolerance) ||
      length(values$arrow_collinear_angle_tolerance) != 1L ||
      is.na(values$arrow_collinear_angle_tolerance) ||
      values$arrow_collinear_angle_tolerance <= 0 ||
      values$arrow_collinear_angle_tolerance > 90) {
    stop(
      "`arrow_collinear_angle_tolerance` must be greater than 0 and at most 90 degrees.",
      call. = FALSE
    )
  }
  if (!is.numeric(values$arrow_overlap_threshold) ||
      length(values$arrow_overlap_threshold) != 1L ||
      is.na(values$arrow_overlap_threshold) ||
      values$arrow_overlap_threshold < 0 ||
      values$arrow_overlap_threshold > 1) {
    stop("`arrow_overlap_threshold` must be between 0 and 1.", call. = FALSE)
  }
  chain_angles <- unlist(values[c(
    "arrow_chain_full_angle", "arrow_chain_moderate_angle",
    "arrow_chain_max_angle"
  )], use.names = FALSE)
  if (!isTRUE(all(diff(chain_angles) > 0)) ||
      chain_angles[[3L]] > 180) {
    stop(
      "Chain angle thresholds must be strictly increasing and at most 180 degrees.",
      call. = FALSE
    )
  }
  for (name in c(
      "arrow_chain_full_multiplier", "arrow_chain_moderate_multiplier",
      "arrow_chain_shallow_multiplier")) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value < 0 || value > 1) {
      stop(sprintf("`%s` must be between 0 and 1.", name), call. = FALSE)
    }
  }
  for (name in c("ghost_grid_rows", "ghost_grid_cols")) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value < 2 || value != as.integer(value)) {
      stop(sprintf("`%s` must be an integer of at least 2.", name), call. = FALSE)
    }
  }
  if (!is.character(values$arrow_lineend) ||
      length(values$arrow_lineend) != 1L || is.na(values$arrow_lineend) ||
      !values$arrow_lineend %in% c("butt", "round", "square")) {
    stop("`arrow_lineend` must be `butt`, `round`, or `square`.", call. = FALSE)
  }
  invisible(values)
}


resolve_movement_overlay_style <- function(movement_style, colors, style) {
  if (!is.null(movement_style)) {
    if (!inherits(movement_style, "backgammon_movement_overlay_style")) {
      stop("`movement_style` must be NULL or created by movement_overlay_style().", call. = FALSE)
    }
    validate_movement_overlay_style(unclass(movement_style))
    return(movement_style)
  }
  movement_overlay_style(
    ghost_fill = colors$movement_ghost_fill,
    ghost_fill_alpha = 0.88,
    ghost_outline = colors$movement_ghost_outline,
    ghost_outline_width = 1.25,
    ghost_dot_colour = colors$movement_ghost_pattern,
    ghost_dot_alpha = 1,
    ghost_grid_rows = 7L,
    ghost_grid_cols = 7L,
    ghost_dot_size = 0.62,
    ghost_grid_inset = 0.015,
    arrow_colour = colors$arrow_primary,
    arrow_alpha = 1,
    arrow_width = style$arrow_linewidth,
    arrow_lineend = "round",
    arrow_outline_colour = colors$arrow_halo_dark,
    arrow_outline_width = style$arrow_linewidth *
      (style$arrow_halo_dark_ratio - 1) / 2,
    arrowhead_length = 0.18,
    arrowhead_width = 0.12,
    arrow_checker_gap = 0,
    arrow_curve_enabled = TRUE,
    arrow_curve_offset = 0.22,
    arrow_curve_step = 0.18,
    arrow_curve_max = 0.65,
    arrow_curve_length_cap = 0.08,
    arrow_chain_full_angle = 6,
    arrow_chain_moderate_angle = 12,
    arrow_chain_max_angle = 20,
    arrow_chain_full_multiplier = 1,
    arrow_chain_moderate_multiplier = 0.60,
    arrow_chain_shallow_multiplier = 0.25,
    arrow_chain_short_length_radii = 2.75,
    arrow_chain_short_curve_max = 0.15,
    arrow_collinear_angle_tolerance = 8,
    arrow_overlap_threshold = 0.40,
    arrow_curve_two_path_split = 0.5
  )
}


#' @export
print.backgammon_movement_overlay_style <- function(x, ...) {
  cat("<backgammon_movement_overlay_style>\n")
  print(unclass(x))
  invisible(x)
}
