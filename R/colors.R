#' Construct a board color preset
#'
#' @param name Preset name. Supported values are `"default"` and `"bms"`.
#' @param overrides Optional named list of color overrides.
#'
#' @return An object of class `backgammon_board_colors`.
#' @export
board_colors <- function(name = "default", overrides = NULL) {
  presets <- board_color_presets()

  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a length-1 character value.", call. = FALSE)
  }

  if (!name %in% names(presets)) {
    stop(
      paste0(
        "Unknown board color preset: `", name,
        "`. Supported presets: ",
        paste(names(presets), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  values <- presets[[name]]

  if (!is.null(overrides)) {
    validate_named_overrides(overrides)

    unknown <- setdiff(names(overrides), names(values))
    if (length(unknown) > 0L) {
      stop(
        paste0(
          "Unknown color override(s): ",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    values[names(overrides)] <- overrides
  }

  validate_board_colors(values)

  structure(
    values,
    preset_id = name,
    class = "backgammon_board_colors"
  )
}

board_color_presets <- function() {
  default <- list(
    # Outside canvas
    outside_fill = "#FFFFFF",

    # Board frame and playing surface
    frame_fill = "#D6CCBE",
    frame_border = "#1B1F2A",
    side_panel_fill = "#F8F4ED",
    field_fill = "#F8F4ED",
    bar_fill = "#CEC2B0",

    # Points
    light_point_fill = "#E4D8C8",
    dark_point_fill = "#7A5B46",
    point_border = "#1B1F2A",
    point_number = "#1B1F2A",

    # White checkers
    white_checker_fill = "#F8F4ED",
    white_checker_ring = "#1B1F2A",
    white_checker_outer_ring = "#1B1F2A",
    white_checker_text = "#1B1F2A",

    # Black checkers
    black_checker_fill = "#11182F",
    black_checker_ring = "#CED6DD",
    black_checker_outer_ring = "#233248",
    black_checker_text = "#F8F4ED",

    # Count badges
    count_badge_fill = "#11182F",
    count_badge_text = "#F8F4ED",

    # White dice
    die_white_fill = "#F8F4ED",
    die_white_pips = "#1B1F2A",
    die_white_border = "#1B1F2A",

    # Black dice
    die_black_fill = "#11182F",
    die_black_pips = "#F8F4ED",
    die_black_border = "#F8F4ED",

    # Cube
    cube_face = "#F8F4ED",
    cube_text = "#1B1F2A",
    cube_border = "#1B1F2A",
    cube_offered_border = "#1B1F2A",
    cube_crosshair = "#D9653B",

    # Arrows and overlays
    arrow_primary = "#D9653B",
    arrow_secondary = "#6E557A",
    on_roll_arrow = "#D9653B",

    # Text
    score_text = "#1B1F2A",
    secondary_text = "#59606A",
    status_text = "#1B1F2A"
  )

  bms <- default
  bms_overrides <- list(
    frame_fill = "#D8C5A5",
    frame_border = "#0B1328",
    side_panel_fill = "#FFFFFF",
    field_fill = "#F6EBDD",
    bar_fill = "#D8C5A5",
    light_point_fill = "#9EB2D2",
    dark_point_fill = "#C29A6B",
    point_border = "#111B35",
    point_number = "#111B35",
    white_checker_fill = "#F8EEDD",
    white_checker_ring = "#111B35",
    white_checker_outer_ring = "#081126",
    white_checker_text = "#111B35",
    black_checker_fill = "#111B35",
    black_checker_ring = "#111B35",
    black_checker_outer_ring = "#081126",
    black_checker_text = "#F8EEDD",
    die_white_fill = "#FFFFFF",
    die_white_pips = "#111B35",
    die_white_border = "#111B35",
    die_black_fill = "#111B35",
    die_black_pips = "#FFFDF8",
    die_black_border = "#081126",
    cube_face = "#FFFFFF",
    cube_text = "#111B35",
    cube_border = "#111B35",
    cube_offered_border = "#111B35",
    cube_crosshair = "#D9653B",
    score_text = "#111B35",
    secondary_text = "#566078",
    status_text = "#111B35"
  )
  bms[names(bms_overrides)] <- bms_overrides

  list(default = default, bms = bms)
}

validate_named_overrides <- function(overrides) {
  if (
    !is.list(overrides) ||
    is.null(names(overrides)) ||
    any(names(overrides) == "")
  ) {
    stop("`overrides` must be a named list.", call. = FALSE)
  }

  invisible(overrides)
}

validate_board_colors <- function(values) {
  valid <- vapply(values, is_valid_color, logical(1))

  if (!all(valid)) {
    bad <- names(values)[!valid]
    stop(
      paste0(
        "Invalid color value(s) for: ",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(values)
}

is_valid_color <- function(x) {
  if (
    !is.character(x) ||
    length(x) != 1L ||
    is.na(x) ||
    !nzchar(x)
  ) {
    return(FALSE)
  }

  result <- try(grDevices::col2rgb(x), silent = TRUE)
  !inherits(result, "try-error")
}

#' @export
print.backgammon_board_colors <- function(x, ...) {
  cat("<backgammon_board_colors>\n")
  cat("  Preset:", attr(x, "preset_id"), "\n")

  for (name in names(x)) {
    cat("  ", name, ": ", x[[name]], "\n", sep = "")
  }

  invisible(x)
}
