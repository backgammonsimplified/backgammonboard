#' Construct a board style preset
#'
#' @param name Preset name. Supported values are `"default"` and `"bms"`.
#' @param overrides Optional named list of style overrides.
#'
#' @return An object of class `backgammon_board_style`.
#' @export
board_style <- function(name = "default", overrides = NULL) {
  presets <- board_style_presets()

  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a length-1 character value.", call. = FALSE)
  }

  if (!name %in% names(presets)) {
    stop(
      paste0(
        "Unknown board style preset: `", name,
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
          "Unknown style override(s): ",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    values[names(overrides)] <- overrides
  }

  validate_board_style(values)

  structure(
    values,
    preset_id = name,
    class = "backgammon_board_style"
  )
}

board_style_presets <- function() {
  default <- list(
    board_width = 17.0,
    board_height = 12.0,
    left_margin_width = 1.2,
    right_margin_width = 1.2,
    rail_size = 0.9,
    play_half_width = 5.9,
    bar_width = 1.0,
    point_gap = 0.03,
    point_tip_gap = 1.2,
    checker_outer_ring_width = 0.028,
    checker_outer_radius = 0.37,
    checker_face_radius = 0.31,
    checker_stack_step = 0.69,
    checker_margin = 0.12,
    max_stack_visible = 5L,
    point_number_inset = 0.45,
    point_number_size = 3.8,
    count_badge_size = 3.0,
    off_marker_outer_radius = 0.37,
    off_marker_face_radius = 0.31,
    off_marker_top_y = 10.45,
    off_marker_bottom_y = 1.55,
    board_border_width = 0.9,
    field_border_width = 0.45,
    point_border_width = 0.25,
    die_scale = 1.0,
    die_gap = 0.20,
    die_border_width = 0.3,
    cube_scale = 1.0,
    cube_inner_scale = 0.76,
    cube_text_size = 4.0,
    cube_border_width = 0.3,
    cube_outside_gap = 0.08,
    cube_crosshair_length = 0.42,
    cube_crosshair_linewidth = 0.45,
    cube_crosshair_alpha = 0.80,
    arrow_linewidth = 1.0,
    arrow_curvature = 0.15,
    move_label_size = 4.0,
    score_text_size = 4.0,
    status_text_size = 3.5,
    information_player_name_size = 5.6,
    information_secondary_text_size = 4.5,
    information_pip_text_size = 5.0,
    information_sentence_text_size = 4.7,
    information_top_band_height = 1.10,
    information_bottom_band_height = 1.62,
    information_pip_offset = 0.42,
    information_top_player_name_offset = 0.86,
    information_top_secondary_offset = 0.42,
    information_bottom_secondary_offset = 0.35,
    information_bottom_player_name_offset = 0.82,
    information_sentence_offset = 1.18,
    information_on_roll_arrow_x_offset = 1.45,
    information_on_roll_arrow_size = 6.0,
    information_player_x_nudge = -0.15,
    information_sentence_x_nudge = 0.00
  )

  bms <- default
  bms$checker_stack_step <- 0.78
  bms$checker_margin <- 0.025
  bms$die_scale <- 1.00
  bms$die_gap <- 0.30
  bms$die_border_width <- 0.80
  bms$cube_scale <- 1.05
  bms$cube_inner_scale <- 0.76
  bms$cube_text_size <- 3.8
  bms$cube_border_width <- 0.3
  bms$cube_outside_gap <- 0.08
  bms$cube_crosshair_length <- 0.42
  bms$cube_crosshair_linewidth <- 0.45
  bms$cube_crosshair_alpha <- 0.80
  bms$information_player_name_size <- 5.6
  bms$information_secondary_text_size <- 4.5
  bms$information_pip_text_size <- 5.0
  bms$information_sentence_text_size <- 4.7
  bms$information_top_band_height <- 1.10
  bms$information_bottom_band_height <- 1.62
  bms$information_pip_offset <- 0.42
  bms$information_top_player_name_offset <- 0.86
  bms$information_top_secondary_offset <- 0.42
  bms$information_bottom_secondary_offset <- 0.35
  bms$information_bottom_player_name_offset <- 0.82
  bms$information_sentence_offset <- 1.18
  bms$information_on_roll_arrow_x_offset <- 1.45
  bms$information_on_roll_arrow_size <- 6.0
  bms$information_player_x_nudge <- -0.15
  bms$information_sentence_x_nudge <- 0.00

  list(default = default, bms = bms)
}

validate_board_style <- function(values) {
  positive_scalars <- c(
    "board_width",
    "board_height",
    "left_margin_width",
    "right_margin_width",
    "rail_size",
    "play_half_width",
    "bar_width",
    "point_gap",
    "point_tip_gap",
    "checker_outer_ring_width",
    "checker_outer_radius",
    "checker_face_radius",
    "checker_stack_step",
    "checker_margin",
    "point_number_inset",
    "point_number_size",
    "count_badge_size",
    "off_marker_outer_radius",
    "off_marker_face_radius",
    "off_marker_top_y",
    "off_marker_bottom_y",
    "board_border_width",
    "field_border_width",
    "point_border_width",
    "die_scale",
    "die_gap",
    "die_border_width",
    "cube_scale",
    "cube_inner_scale",
    "cube_text_size",
    "cube_border_width",
    "cube_outside_gap",
    "cube_crosshair_length",
    "cube_crosshair_linewidth",
    "arrow_linewidth",
    "move_label_size",
    "score_text_size",
    "status_text_size",
    "information_player_name_size",
    "information_secondary_text_size",
    "information_pip_text_size",
    "information_sentence_text_size",
    "information_top_band_height",
    "information_bottom_band_height",
    "information_pip_offset",
    "information_top_player_name_offset",
    "information_top_secondary_offset",
    "information_bottom_secondary_offset",
    "information_bottom_player_name_offset",
    "information_sentence_offset",
    "information_on_roll_arrow_x_offset",
    "information_on_roll_arrow_size"
  )

  for (name in positive_scalars) {
    value <- values[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      stop(
        paste0("`", name, "` must be a positive numeric scalar."),
        call. = FALSE
      )
    }
  }

  unrestricted_scalars <- c(
    "information_player_x_nudge",
    "information_sentence_x_nudge"
  )

  for (name in unrestricted_scalars) {
    value <- values[[name]]
    if (
      !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
    ) {
      stop(
        paste0("`", name, "` must be a finite numeric scalar."),
        call. = FALSE
      )
    }
  }

  if (
    !is.numeric(values$cube_crosshair_alpha) ||
    length(values$cube_crosshair_alpha) != 1L ||
    is.na(values$cube_crosshair_alpha) ||
    values$cube_crosshair_alpha < 0 ||
    values$cube_crosshair_alpha > 1
  ) {
    stop("`cube_crosshair_alpha` must be between 0 and 1.", call. = FALSE)
  }

  if (
    !is.numeric(values$cube_inner_scale) ||
    length(values$cube_inner_scale) != 1L ||
    is.na(values$cube_inner_scale) ||
    values$cube_inner_scale <= 0 ||
    values$cube_inner_scale >= 1
  ) {
    stop("`cube_inner_scale` must be greater than 0 and less than 1.", call. = FALSE)
  }

  if (!is.numeric(values$max_stack_visible) ||
      length(values$max_stack_visible) != 1L ||
      is.na(values$max_stack_visible) ||
      values$max_stack_visible < 1 ||
      values$max_stack_visible != as.integer(values$max_stack_visible)) {
    stop("`max_stack_visible` must be a positive integer scalar.", call. = FALSE)
  }

  if (!is.numeric(values$arrow_curvature) ||
      length(values$arrow_curvature) != 1L ||
      is.na(values$arrow_curvature) ||
      values$arrow_curvature < -1 ||
      values$arrow_curvature > 1) {
    stop("`arrow_curvature` must be between -1 and 1.", call. = FALSE)
  }

  expected_width <-
    values$left_margin_width +
    values$right_margin_width +
    (2 * values$rail_size) +
    (2 * values$play_half_width) +
    values$bar_width

  if (abs(values$board_width - expected_width) > 1e-8) {
    stop(
      "`board_width` must equal the sum of the horizontal layout components.",
      call. = FALSE
    )
  }

  if (2 * values$rail_size >= values$board_height) {
    stop("`rail_size` leaves no usable playing-field height.", call. = FALSE)
  }

  point_step <- values$play_half_width / 6
  if (values$point_gap >= point_step) {
    stop("`point_gap` must be smaller than one point base width.", call. = FALSE)
  }

  if (values$checker_face_radius >= values$checker_outer_radius) {
    stop(
      "`checker_face_radius` must be smaller than `checker_outer_radius`.",
      call. = FALSE
    )
  }

  if (values$checker_outer_ring_width >= values$checker_outer_radius) {
    stop(
      "`checker_outer_ring_width` must be smaller than `checker_outer_radius`.",
      call. = FALSE
    )
  }

  if (values$off_marker_face_radius >= values$off_marker_outer_radius) {
    stop(
      "`off_marker_face_radius` must be smaller than `off_marker_outer_radius`.",
      call. = FALSE
    )
  }

  if (abs(values$off_marker_outer_radius - values$checker_outer_radius) > 1e-8 ||
      abs(values$off_marker_face_radius - values$checker_face_radius) > 1e-8) {
    stop("Off markers must use the same size as board checkers.", call. = FALSE)
  }

  if (values$off_marker_top_y >= values$board_height ||
      values$off_marker_bottom_y <= 0) {
    stop("Off-marker positions must remain inside the canvas.", call. = FALSE)
  }

  invisible(values)
}

#' @export
print.backgammon_board_style <- function(x, ...) {
  cat("<backgammon_board_style>\n")
  cat("  Preset:", attr(x, "preset_id"), "\n")

  for (name in names(x)) {
    cat("  ", name, ": ", paste(x[[name]], collapse = ", "), "\n", sep = "")
  }

  invisible(x)
}
