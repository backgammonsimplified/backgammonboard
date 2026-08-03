layout_transform_bounds <- function(style) {
  list(x_min = 0, x_max = style$board_width, y_min = 0, y_max = style$board_height)
}


displayed_point_value <- function(point_id, near_player = "player_1") {
  point_id <- as.integer(point_id)
  if (anyNA(point_id) || any(!point_id %in% 1:24)) {
    stop("Canonical point IDs must be integers from 1 through 24.", call. = FALSE)
  }
  if (identical(near_player, "player_1")) return(point_id)
  if (identical(near_player, "player_0")) return(25L - point_id)
  stop("`near_player` must be `player_0` or `player_1`.", call. = FALSE)
}


transform_coordinate_frame <- function(
    data,
    bounds,
    mirror_horizontal = FALSE,
    flip_vertical = FALSE) {
  if (!is.data.frame(data) || nrow(data) == 0L) return(data)
  result <- data

  reflect <- function(value, minimum, maximum) minimum + maximum - value
  if (isTRUE(mirror_horizontal)) {
    for (column in intersect(c("x", "xend", "source_x", "ghost_x", "destination_x", "control_x", "control2_x", "player_x", "pip_x", "on_roll_arrow_x"), names(result))) {
      result[[column]] <- reflect(result[[column]], bounds$x_min, bounds$x_max)
    }
    if ("final_tangent_x" %in% names(result)) {
      result$final_tangent_x <- -result$final_tangent_x
    }
    if (all(c("xmin", "xmax") %in% names(result))) {
      old_min <- result$xmin
      result$xmin <- reflect(result$xmax, bounds$x_min, bounds$x_max)
      result$xmax <- reflect(old_min, bounds$x_min, bounds$x_max)
    }
  }
  if (isTRUE(flip_vertical)) {
    for (column in intersect(c("y", "yend", "source_y", "ghost_y", "destination_y", "control_y", "control2_y", "player_name_y", "secondary_y", "pip_y", "on_roll_arrow_y"), names(result))) {
      result[[column]] <- reflect(result[[column]], bounds$y_min, bounds$y_max)
    }
    if ("final_tangent_y" %in% names(result)) {
      result$final_tangent_y <- -result$final_tangent_y
    }
    if (all(c("ymin", "ymax") %in% names(result))) {
      old_min <- result$ymin
      result$ymin <- reflect(result$ymax, bounds$y_min, bounds$y_max)
      result$ymax <- reflect(old_min, bounds$y_min, bounds$y_max)
    }
  }
  if (xor(isTRUE(mirror_horizontal), isTRUE(flip_vertical))) {
    for (column in intersect(
        c("curvature", "curve_offset", "curve_level"), names(result)
    )) {
      result[[column]] <- -result[[column]]
    }
  }
  if ("side" %in% names(result) && isTRUE(flip_vertical)) {
    result$side <- ifelse(result$side == "top", "bottom", ifelse(result$side == "bottom", "top", result$side))
  }
  result
}


transform_layout_object <- function(
    object,
    bounds,
    mirror_horizontal = FALSE,
    flip_vertical = FALSE) {
  if (is.null(object)) return(NULL)
  if (is.data.frame(object)) {
    return(transform_coordinate_frame(object, bounds, mirror_horizontal, flip_vertical))
  }
  if (!is.list(object)) return(object)
  result <- lapply(
    object,
    transform_layout_object,
    bounds = bounds,
    mirror_horizontal = mirror_horizontal,
    flip_vertical = flip_vertical
  )
  attributes(result) <- attributes(object)
  result
}


transform_board_geometry <- function(geometry, bounds, mirror_horizontal, flip_vertical) {
  result <- transform_layout_object(geometry, bounds, mirror_horizontal, flip_vertical)
  if (isTRUE(mirror_horizontal)) {
    fields <- result$left_field
    result$left_field <- result$right_field
    result$right_field <- fields
  }
  result
}


transform_information_layout <- function(information, bounds, mirror_horizontal, flip_vertical) {
  result <- transform_layout_object(information, bounds, mirror_horizontal, flip_vertical)
  # The match/game status sentence is a viewport annotation, not a player row.
  # It follows the horizontal anchor transform but remains in the bottom band.
  if (isTRUE(flip_vertical)) {
    result$sentence <- transform_layout_object(
      information$sentence, bounds, mirror_horizontal, FALSE
    )
  }
  if (isTRUE(mirror_horizontal)) {
    for (name in c("top", "bottom")) {
      result[[name]]$player_hjust <- 1 - result[[name]]$player_hjust
      result[[name]]$on_roll_arrow_hjust <- 1 - result[[name]]$on_roll_arrow_hjust
      result[[name]]$on_roll_arrow <- ifelse(
        nzchar(result[[name]]$on_roll_arrow), "\u2190 on roll", ""
      )
    }
  }
  result
}


transform_prepared_layout <- function(
    layout,
    style,
    near_player = "player_1",
    mirror_horizontal = FALSE) {
  if (!near_player %in% c("player_0", "player_1")) {
    stop("`near_player` must be `player_0` or `player_1`.", call. = FALSE)
  }
  if (!is.logical(mirror_horizontal) || length(mirror_horizontal) != 1L || is.na(mirror_horizontal)) {
    stop("`mirror_horizontal` must be TRUE or FALSE.", call. = FALSE)
  }
  bounds <- layout_transform_bounds(style)
  vertical <- identical(near_player, "player_0")
  result <- layout
  result$geometry <- transform_board_geometry(
    layout$geometry, bounds, mirror_horizontal, vertical
  )
  for (name in intersect(c("checkers", "cube", "crawford", "selected_overlay", "alternative_overlay"), names(layout))) {
    result[[name]] <- transform_layout_object(
      layout[[name]], bounds, mirror_horizontal, vertical
    )
  }
  if ("dice" %in% names(layout) && !is.null(layout$dice)) {
    # Dice stay on the roller's physical right: screen-right for the near
    # player and screen-left for the far player. Changing the near player
    # therefore changes the dice field; mirroring point orientation does not.
    result$dice <- transform_layout_object(
      layout$dice, bounds, mirror_horizontal = vertical,
      flip_vertical = vertical
    )
  }
  if ("information" %in% names(layout) && !is.null(layout$information)) {
    result$information <- transform_information_layout(
      layout$information, bounds, mirror_horizontal, vertical
    )
  }
  result$geometry$point_labels$label <- as.character(
    if (identical(near_player, "player_1")) {
      result$geometry$point_labels$point
    } else {
      25L - result$geometry$point_labels$point
    }
  )
  result$near_player <- near_player
  result$mirror_horizontal <- mirror_horizontal
  result
}
