# Map semantic resolved display state to the frozen visual-placement states.
cube_visual_state <- function(cube_display) {
  if (!inherits(cube_display, "backgammon_cube_display")) {
    stop("`cube_display` must be created by resolve_cube_display().", call. = FALSE)
  }

  switch(
    cube_display$state,
    hidden = "hidden",
    centered = "centered",
    owned = if (identical(cube_display$owner, "white")) {
      "owned_white"
    } else {
      "owned_black"
    },
    offered = if (identical(cube_display$receiver, "white")) {
      "offered_white"
    } else if (identical(cube_display$receiver, "black")) {
      "offered_black"
    } else {
      stop("An offered cube requires a semantic receiver.", call. = FALSE)
    },
    stop("Unsupported resolved cube-display state.", call. = FALSE)
  )
}


# Legacy visual override used only by development preview scripts.
legacy_cube_display <- function(position, cube_state, cube_value = NULL) {
  valid_states <- c(
    "centered",
    "owned_white",
    "owned_black",
    "offered_white",
    "offered_black"
  )

  if (
    !is.character(cube_state) ||
    length(cube_state) != 1L ||
    is.na(cube_state) ||
    !cube_state %in% valid_states
  ) {
    stop(
      paste0(
        "`cube_state` must be one of: ",
        paste(valid_states, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  value <- resolve_cube_value(position, cube_state, cube_value)

  switch(
    cube_state,
    centered = new_cube_display(
      TRUE, "centered", 1L, placement = "outside_center"
    ),
    owned_white = new_cube_display(
      TRUE, "owned", value, owner = "white", placement = "white_side"
    ),
    owned_black = new_cube_display(
      TRUE, "owned", value, owner = "black", placement = "black_side"
    ),
    offered_white = new_cube_display(
      TRUE,
      "offered",
      value,
      owner = if (identical(position$cube_owner, "center")) NULL else position$cube_owner,
      offerer = "black",
      receiver = "white",
      placement = "offered_to_white"
    ),
    offered_black = new_cube_display(
      TRUE,
      "offered",
      value,
      owner = if (identical(position$cube_owner, "center")) NULL else position$cube_owner,
      offerer = "white",
      receiver = "black",
      placement = "offered_to_black"
    )
  )
}


resolve_cube_state <- function(position, cube_state = NULL) {
  assert_backgammon_position(position)

  if (!is.null(cube_state)) {
    return(cube_visual_state(
      legacy_cube_display(position, cube_state, cube_value = NULL)
    ))
  }

  cube_visual_state(resolve_cube_display(position))
}


resolve_cube_value <- function(position, cube_state, cube_value = NULL) {
  assert_backgammon_position(position)

  if (identical(cube_state, "centered")) {
    return(1L)
  }

  if (identical(cube_state, "hidden")) {
    return(NA_integer_)
  }

  value <- if (is.null(cube_value)) {
    position$cube_value
  } else {
    cube_value
  }

  valid_values <- c(2L, 4L, 8L, 16L, 32L, 64L)

  if (
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    value != as.integer(value) ||
    !as.integer(value) %in% valid_values
  ) {
    stop(
      paste0(
        "Owned and offered cubes must show one of: ",
        paste(valid_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  as.integer(value)
}


cube_face_polygon <- function(
    center_x,
    center_y,
    size,
    group,
    corner_power = 5,
    resolution = 160L
) {
  theta <- seq(
    from = 0,
    to = 2 * pi,
    length.out = resolution + 1L
  )

  half_size <- size / 2
  exponent <- 2 / corner_power

  horizontal <- sign(cos(theta)) * abs(cos(theta))^exponent
  vertical <- sign(sin(theta)) * abs(sin(theta))^exponent

  data.frame(
    x = center_x + half_size * horizontal,
    y = center_y + half_size * vertical,
    group = group,
    stringsAsFactors = FALSE
  )
}


cube_center <- function(
    state,
    x_mode,
    geometry,
    style,
    x_nudge = 0,
    centered_y_nudge = 0,
    white_y_nudge = 0,
    black_y_nudge = 0,
    offered_y_nudge = 0,
    perspective = "white"
) {
  valid_x_modes <- c("outside", "inside")

  if (
    !is.character(x_mode) ||
    length(x_mode) != 1L ||
    is.na(x_mode) ||
    !x_mode %in% valid_x_modes
  ) {
    stop(
      "`x_mode` must be either `outside` or `inside`.",
      call. = FALSE
    )
  }

  perspective <- normalize_board_perspective(perspective)

  frame_xmin <- geometry$frame$xmin[[1L]]
  left_field_xmin <- geometry$left_field$xmin[[1L]]
  left_field_xmax <- geometry$left_field$xmax[[1L]]
  right_field_xmin <- geometry$right_field$xmin[[1L]]
  right_field_xmax <- geometry$right_field$xmax[[1L]]
  field_ymin <- geometry$left_field$ymin[[1L]]
  field_ymax <- geometry$left_field$ymax[[1L]]

  board_y_center <- mean(c(
    geometry$frame$ymin[[1L]],
    geometry$frame$ymax[[1L]]
  ))

  inside_left_x <- mean(c(
    left_field_xmin,
    left_field_xmax
  ))
  inside_right_x <- mean(c(
    right_field_xmin,
    right_field_xmax
  ))

  outside_x <-
    frame_xmin -
    style$cube_scale / 2 -
    style$cube_outside_gap

  is_offered <- state %in% c("offered_white", "offered_black")
  resolved_x_mode <- if (is_offered) "inside" else x_mode

  receiver <- if (identical(state, "offered_white")) {
    "white"
  } else if (identical(state, "offered_black")) {
    "black"
  } else {
    NULL
  }

  x <- if (is_offered) {
    if (identical(receiver, perspective)) inside_left_x else inside_right_x
  } else if (identical(resolved_x_mode, "inside")) {
    inside_left_x
  } else {
    outside_x
  }

  white_first_checker_y <-
    field_ymin +
    style$checker_margin +
    style$checker_outer_radius

  black_first_checker_y <-
    field_ymax -
    style$checker_margin -
    style$checker_outer_radius

  white_is_bottom <- identical(perspective, "white")

  y <- switch(
    state,
    centered = board_y_center + centered_y_nudge,
    owned_white = if (white_is_bottom) {
      white_first_checker_y + white_y_nudge
    } else {
      black_first_checker_y + white_y_nudge
    },
    owned_black = if (white_is_bottom) {
      black_first_checker_y + black_y_nudge
    } else {
      white_first_checker_y + black_y_nudge
    },
    offered_white = board_y_center + offered_y_nudge,
    offered_black = board_y_center + offered_y_nudge
  )

  data.frame(
    x = x + x_nudge,
    y = y,
    x_mode = resolved_x_mode,
    stringsAsFactors = FALSE
  )
}


cube_layout <- function(
    position,
    geometry,
    style,
    cube_display = NULL,
    cube_state = NULL,
    cube_x_mode = c("outside", "inside"),
    cube_value = NULL,
    number_x_nudge = 0,
    number_y_nudge = 0,
    cube_x_nudge = 0,
    centered_y_nudge = 0,
    white_y_nudge = 0,
    black_y_nudge = 0,
    offered_y_nudge = 0,
    perspective = "white"
) {
  assert_backgammon_position(position)
  cube_x_mode <- match.arg(cube_x_mode)

  if (!is.null(cube_display) && !is.null(cube_state)) {
    stop("Supply `cube_display` or the development `cube_state`, not both.", call. = FALSE)
  }

  display <- if (!is.null(cube_display)) {
    if (!inherits(cube_display, "backgammon_cube_display")) {
      stop("`cube_display` must be created by resolve_cube_display().", call. = FALSE)
    }
    cube_display
  } else if (!is.null(cube_state)) {
    legacy_cube_display(position, cube_state, cube_value)
  } else {
    resolve_cube_display(position)
  }

  state <- cube_visual_state(display)

  if (!isTRUE(display$visible) || identical(state, "hidden")) {
    return(list(
      visible = FALSE,
      display = display,
      display_state = "hidden",
      state = "hidden",
      value = display$value,
      center = data.frame(),
      outer = data.frame(),
      inner = data.frame(),
      number = data.frame(),
      crosshair = data.frame()
    ))
  }

  value <- if (!is.null(cube_state)) {
    resolve_cube_value(position, state, cube_value)
  } else {
    as.integer(display$value)
  }

  center <- cube_center(
    state = state,
    x_mode = cube_x_mode,
    geometry = geometry,
    style = style,
    x_nudge = cube_x_nudge,
    centered_y_nudge = centered_y_nudge,
    white_y_nudge = white_y_nudge,
    black_y_nudge = black_y_nudge,
    offered_y_nudge = offered_y_nudge,
    perspective = perspective
  )

  outer <- cube_face_polygon(
    center_x = center$x[[1L]],
    center_y = center$y[[1L]],
    size = style$cube_scale,
    group = 1L
  )

  inner <- cube_face_polygon(
    center_x = center$x[[1L]],
    center_y = center$y[[1L]],
    size = style$cube_scale * style$cube_inner_scale,
    group = 2L
  )

  number <- data.frame(
    x = center$x[[1L]] + number_x_nudge,
    y = center$y[[1L]] + number_y_nudge,
    label = as.character(value),
    stringsAsFactors = FALSE
  )

  crosshair_half_length <- style$cube_crosshair_length / 2

  crosshair <- rbind(
    data.frame(
      x = center$x[[1L]] - crosshair_half_length,
      xend = center$x[[1L]] + crosshair_half_length,
      y = center$y[[1L]],
      yend = center$y[[1L]],
      stringsAsFactors = FALSE
    ),
    data.frame(
      x = center$x[[1L]],
      xend = center$x[[1L]],
      y = center$y[[1L]] - crosshair_half_length,
      yend = center$y[[1L]] + crosshair_half_length,
      stringsAsFactors = FALSE
    )
  )

  list(
    visible = TRUE,
    display = display,
    display_state = display$state,
    state = state,
    value = value,
    center = center,
    outer = outer,
    inner = inner,
    number = number,
    crosshair = crosshair
  )
}


add_cube_layers <- function(
    plot,
    position,
    geometry,
    colors,
    style,
    cube_display = NULL,
    cube_state = NULL,
    cube_x_mode = c("outside", "inside"),
    cube_value = NULL,
    show_crosshair = FALSE,
    number_x_nudge = 0,
    number_y_nudge = 0,
    cube_x_nudge = 0,
    centered_y_nudge = 0,
    white_y_nudge = 0,
    black_y_nudge = 0,
    offered_y_nudge = 0,
    perspective = "white"
) {
  cube_x_mode <- match.arg(cube_x_mode)

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style,
    cube_display = cube_display,
    cube_state = cube_state,
    cube_x_mode = cube_x_mode,
    cube_value = cube_value,
    number_x_nudge = number_x_nudge,
    number_y_nudge = number_y_nudge,
    cube_x_nudge = cube_x_nudge,
    centered_y_nudge = centered_y_nudge,
    white_y_nudge = white_y_nudge,
    black_y_nudge = black_y_nudge,
    offered_y_nudge = offered_y_nudge,
    perspective = perspective
  )

  if (!isTRUE(cube$visible)) {
    return(plot)
  }

  outer_color <- if (identical(cube$display_state, "offered")) {
    colors$cube_offered_border
  } else {
    colors$cube_border
  }

  plot <- plot +
    ggplot2::geom_polygon(
      data = cube$outer,
      ggplot2::aes(
        x = x,
        y = y,
        group = group
      ),
      inherit.aes = FALSE,
      fill = outer_color,
      color = outer_color,
      linewidth = style$cube_border_width,
      linejoin = "round"
    ) +
    ggplot2::geom_polygon(
      data = cube$inner,
      ggplot2::aes(
        x = x,
        y = y,
        group = group
      ),
      inherit.aes = FALSE,
      fill = colors$cube_face,
      color = NA,
      linewidth = 0,
      linejoin = "round"
    )

  if (isTRUE(show_crosshair)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = cube$crosshair,
        ggplot2::aes(
          x = x,
          xend = xend,
          y = y,
          yend = yend
        ),
        inherit.aes = FALSE,
        color = colors$cube_crosshair,
        alpha = style$cube_crosshair_alpha,
        linewidth = style$cube_crosshair_linewidth,
        lineend = "butt"
      )
  }

  plot +
    ggplot2::geom_text(
      data = cube$number,
      ggplot2::aes(
        x = x,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      color = colors$cube_text,
      size = style$cube_text_size,
      fontface = "bold",
      hjust = 0.5,
      vjust = 0.5
    )
}
