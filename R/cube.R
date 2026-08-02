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
    perspective = "white",
    cube_display_side = c("left", "right")
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
  cube_display_side <- match.arg(cube_display_side)

  frame_xmin <- geometry$frame$xmin[[1L]]
  frame_xmax <- geometry$frame$xmax[[1L]]
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

  outside_left_x <-
    frame_xmin -
    style$cube_scale / 2 -
    style$cube_outside_gap
  outside_right_x <-
    frame_xmax +
    style$cube_scale / 2 +
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
    if (identical(cube_display_side, "left")) inside_left_x else inside_right_x
  } else {
    if (identical(cube_display_side, "left")) outside_left_x else outside_right_x
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


add_crawford_marker <- function(
    plot,
    position,
    geometry,
    colors,
    style,
    perspective = "white",
    cube_display_side = c("left", "right")) {
  if (!identical(position$crawford_status, "crawford") && !isTRUE(position$is_crawford)) {
    return(plot)
  }
  cube_display_side <- match.arg(cube_display_side)
  center <- cube_center(
    state = "centered",
    x_mode = "outside",
    geometry = geometry,
    style = style,
    centered_y_nudge = 0,
    perspective = perspective,
    cube_display_side = cube_display_side
  )
  label <- data.frame(x = center$x, y = center$y, label = "Crawford")
  plot + ggplot2::geom_text(
    data = label,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = colors$cube_text,
    size = style$crawford_text_size,
    fontface = "bold",
    angle = 90
  )
}


cube_layout <- function(
    position,
    geometry,
    style,
    cube_display = NULL,
    cube_x_mode = c("outside", "inside"),
    number_x_nudge = 0,
    number_y_nudge = 0,
    cube_x_nudge = 0,
    centered_y_nudge = 0,
    white_y_nudge = 0,
    black_y_nudge = 0,
    offered_y_nudge = 0,
    perspective = "white",
    cube_display_side = c("left", "right")
) {
  assert_backgammon_position(position)
  cube_x_mode <- match.arg(cube_x_mode)
  cube_display_side <- match.arg(cube_display_side)

  display <- if (!is.null(cube_display)) {
    if (!inherits(cube_display, "backgammon_cube_display")) {
      stop("`cube_display` must be created by resolve_cube_display().", call. = FALSE)
    }
    cube_display
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

  value <- as.integer(display$value)

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
    perspective = perspective,
    cube_display_side = cube_display_side
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
    cube_x_mode = c("outside", "inside"),
    show_crosshair = FALSE,
    number_x_nudge = 0,
    number_y_nudge = 0,
    cube_x_nudge = 0,
    centered_y_nudge = 0,
    white_y_nudge = 0,
    black_y_nudge = 0,
    offered_y_nudge = 0,
    perspective = "white",
    cube_display_side = c("left", "right")
) {
  cube_x_mode <- match.arg(cube_x_mode)
  cube_display_side <- match.arg(cube_display_side)

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style,
    cube_display = cube_display,
    cube_x_mode = cube_x_mode,
    number_x_nudge = number_x_nudge,
    number_y_nudge = number_y_nudge,
    cube_x_nudge = cube_x_nudge,
    centered_y_nudge = centered_y_nudge,
    white_y_nudge = white_y_nudge,
    black_y_nudge = black_y_nudge,
    offered_y_nudge = offered_y_nudge,
    perspective = perspective,
    cube_display_side = cube_display_side
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
