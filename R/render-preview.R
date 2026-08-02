player_checker_colors <- function(colors, player) {
  if (identical(player, "white")) {
    return(list(
      face = colors$white_checker_fill,
      ring = colors$white_checker_ring,
      outer_ring = colors$white_checker_outer_ring,
      text = colors$white_checker_text
    ))
  }

  list(
    face = colors$black_checker_fill,
    ring = colors$black_checker_ring,
    outer_ring = colors$black_checker_outer_ring,
    text = colors$black_checker_text
  )
}

add_checker_layers <- function(plot,
                               data,
                               player,
                               colors,
                               outer_radius,
                               face_radius,
                               outer_ring_width,
                               label_column = "count_label",
                               label_size) {
  player_data <- data[data$player == player, , drop = FALSE]
  if (nrow(player_data) == 0L) {
    return(plot)
  }

  palette <- player_checker_colors(colors, player)
  player_data$outer_ring_radius <- outer_radius + outer_ring_width
  player_data$outer_radius <- outer_radius
  player_data$face_radius <- face_radius

  plot <- plot +
    ggforce::geom_circle(
      data = player_data,
      ggplot2::aes(x0 = x, y0 = y, r = outer_ring_radius),
      fill = palette$outer_ring,
      color = NA,
      n = 360
    ) +
    ggforce::geom_circle(
      data = player_data,
      ggplot2::aes(x0 = x, y0 = y, r = outer_radius),
      fill = palette$ring,
      color = NA,
      n = 360
    ) +
    ggforce::geom_circle(
      data = player_data,
      ggplot2::aes(x0 = x, y0 = y, r = face_radius),
      fill = palette$face,
      color = NA,
      n = 360
    )

  labels <- player_data[[label_column]]
  label_data <- player_data[
    !is.na(labels) & nzchar(labels),
    ,
    drop = FALSE
  ]

  if (nrow(label_data) == 0L) {
    return(plot)
  }

  label_data$display_label <- label_data[[label_column]]

  plot +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x, y = y, label = display_label),
      color = palette$text,
      size = label_size,
      fontface = "bold"
    )
}

add_board_checkers <- function(plot, checkers, colors, style) {
  checker_data <- rbind(checkers$points, checkers$bar)

  for (player in c("white", "black")) {
    plot <- add_checker_layers(
      plot = plot,
      data = checker_data,
      player = player,
      colors = colors,
      outer_radius = style$checker_outer_radius,
      face_radius = style$checker_face_radius,
      outer_ring_width = style$checker_outer_ring_width,
      label_size = style$count_badge_size
    )
  }

  plot
}

add_off_checkers <- function(plot, off, colors, style) {
  if (nrow(off) == 0L) {
    return(plot)
  }

  off$count_label <- off$label

  for (player in c("white", "black")) {
    plot <- add_checker_layers(
      plot = plot,
      data = off,
      player = player,
      colors = colors,
      outer_radius = style$off_marker_outer_radius,
      face_radius = style$off_marker_face_radius,
      outer_ring_width = style$checker_outer_ring_width,
      label_size = style$count_badge_size
    )
  }

  plot
}

resolve_board_brand_side <- function(
    side = c("auto", "left", "right"),
    cube_display = NULL,
    perspective = "white"
) {
  side <- match.arg(side)

  if (!identical(side, "auto")) {
    return(side)
  }

  if (
    is.null(cube_display) ||
    !inherits(cube_display, "backgammon_cube_display") ||
    !identical(cube_display$state, "offered")
  ) {
    return("left")
  }

  offered_side <- offered_cube_field_side(
    receiver = cube_display$receiver,
    perspective = perspective
  )

  if (identical(offered_side, "left")) "right" else "left"
}


add_board_brand <- function(plot,
                            geometry,
                            text,
                            side = c("left", "right"),
                            color,
                            size = 6.0,
                            alpha = 0.90,
                            y_nudge = 0) {
  if (is.null(text) || !nzchar(text)) {
    return(plot)
  }

  side <- match.arg(side)
  label <- data.frame(
    # Branding belongs in the lower rail, never over playable points or bars.
    x = mean(c(geometry$frame$xmin, geometry$frame$xmax)),
    y = geometry$frame$ymin + (geometry$layout$field_ymin / 2) + y_nudge,
    label = text,
    stringsAsFactors = FALSE
  )

  plot +
    ggplot2::geom_text(
      data = label,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = color,
      alpha = alpha,
      size = size,
      fontface = "bold",
      lineheight = 0.95,
      hjust = 0.5,
      vjust = 0.5
    )
}

add_board_guides <- function(plot,
                             geometry,
                             color = "#D9653B",
                             linewidth = 0.60,
                             alpha = 0.80) {
  frame <- geometry$frame
  board_y_mid <- mean(c(frame$ymin, frame$ymax))

  horizontal <- data.frame(
    x = frame$xmin,
    xend = frame$xmax,
    y = board_y_mid,
    yend = board_y_mid
  )

  vertical <- data.frame(
    x = c(
      mean(c(geometry$left_field$xmin, geometry$left_field$xmax)),
      mean(c(geometry$right_field$xmin, geometry$right_field$xmax))
    ),
    xend = c(
      mean(c(geometry$left_field$xmin, geometry$left_field$xmax)),
      mean(c(geometry$right_field$xmin, geometry$right_field$xmax))
    ),
    y = rep(frame$ymin, 2L),
    yend = rep(frame$ymax, 2L)
  )

  plot +
    ggplot2::geom_segment(
      data = horizontal,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      lineend = "butt"
    ) +
    ggplot2::geom_segment(
      data = vertical,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      lineend = "butt"
    )
}

render_board_preview <- function(
    x,
    colors = board_colors("bms"),
    style = board_style("bms"),
    point_1_side = c("right", "left"),
    perspective = NULL,
    bottom_home_board_side = NULL,
    point_labels_for = NULL,
    brand_text = "Backgammon\nMade Simple",
    brand_side = c("auto", "left", "right"),
    brand_size = 6.0,
    brand_alpha = 0.90,
    brand_y_nudge = 0,
    show_cube = TRUE,
    cube_offer = NULL,
    cube_x_mode = c("outside", "inside"),
    cube_display_side = c("left", "right"),
    show_cube_crosshair = FALSE,
    show_information = TRUE,
    white_name = "White",
    black_name = "Black",
    white_wins = NULL,
    black_wins = NULL,
    context = NULL,
    score_format = c("away", "raw", "both"),
    information_family = "sans",
    show_guides = FALSE,
    guide_color = "#D9653B",
    guide_width = 0.60,
    guide_alpha = 0.80,
    moves = NULL,
    alternative_moves = NULL
) {
  point_1_side <- match.arg(point_1_side)
  score_format <- match.arg(score_format)
  resolved_perspective <- if (is.null(perspective)) {
    "white"
  } else {
    normalize_board_perspective(perspective)
  }
  brand_side <- match.arg(brand_side)
  cube_x_mode <- match.arg(cube_x_mode)
  cube_display_side <- match.arg(cube_display_side)

  if (!inherits(colors, "backgammon_board_colors")) {
    stop("`colors` must be created by board_colors().", call. = FALSE)
  }

  if (!inherits(style, "backgammon_board_style")) {
    stop("`style` must be created by board_style().", call. = FALSE)
  }

  position <- if (inherits(x, "backgammon_position")) {
    x
  } else {
    backgammon_position(x)
  }

  selected_moves <- normalize_move_overlay_input(moves, "moves")
  alternative_moves <- normalize_move_overlay_input(
    alternative_moves,
    "alternative_moves"
  )

  context <- normalize_board_context(context)

  if (!is.null(cube_offer) && !inherits(cube_offer, "backgammon_cube_offer_context")) {
    stop("`cube_offer` must be NULL or created by cube_offer_context().", call. = FALSE)
  }

  if (!is.null(cube_offer) && !is.null(context$offer)) {
    stop("Supply a cube offer through `cube_offer` or `context`, not both.", call. = FALSE)
  }

  resolved_offer <- if (!is.null(cube_offer)) cube_offer else context$offer
  cube_display <- resolve_cube_display(
    position = position,
    offer = resolved_offer
  )
  resolved_brand_side <- resolve_board_brand_side(
    side = brand_side,
    cube_display = cube_display,
    perspective = resolved_perspective
  )

  geometry <- board_geometry(
    style,
    point_1_side = point_1_side,
    perspective = if (is.null(perspective)) NULL else resolved_perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for
  )
  selected_overlay <- if (is.null(selected_moves)) {
    NULL
  } else {
    move_overlay_geometry(
      position = position,
      moves = selected_moves,
      style = style,
      perspective = resolved_perspective,
      point_1_side = point_1_side,
      bottom_home_board_side = bottom_home_board_side,
      point_labels_for = point_labels_for,
      role = "selected"
    )
  }

  alternative_overlay <- if (is.null(alternative_moves)) {
    NULL
  } else {
    move_overlay_geometry(
      position = position,
      moves = alternative_moves,
      style = style,
      perspective = resolved_perspective,
      point_1_side = point_1_side,
      bottom_home_board_side = bottom_home_board_side,
      point_labels_for = point_labels_for,
      role = "alternative"
    )
  }

  display_position <- if (is.null(selected_overlay)) {
    position
  } else {
    move_overlay_position_after(position, selected_overlay$applied_moves)
  }
  checkers <- checker_layout(
    display_position,
    style,
    point_1_side = point_1_side,
    perspective = if (is.null(perspective)) NULL else resolved_perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for
  )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = geometry$canvas,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$outside_fill,
      color = NA
    ) +
    ggplot2::geom_rect(
      data = geometry$frame,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$frame_fill,
      color = colors$frame_border,
      linewidth = style$board_border_width
    ) +
    ggplot2::geom_rect(
      data = geometry$left_field,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$field_fill,
      color = colors$frame_border,
      linewidth = style$field_border_width
    ) +
    ggplot2::geom_rect(
      data = geometry$bar,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$bar_fill,
      color = colors$frame_border,
      linewidth = style$field_border_width
    ) +
    ggplot2::geom_rect(
      data = geometry$right_field,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = colors$field_fill,
      color = colors$frame_border,
      linewidth = style$field_border_width
    ) +
    ggplot2::geom_polygon(
      data = geometry$points,
      ggplot2::aes(x = x, y = y, group = group, fill = point_role),
      color = colors$point_border,
      linewidth = style$point_border_width,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        light = colors$light_point_fill,
        dark = colors$dark_point_fill
      )
    ) +
    ggplot2::geom_text(
      data = geometry$point_labels,
      ggplot2::aes(x = x, y = y, label = label),
      color = colors$point_number,
      size = style$point_number_size
    )

  plot <- add_board_checkers(plot, checkers, colors, style)
  plot <- add_off_checkers(plot, checkers$off, colors, style)
  plot <- add_dice_layers(
    plot = plot,
    position = display_position,
    geometry = geometry,
    colors = colors,
    style = style,
    perspective = resolved_perspective
  )

  if (isTRUE(show_cube) && isTRUE(cube_display$visible)) {
    plot <- add_cube_layers(
      plot = plot,
      position = display_position,
      geometry = geometry,
      colors = colors,
      style = style,
      cube_display = cube_display,
      cube_x_mode = cube_x_mode,
      show_crosshair = show_cube_crosshair,
      perspective = resolved_perspective,
      cube_display_side = cube_display_side
    )
  }

  plot <- add_board_brand(
    plot = plot,
    geometry = geometry,
    text = brand_text,
    side = resolved_brand_side,
    color = colors$bar_fill,
    size = brand_size,
    alpha = brand_alpha,
    y_nudge = brand_y_nudge
  )

  plot <- add_move_overlay_layers(
    plot = plot,
    overlay = alternative_overlay,
    colors = colors,
    style = style
  )
  plot <- add_move_overlay_layers(
    plot = plot,
    overlay = selected_overlay,
    colors = colors,
    style = style
  )

  if (isTRUE(show_information)) {
    plot <- add_board_information(
      plot = plot,
      position = display_position,
      geometry = geometry,
      colors = colors,
      style = style,
      white_name = white_name,
      black_name = black_name,
      white_wins = white_wins,
      black_wins = black_wins,
      context = context,
      family = information_family,
      perspective = resolved_perspective,
      score_format = score_format
    )
  }

  if (isTRUE(show_guides)) {
    plot <- add_board_guides(
      plot = plot,
      geometry = geometry,
      color = guide_color,
      linewidth = guide_width,
      alpha = guide_alpha
    )
  }

  y_limits <- if (isTRUE(show_information)) {
    c(
      -style$information_bottom_band_height,
      style$board_height + style$information_top_band_height
    )
  } else {
    c(0, style$board_height)
  }

  plot <- plot +
    ggplot2::coord_fixed(
      xlim = c(0, style$board_width),
      ylim = y_limits,
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = colors$outside_fill,
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = colors$outside_fill,
        color = NA
      ),
      plot.margin = ggplot2::margin(
        t = 8,
        r = 8,
        b = 8,
        l = 8,
        unit = "pt"
      )
    )

  attr(plot, "backgammon_brand_side") <- resolved_brand_side
  attr(plot, "backgammon_move_overlays") <- list(
    selected = selected_overlay,
    alternative = alternative_overlay
  )
  attr(plot, "backgammon_display_position") <- display_position
  plot
}
