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
    geometry = NULL,
    dice = NULL,
    cube = NULL
) {
  side <- match.arg(side)

  if (!identical(side, "auto")) {
    return(side)
  }

  if (is.null(geometry)) {
    return("left")
  }

  field_side <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) return(character())
    in_left <- any(x >= geometry$left_field$xmin & x <= geometry$left_field$xmax)
    in_right <- any(x >= geometry$right_field$xmin & x <= geometry$right_field$xmax)
    if (xor(in_left, in_right)) if (in_left) "left" else "right" else character()
  }

  occupied <- character()
  if (!is.null(dice) && nrow(dice$faces) > 0L) {
    occupied <- c(occupied, field_side(dice$faces$x))
  }
  if (!is.null(cube) && isTRUE(cube$visible) && nrow(cube$center) > 0L) {
    occupied <- c(occupied, field_side(cube$center$x))
  }
  occupied <- unique(occupied)

  if (length(occupied) != 1L) {
    return("left")
  }
  if (identical(occupied, "left")) "right" else "left"
}


colors_for_light_player <- function(colors, light_player = "white") {
  light_player <- normalize_board_perspective(light_player)
  if (identical(light_player, "white")) return(colors)

  result <- colors
  pairs <- list(
    c("white_checker_fill", "black_checker_fill"),
    c("white_checker_ring", "black_checker_ring"),
    c("white_checker_outer_ring", "black_checker_outer_ring"),
    c("white_checker_text", "black_checker_text"),
    c("die_white_fill", "die_black_fill"),
    c("die_white_pips", "die_black_pips"),
    c("die_white_border", "die_black_border")
  )
  for (pair in pairs) {
    first <- result[[pair[[1L]]]]
    result[[pair[[1L]]]] <- result[[pair[[2L]]]]
    result[[pair[[2L]]]] <- first
  }
  attr(result, "light_render_player") <- light_player
  result
}


add_board_brand <- function(plot,
                            geometry,
                            text,
                            side = c("left", "right"),
                            color,
                            size = 6.0,
                            alpha = 0.90,
                            y_nudge = 0,
                            family = "Source Sans 3",
                            fontface = "bold") {
  if (is.null(text) || !nzchar(text)) {
    return(plot)
  }

  side <- match.arg(side)
  label <- data.frame(
    x = if (identical(side, "left")) {
      mean(c(geometry$left_field$xmin, geometry$left_field$xmax))
    } else {
      mean(c(geometry$right_field$xmin, geometry$right_field$xmax))
    },
    y = mean(c(geometry$frame$ymin, geometry$frame$ymax)) + y_nudge,
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
      family = family,
      fontface = fontface,
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
    colors = board_colors("bs"),
    style = board_style("bs"),
    point_1_side = c("right", "left"),
    perspective = NULL,
    mirror_horizontal = NULL,
    light_player = "white",
    bottom_home_board_side = NULL,
    point_labels_for = NULL,
    brand_text = "Backgammon\nSimplified",
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
    player_name_style = c("neutral", "checker"),
    show_guides = FALSE,
    guide_color = "#D9653B",
    guide_width = 0.60,
    guide_alpha = 0.80,
    moves = NULL,
    alternative_moves = NULL,
    movement_style = NULL
) {
  point_1_side <- match.arg(point_1_side)
  score_format <- match.arg(score_format)
  player_name_style <- match.arg(player_name_style)
  resolved_perspective <- if (is.null(perspective)) {
    "white"
  } else {
    normalize_board_perspective(perspective)
  }
  if (is.null(mirror_horizontal)) {
    mirror_horizontal <- identical(point_1_side, "left")
  }
  if (!is.logical(mirror_horizontal) || length(mirror_horizontal) != 1L ||
      is.na(mirror_horizontal)) {
    stop("`mirror_horizontal` must be TRUE or FALSE.", call. = FALSE)
  }
  point_1_side <- if (isTRUE(mirror_horizontal)) "left" else "right"
  brand_side <- match.arg(brand_side)
  cube_x_mode <- match.arg(cube_x_mode)
  cube_display_side <- match.arg(cube_display_side)

  if (!inherits(colors, "backgammon_board_colors")) {
    stop("`colors` must be created by board_colors().", call. = FALSE)
  }

  if (!inherits(style, "backgammon_board_style")) {
    stop("`style` must be created by board_style().", call. = FALSE)
  }
  movement_style <- resolve_movement_overlay_style(movement_style, colors, style)

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
  canonical_geometry <- board_geometry(
    style,
    point_1_side = "right",
    perspective = "white",
    bottom_home_board_side = "right",
    point_labels_for = "white"
  )
  selected_overlay <- if (is.null(selected_moves)) {
    NULL
  } else {
    move_overlay_geometry(
      position = position,
      moves = selected_moves,
      style = style,
      movement_style = movement_style,
      perspective = "white",
      point_1_side = "right",
      bottom_home_board_side = "right",
      point_labels_for = "white",
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
      movement_style = movement_style,
      perspective = "white",
      point_1_side = "right",
      bottom_home_board_side = "right",
      point_labels_for = "white",
      role = "alternative"
    )
  }

  display_position <- if (is.null(selected_overlay)) {
    position
  } else {
    move_overlay_position_after(position, selected_overlay$applied_moves)
  }
  # Movement overlays annotate the factual starting position. The applied
  # result remains available as `display_position` for validation and public
  # metadata, but it must not replace the checkers shown beneath arrows and
  # destination ghosts.
  visual_position <- position
  canonical_checkers <- checker_layout(
    visual_position,
    style,
    point_1_side = "right",
    perspective = "white",
    bottom_home_board_side = "right",
    point_labels_for = "white"
  )
  canonical_dice <- dice_layout(
    position = visual_position,
    geometry = canonical_geometry,
    style = style,
    perspective = "white"
  )
  canonical_cube <- if (isTRUE(show_cube)) {
    cube_layout(
      position = visual_position,
      geometry = canonical_geometry,
      style = style,
      cube_display = cube_display,
      cube_x_mode = cube_x_mode,
      centered_y_nudge = style$cube_centered_y_nudge,
      perspective = "white",
      cube_display_side = "left"
    )
  } else {
    NULL
  }
  light_player <- normalize_board_perspective(light_player)
  colors <- colors_for_light_player(colors, light_player)
  canonical_crawford <- if (
      identical(visual_position$crawford_status, "crawford") ||
      isTRUE(visual_position$is_crawford)) {
    center <- cube_center(
      state = "centered", x_mode = "outside",
      geometry = canonical_geometry, style = style,
      centered_y_nudge = 0, perspective = "white",
      cube_display_side = "left"
    )
    data.frame(x = center$x, y = center$y, label = "Crawford")
  } else {
    data.frame(x = numeric(), y = numeric(), label = character())
  }
  canonical_information <- if (isTRUE(show_information)) {
    board_information_layout(
      position = visual_position,
      geometry = canonical_geometry,
      style = style,
      white_name = white_name,
      black_name = black_name,
      white_wins = white_wins,
      black_wins = black_wins,
      context = context,
      perspective = "white",
      information_side = "right",
      score_format = score_format
    )
  } else {
    NULL
  }
  prepared <- transform_prepared_layout(
    list(
      geometry = canonical_geometry,
      checkers = canonical_checkers,
      dice = canonical_dice,
      cube = canonical_cube,
      crawford = canonical_crawford,
      information = canonical_information,
      selected_overlay = selected_overlay,
      alternative_overlay = alternative_overlay
    ),
    style = style,
    near_player = render_player_to_project_player(resolved_perspective),
    mirror_horizontal = mirror_horizontal
  )
  geometry <- prepared$geometry
  checkers <- prepared$checkers
  brand_dice <- prepared$dice
  brand_cube <- prepared$cube
  selected_overlay <- prepared$selected_overlay
  alternative_overlay <- prepared$alternative_overlay
  resolved_brand_side <- resolve_board_brand_side(
    side = brand_side,
    geometry = geometry,
    dice = brand_dice,
    cube = brand_cube
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
    position = visual_position,
    geometry = geometry,
    colors = colors,
    style = style,
    perspective = resolved_perspective,
    dice = prepared$dice
  )

  if (isTRUE(show_cube) && isTRUE(cube_display$visible)) {
    plot <- add_cube_layers(
      plot = plot,
      position = visual_position,
      geometry = geometry,
      colors = colors,
      style = style,
      cube_display = cube_display,
      cube_x_mode = cube_x_mode,
      show_crosshair = show_cube_crosshair,
      centered_y_nudge = style$cube_centered_y_nudge,
      perspective = resolved_perspective,
      cube_display_side = cube_display_side,
      cube = prepared$cube
    )
  }

  plot <- add_crawford_marker(
    plot = plot,
    position = visual_position,
    geometry = geometry,
    colors = colors,
    style = style,
    perspective = resolved_perspective,
    cube_display_side = cube_display_side,
    label_data = prepared$crawford
  )

  plot <- add_board_brand(
    plot = plot,
    geometry = geometry,
    text = brand_text,
    side = resolved_brand_side,
    color = colors$brand_text,
    size = brand_size,
    alpha = brand_alpha,
    y_nudge = brand_y_nudge
  )

  if (isTRUE(show_information)) {
    plot <- add_board_information(
      plot = plot,
      position = visual_position,
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
      information_side = point_1_side,
      player_name_style = player_name_style,
      score_format = score_format,
      information = prepared$information
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

  # Keep movement overlays above the factual board. Destination ghosts are
  # painted first, then arrows so every shaft and arrowhead remains visible.
  movement_layer_start <- length(plot$layers) + 1L
  plot <- add_move_ghost_layers(
    plot = plot,
    overlay = alternative_overlay,
    style = style,
    movement_style = movement_style
  )
  plot <- add_move_ghost_layers(
    plot = plot,
    overlay = selected_overlay,
    style = style,
    movement_style = movement_style
  )
  plot <- add_move_arrow_layers(
    plot = plot,
    overlay = alternative_overlay,
    movement_style = movement_style
  )
  plot <- add_move_arrow_layers(
    plot = plot,
    overlay = selected_overlay,
    movement_style = movement_style
  )
  # Coincident-path multipliers are the final paint layer so their on-roll
  # typography remains legible above checkers, ghosts, arrows, dice and text.
  plot <- add_move_multiplier_layers(
    plot = plot,
    overlay = alternative_overlay,
    geometry = geometry,
    colors = colors,
    style = style,
    family = information_family
  )
  plot <- add_move_multiplier_layers(
    plot = plot,
    overlay = selected_overlay,
    geometry = geometry,
    colors = colors,
    style = style,
    family = information_family
  )
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
  attr(plot, "backgammon_movement_style") <- movement_style
  attr(plot, "backgammon_movement_layer_start") <- movement_layer_start
  attr(plot, "backgammon_display_position") <- display_position
  attr(plot, "backgammon_prepared_layout") <- prepared
  plot
}
