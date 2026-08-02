normalize_move_overlay_input <- function(x, argument = "moves") {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "backgammon_board_moves")) {
    validate_board_moves(x)
    return(x)
  }

  stop(
    paste0(
      "`", argument,
      "` must be NULL or an object created by `board_moves()`."
    ),
    call. = FALSE
  )
}


move_overlay_role <- function(role) {
  match.arg(role, c("selected", "alternative"))
}


move_overlay_one_step <- function(step) {
  one <- step[, move_step_columns(), drop = FALSE]
  one$step_id <- 1L
  rownames(one) <- NULL

  structure(
    one,
    die = NA_integer_,
    label = NA_character_,
    mover_relative = FALSE,
    class = c("backgammon_board_moves", "data.frame")
  )
}


move_overlay_position_after <- function(position, result) {
  position$points <- result$points
  position$bar <- result$bar
  position$off <- result$off
  if (!is.null(position$point_occupancy)) {
    position$point_occupancy <- signed_points_to_occupancy(position$points)
  }
  position
}


move_overlay_fallback_point_anchor <- function(point, geometry, style) {
  item <- geometry$point_layout[
    geometry$point_layout$point == point,
    ,
    drop = FALSE
  ]

  if (nrow(item) != 1L) {
    stop("Unable to resolve move-overlay point geometry.", call. = FALSE)
  }

  depth <- min(
    2.25,
    (geometry$layout$field_ymax - geometry$layout$field_ymin) / 3
  )

  y <- if (identical(item$side[[1L]], "bottom")) {
    geometry$layout$field_ymin + depth
  } else {
    geometry$layout$field_ymax - depth
  }

  c(x = item$x[[1L]], y = y)
}


move_overlay_exposed_checker <- function(data, player) {
  player_data <- data[data$player == player, , drop = FALSE]

  if (nrow(player_data) == 0L) {
    return(NULL)
  }

  side <- player_data$side[[1L]]
  index <- if (identical(side, "bottom")) {
    which.max(player_data$y)
  } else {
    which.min(player_data$y)
  }

  c(x = player_data$x[[index]], y = player_data$y[[index]])
}


move_overlay_location_anchor <- function(
    position,
    type,
    point,
    player,
    geometry,
    style,
    perspective,
    point_1_side = "right",
    bottom_home_board_side = NULL,
    point_labels_for = NULL
) {
  checkers <- checker_layout(
    position,
    style,
    point_1_side = point_1_side,
    perspective = perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for
  )

  if (identical(type, "point")) {
    point_rows <- checkers$points[
      checkers$points$point == point,
      ,
      drop = FALSE
    ]
    anchor <- move_overlay_exposed_checker(point_rows, player)

    if (!is.null(anchor)) {
      return(anchor)
    }

    return(move_overlay_fallback_point_anchor(point, geometry, style))
  }

  if (identical(type, "bar")) {
    anchor <- move_overlay_exposed_checker(checkers$bar, player)

    if (!is.null(anchor)) {
      return(anchor)
    }

    bar_x <- mean(c(geometry$layout$bar_xmin, geometry$layout$bar_xmax))
    player_is_bottom <- identical(player, perspective)
    bar_y <- if (player_is_bottom) {
      geometry$layout$field_ymin + style$checker_outer_radius
    } else {
      geometry$layout$field_ymax - style$checker_outer_radius
    }

    return(c(x = bar_x, y = bar_y))
  }

  if (identical(type, "off")) {
    off_rows <- checkers$off[checkers$off$player == player, , drop = FALSE]

    if (nrow(off_rows) > 0L) {
      return(c(x = off_rows$x[[1L]], y = off_rows$y[[1L]]))
    }

    off_x <- mean(c(
      geometry$layout$right_margin_xmin,
      geometry$layout$right_margin_xmax
    ))
    off_y <- if (identical(player, perspective)) {
      style$off_marker_bottom_y
    } else {
      style$off_marker_top_y
    }

    return(c(x = off_x, y = off_y))
  }

  stop("Unknown move-overlay location type.", call. = FALSE)
}


move_overlay_midpoint <- function(x, y, xend, yend, curvature) {
  dx <- xend - x
  dy <- yend - y
  distance <- sqrt(dx^2 + dy^2)

  if (distance == 0) {
    return(c(x = x, y = y))
  }

  perpendicular_x <- -dy / distance
  perpendicular_y <- dx / distance
  offset <- curvature * distance * 0.28

  c(
    x = (x + xend) / 2 + perpendicular_x * offset,
    y = (y + yend) / 2 + perpendicular_y * offset
  )
}


move_overlay_trim_segments <- function(segments, clearance) {
  for (index in seq_len(nrow(segments))) {
    dx <- segments$xend[[index]] - segments$x[[index]]
    dy <- segments$yend[[index]] - segments$y[[index]]
    distance <- sqrt(dx^2 + dy^2)

    if (distance <= 0) {
      next
    }

    trim <- min(clearance, distance * 0.20)
    unit_x <- dx / distance
    unit_y <- dy / distance

    segments$x[[index]] <- segments$x[[index]] + unit_x * trim
    segments$y[[index]] <- segments$y[[index]] + unit_y * trim
    segments$xend[[index]] <- segments$xend[[index]] - unit_x * trim
    segments$yend[[index]] <- segments$yend[[index]] - unit_y * trim
  }

  segments
}


move_overlay_duplicate_curvatures <- function(segments, style) {
  keys <- paste(
    round(segments$x, 4),
    round(segments$y, 4),
    round(segments$xend, 4),
    round(segments$yend, 4),
    sep = ":"
  )

  occurrence <- stats::ave(seq_along(keys), keys, FUN = seq_along)
  total <- stats::ave(seq_along(keys), keys, FUN = length)
  centered <- occurrence - (total + 1) / 2

  style$arrow_curvature + centered * style$arrow_parallel_curvature_step
}


move_overlay_geometry <- function(
    position,
    moves,
    style = board_style("bms"),
    perspective = "white",
    point_1_side = "right",
    bottom_home_board_side = NULL,
    point_labels_for = NULL,
    role = c("selected", "alternative")
) {
  validate_move_application_position(position)
  validate_board_moves(moves)

  if (!inherits(style, "backgammon_board_style")) {
    stop("`style` must be created by board_style().", call. = FALSE)
  }

  perspective <- normalize_board_perspective(perspective)
  point_1_side <- match.arg(point_1_side, c("right", "left"))
  role <- move_overlay_role(role)

  applied <- apply_board_moves(position, moves)
  geometry <- board_geometry(
    style,
    point_1_side = point_1_side,
    perspective = perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for
  )
  state <- position
  rows <- vector("list", nrow(applied$applied_steps))

  for (index in seq_len(nrow(applied$applied_steps))) {
    step <- applied$applied_steps[index, , drop = FALSE]

    from <- move_overlay_location_anchor(
      position = state,
      type = step$from_type[[1L]],
      point = step$from_point[[1L]],
      player = applied$player,
      geometry = geometry,
      style = style,
      perspective = perspective,
      point_1_side = point_1_side,
      bottom_home_board_side = bottom_home_board_side,
      point_labels_for = point_labels_for
    )

    one_step <- move_overlay_one_step(step)
    one_result <- apply_board_moves(state, one_step)
    next_state <- move_overlay_position_after(state, one_result)

    to <- move_overlay_location_anchor(
      position = next_state,
      type = step$to_type[[1L]],
      point = step$to_point[[1L]],
      player = applied$player,
      geometry = geometry,
      style = style,
      perspective = perspective,
      point_1_side = point_1_side,
      bottom_home_board_side = bottom_home_board_side,
      point_labels_for = point_labels_for
    )

    rows[[index]] <- data.frame(
      step_id = step$step_id[[1L]],
      chain_id = step$step_id[[1L]],
      source_token = paste0(
        format_applied_location(step$from_type[[1L]], step$from_point[[1L]]),
        "/",
        format_applied_location(step$to_type[[1L]], step$to_point[[1L]])
      ),
      # Preserve untrimmed semantic anchors for review and testing; rendered
      # endpoints are then inset so arrowheads do not cover checkers.
      source_x = unname(from[["x"]]),
      source_y = unname(from[["y"]]),
      x = unname(from[["x"]]),
      y = unname(from[["y"]]),
      # The destination ghost occupies the slot immediately above the
      # pre-move destination stack (the moved checker slot after this step).
      ghost_x = unname(to[["x"]]),
      ghost_y = unname(to[["y"]]),
      xend = unname(to[["x"]]),
      yend = unname(to[["y"]]),
      destination_x = unname(to[["x"]]),
      destination_y = unname(to[["y"]]),
      role = role,
      linetype = if (identical(role, "selected")) "solid" else "dashed",
      hit_confirmed = step$hit_confirmed[[1L]],
      stringsAsFactors = FALSE
    )

    state <- next_state
  }

  segments <- do.call(rbind, rows)
  rownames(segments) <- NULL
  segments <- move_overlay_trim_segments(
    segments,
    clearance = style$arrow_endpoint_clearance
  )
  segments$curvature <- move_overlay_duplicate_curvatures(segments, style)

  movement_labels <- attr(moves, "label")
  labelled <- which(!is.na(movement_labels) & nzchar(movement_labels))
  markers <- data.frame(
    step_id = segments$step_id[labelled],
    x = (segments$source_x[labelled] + segments$destination_x[labelled]) / 2,
    y = (segments$source_y[labelled] + segments$destination_y[labelled]) / 2,
    label = movement_labels[labelled],
    role = segments$role[labelled],
    stringsAsFactors = FALSE
  )

  hit_rows <- segments[segments$hit_confirmed, , drop = FALSE]
  hits <- data.frame(
    step_id = hit_rows$step_id,
    x = hit_rows$destination_x,
    y = hit_rows$destination_y,
    label = rep("\u00d7", nrow(hit_rows)),
    role = hit_rows$role,
    stringsAsFactors = FALSE
  )

  ghosts <- data.frame(
    step_id = segments$step_id,
    x = segments$ghost_x,
    y = segments$ghost_y,
    player = rep(applied$player, nrow(segments)),
    role = segments$role,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      role = role,
      movements = moves,
      applied_moves = applied,
      segments = segments,
      ghosts = ghosts,
      markers = markers,
      hits = hits
    ),
    class = "backgammon_move_overlay"
  )
}


move_overlay_role_color <- function(colors, role) {
  if (identical(role, "selected")) {
    colors$arrow_primary
  } else {
    colors$arrow_secondary
  }
}


add_move_ghost_layers <- function(plot, overlay, colors, style) {
  if (is.null(overlay) || nrow(overlay$ghosts) == 0L) {
    return(plot)
  }

  role_color <- move_overlay_role_color(colors, overlay$role)

  for (player in c("white", "black")) {
    ghosts <- overlay$ghosts[overlay$ghosts$player == player, , drop = FALSE]
    if (nrow(ghosts) == 0L) {
      next
    }
    palette <- player_checker_colors(colors, player)
    ghosts$radius <- style$checker_outer_radius

    plot <- plot +
      ggforce::geom_circle(
        data = ghosts,
        ggplot2::aes(x0 = x, y0 = y, r = radius),
        inherit.aes = FALSE,
        fill = grDevices::adjustcolor(palette$face, alpha.f = 0.28),
        color = role_color,
        linewidth = 0.55,
        linetype = "dashed",
        n = 180
      )
  }

  plot
}


add_one_move_curve <- function(
    plot,
    segment,
    color,
    linewidth,
    style
) {
  plot +
    ggplot2::geom_curve(
      data = segment,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      color = color,
      linewidth = linewidth,
      linetype = segment$linetype[[1L]],
      curvature = segment$curvature[[1L]],
      lineend = "round",
      arrow = grid::arrow(
        angle = 25,
        length = grid::unit(
          style$arrow_head_length_mm *
            sqrt(linewidth / style$arrow_linewidth),
          "mm"
        ),
        type = "closed"
      ),
      arrow.fill = color
    )
}


add_move_overlay_layers <- function(plot, overlay, colors, style) {
  if (is.null(overlay)) {
    return(plot)
  }

  if (!inherits(overlay, "backgammon_move_overlay")) {
    stop("`overlay` must be created by move_overlay_geometry().", call. = FALSE)
  }

  role_color <- move_overlay_role_color(colors, overlay$role)

  plot <- add_move_ghost_layers(plot, overlay, colors, style)

  for (index in seq_len(nrow(overlay$segments))) {
    segment <- overlay$segments[index, , drop = FALSE]

    plot <- add_one_move_curve(
      plot,
      segment,
      color = colors$arrow_halo_dark,
      linewidth = style$arrow_linewidth * style$arrow_halo_dark_ratio,
      style = style
    )
    plot <- add_one_move_curve(
      plot,
      segment,
      color = colors$arrow_halo_light,
      linewidth = style$arrow_linewidth * style$arrow_halo_light_ratio,
      style = style
    )
    plot <- add_one_move_curve(
      plot,
      segment,
      color = role_color,
      linewidth = style$arrow_linewidth,
      style = style
    )
  }

  if (nrow(overlay$hits) > 0L) {
    plot <- plot +
      ggplot2::geom_point(
        data = overlay$hits,
        ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,
        shape = 21,
        size = style$hit_marker_size,
        stroke = style$move_marker_border_width,
        fill = colors$arrow_marker_fill,
        color = colors$arrow_hit
      ) +
      ggplot2::geom_text(
        data = overlay$hits,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        color = colors$arrow_hit,
        size = style$move_marker_text_size,
        fontface = "bold"
      )
  }

  if (nrow(overlay$markers) > 0L) {
    plot <- plot + ggplot2::geom_text(
      data = overlay$markers,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = role_color,
      size = style$move_marker_text_size,
      fontface = "bold",
      angle = 0
    )
  }

  plot
}
