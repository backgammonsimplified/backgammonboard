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


move_overlay_trim_straight_segments <- function(
    segments, source_clearance, destination_clearance = source_clearance
) {
  for (index in seq_len(nrow(segments))) {
    dx <- segments$xend[[index]] - segments$x[[index]]
    dy <- segments$yend[[index]] - segments$y[[index]]
    distance <- sqrt(dx^2 + dy^2)

    if (distance <= 0) {
      next
    }

    source_trim <- min(source_clearance, distance * 0.20)
    destination_trim <- min(destination_clearance, distance * 0.20)
    unit_x <- dx / distance
    unit_y <- dy / distance

    segments$x[[index]] <- segments$x[[index]] + unit_x * source_trim
    segments$y[[index]] <- segments$y[[index]] + unit_y * source_trim
    segments$xend[[index]] <- segments$xend[[index]] - unit_x * destination_trim
    segments$yend[[index]] <- segments$yend[[index]] - unit_y * destination_trim
  }

  segments
}


move_overlay_edge_depth <- function(y, geometry) {
  pmin(
    abs(y - geometry$layout$field_ymin),
    abs(geometry$layout$field_ymax - y)
  )
}


move_overlay_cross_product <- function(first, second) {
  first[[1L]] * second[[2L]] - first[[2L]] * second[[1L]]
}


move_overlay_unit_vector <- function(dx, dy) {
  distance <- sqrt(dx^2 + dy^2)
  if (!is.finite(distance) || distance <= 0) {
    return(c(x = 0, y = 0))
  }
  c(x = dx / distance, y = dy / distance)
}


move_overlay_angle_degrees <- function(first, second, undirected = FALSE) {
  value <- sum(first * second)
  if (isTRUE(undirected)) value <- abs(value)
  value <- max(-1, min(1, value))
  acos(value) * 180 / pi
}


move_overlay_projected_overlap <- function(first, second) {
  axis <- move_overlay_unit_vector(
    first$xend[[1L]] - first$x[[1L]],
    first$yend[[1L]] - first$y[[1L]]
  )
  origin <- c(first$x[[1L]], first$y[[1L]])
  project <- function(x, y) sum((c(x, y) - origin) * axis)
  first_interval <- sort(c(
    project(first$x[[1L]], first$y[[1L]]),
    project(first$xend[[1L]], first$yend[[1L]])
  ))
  second_interval <- sort(c(
    project(second$x[[1L]], second$y[[1L]]),
    project(second$xend[[1L]], second$yend[[1L]])
  ))
  overlap <- max(
    0,
    min(first_interval[[2L]], second_interval[[2L]]) -
      max(first_interval[[1L]], second_interval[[1L]])
  )
  denominator <- min(diff(first_interval), diff(second_interval))
  if (!is.finite(denominator) || denominator <= 0) return(0)
  overlap / denominator
}


move_overlay_segments_conflict <- function(
    first, second, movement_style, checker_radius
) {
  first_vector <- move_overlay_unit_vector(
    first$xend[[1L]] - first$x[[1L]],
    first$yend[[1L]] - first$y[[1L]]
  )
  second_vector <- move_overlay_unit_vector(
    second$xend[[1L]] - second$x[[1L]],
    second$yend[[1L]] - second$y[[1L]]
  )
  if (all(first_vector == 0) || all(second_vector == 0)) return(FALSE)

  tolerance <- movement_style$arrow_collinear_angle_tolerance
  directed_angle <- move_overlay_angle_degrees(first_vector, second_vector)
  # Movement ambiguity is directional: opposite or merely crossing paths do
  # not visually collapse into one movement line.
  collinear_angle <- directed_angle
  first_start <- c(first$x[[1L]], first$y[[1L]])
  first_end <- c(first$xend[[1L]], first$yend[[1L]])
  second_start <- c(second$x[[1L]], second$y[[1L]])
  second_end <- c(second$xend[[1L]], second$yend[[1L]])
  lateral_distance <- max(c(
    abs(move_overlay_cross_product(second_start - first_start, first_vector)),
    abs(move_overlay_cross_product(second_end - first_start, first_vector)),
    abs(move_overlay_cross_product(first_start - second_start, second_vector)),
    abs(move_overlay_cross_product(first_end - second_start, second_vector))
  ))
  substantially_overlaps <-
    move_overlay_projected_overlap(first, second) >=
      movement_style$arrow_overlap_threshold
  nearly_same_line <- lateral_distance <= checker_radius * 0.60
  same_source <- identical(
    first$source_id[[1L]], second$source_id[[1L]]
  )
  same_destination <- identical(
    first$destination_id[[1L]], second$destination_id[[1L]]
  )

  # Repeated arrows into one destination retain the established straight
  # treatment. They are a stacking concern, not the same-line ambiguity this
  # adaptive curve is intended to solve.
  if (same_destination) return(FALSE)

  collinear_conflict <- collinear_angle <= tolerance &&
    substantially_overlaps && nearly_same_line && !same_source

  # Near-vertical paths can share a long initial run while their endpoints
  # diverge slightly. Recognise that narrow special case without broadening
  # ordinary shared-source arrows such as two horizontal dice moves.
  near_vertical <-
    abs(first_vector[[2L]]) >= 3 * abs(first_vector[[1L]]) &&
    abs(second_vector[[2L]]) >= 3 * abs(second_vector[[1L]])
  shared_source_vertical_conflict <- same_source && near_vertical &&
    directed_angle <= tolerance

  collinear_conflict || shared_source_vertical_conflict
}


move_overlay_curve_magnitude <- function(level, movement_style) {
  absolute <- abs(level)
  if (absolute == 0) return(0)
  if (absolute < 1) {
    magnitude <- movement_style$arrow_curve_offset * absolute
  } else {
    magnitude <- movement_style$arrow_curve_offset +
      (absolute - 1) * movement_style$arrow_curve_step
  }
  sign(level) * min(magnitude, movement_style$arrow_curve_max)
}


move_overlay_inward_normal <- function(
    source, destination, board_center
) {
  direction <- move_overlay_unit_vector(
    destination[[1L]] - source[[1L]],
    destination[[2L]] - source[[2L]]
  )
  perpendicular <- c(x = -direction[[2L]], y = direction[[1L]])
  midpoint <- (source + destination) / 2
  near_vertical <- abs(direction[[2L]]) >= 3 * abs(direction[[1L]])
  desired <- if (near_vertical) {
    c(x = board_center[[1L]] - midpoint[[1L]], y = 0)
  } else {
    c(x = 0, y = board_center[[2L]] - midpoint[[2L]])
  }
  if (sum(perpendicular * desired) < 0) perpendicular <- -perpendicular
  perpendicular
}


move_overlay_conflict_components <- function(
    visible_segments, movement_style, checker_radius
) {
  count <- nrow(visible_segments)
  if (count == 0L) return(list())
  adjacency <- matrix(FALSE, nrow = count, ncol = count)
  if (count > 1L) {
    for (first in seq_len(count - 1L)) {
      for (second in seq.int(first + 1L, count)) {
        conflict <- move_overlay_segments_conflict(
          visible_segments[first, , drop = FALSE],
          visible_segments[second, , drop = FALSE],
          movement_style,
          checker_radius
        )
        adjacency[first, second] <- conflict
        adjacency[second, first] <- conflict
      }
    }
  }

  components <- list()
  visited <- rep(FALSE, count)
  for (start in seq_len(count)) {
    if (visited[[start]]) next
    queue <- start
    component <- integer()
    visited[[start]] <- TRUE
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      component <- c(component, current)
      neighbours <- which(adjacency[current, ] & !visited)
      if (length(neighbours) > 0L) {
        visited[neighbours] <- TRUE
        queue <- c(queue, neighbours)
      }
    }
    components[[length(components) + 1L]] <- component
  }
  components
}


move_overlay_chain_direction <- function(segment, geometry) {
  anchor <- function(type, point, fallback_x, fallback_y) {
    if (!identical(type, "point")) return(c(x = fallback_x, y = fallback_y))
    point_row <- geometry$point_layout[
      geometry$point_layout$point == point, , drop = FALSE
    ]
    if (nrow(point_row) != 1L) return(c(x = fallback_x, y = fallback_y))
    c(
      x = point_row$x[[1L]],
      y = if (identical(point_row$side[[1L]], "top")) {
        geometry$layout$field_ymax
      } else {
        geometry$layout$field_ymin
      }
    )
  }
  source <- anchor(
    segment$source_type[[1L]], segment$source_point[[1L]],
    segment$source_x[[1L]], segment$source_y[[1L]]
  )
  destination <- anchor(
    segment$destination_type[[1L]], segment$destination_point[[1L]],
    segment$destination_x[[1L]], segment$destination_y[[1L]]
  )
  list(
    vector = move_overlay_unit_vector(
      destination[[1L]] - source[[1L]],
      destination[[2L]] - source[[2L]]
    ),
    length = sqrt(sum((destination - source)^2))
  )
}


move_overlay_chain_curve_multiplier <- function(angle, movement_style) {
  if (angle <= movement_style$arrow_chain_full_angle) {
    return(movement_style$arrow_chain_full_multiplier)
  }
  if (angle <= movement_style$arrow_chain_moderate_angle) {
    return(movement_style$arrow_chain_moderate_multiplier)
  }
  if (angle <= movement_style$arrow_chain_max_angle) {
    return(movement_style$arrow_chain_shallow_multiplier)
  }
  0
}


move_overlay_chain_pair <- function(
    first, second, movement_style, geometry
) {
  ordered <- second$step_id[[1L]] == first$step_id[[1L]] + 1L
  connected <- identical(
    first$destination_id[[1L]], second$source_id[[1L]]
  )
  if (!ordered || !connected) {
    return(list(ordered = FALSE, angle = NA_real_, multiplier = 0))
  }
  first_direction <- move_overlay_chain_direction(first, geometry)
  second_direction <- move_overlay_chain_direction(second, geometry)
  if (all(first_direction$vector == 0) || all(second_direction$vector == 0)) {
    return(list(ordered = TRUE, angle = 180, multiplier = 0))
  }
  semantic_angle <- move_overlay_angle_degrees(
    first_direction$vector, second_direction$vector
  )
  first_visible_direction <- move_overlay_unit_vector(
    first$destination_x[[1L]] - first$source_x[[1L]],
    first$destination_y[[1L]] - first$source_y[[1L]]
  )
  second_visible_direction <- move_overlay_unit_vector(
    second$destination_x[[1L]] - second$source_x[[1L]],
    second$destination_y[[1L]] - second$source_y[[1L]]
  )
  visible_angle <- move_overlay_angle_degrees(
    first_visible_direction, second_visible_direction
  )
  # A chained path only reads as one continuous line when both its factual
  # point-to-point direction and its rendered stack-level direction agree.
  # Taking the larger turn prevents crowded stacks from manufacturing a
  # same-line chain in either representation.
  angle <- max(semantic_angle, visible_angle)
  list(
    ordered = TRUE,
    angle = angle,
    multiplier = move_overlay_chain_curve_multiplier(angle, movement_style)
  )
}


move_overlay_chain_assignments <- function(
    segments, movement_style, geometry
) {
  result <- list(
    ordered = rep(FALSE, nrow(segments)),
    angle = rep(NA_real_, nrow(segments)),
    multiplier = rep(0, nrow(segments)),
    length = rep(0, nrow(segments))
  )
  for (row in seq_len(nrow(segments))) {
    result$length[[row]] <- move_overlay_chain_direction(
      segments[row, , drop = FALSE], geometry
    )$length
  }
  if (nrow(segments) < 2L) return(result)
  for (first in seq_len(nrow(segments) - 1L)) {
    second <- first + 1L
    pair <- move_overlay_chain_pair(
      segments[first, , drop = FALSE],
      segments[second, , drop = FALSE],
      movement_style,
      geometry
    )
    if (!pair$ordered) next
    result$ordered[c(first, second)] <- TRUE
    if (is.na(result$angle[[first]])) result$angle[[first]] <- pair$angle
    result$angle[[second]] <- pair$angle
    if (result$multiplier[[first]] == 0) {
      result$multiplier[[first]] <- pair$multiplier
    }
    result$multiplier[[second]] <- pair$multiplier
  }
  result
}


move_overlay_assign_curves <- function(
    segments, visible_segments, movement_style, checker_radius, board_center,
    geometry
) {
  segments$curve_level <- rep(0, nrow(segments))
  segments$curve_offset <- rep(0, nrow(segments))
  segments$control_x <- segments$source_x +
    (segments$destination_x - segments$source_x) / 3
  segments$control_y <- segments$source_y +
    (segments$destination_y - segments$source_y) / 3
  segments$control2_x <- segments$source_x +
    2 * (segments$destination_x - segments$source_x) / 3
  segments$control2_y <- segments$source_y +
    2 * (segments$destination_y - segments$source_y) / 3
  segments$same_line_chain <- rep(FALSE, nrow(segments))
  segments$ordered_chain <- rep(FALSE, nrow(segments))
  segments$chain_turn_angle <- rep(NA_real_, nrow(segments))
  segments$chain_curve_multiplier <- rep(0, nrow(segments))

  if (!isTRUE(movement_style$arrow_curve_enabled) || nrow(segments) < 2L) {
    segments$curvature <- segments$curve_offset
    return(segments)
  }

  apply_curve <- function(
      row, level, multiplier = 1, cap_length = NULL,
      apply_short_chain_cap = FALSE
  ) {
    dx <- segments$destination_x[[row]] - segments$source_x[[row]]
    dy <- segments$destination_y[[row]] - segments$source_y[[row]]
    direct_distance <- sqrt(dx^2 + dy^2)
    requested_offset <- move_overlay_curve_magnitude(
      level, movement_style
    ) * checker_radius * multiplier
    if (is.null(cap_length)) cap_length <- direct_distance
    caps <- c(
      requested_offset,
      movement_style$arrow_curve_max * checker_radius,
      cap_length * movement_style$arrow_curve_length_cap
    )
    if (isTRUE(apply_short_chain_cap) &&
        cap_length <= movement_style$arrow_chain_short_length_radii *
          checker_radius) {
      caps <- c(
        caps,
        movement_style$arrow_chain_short_curve_max * checker_radius
      )
    }
    physical_offset <- min(caps)
    if (!is.finite(physical_offset) || physical_offset <= 0) return(invisible())
    source <- c(segments$source_x[[row]], segments$source_y[[row]])
    destination <- c(
      segments$destination_x[[row]], segments$destination_y[[row]]
    )
    inward <- move_overlay_inward_normal(source, destination, board_center)
    control_displacement <- physical_offset * 4 / 3
    segments$curve_level[[row]] <<- level
    segments$curve_offset[[row]] <<- physical_offset / checker_radius
    segments$control_x[[row]] <<- source[[1L]] + dx / 3 +
      inward[[1L]] * control_displacement
    segments$control_y[[row]] <<- source[[2L]] + dy / 3 +
      inward[[2L]] * control_displacement
    segments$control2_x[[row]] <<- source[[1L]] + 2 * dx / 3 +
      inward[[1L]] * control_displacement
    segments$control2_y[[row]] <<- source[[2L]] + 2 * dy / 3 +
      inward[[2L]] * control_displacement
  }

  chain <- move_overlay_chain_assignments(
    segments, movement_style, geometry
  )
  segments$ordered_chain <- chain$ordered
  segments$chain_turn_angle <- chain$angle
  segments$chain_curve_multiplier <- chain$multiplier
  segments$same_line_chain <- chain$multiplier > 0
  for (row in which(chain$multiplier > 0)) {
    apply_curve(
      row,
      1,
      multiplier = chain$multiplier[[row]],
      cap_length = chain$length[[row]],
      apply_short_chain_cap = TRUE
    )
  }

  # A sharp ordered turn does not receive chain curvature, but it may still
  # participate in the separate independent-overlap rule when its path truly
  # overlaps another arrow (as in the accepted M18 setup).
  independent_rows <- which(chain$multiplier <= 0)
  components <- move_overlay_conflict_components(
    visible_segments[independent_rows, , drop = FALSE],
    movement_style,
    checker_radius
  )
  for (component in components) {
    if (length(component) < 2L) next
    component <- independent_rows[component]
    stable_order <- order(
      segments$step_id[component],
      segments$source_id[component],
      segments$destination_id[component]
    )
    rows <- component[stable_order]
    # Preserve the first movement exactly. Only later paths that would merge
    # with an earlier path receive progressively shallow inward bends.
    levels <- seq_along(rows) - 1L
    segments$curve_level[rows] <- levels

    for (index in seq_along(rows)) {
      row <- rows[[index]]
      if (levels[[index]] == 0) next
      apply_curve(row, levels[[index]])
    }
  }
  segments$curvature <- segments$curve_offset
  segments
}


move_overlay_tangent_intersection <- function(
    start, start_tangent, end, end_tangent, fallback
) {
  first_direction <- start_tangent
  second_direction <- -end_tangent
  denominator <- move_overlay_cross_product(
    first_direction, second_direction
  )
  if (!is.finite(denominator) || abs(denominator) < 1e-9) return(fallback)
  distance <- move_overlay_cross_product(
    end - start, second_direction
  ) / denominator
  start + distance * first_direction
}


move_overlay_trim_curved_segments <- function(
    segments, source_clearance, destination_clearance = source_clearance
) {
  segments$final_tangent_x <- rep(0, nrow(segments))
  segments$final_tangent_y <- rep(0, nrow(segments))
  for (index in seq_len(nrow(segments))) {
    source <- c(
      segments$source_x[[index]], segments$source_y[[index]]
    )
    destination <- c(
      segments$destination_x[[index]], segments$destination_y[[index]]
    )
    control1 <- c(
      segments$control_x[[index]], segments$control_y[[index]]
    )
    control2 <- c(
      segments$control2_x[[index]], segments$control2_y[[index]]
    )
    direct_distance <- sqrt(sum((destination - source)^2))
    if (!is.finite(direct_distance) || direct_distance <= 0) next

    source_tangent <- move_overlay_unit_vector(
      control1[[1L]] - source[[1L]], control1[[2L]] - source[[2L]]
    )
    destination_tangent <- move_overlay_unit_vector(
      destination[[1L]] - control2[[1L]],
      destination[[2L]] - control2[[2L]]
    )
    source_trim <- min(source_clearance, direct_distance * 0.20)
    destination_trim <- min(destination_clearance, direct_distance * 0.20)
    drawn_start <- source + source_tangent * source_trim
    drawn_end <- destination - destination_tangent * destination_trim
    segments$x[[index]] <- drawn_start[[1L]]
    segments$y[[index]] <- drawn_start[[2L]]
    segments$xend[[index]] <- drawn_end[[1L]]
    segments$yend[[index]] <- drawn_end[[2L]]
    segments$control_x[[index]] <-
      control1[[1L]] + source_tangent[[1L]] * source_trim
    segments$control_y[[index]] <-
      control1[[2L]] + source_tangent[[2L]] * source_trim
    segments$control2_x[[index]] <-
      control2[[1L]] - destination_tangent[[1L]] * destination_trim
    segments$control2_y[[index]] <-
      control2[[2L]] - destination_tangent[[2L]] * destination_trim
    segments$final_tangent_x[[index]] <- destination_tangent[[1L]]
    segments$final_tangent_y[[index]] <- destination_tangent[[2L]]
  }
  segments
}


move_overlay_mark_coincident_segments <- function(
    segments, tolerance = 1e-8
) {
  segments$coincident_group <- seq_len(nrow(segments))
  segments$coincident_count <- rep(1L, nrow(segments))
  segments$draw_arrow <- rep(TRUE, nrow(segments))
  if (nrow(segments) < 2L) return(segments)

  geometry_columns <- c(
    "x", "y", "xend", "yend", "control_x", "control_y",
    "control2_x", "control2_y", "final_tangent_x", "final_tangent_y"
  )
  quantized <- lapply(segments[geometry_columns], function(value) {
    round(value / tolerance)
  })
  geometry_key <- do.call(
    paste,
    c(quantized, sep = ":")
  )
  semantic_key <- paste(
    segments$source_id,
    segments$destination_id,
    segments$role,
    segments$linetype,
    geometry_key,
    sep = "|"
  )
  groups <- split(seq_len(nrow(segments)), semantic_key)
  group_id <- 1L
  for (rows in groups) {
    rows <- rows[order(segments$step_id[rows])]
    segments$coincident_group[rows] <- group_id
    segments$coincident_count[rows] <- length(rows)
    if (length(rows) > 1L) segments$draw_arrow[rows[-1L]] <- FALSE
    group_id <- group_id + 1L
  }
  segments
}


move_overlay_order_destination_slots <- function(
    segments, position, player, geometry, style, perspective
) {
  groups <- split(
    seq_len(nrow(segments)),
    paste(segments$destination_type, segments$destination_point, sep = ":")
  )

  for (indices in groups) {
    destination_type <- segments$destination_type[[indices[[1L]]]]
    if (identical(destination_type, "point")) {
      destination_point <- segments$destination_point[[indices[[1L]]]]
      point <- geometry$point_layout[
        geometry$point_layout$point == destination_point,
        ,
        drop = FALSE
      ]
      if (nrow(point) != 1L) {
        stop("Unable to resolve move-overlay destination geometry.", call. = FALSE)
      }

      # The board deliberately retains the factual starting checkers while a
      # move is illustrated. Build destination slots from that same starting
      # occupancy and overlap every arrival beyond the sixth visible slot at
      # slot six.
      initial_count <- abs(position$points[[destination_point]])
      levels <- pmin(initial_count + seq_along(indices), 6L)
      direction <- if (identical(point$side[[1L]], "bottom")) 1 else -1
      base_y <- if (identical(point$side[[1L]], "bottom")) {
        geometry$layout$field_ymin + style$checker_margin +
          style$checker_outer_radius
      } else {
        geometry$layout$field_ymax - style$checker_margin -
          style$checker_outer_radius
      }
      assigned_x <- rep(point$x[[1L]], length(indices))
      assigned_y <- base_y +
        direction * (levels - 1L) * style$checker_stack_step

      source_locations <- sub("/.*$", "", segments$source_token[indices])
      if (length(unique(source_locations)) == 1L) {
        midpoint <- mean(c(
          geometry$layout$field_ymin,
          geometry$layout$field_ymax
        ))
        source_side <- if (mean(segments$source_y[indices]) < midpoint) {
          "bottom"
        } else {
          "top"
        }
        destination_side <- if (mean(assigned_y) < midpoint) "bottom" else "top"

        if (identical(source_side, destination_side)) {
          # Whenever a repeated move stays on one visual row, align stack
          # depths so lower maps to lower and upper maps to upper. This avoids
          # crossed arrows on both rows and for both players, and lets a later
          # leg of a chain originate from ghosts created by an earlier leg.
          source_depth <- move_overlay_edge_depth(
            segments$source_y[indices], geometry
          )
          slot_depth <- move_overlay_edge_depth(assigned_y, geometry)
          target_rows <- indices[order(
            source_depth, segments$step_id[indices]
          )]
          slot_order <- order(slot_depth, seq_along(slot_depth))
          assigned_x <- assigned_x[slot_order]
          assigned_y <- assigned_y[slot_order]
        } else {
          # Between rows, preserve the atomic move order: each newly
          # exposed source checker maps to the next destination slot.
          target_rows <- indices
        }
      } else {
        # Different source points retain the established edge/proximity
        # ordering that minimizes crossings (M08/M21/M23).
        source_depth <- move_overlay_edge_depth(
          segments$source_y[indices], geometry
        )
        midpoint <- mean(c(
          geometry$layout$field_ymin,
          geometry$layout$field_ymax
        ))
        source_side <- ifelse(
          segments$source_y[indices] < midpoint, "bottom", "top"
        )
        destination_side <- if (mean(assigned_y) < midpoint) "bottom" else "top"
        crosses_board <- source_side != destination_side
        horizontal_distance <- abs(
          segments$source_x[indices] - mean(assigned_x)
        )
        source_order <- order(
          crosses_board,
          source_depth,
          horizontal_distance,
          segments$step_id[indices]
        )
        slot_depth <- move_overlay_edge_depth(assigned_y, geometry)
        slot_order <- order(slot_depth, seq_along(slot_depth))
        target_rows <- indices[source_order]
        assigned_x <- assigned_x[slot_order]
        assigned_y <- assigned_y[slot_order]
      }

      for (name in c("ghost_x", "xend", "destination_x")) {
        segments[[name]][target_rows] <- assigned_x
      }
      for (name in c("ghost_y", "yend", "destination_y")) {
        segments[[name]][target_rows] <- assigned_y
      }
      next
    }

    if (identical(destination_type, "off")) {
      existing <- unname(position$off[[player]])
      levels <- if (existing > 0L) seq_along(indices) else seq_along(indices) - 1L
      direction <- if (identical(player, perspective)) 1 else -1
      base_y <- if (identical(player, perspective)) {
        style$off_marker_bottom_y
      } else {
        style$off_marker_top_y
      }
      slot_x <- rep(segments$destination_x[[indices[[1L]]]], length(indices))
      slot_y <- base_y + direction * levels * style$checker_stack_step
    }

    source_depth <- move_overlay_edge_depth(
      segments$source_y[indices], geometry
    )
    horizontal_distance <- abs(
      segments$source_x[indices] - mean(slot_x)
    )
    source_order <- order(
      source_depth,
      horizontal_distance,
      segments$step_id[indices]
    )
    slot_depth <- move_overlay_edge_depth(slot_y, geometry)
    slot_order <- order(slot_depth, seq_along(slot_depth))

    target_rows <- indices[source_order]
    assigned_x <- slot_x[slot_order]
    assigned_y <- slot_y[slot_order]
    for (name in c("ghost_x", "xend", "destination_x")) {
      segments[[name]][target_rows] <- assigned_x
    }
    for (name in c("ghost_y", "yend", "destination_y")) {
      segments[[name]][target_rows] <- assigned_y
    }
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
    movement_style = movement_overlay_style(arrow_checker_gap = 0),
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
  if (!inherits(movement_style, "backgammon_movement_overlay_style")) {
    stop("`movement_style` must be created by movement_overlay_style().", call. = FALSE)
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
    if (isTRUE(step$hit_confirmed[[1L]])) {
      opponent <- if (identical(applied$player, "white")) "black" else "white"
      to <- move_overlay_location_anchor(
        position = state,
        type = step$to_type[[1L]],
        point = step$to_point[[1L]],
        player = opponent,
        geometry = geometry,
        style = style,
        perspective = perspective,
        point_1_side = point_1_side,
        bottom_home_board_side = bottom_home_board_side,
        point_labels_for = point_labels_for
      )
      destination_point <- geometry$point_layout[
        geometry$point_layout$point == step$to_point[[1L]],
        ,
        drop = FALSE
      ]
      stack_direction <- if (identical(destination_point$side[[1L]], "bottom")) {
        1
      } else {
        -1
      }
      to[["y"]] <- to[["y"]] + stack_direction * style$checker_stack_step
    }

    ghost_visible <- TRUE

    rows[[index]] <- data.frame(
      step_id = step$step_id[[1L]],
      chain_id = step$step_id[[1L]],
      source_id = format_applied_location(
        step$from_type[[1L]], step$from_point[[1L]]
      ),
      source_type = step$from_type[[1L]],
      source_point = step$from_point[[1L]],
      destination_id = format_applied_location(
        step$to_type[[1L]], step$to_point[[1L]]
      ),
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
      destination_type = step$to_type[[1L]],
      destination_point = step$to_point[[1L]],
      role = role,
      linetype = if (identical(role, "selected")) "solid" else "dashed",
      hit_confirmed = step$hit_confirmed[[1L]],
      ghost_visible = ghost_visible,
      stringsAsFactors = FALSE
    )

    state <- next_state
  }

  segments <- do.call(rbind, rows)
  rownames(segments) <- NULL
  segments <- move_overlay_order_destination_slots(
    segments = segments,
    position = position,
    player = applied$player,
    geometry = geometry,
    style = style,
    perspective = perspective
  )
  destination_clearance <- style$checker_outer_radius +
    style$checker_outer_ring_width + movement_style$arrow_checker_gap
  if (destination_clearance < 0) {
    stop(
      "`arrow_checker_gap` moves the arrow past the destination center.",
      call. = FALSE
    )
  }
  visible_segments <- move_overlay_trim_straight_segments(
    segments,
    source_clearance = destination_clearance,
    destination_clearance = destination_clearance
  )
  segments <- move_overlay_assign_curves(
    segments = segments,
    visible_segments = visible_segments,
    movement_style = movement_style,
    checker_radius = style$checker_outer_radius,
    board_center = c(style$board_width / 2, style$board_height / 2),
    geometry = geometry
  )
  segments <- move_overlay_trim_curved_segments(
    segments,
    source_clearance = destination_clearance,
    destination_clearance = destination_clearance
  )
  segments <- move_overlay_mark_coincident_segments(segments)

  # Move notation belongs outside the board image. The overlay itself shows
  # only the starting checkers, arrows, and destination ghosts.
  markers <- data.frame(
    step_id = integer(),
    x = numeric(),
    y = numeric(),
    label = character(),
    role = character(),
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

  ghost_rows <- segments$ghost_visible
  ghosts <- data.frame(
    step_id = segments$step_id[ghost_rows],
    x = segments$ghost_x[ghost_rows],
    y = segments$ghost_y[ghost_rows],
    player = rep(applied$player, sum(ghost_rows)),
    role = segments$role[ghost_rows],
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


add_move_ghost_layers <- function(plot, overlay, style, movement_style) {
  if (is.null(overlay) || nrow(overlay$ghosts) == 0L) {
    return(plot)
  }

  ghosts <- overlay$ghosts
  ghost_fill <- if (is.na(movement_style$ghost_fill)) {
    NA
  } else {
    grDevices::adjustcolor(
      movement_style$ghost_fill,
      alpha.f = movement_style$ghost_fill_alpha
    )
  }
  # Use the factual checker's outer-body radius. The configurable outline is
  # centered on this path, bringing the complete ghost marker into line with
  # the factual checker's filled outer ring instead of making it oversized.
  ghosts$radius <- style$checker_outer_radius
  pattern_radius <- ghosts$radius[[1L]] - movement_style$ghost_grid_inset
  if (pattern_radius <= 0) {
    stop("`ghost_grid_inset` must be smaller than the ghost radius.", call. = FALSE)
  }
  x_offsets <- seq(
    -pattern_radius, pattern_radius,
    length.out = movement_style$ghost_grid_cols
  )
  y_offsets <- seq(
    -pattern_radius, pattern_radius,
    length.out = movement_style$ghost_grid_rows
  )
  dot_template <- expand.grid(dx = x_offsets, dy = y_offsets)
  dot_template <- dot_template[
    sqrt(dot_template$dx^2 + dot_template$dy^2) <= pattern_radius,
    ,
    drop = FALSE
  ]
  dot_rows <- vector("list", nrow(ghosts))
  for (ghost_index in seq_len(nrow(ghosts))) {
    dot_rows[[ghost_index]] <- data.frame(
      x = ghosts$x[[ghost_index]] + dot_template$dx,
      y = ghosts$y[[ghost_index]] + dot_template$dy
    )
  }
  dot_data <- do.call(rbind, dot_rows)

  plot <- plot +
    ggforce::geom_circle(
      data = ghosts,
      ggplot2::aes(x0 = x, y0 = y, r = radius),
      inherit.aes = FALSE,
      fill = ghost_fill,
      color = NA,
      n = 180
    ) +
    ggplot2::geom_point(
      data = dot_data,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      color = movement_style$ghost_dot_colour,
      alpha = movement_style$ghost_dot_alpha,
      shape = 16,
      size = movement_style$ghost_dot_size
    ) +
    ggforce::geom_circle(
      data = ghosts,
      ggplot2::aes(x0 = x, y0 = y, r = radius),
      inherit.aes = FALSE,
      fill = NA,
      color = movement_style$ghost_outline,
      linewidth = movement_style$ghost_outline_width,
      linetype = "solid",
      n = 180
    )

  plot
}


move_arrow_parts <- function(segment, movement_style) {
  ux <- segment$final_tangent_x[[1L]]
  uy <- segment$final_tangent_y[[1L]]
  px <- -uy
  py <- ux
  base_x <- segment$xend[[1L]] - movement_style$arrowhead_length * ux
  base_y <- segment$yend[[1L]] - movement_style$arrowhead_length * uy
  half_width <- movement_style$arrowhead_width / 2

  sample_count <- if (abs(segment$curve_offset[[1L]]) > 0) 61L else 2L
  parameter <- seq(0, 1, length.out = sample_count)
  inverse <- 1 - parameter
  shaft <- data.frame(
    x = inverse^3 * segment$x[[1L]] +
      3 * inverse^2 * parameter * segment$control_x[[1L]] +
      3 * inverse * parameter^2 * segment$control2_x[[1L]] +
      parameter^3 * segment$xend[[1L]],
    y = inverse^3 * segment$y[[1L]] +
      3 * inverse^2 * parameter * segment$control_y[[1L]] +
      3 * inverse * parameter^2 * segment$control2_y[[1L]] +
      parameter^3 * segment$yend[[1L]],
    group = 1L,
    linetype = segment$linetype[[1L]]
  )

  list(
    shaft = shaft,
    head = data.frame(
      x = c(
        segment$xend[[1L]],
        base_x + half_width * px,
        base_x - half_width * px
      ),
      y = c(
        segment$yend[[1L]],
        base_y + half_width * py,
        base_y - half_width * py
      ),
      group = 1L
    )
  )
}


add_one_move_arrow <- function(
    plot, parts, color, alpha, linewidth, lineend
) {
  plot +
    ggplot2::geom_path(
      data = parts$shaft,
      ggplot2::aes(x = x, y = y, group = group),
      inherit.aes = FALSE,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      linetype = parts$shaft$linetype,
      lineend = lineend,
      linejoin = "round",
      show.legend = FALSE
    ) +
    ggplot2::geom_polygon(
      data = parts$head,
      ggplot2::aes(x = x, y = y, group = group),
      inherit.aes = FALSE,
      fill = color,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      linejoin = "round",
      show.legend = FALSE
    )
}


add_move_arrow_layers <- function(plot, overlay, movement_style) {
  if (is.null(overlay)) {
    return(plot)
  }

  if (!inherits(overlay, "backgammon_move_overlay")) {
    stop("`overlay` must be created by move_overlay_geometry().", call. = FALSE)
  }

  arrow_rows <- if ("draw_arrow" %in% names(overlay$segments)) {
    which(overlay$segments$draw_arrow)
  } else {
    seq_len(nrow(overlay$segments))
  }
  for (index in arrow_rows) {
    segment <- overlay$segments[index, , drop = FALSE]
    parts <- move_arrow_parts(segment, movement_style)

    if (movement_style$arrow_outline_width > 0) {
      plot <- add_one_move_arrow(
        plot, parts,
        color = movement_style$arrow_outline_colour,
        alpha = movement_style$arrow_alpha,
        linewidth = movement_style$arrow_width +
          2 * movement_style$arrow_outline_width,
        lineend = movement_style$arrow_lineend
      )
    }
    plot <- add_one_move_arrow(
      plot, parts,
      color = movement_style$arrow_colour,
      alpha = movement_style$arrow_alpha,
      linewidth = movement_style$arrow_width,
      lineend = movement_style$arrow_lineend
    )
  }

  plot
}


move_overlay_multiplier_labels <- function(overlay, geometry, style) {
  if (is.null(overlay) || nrow(overlay$segments) == 0L ||
      !"coincident_count" %in% names(overlay$segments)) {
    return(data.frame())
  }
  segments <- overlay$segments[
    overlay$segments$draw_arrow & overlay$segments$coincident_count > 1L,
    ,
    drop = FALSE
  ]
  if (nrow(segments) == 0L) return(data.frame())

  frame <- geometry$frame
  board_center <- c(
    mean(c(frame$xmin, frame$xmax)),
    mean(c(frame$ymin, frame$ymax))
  )
  label_offset <- max(0.70, style$checker_outer_radius * 1.75)
  rows <- vector("list", nrow(segments))
  for (index in seq_len(nrow(segments))) {
    segment <- segments[index, , drop = FALSE]
    parameter <- 0.5
    inverse <- 1 - parameter
    midpoint <- c(
      x = inverse^3 * segment$x[[1L]] +
        3 * inverse^2 * parameter * segment$control_x[[1L]] +
        3 * inverse * parameter^2 * segment$control2_x[[1L]] +
        parameter^3 * segment$xend[[1L]],
      y = inverse^3 * segment$y[[1L]] +
        3 * inverse^2 * parameter * segment$control_y[[1L]] +
        3 * inverse * parameter^2 * segment$control2_y[[1L]] +
        parameter^3 * segment$yend[[1L]]
    )
    tangent <- move_overlay_unit_vector(
      3 * inverse^2 *
        (segment$control_x[[1L]] - segment$x[[1L]]) +
        6 * inverse * parameter *
        (segment$control2_x[[1L]] - segment$control_x[[1L]]) +
        3 * parameter^2 *
        (segment$xend[[1L]] - segment$control2_x[[1L]]),
      3 * inverse^2 *
        (segment$control_y[[1L]] - segment$y[[1L]]) +
        6 * inverse * parameter *
        (segment$control2_y[[1L]] - segment$control_y[[1L]]) +
        3 * parameter^2 *
        (segment$yend[[1L]] - segment$control2_y[[1L]])
    )
    normal <- c(x = -tangent[[2L]], y = tangent[[1L]])
    if (sum(normal * (board_center - midpoint)) < 0) normal <- -normal
    label_position <- midpoint + normal * label_offset
    # Keep the complete label visibly inside the board frame.
    label_position[[1L]] <- min(
      frame$xmax - 0.80, max(frame$xmin + 0.80, label_position[[1L]])
    )
    label_position[[2L]] <- min(
      frame$ymax - 0.55, max(frame$ymin + 0.55, label_position[[2L]])
    )
    rows[[index]] <- data.frame(
      group = segment$coincident_group[[1L]],
      count = segment$coincident_count[[1L]],
      label = paste0(segment$coincident_count[[1L]], "\u00d7"),
      x = label_position[[1L]],
      y = label_position[[2L]],
      path_midpoint_x = midpoint[[1L]],
      path_midpoint_y = midpoint[[2L]],
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}


add_move_multiplier_layers <- function(
    plot, overlay, geometry, colors, style, family = "sans"
) {
  labels <- move_overlay_multiplier_labels(overlay, geometry, style)
  if (nrow(labels) == 0L) return(plot)
  plot + ggplot2::geom_text(
    data = labels,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = colors$on_roll_arrow,
    size = style$information_on_roll_arrow_size,
    family = family,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5,
    show.legend = FALSE
  )
}


add_move_overlay_layers <- function(
    plot, overlay, geometry, colors, style, movement_style, family = "sans"
) {
  plot <- add_move_ghost_layers(plot, overlay, style, movement_style)
  plot <- add_move_arrow_layers(plot, overlay, movement_style)
  add_move_multiplier_layers(
    plot, overlay, geometry, colors, style, family
  )
}
