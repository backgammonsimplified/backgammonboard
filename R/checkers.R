empty_checker_rows <- function() {
  data.frame(
    area = character(),
    point = integer(),
    x = numeric(),
    y = numeric(),
    player = character(),
    total = integer(),
    count_label = character(),
    side = character(),
    stringsAsFactors = FALSE
  )
}

empty_off_rows <- function() {
  data.frame(
    player = character(),
    x = numeric(),
    y = numeric(),
    total = integer(),
    label = character(),
    stringsAsFactors = FALSE
  )
}

checker_layout <- function(
    position,
    style,
    point_1_side = c("right", "left"),
    perspective = NULL,
    bottom_home_board_side = NULL,
    point_labels_for = NULL
) {
  point_1_side <- match.arg(point_1_side)

  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  geometry <- board_geometry(
    style,
    point_1_side = point_1_side,
    perspective = perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for
  )
  bottom_player <- if (is.null(perspective)) {
    "white"
  } else {
    normalize_board_perspective(perspective)
  }
  point_layout <- geometry$point_layout
  max_visible <- as.integer(style$max_stack_visible)

  point_rows <- list()
  row_index <- 1L

  for (index in seq_len(nrow(point_layout))) {
    point <- point_layout$point[[index]]
    signed_count <- position$points[[point]]

    if (signed_count == 0L) {
      next
    }

    player <- if (signed_count > 0L) "white" else "black"
    total <- abs(signed_count)
    visible <- min(total, max_visible)
    side <- point_layout$side[[index]]
    x <- point_layout$x[[index]]

    if (identical(side, "bottom")) {
      y <-
        geometry$layout$field_ymin +
        style$checker_margin +
        style$checker_outer_radius +
        (seq_len(visible) - 1L) * style$checker_stack_step
    } else {
      y <-
        geometry$layout$field_ymax -
        style$checker_margin -
        style$checker_outer_radius -
        (seq_len(visible) - 1L) * style$checker_stack_step
    }

    count_label <- rep("", visible)
    if (total > visible) {
      count_label[[visible]] <- as.character(total)
    }

    point_rows[[row_index]] <- data.frame(
      area = rep("point", visible),
      point = rep(point, visible),
      x = rep(x, visible),
      y = y,
      player = rep(player, visible),
      total = rep(as.integer(total), visible),
      count_label = count_label,
      side = rep(side, visible),
      stringsAsFactors = FALSE
    )
    row_index <- row_index + 1L
  }

  points <- if (length(point_rows) == 0L) {
    empty_checker_rows()
  } else {
    do.call(rbind, point_rows)
  }

  bar_rows <- list()
  row_index <- 1L
  bar_x <- mean(c(geometry$layout$bar_xmin, geometry$layout$bar_xmax))

  for (player in c("white", "black")) {
    total <- unname(position$bar[[player]])

    if (total <= 0L) {
      next
    }

    visible <- min(total, max_visible)

    if (identical(player, bottom_player)) {
      y <-
        geometry$layout$field_ymin +
        style$checker_margin +
        style$checker_outer_radius +
        (seq_len(visible) - 1L) * style$checker_stack_step
      side <- "bottom"
    } else {
      y <-
        geometry$layout$field_ymax -
        style$checker_margin -
        style$checker_outer_radius -
        (seq_len(visible) - 1L) * style$checker_stack_step
      side <- "top"
    }

    count_label <- rep("", visible)
    if (total > visible) {
      count_label[[visible]] <- as.character(total)
    }

    bar_rows[[row_index]] <- data.frame(
      area = rep("bar", visible),
      point = rep(NA_integer_, visible),
      x = rep(bar_x, visible),
      y = y,
      player = rep(player, visible),
      total = rep(as.integer(total), visible),
      count_label = count_label,
      side = rep(side, visible),
      stringsAsFactors = FALSE
    )
    row_index <- row_index + 1L
  }

  bar <- if (length(bar_rows) == 0L) {
    empty_checker_rows()
  } else {
    do.call(rbind, bar_rows)
  }

  off_x <- mean(c(
    geometry$layout$right_margin_xmin,
    geometry$layout$right_margin_xmax
  ))

  top_player <- other_semantic_player(bottom_player)

  off_rows <- list()
  row_index <- 1L

  top_off <- unname(position$off[[top_player]])
  if (top_off > 0L) {
    off_rows[[row_index]] <- data.frame(
      player = top_player,
      x = off_x,
      y = style$off_marker_top_y,
      total = as.integer(top_off),
      label = as.character(top_off),
      stringsAsFactors = FALSE
    )
    row_index <- row_index + 1L
  }

  bottom_off <- unname(position$off[[bottom_player]])
  if (bottom_off > 0L) {
    off_rows[[row_index]] <- data.frame(
      player = bottom_player,
      x = off_x,
      y = style$off_marker_bottom_y,
      total = as.integer(bottom_off),
      label = as.character(bottom_off),
      stringsAsFactors = FALSE
    )
  }

  off <- if (length(off_rows) == 0L) {
    empty_off_rows()
  } else {
    do.call(rbind, off_rows)
  }

  list(points = points, bar = bar, off = off)
}
