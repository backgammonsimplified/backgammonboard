board_layout <- function(style) {
  validate_board_style(unclass(style))

  frame_xmin <- style$left_margin_width
  frame_xmax <- style$board_width - style$right_margin_width
  field_ymin <- style$rail_size
  field_ymax <- style$board_height - style$rail_size

  left_play_xmin <- frame_xmin + style$rail_size
  left_play_xmax <- left_play_xmin + style$play_half_width

  bar_xmin <- left_play_xmax
  bar_xmax <- bar_xmin + style$bar_width

  right_play_xmin <- bar_xmax
  right_play_xmax <- right_play_xmin + style$play_half_width

  expected_right_edge <- frame_xmax - style$rail_size
  if (abs(right_play_xmax - expected_right_edge) > 1e-8) {
    stop("The board geometry is internally inconsistent.", call. = FALSE)
  }

  list(
    frame_xmin = frame_xmin,
    frame_xmax = frame_xmax,
    field_ymin = field_ymin,
    field_ymax = field_ymax,
    left_play_xmin = left_play_xmin,
    left_play_xmax = left_play_xmax,
    bar_xmin = bar_xmin,
    bar_xmax = bar_xmax,
    right_play_xmin = right_play_xmin,
    right_play_xmax = right_play_xmax,
    left_margin_xmin = 0,
    left_margin_xmax = frame_xmin,
    right_margin_xmin = frame_xmax,
    right_margin_xmax = style$board_width
  )
}

point_layout_table <- function(
    style,
    point_1_side = c("right", "left"),
    perspective = NULL
) {
  point_1_side <- match.arg(point_1_side)
  layout <- board_layout(style)

  left_step <- style$play_half_width / 6
  right_step <- style$play_half_width / 6

  left_centers <-
    layout$left_play_xmin + (seq_len(6) - 0.5) * left_step
  right_centers <-
    layout$right_play_xmin + (seq_len(6) - 0.5) * right_step

  if (!is.null(perspective)) {
    perspective <- normalize_board_perspective(perspective)

    if (identical(perspective, "white")) {
      bottom_left <- 12:7
      bottom_right <- 6:1
      top_left <- 13:18
      top_right <- 19:24
    } else {
      bottom_left <- 24:19
      bottom_right <- 18:13
      top_left <- 1:6
      top_right <- 7:12
    }
  } else if (identical(point_1_side, "right")) {
    bottom_left <- 12:7
    bottom_right <- 6:1
    top_left <- 13:18
    top_right <- 19:24
  } else {
    bottom_left <- 1:6
    bottom_right <- 7:12
    top_left <- 24:19
    top_right <- 18:13
  }

  rbind(
    data.frame(
      point = bottom_left,
      x = left_centers,
      side = "bottom",
      base_width = rep(left_step - style$point_gap, 6L),
      stringsAsFactors = FALSE
    ),
    data.frame(
      point = bottom_right,
      x = right_centers,
      side = "bottom",
      base_width = rep(right_step - style$point_gap, 6L),
      stringsAsFactors = FALSE
    ),
    data.frame(
      point = top_left,
      x = left_centers,
      side = "top",
      base_width = rep(left_step - style$point_gap, 6L),
      stringsAsFactors = FALSE
    ),
    data.frame(
      point = top_right,
      x = right_centers,
      side = "top",
      base_width = rep(right_step - style$point_gap, 6L),
      stringsAsFactors = FALSE
    )
  )
}

point_polygon_data <- function(layout, style) {
  middle <- style$board_height / 2
  bottom_tip <- middle - style$point_tip_gap / 2
  top_tip <- middle + style$point_tip_gap / 2
  board <- board_layout(style)

  polygons <- lapply(seq_len(nrow(layout)), function(index) {
    item <- layout[index, , drop = FALSE]
    half_width <- item$base_width[[1L]] / 2

    if (item$side[[1L]] == "bottom") {
      y <- c(board$field_ymin, board$field_ymin, bottom_tip)
    } else {
      y <- c(board$field_ymax, board$field_ymax, top_tip)
    }

    data.frame(
      point = rep(item$point[[1L]], 3L),
      group = rep(item$point[[1L]], 3L),
      x = c(
        item$x[[1L]] - half_width,
        item$x[[1L]] + half_width,
        item$x[[1L]]
      ),
      y = y,
      point_role = if (item$point[[1L]] %% 2L == 0L) "light" else "dark",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, polygons)
}

point_number_data <- function(layout, style) {
  data.frame(
    point = layout$point,
    x = layout$x,
    y = ifelse(
      layout$side == "bottom",
      style$point_number_inset,
      style$board_height - style$point_number_inset
    ),
    label = as.character(layout$point),
    side = layout$side,
    stringsAsFactors = FALSE
  )
}

board_geometry <- function(
    style,
    point_1_side = c("right", "left"),
    perspective = NULL
) {
  point_1_side <- match.arg(point_1_side)
  layout <- board_layout(style)
  point_layout <- point_layout_table(
    style,
    point_1_side = point_1_side,
    perspective = perspective
  )

  list(
    canvas = data.frame(
      xmin = 0,
      xmax = style$board_width,
      ymin = 0,
      ymax = style$board_height
    ),
    frame = data.frame(
      xmin = layout$frame_xmin,
      xmax = layout$frame_xmax,
      ymin = 0,
      ymax = style$board_height
    ),
    left_field = data.frame(
      xmin = layout$left_play_xmin,
      xmax = layout$left_play_xmax,
      ymin = layout$field_ymin,
      ymax = layout$field_ymax
    ),
    bar = data.frame(
      xmin = layout$bar_xmin,
      xmax = layout$bar_xmax,
      ymin = layout$field_ymin,
      ymax = layout$field_ymax
    ),
    right_field = data.frame(
      xmin = layout$right_play_xmin,
      xmax = layout$right_play_xmax,
      ymin = layout$field_ymin,
      ymax = layout$field_ymax
    ),
    points = point_polygon_data(point_layout, style),
    point_labels = point_number_data(point_layout, style),
    point_layout = point_layout,
    layout = layout
  )
}
