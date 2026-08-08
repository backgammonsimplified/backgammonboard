coincident_stack_plot <- function() {
  ggboard(
    "XGID=--------E----I----------b-:0:0:1:55:0:0:0:0:10",
    colors = board_colors("bs"),
    style = board_style("bs"),
    movement_style = movement_overlay_style(
      ghost_grid_inset = 0.04,
      arrowhead_length = 0.08,
      arrowhead_width = 0.05,
      arrow_checker_gap = -0.20
    ),
    moves = board_moves(
      rep(13, 5), rep(8, 5), die = rep(5, 5)
    ),
    decision = "checker_play"
  )
}


test_that("fully coincident arrows collapse to one representative", {
  plot <- coincident_stack_plot()
  segments <- attr(
    plot, "backgammon_prepared_layout"
  )$selected_overlay$segments

  expect_equal(segments$coincident_count, rep(5L, 5L))
  expect_equal(segments$coincident_group, rep(1L, 5L))
  expect_identical(segments$draw_arrow, c(TRUE, rep(FALSE, 4L)))
})


test_that("distinct stack paths retain every arrow", {
  plot <- ggboard(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`13` = 5L, `10` = 3L),
      dice = c(3L, 3L)
    ),
    moves = board_moves(rep(13, 4), rep(10, 4), die = rep(3, 4))
  )
  segments <- attr(
    plot, "backgammon_prepared_layout"
  )$selected_overlay$segments

  expect_equal(segments$coincident_count, rep(1L, 4L))
  expect_true(all(segments$draw_arrow))
})


test_that("multiplier uses on-roll typography and is the top layer", {
  plot <- coincident_stack_plot()
  style <- board_style("bs")
  colors <- board_colors("bs")
  final_layer <- tail(plot$layers, 1L)[[1L]]

  expect_s3_class(final_layer$geom, "GeomText")
  expect_equal(final_layer$data$label, "5\u00d7")
  expect_identical(final_layer$aes_params$colour, colors$on_roll_arrow)
  expect_identical(
    final_layer$aes_params$size,
    style$information_on_roll_arrow_size
  )
  expect_identical(final_layer$aes_params$family, "sans")
  expect_identical(final_layer$aes_params$fontface, "bold")
})


test_that("multiplier clears the arrow and remains inside the board", {
  plot <- coincident_stack_plot()
  final_layer <- tail(plot$layers, 1L)[[1L]]
  label <- final_layer$data
  geometry <- attr(plot, "backgammon_prepared_layout")$geometry
  frame <- geometry$frame
  clearance <- sqrt(
    (label$x - label$path_midpoint_x)^2 +
      (label$y - label$path_midpoint_y)^2
  )

  expect_gte(clearance, 0.69)
  expect_gt(label$x, frame$xmin)
  expect_lt(label$x, frame$xmax)
  expect_gt(label$y, frame$ymin)
  expect_lt(label$y, frame$ymax)
})
