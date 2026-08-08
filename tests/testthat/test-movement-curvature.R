same_line_style <- function(...) {
  arguments <- list(
    arrow_curve_enabled = TRUE,
    arrow_curve_offset = 0.22,
    arrow_curve_step = 0.18,
    arrow_curve_max = 0.65,
    arrow_collinear_angle_tolerance = 8,
    arrow_overlap_threshold = 0.40,
    arrow_curve_two_path_split = 0.5,
    arrow_checker_gap = -0.20
  )
  overrides <- list(...)
  arguments[names(overrides)] <- overrides
  do.call(movement_overlay_style, arguments)
}

movement_segments <- function(position, moves, movement_style = same_line_style()) {
  plot <- ggboard(position, moves = moves, movement_style = movement_style)
  attr(plot, "backgammon_prepared_layout")$selected_overlay$segments
}

cubic_midpoint <- function(segment) {
  c(
    x = 0.125 * segment$x + 0.375 * segment$control_x +
      0.375 * segment$control2_x + 0.125 * segment$xend,
    y = 0.125 * segment$y + 0.375 * segment$control_y +
      0.375 * segment$control2_y + 0.125 * segment$yend
  )
}

test_that("isolated crossings and non-overlapping paths stay straight", {
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`15` = 1L, `14` = 1L),
    dice = c(4L, 4L)
  )
  segments <- movement_segments(
    position,
    board_moves(c(14, 15), c(10, 11), die = c(4, 4))
  )
  expect_equal(segments$curve_level, c(0, 0))
  expect_equal(segments$curve_offset, c(0, 0))
})

test_that("overlapping chained movement lines curve only later segments", {
  position <- backgammon_position(
    "XGID=---------AA-------------b-:0:0:1:33:0:0:0:0:10"
  )
  moves <- board_moves(c(10, 9, 7, 6), c(7, 6, 4, 3), die = rep(3, 4))
  first <- movement_segments(position, moves)
  second <- movement_segments(position, moves)
  expect_equal(first$curve_level, c(0, 1, 2, 3))
  expect_equal(first$curve_offset, c(0, 0.22, 0.40, 0.58))
  expect_identical(first$curve_level, second$curve_level)
  expect_equal(first$control_x, second$control_x)
  expect_equal(first$control_y, second$control_y)
  expect_equal(first$control2_x, second$control2_x)
  expect_equal(first$control2_y, second$control2_y)
})

test_that("same-line bows point inward in both board halves", {
  top <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`18` = 1L, `17` = 1L),
      dice = c(3L, 3L)
    ),
    board_moves(c(17, 18), c(14, 15), die = c(3, 3))
  )
  bottom <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`11` = 1L, `10` = 1L),
      dice = c(3L, 3L)
    ),
    board_moves(c(10, 11), c(7, 8), die = c(3, 3))
  )
  top_midpoint <- cubic_midpoint(top[2, , drop = FALSE])
  bottom_midpoint <- cubic_midpoint(bottom[2, , drop = FALSE])
  expect_lt(top_midpoint[["y"]], mean(c(top$y[[2]], top$yend[[2]])))
  expect_gt(bottom_midpoint[["y"]], mean(c(bottom$y[[2]], bottom$yend[[2]])))
})

test_that("near-vertical overlap bows toward the horizontal centre", {
  segments <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`15` = 2L),
      dice = c(5L, 4L)
    ),
    board_moves(c(15, 15), c(10, 11), die = c(5, 4))
  )
  curved <- segments[2, , drop = FALSE]
  midpoint <- cubic_midpoint(curved)
  direct_midpoint_x <- mean(c(curved$x, curved$xend))
  expect_equal(segments$curve_level, c(0, 1))
  expect_gt(midpoint[["x"]], direct_midpoint_x)
})

test_that("curvature respects checker-radius and arrow-length caps", {
  movement_style <- same_line_style(
    arrow_curve_offset = 2,
    arrow_curve_step = 2,
    arrow_curve_max = 0.15
  )
  segments <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`18` = 1L, `17` = 1L),
      dice = c(3L, 3L)
    ),
    board_moves(c(17, 18), c(14, 15), die = c(3, 3)),
    movement_style
  )
  radius <- board_style("bs")$checker_outer_radius
  curved <- segments[2, , drop = FALSE]
  direct_length <- sqrt(
    (curved$destination_x - curved$source_x)^2 +
      (curved$destination_y - curved$source_y)^2
  )
  expect_lte(curved$curve_offset, 0.15)
  expect_lte(curved$curve_offset * radius, direct_length * 0.08)
})

test_that("cubic paths trim to checker edges and heads use final tangents", {
  gap <- 0.04
  movement_style <- same_line_style(
    arrow_checker_gap = gap,
    arrowhead_length = 0.08,
    arrowhead_width = 0.05
  )
  segments <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`18` = 1L, `17` = 1L),
      dice = c(3L, 3L)
    ),
    board_moves(c(17, 18), c(14, 15), die = c(3, 3)),
    movement_style
  )
  style <- board_style("bs")
  expected_clearance <- style$checker_outer_radius +
    style$checker_outer_ring_width + gap
  expect_equal(
    sqrt((segments$x - segments$source_x)^2 +
      (segments$y - segments$source_y)^2),
    rep(expected_clearance, 2L)
  )
  expect_equal(
    sqrt((segments$xend - segments$destination_x)^2 +
      (segments$yend - segments$destination_y)^2),
    rep(expected_clearance, 2L)
  )
  for (index in seq_len(nrow(segments))) {
    parts <- backgammonboard:::move_arrow_parts(
      segments[index, , drop = FALSE], movement_style
    )
    tip <- c(parts$head$x[[1L]], parts$head$y[[1L]])
    base_midpoint <- c(mean(parts$head$x[2:3]), mean(parts$head$y[2:3]))
    head_direction <- backgammonboard:::move_overlay_unit_vector(
      tip[[1L]] - base_midpoint[[1L]],
      tip[[2L]] - base_midpoint[[2L]]
    )
    expect_equal(
      unname(head_direction),
      unname(c(
        segments$final_tangent_x[[index]],
        segments$final_tangent_y[[index]]
      ))
    )
  }
})

test_that("adjacent collinear chain hops curve despite zero overlap", {
  segments <- movement_segments(
    custom_position(
      on_roll = "player_1", player_1 = c(`12` = 1L), dice = c(1L, 1L)
    ),
    board_moves(c(12, 11), c(11, 10), die = c(1, 1))
  )
  expect_equal(
    backgammonboard:::move_overlay_projected_overlap(
      segments[1, , drop = FALSE], segments[2, , drop = FALSE]
    ),
    0
  )
  expect_true(all(segments$same_line_chain))
  expect_true(all(segments$curve_offset > 0))
})

test_that("three and four consecutive doubles hops each retain a curve", {
  position <- custom_position(
    on_roll = "player_1", player_1 = c(`12` = 1L), dice = c(1L, 1L)
  )
  three <- movement_segments(
    position,
    board_moves(c(12, 11, 10), c(11, 10, 9), die = rep(1, 3))
  )
  four <- movement_segments(
    position,
    board_moves(c(12, 11, 10, 9), c(11, 10, 9, 8), die = rep(1, 4))
  )
  expect_true(all(three$same_line_chain))
  expect_true(all(three$curve_offset > 0))
  expect_true(all(four$same_line_chain))
  expect_true(all(four$curve_offset > 0))
  expect_equal(length(unique(four$destination_id)), 4L)
})

test_that("same-line chain bows point inward by board half", {
  top <- movement_segments(
    custom_position(
      on_roll = "player_1", player_1 = c(`24` = 1L), dice = c(1L, 1L)
    ),
    board_moves(c(24, 23), c(23, 22), die = c(1, 1))
  )
  bottom <- movement_segments(
    custom_position(
      on_roll = "player_1", player_1 = c(`12` = 1L), dice = c(1L, 1L)
    ),
    board_moves(c(12, 11), c(11, 10), die = c(1, 1))
  )
  for (index in seq_len(nrow(top))) {
    expect_lt(
      cubic_midpoint(top[index, , drop = FALSE])[["y"]],
      mean(c(top$y[[index]], top$yend[[index]]))
    )
  }
  for (index in seq_len(nrow(bottom))) {
    expect_gt(
      cubic_midpoint(bottom[index, , drop = FALSE])[["y"]],
      mean(c(bottom$y[[index]], bottom$yend[[index]]))
    )
  }
})

test_that("non-collinear consecutive moves remain straight", {
  segments <- movement_segments(
    custom_position(
      on_roll = "player_1", player_1 = c(`13` = 1L), dice = c(3L, 1L)
    ),
    board_moves(c(13, 10), c(10, 9), die = c(3, 1))
  )
  expect_false(any(segments$same_line_chain))
  expect_equal(segments$curve_offset, c(0, 0))
})

test_that("ordinary separated and shared-destination cases stay straight", {
  separated <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`13` = 1L, `6` = 1L),
      dice = c(3L, 1L)
    ),
    board_moves(c(13, 6), c(10, 5), die = c(3, 1))
  )
  stacked <- movement_segments(
    custom_position(
      on_roll = "player_1",
      player_1 = c(`13` = 5L, `10` = 3L),
      dice = c(3L, 3L)
    ),
    board_moves(rep(13, 4), rep(10, 4), die = rep(3, 4))
  )
  expect_equal(separated$curve_offset, c(0, 0))
  expect_false(any(separated$same_line_chain))
  expect_equal(stacked$curve_offset, rep(0, 4L))
  expect_false(any(stacked$same_line_chain))
})

test_that("chain turn-angle bands scale curvature deterministically", {
  geometry <- board_geometry(board_style("bs"))
  movement_style <- same_line_style()
  make_segment <- function(
      step_id, source_id, destination_id, source, destination
  ) {
    data.frame(
      step_id = step_id,
      source_id = source_id,
      destination_id = destination_id,
      source_type = "bar",
      source_point = NA_integer_,
      destination_type = "bar",
      destination_point = NA_integer_,
      source_x = source[[1L]],
      source_y = source[[2L]],
      destination_x = destination[[1L]],
      destination_y = destination[[2L]],
      stringsAsFactors = FALSE
    )
  }
  first <- make_segment(1L, "A", "B", c(0, 0), c(10, 0))
  moderate <- make_segment(
    2L, "B", "C", c(10, 0),
    c(10 + 10 * cos(9 * pi / 180), 10 * sin(9 * pi / 180))
  )
  shallow <- make_segment(
    2L, "B", "C", c(10, 0),
    c(10 + 10 * cos(16 * pi / 180), 10 * sin(16 * pi / 180))
  )
  sharp <- make_segment(
    2L, "B", "C", c(10, 0),
    c(10 + 10 * cos(30 * pi / 180), 10 * sin(30 * pi / 180))
  )
  expect_equal(
    backgammonboard:::move_overlay_chain_pair(
      first, moderate, movement_style, geometry
    )$multiplier,
    0.60
  )
  expect_equal(
    backgammonboard:::move_overlay_chain_pair(
      first, shallow, movement_style, geometry
    )$multiplier,
    0.25
  )
  expect_equal(
    backgammonboard:::move_overlay_chain_pair(
      first, sharp, movement_style, geometry
    )$multiplier,
    0
  )
})

test_that("short chained hops obey the independent short-curve cap", {
  movement_style <- same_line_style(
    arrow_curve_offset = 2,
    arrow_curve_max = 3,
    arrow_curve_length_cap = 0.40
  )
  plot <- ggboard(
    custom_position(
      on_roll = "player_1", player_1 = c(`12` = 1L), dice = c(1L, 1L)
    ),
    style = board_style("bs"),
    moves = board_moves(c(12, 11), c(11, 10), die = c(1, 1)),
    movement_style = movement_style
  )
  segments <- attr(plot, "backgammon_prepared_layout")$selected_overlay$segments
  expect_true(all(segments$same_line_chain))
  expect_lte(max(segments$curve_offset), 0.15)
})

test_that("the long-diagonal then 8/7 review chain stays straight", {
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L, `8` = 2L),
    dice = c(5L, 1L)
  )
  plot <- ggboard(
    position,
    colors = board_colors("bs"),
    style = board_style("bs"),
    moves = board_moves(c(13, 8), c(8, 7), die = c(5, 1)),
    movement_style = same_line_style(
      arrow_curve_offset = 2,
      arrow_curve_max = 3,
      arrow_curve_length_cap = 0.40
    )
  )
  segments <- attr(plot, "backgammon_prepared_layout")$selected_overlay$segments
  expect_true(all(segments$ordered_chain))
  expect_gt(segments$chain_turn_angle[[2L]], 20)
  expect_equal(segments$chain_curve_multiplier, c(0, 0))
  expect_equal(segments$curve_offset, c(0, 0))
})

test_that("stack-level turns do not curve merely collinear point centres", {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:65:0:0:0:0:10"
  )
  plot <- ggboard(
    position,
    colors = board_colors("bs"),
    style = board_style("bs"),
    moves = board_moves(c(24, 18), c(18, 13), die = c(6, 5)),
    movement_style = same_line_style(
      arrow_curve_offset = 2,
      arrow_curve_max = 3,
      arrow_curve_length_cap = 0.40
    )
  )
  segments <- attr(plot, "backgammon_prepared_layout")$selected_overlay$segments
  expect_true(all(segments$ordered_chain))
  expect_gt(segments$chain_turn_angle[[2L]], 20)
  expect_equal(segments$chain_curve_multiplier, c(0, 0))
  expect_equal(segments$curve_offset, c(0, 0))
})
