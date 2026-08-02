test_that("canonical geometry contains 24 grouped points", {
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style)

  expect_equal(nrow(geometry$points), 72L)
  expect_setequal(unique(geometry$points$point), 1:24)
  expect_equal(length(unique(geometry$points$group)), 24L)
  expect_equal(nrow(geometry$point_labels), 24L)

  bottom <- geometry$point_layout$point[geometry$point_layout$side == "bottom"]
  top <- geometry$point_layout$point[geometry$point_layout$side == "top"]
  expect_identical(bottom, c(12:7, 6:1))
  expect_identical(top, c(13:18, 19:24))
})

test_that("alternate point-one side mirrors the numbering convention", {
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style, point_1_side = "left")

  bottom <- geometry$point_layout$point[geometry$point_layout$side == "bottom"]
  top <- geometry$point_layout$point[geometry$point_layout$side == "top"]
  expect_identical(bottom, c(1:6, 7:12))
  expect_identical(top, c(24:19, 18:13))
})

test_that("compact board has equal rails and no internal side panels", {
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style)

  expect_equal(geometry$frame$xmin, style$left_margin_width)
  expect_equal(
    geometry$frame$xmax,
    style$board_width - style$right_margin_width
  )
  expect_equal(
    geometry$left_field$xmin - geometry$frame$xmin,
    style$rail_size
  )
  expect_equal(
    geometry$frame$xmax - geometry$right_field$xmax,
    style$rail_size
  )
  expect_equal(geometry$left_field$ymin, style$rail_size)
  expect_equal(geometry$left_field$ymax, style$board_height - style$rail_size)
  expect_equal(geometry$left_field$xmax, geometry$bar$xmin)
  expect_equal(geometry$bar$xmax, geometry$right_field$xmin)
  expect_false("left_panel" %in% names(geometry))
  expect_false("right_panel" %in% names(geometry))
})

test_that("opening checker layout preserves all point checkers", {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10"
  )
  layout <- backgammonboard:::checker_layout(position, board_style("bms"))

  expect_equal(nrow(layout$points), 30L)
  expect_equal(sum(layout$points$player == "white"), 15L)
  expect_equal(sum(layout$points$player == "black"), 15L)
  expect_equal(nrow(layout$bar), 0L)
  expect_equal(nrow(layout$off), 0L)
})

test_that("asymmetric layout shows bar, off counts, and excess label", {
  position <- backgammon_position(
    "XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10"
  )
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style)
  layout <- backgammonboard:::checker_layout(position, style)

  expect_equal(nrow(layout$bar), 1L)
  expect_identical(layout$bar$player, "black")
  expect_equal(nrow(layout$off), 2L)
  expect_identical(layout$off$total[layout$off$player == "white"], 3L)
  expect_identical(layout$off$total[layout$off$player == "black"], 5L)
  expect_true(all(layout$off$x > geometry$frame$xmax))

  point_one <- layout$points[layout$points$point == 1L, , drop = FALSE]
  expect_equal(nrow(point_one), 5L)
  expect_identical(tail(point_one$count_label, 1L), "6")
})


test_that("bar checkers stay in their owner half under either orientation", {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10"
  )
  position$bar <- c(white = 2L, black = 3L)
  position$off <- c(white = 13L, black = 12L)
  style <- board_style("bms")

  for (perspective in c("white", "black")) {
    geometry <- backgammonboard:::board_geometry(style, perspective = perspective)
    bar <- backgammonboard:::checker_layout(position, style, perspective = perspective)$bar
    midpoint <- mean(c(geometry$bar$ymin, geometry$bar$ymax))

    expect_true(all(bar$x > geometry$bar$xmin & bar$x < geometry$bar$xmax))
    expect_true(all(bar$y[bar$player == perspective] < midpoint))
    expect_true(all(bar$y[bar$player != perspective] > midpoint))
    expect_equal(
      mean(bar$y[bar$player == perspective]),
      mean(c(geometry$bar$ymin, midpoint))
    )
    expect_equal(
      mean(bar$y[bar$player != perspective]),
      mean(c(midpoint, geometry$bar$ymax))
    )
  }
})

test_that("borne-off markers use normal checker dimensions", {
  style <- board_style("bms")

  expect_identical(style$off_marker_outer_radius, style$checker_outer_radius)
  expect_identical(style$off_marker_face_radius, style$checker_face_radius)
})

test_that("preview renderer returns a ggplot", {
  plot <- backgammonboard:::render_board_preview(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10"
  )
  expect_s3_class(plot, "ggplot")
})
