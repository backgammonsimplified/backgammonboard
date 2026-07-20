test_that("pip counts use semantic White and Black distances", {
  points <- integer(24L)
  points[[1L]] <- 2L
  points[[24L]] <- -3L

  position <- structure(
    list(
      points = points,
      bar = c(white = 1L, black = 2L)
    ),
    class = "backgammon_position"
  )

  expect_identical(position_pip_count(position, "white"), 27L)
  expect_identical(position_pip_count(position, "black"), 53L)
})


test_that("BMS information typography and spacing are frozen", {
  colors <- board_colors("bms")
  style <- board_style("bms")

  expect_identical(colors$score_text, "#111B35")
  expect_identical(colors$secondary_text, "#566078")
  expect_identical(colors$status_text, "#111B35")

  expect_equal(style$information_player_name_size, 5.6)
  expect_equal(style$information_secondary_text_size, 4.5)
  expect_equal(style$information_pip_text_size, 5.0)
  expect_equal(style$information_sentence_text_size, 4.7)
  expect_equal(style$information_top_band_height, 1.10)
  expect_equal(style$information_bottom_band_height, 1.62)
  expect_equal(style$information_pip_offset, 0.42)
  expect_equal(style$information_top_player_name_offset, 0.86)
  expect_equal(style$information_top_secondary_offset, 0.42)
  expect_equal(style$information_bottom_secondary_offset, 0.35)
  expect_equal(style$information_bottom_player_name_offset, 0.82)
  expect_equal(style$information_sentence_offset, 1.18)
  expect_equal(style$information_player_x_nudge, -0.15)
  expect_equal(style$information_sentence_x_nudge, 0.00)
})


test_that("match information uses the frozen short centered sentence", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:6:2:1:7:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expect_identical(
    information$sentence$context[[1L]],
    "7-pt Match, White 6 - Black 2, Crawford,"
  )

  expect_identical(
    information$sentence$status[[1L]],
    "White on roll, to play: 5-2"
  )

  expect_identical(information$top$secondary[[1L]], "5-away")
  expect_identical(information$bottom$secondary[[1L]], "1-away")

  expected_center <- mean(c(
    geometry$frame$xmin[[1L]],
    geometry$frame$xmax[[1L]]
  ))

  expect_equal(information$sentence$x[[1L]], expected_center)
})


test_that("money-game information retains win counts", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:51:6:2:0:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expect_identical(information$sentence$context[[1L]], "Unlimited game,")
  expect_identical(
    information$sentence$status[[1L]],
    "White on roll, to play: 5-1"
  )
  expect_identical(information$top$secondary[[1L]], "2 wins")
  expect_identical(information$bottom$secondary[[1L]], "6 wins")
})


test_that("money-game win counts can be overridden", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:51:0:0:0:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style,
    white_wins = 1L,
    black_wins = 4L
  )

  expect_identical(information$top$secondary[[1L]], "4 wins")
  expect_identical(information$bottom$secondary[[1L]], "1 win")
})


test_that("pip labels have matching distances and plain text", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:51:6:2:1:7:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  top_distance <-
    information$top$pip_y[[1L]] - geometry$frame$ymax[[1L]]

  bottom_distance <-
    geometry$frame$ymin[[1L]] - information$bottom$pip_y[[1L]]

  expect_equal(top_distance, bottom_distance)
  expect_match(information$top$pip_label[[1L]], "^Black pips: ")
  expect_match(information$bottom$pip_label[[1L]], "^White pips: ")
})


test_that("status labels cover roll, double, redouble, and offer states", {
  money <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:00:0:0:0:0:10"
    )
  )

  expect_identical(
    position_status_label(money),
    "White on roll, cube action: Roll or Double?"
  )

  money$cube_owner <- "white"
  money$cube_value <- 2L

  expect_identical(
    position_status_label(money),
    "White on roll, cube action: Roll or Redouble?"
  )

  money$cube_owner <- "black"

  expect_identical(
    position_status_label(money),
    "White on roll"
  )

  offered <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":1:1:-1:D:0:0:0:0:10"
    )
  )

  expect_identical(
    position_status_label(offered),
    "Black on roll, cube offered: Take or Pass?"
  )
})


test_that("adding board information returns a ggplot", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:51:6:2:1:7:10"
    )
  )

  style <- board_style("bms")
  colors <- board_colors("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  plot <- add_board_information(
    plot = ggplot2::ggplot(),
    position = position,
    geometry = geometry,
    colors = colors,
    style = style
  )

  expect_s3_class(plot, "ggplot")
})
