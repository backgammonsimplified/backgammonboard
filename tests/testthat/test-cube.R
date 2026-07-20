test_that("BMS cube colors and dimensions are frozen", {
  colors <- board_colors("bms")
  style <- board_style("bms")

  expect_identical(colors$cube_face, "#FFFFFF")
  expect_identical(colors$cube_text, "#111B35")
  expect_identical(colors$cube_border, "#111B35")
  expect_identical(colors$cube_offered_border, "#111B35")
  expect_identical(colors$cube_crosshair, "#D9653B")

  expect_equal(style$cube_scale, 1.05)
  expect_equal(style$cube_inner_scale, 0.76)
  expect_equal(style$cube_text_size, 3.8)
  expect_equal(style$cube_border_width, 0.3)
  expect_equal(style$cube_outside_gap, 0.08)
  expect_equal(style$cube_crosshair_length, 0.42)
  expect_equal(style$cube_crosshair_linewidth, 0.45)
  expect_equal(style$cube_crosshair_alpha, 0.80)
})


test_that("centered cube is outside the left frame and always shows 1", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expected_x <-
    geometry$frame$xmin[[1L]] -
    style$cube_scale / 2 -
    style$cube_outside_gap

  expected_y <- mean(c(
    geometry$frame$ymin[[1L]],
    geometry$frame$ymax[[1L]]
  ))

  expect_identical(cube$state, "centered")
  expect_identical(cube$value, 1L)
  expect_identical(cube$center$x_mode[[1L]], "outside")
  expect_equal(cube$center$x[[1L]], expected_x)
  expect_equal(cube$center$y[[1L]], expected_y)
})


test_that("White-owned cube aligns with the first bottom checker", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  position$cube_owner <- "white"
  position$cube_value <- 4L

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expected_y <-
    geometry$left_field$ymin[[1L]] +
    style$checker_margin +
    style$checker_outer_radius

  expect_identical(cube$state, "owned_white")
  expect_identical(cube$value, 4L)
  expect_equal(cube$center$y[[1L]], expected_y)
})


test_that("Black-owned cube aligns with the first top checker", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  position$cube_owner <- "black"
  position$cube_value <- 8L

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expected_y <-
    geometry$left_field$ymax[[1L]] -
    style$checker_margin -
    style$checker_outer_radius

  expect_identical(cube$state, "owned_black")
  expect_identical(cube$value, 8L)
  expect_equal(cube$center$y[[1L]], expected_y)
})


test_that("cube offered to White is centered inside the left playing field", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style,
    cube_state = "offered_white",
    cube_x_mode = "outside",
    cube_value = 2L
  )

  expected_x <- mean(c(
    geometry$left_field$xmin[[1L]],
    geometry$left_field$xmax[[1L]]
  ))

  expected_y <- mean(c(
    geometry$frame$ymin[[1L]],
    geometry$frame$ymax[[1L]]
  ))

  expect_identical(cube$state, "offered_white")
  expect_identical(cube$value, 2L)
  expect_identical(cube$center$x_mode[[1L]], "inside")
  expect_equal(cube$center$x[[1L]], expected_x)
  expect_equal(cube$center$y[[1L]], expected_y)
})


test_that("cube crosshair stays centered when the number is nudged", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  cube <- cube_layout(
    position = position,
    geometry = geometry,
    style = style,
    number_x_nudge = 0.1,
    number_y_nudge = -0.1
  )

  expect_equal(cube$number$x[[1L]], cube$center$x[[1L]] + 0.1)
  expect_equal(cube$number$y[[1L]], cube$center$y[[1L]] - 0.1)

  expect_true(any(
    cube$crosshair$x == cube$center$x[[1L]] &
    cube$crosshair$xend == cube$center$x[[1L]]
  ))

  expect_true(any(
    cube$crosshair$y == cube$center$y[[1L]] &
    cube$crosshair$yend == cube$center$y[[1L]]
  ))
})


test_that("invalid cube states and values are rejected", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  expect_error(
    cube_layout(
      position,
      geometry,
      style,
      cube_state = "not_a_state"
    ),
    "cube_state"
  )

  expect_error(
    cube_layout(
      position,
      geometry,
      style,
      cube_state = "offered_white",
      cube_value = 3L
    ),
    "must show one of"
  )
})


test_that("adding a cube returns a ggplot", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  colors <- board_colors("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  plot <- add_cube_layers(
    plot = ggplot2::ggplot(),
    position = position,
    geometry = geometry,
    colors = colors,
    style = style,
    show_crosshair = TRUE
  )

  expect_s3_class(plot, "ggplot")
})
