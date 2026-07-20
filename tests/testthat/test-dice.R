test_that("die pip patterns contain the correct number of pips", {
  for (value in 1:6) {
    pattern <- die_pip_pattern(value)

    expect_s3_class(pattern, "data.frame")
    expect_equal(nrow(pattern), value)
    expect_named(pattern, c("horizontal", "vertical"))
  }
})


test_that("die pip patterns reject invalid values", {
  expect_error(
    die_pip_pattern(0),
    "one integer from 1 through 6"
  )

  expect_error(
    die_pip_pattern(7),
    "one integer from 1 through 6"
  )

  expect_error(
    die_pip_pattern(2.5),
    "one integer from 1 through 6"
  )

  expect_error(
    die_pip_pattern(c(1, 2)),
    "one integer from 1 through 6"
  )

  expect_error(
    die_pip_pattern(NA_real_),
    "one integer from 1 through 6"
  )
})


test_that("BMS dice colors and dimensions are frozen", {
  colors <- board_colors("bms")
  style <- board_style("bms")

  expect_identical(colors$die_white_fill, "#FFFFFF")
  expect_identical(colors$die_white_pips, "#111B35")
  expect_identical(colors$die_white_border, "#111B35")

  expect_identical(colors$die_black_fill, "#111B35")
  expect_identical(colors$die_black_pips, "#FFFDF8")
  expect_identical(colors$die_black_border, "#081126")

  expect_equal(style$die_scale, 1.00)
  expect_equal(style$die_gap, 0.30)
  expect_equal(style$die_border_width, 0.80)
})


test_that("dice layout creates two faces and the correct pips", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  dice <- dice_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expect_equal(length(unique(dice$faces$die)), 2L)
  expect_equal(sort(unique(dice$faces$value)), c(2L, 5L))
  expect_equal(nrow(dice$pips), 7L)
})


test_that("dice layout places White dice on the right field", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  dice <- dice_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  right_center <- mean(c(
    geometry$right_field$xmin,
    geometry$right_field$xmax
  ))

  expect_equal(
    mean(range(dice$faces$x)),
    right_center,
    tolerance = 0.01
  )
})


test_that("dice layout places Black dice on the left field", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  position$on_roll <- "black"

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  dice <- dice_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  left_center <- mean(c(
    geometry$left_field$xmin,
    geometry$left_field$xmax
  ))

  expect_equal(
    mean(range(dice$faces$x)),
    left_center,
    tolerance = 0.01
  )
})


test_that("dice layout uses the frozen spacing", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  dice <- dice_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  centers <- tapply(
    dice$faces$x,
    dice$faces$die,
    mean
  )

  actual_center_distance <- abs(diff(centers))
  expected_center_distance <-
    (1.05 * style$die_scale) +
    (style$die_gap * style$die_scale)

  expect_equal(
    as.numeric(actual_center_distance),
    expected_center_distance,
    tolerance = 1e-8
  )
})


test_that("dice layout is empty when the position has no rolled dice", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:00:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  dice <- dice_layout(
    position = position,
    geometry = geometry,
    style = style
  )

  expect_equal(nrow(dice$faces), 0L)
  expect_equal(nrow(dice$pips), 0L)
})


test_that("adding dice returns a ggplot", {
  position <- backgammon_position(
    paste0(
      "XGID=-b----E-C---eE---c-e----B-",
      ":0:0:1:52:0:0:3:0:10"
    )
  )

  style <- board_style("bms")
  colors <- board_colors("bms")
  geometry <- board_geometry(style, point_1_side = "right")

  plot <- add_dice_layers(
    plot = ggplot2::ggplot(),
    position = position,
    geometry = geometry,
    colors = colors,
    style = style
  )

  expect_s3_class(plot, "ggplot")
})
