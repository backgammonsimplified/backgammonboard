test_that("bar stacks begin beside the fifth checker and count after four", {
  style <- board_style("bms")
  position <- custom_position(
    player_0 = c(`6` = 5L),
    player_0_bar = 5L,
    dice = integer()
  )
  layout <- backgammonboard:::checker_layout(
    backgammonboard:::as_render_position(position),
    style,
    perspective = "white"
  )
  point_stack <- layout$points[layout$points$point == 6L, , drop = FALSE]
  bar_stack <- layout$bar[layout$bar$player == "white", , drop = FALSE]

  expect_equal(nrow(bar_stack), 4L)
  expect_equal(bar_stack$y[[1L]], point_stack$y[[5L]])
  expect_true(all(diff(bar_stack$y) < 0))
  expect_identical(bar_stack$count_label, c("5", "", "", ""))
})


test_that("outside checkers follow the 1-point side", {
  style <- board_style("bms")
  position <- backgammonboard:::as_render_position(custom_position(dice = integer()))
  right <- backgammonboard:::checker_layout(
    position, style, point_1_side = "right", perspective = "white"
  )
  left <- backgammonboard:::checker_layout(
    position, style, point_1_side = "left", perspective = "white"
  )
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")

  expect_true(all(right$off$x > geometry$frame$xmax))
  expect_true(all(left$off$x < geometry$frame$xmin))
})


test_that("information wording, side, arrows, and name palettes follow BMS review", {
  xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:4:2:0:7:10"
  style <- board_style("bms")
  colors <- board_colors("bms")
  position <- backgammonboard:::as_render_position(backgammon_position(xgid))
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  right <- backgammonboard:::board_information_layout(
    position, geometry, style,
    white_name = "Homey", black_name = "Foey",
    perspective = "white", information_side = "right", score_format = "both"
  )
  left <- backgammonboard:::board_information_layout(
    position, geometry, style,
    white_name = "Homey", black_name = "Foey",
    perspective = "white", information_side = "left", score_format = "both"
  )
  light <- backgammonboard:::information_name_palette("white", colors, "checker")
  dark <- backgammonboard:::information_name_palette("black", colors, "checker")

  expect_identical(right$bottom$secondary, "4 points to 7 \u00b7 3-away")
  expect_identical(right$top$secondary, "2 points to 7 \u00b7 5-away")
  expect_match(right$bottom$pip_label, "^Pip count: [0-9]+$")
  expect_false(grepl("Homey|Foey", right$bottom$pip_label))
  expect_identical(right$bottom$on_roll_arrow, "on roll \u2192")
  expect_identical(left$bottom$on_roll_arrow, "\u2190 on roll")
  expect_gt(right$bottom$player_x, geometry$canvas$xmax / 2)
  expect_lt(left$bottom$player_x, geometry$canvas$xmax / 2)
  expect_identical(light, list(
    text = colors$white_checker_fill,
    fill = colors$black_checker_fill
  ))
  expect_identical(dark, list(
    text = colors$black_checker_fill,
    fill = colors$outside_fill
  ))
})


test_that("left setup mirrors information, outside cube, and point one", {
  xgid <- fixture_xgid("centered_cube")
  right <- ggboard(xgid, perspective = "player_0", point_1_side = "right")
  left <- ggboard(xgid, perspective = "player_0", point_1_side = "left")
  style <- board_style()
  right_geometry <- backgammonboard:::board_geometry(
    style, point_1_side = "right", perspective = "white"
  )
  left_geometry <- backgammonboard:::board_geometry(
    style, point_1_side = "left", perspective = "white"
  )
  right_point_1 <- right_geometry$point_layout$x[right_geometry$point_layout$point == 1L]
  left_point_1 <- left_geometry$point_layout$x[left_geometry$point_layout$point == 1L]

  expect_identical(attr(right, "backgammon_information_side"), "right")
  expect_identical(attr(right, "backgammon_cube_display_side"), "left")
  expect_identical(attr(left, "backgammon_information_side"), "left")
  expect_identical(attr(left, "backgammon_cube_display_side"), "right")
  expect_gt(right_point_1, style$board_width / 2)
  expect_lt(left_point_1, style$board_width / 2)
})


test_that("dice remain on the roller's semantic right", {
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  homey <- backgammonboard:::as_render_position(backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:42:0:0:0:0:10"
  ))
  foey <- backgammonboard:::as_render_position(backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:-1:31:0:0:0:0:10"
  ))
  homey_dice <- backgammonboard:::dice_layout(homey, geometry, style, "white")
  foey_dice <- backgammonboard:::dice_layout(foey, geometry, style, "white")
  center <- style$board_width / 2

  expect_gt(min(homey_dice$faces$x), center)
  expect_lt(max(foey_dice$faces$x), center)
})


test_that("centered cubes sit higher and Crawford occupies the middle outside lane", {
  style <- board_style("bms")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  render <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("centered_cube"))
  )
  cube <- backgammonboard:::cube_layout(
    render, geometry, style,
    centered_y_nudge = style$cube_centered_y_nudge,
    perspective = "white", cube_display_side = "left"
  )
  crawford <- ggboard(
    fixture_xgid("crawford"),
    perspective = "player_0",
    point_1_side = "right"
  )
  layer_labels <- unlist(lapply(crawford$layers, function(layer) {
    if ("label" %in% names(layer$data)) as.character(layer$data$label) else character()
  }), use.names = FALSE)
  crawford_y <- unlist(lapply(crawford$layers, function(layer) {
    if ("label" %in% names(layer$data) && any(layer$data$label == "Crawford")) {
      layer$data$y[layer$data$label == "Crawford"]
    } else {
      numeric()
    }
  }), use.names = FALSE)

  expect_gt(cube$center$y, style$board_height / 2)
  expect_true("Crawford" %in% layer_labels)
  expect_equal(crawford_y, style$board_height / 2)
  expect_identical(attr(crawford, "backgammon_cube_display")$state, "hidden")
})
