test_that("semantic perspective rotates point labels without mutating facts", {
  position <- backgammon_position(
    "XGID=-FDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10"
  )
  original <- position
  style <- board_style("bms")

  white <- board_geometry(style, perspective = "white")$point_layout
  black <- board_geometry(style, perspective = "black")$point_layout

  expect_identical(white$point[white$side == "bottom"], c(12:7, 6:1))
  expect_identical(black$point[black$side == "bottom"], c(24:19, 18:13))
  expect_identical(position, original)
})


test_that("bar, off, dice, and information rows follow the selected perspective", {
  position <- backgammon_position(
    "XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10"
  )
  style <- board_style("bms")

  white_geometry <- board_geometry(style, perspective = "white")
  black_geometry <- board_geometry(style, perspective = "black")

  white_checkers <- checker_layout(position, style, perspective = "white")
  black_checkers <- checker_layout(position, style, perspective = "black")

  expect_identical(
    unique(white_checkers$bar$side[white_checkers$bar$player == "black"]),
    "top"
  )
  expect_identical(
    unique(black_checkers$bar$side[black_checkers$bar$player == "black"]),
    "bottom"
  )

  expect_true(white_checkers$off$y[white_checkers$off$player == "white"] < style$board_height / 2)
  expect_true(black_checkers$off$y[black_checkers$off$player == "white"] > style$board_height / 2)

  white_dice <- dice_layout(position, white_geometry, style, perspective = "white")
  black_dice <- dice_layout(position, black_geometry, style, perspective = "black")
  white_right_center <- mean(c(white_geometry$right_field$xmin, white_geometry$right_field$xmax))
  black_left_center <- mean(c(black_geometry$left_field$xmin, black_geometry$left_field$xmax))

  expect_equal(mean(range(white_dice$faces$x)), white_right_center, tolerance = 1e-7)
  expect_equal(mean(range(black_dice$faces$x)), black_left_center, tolerance = 1e-7)

  white_information <- board_information_layout(
    position,
    white_geometry,
    style,
    perspective = "white"
  )
  black_information <- board_information_layout(
    position,
    black_geometry,
    style,
    perspective = "black"
  )

  expect_identical(white_information$bottom$player, "white")
  expect_identical(white_information$top$player, "black")
  expect_identical(black_information$bottom$player, "black")
  expect_identical(black_information$top$player, "white")
})

test_that("offered cube field side follows receiver and perspective", {
  expect_identical(offered_cube_field_side("white", "white"), "left")
  expect_identical(offered_cube_field_side("black", "black"), "left")
  expect_identical(offered_cube_field_side("white", "black"), "right")
  expect_identical(offered_cube_field_side("black", "white"), "right")
})
