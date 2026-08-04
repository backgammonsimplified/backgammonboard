test_that("malformed positions and movement locations are rejected", {
  expect_error(
    ggboard("not-an-xgid"),
    "complete XGID"
  )
  expect_error(
    board_moves(0, 1),
    "points 1 through 24 or `bar`",
    class = "backgammon_move_invalid_location"
  )
  expect_error(
    board_moves(1, 25),
    "points 1 through 24 or `off`",
    class = "backgammon_move_invalid_location"
  )
})

test_that("missing and opposing source checkers are rejected", {
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L),
    player_0 = c(`8` = 1L)
  )

  expect_error(
    ggboard(position, moves = board_moves(12, 7, die = 5)),
    "source point 12 is empty",
    class = "backgammon_move_empty_source"
  )
  expect_error(
    ggboard(position, moves = board_moves(8, 7, die = 1)),
    "opposing checker",
    class = "backgammon_move_wrong_player"
  )
})

test_that("unsupported records and impossible applications are rejected", {
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L),
    player_0 = c(`8` = 2L)
  )

  expect_error(
    ggboard(position, moves = data.frame(from = 13, to = 8)),
    "created by `board_moves()`",
    fixed = TRUE
  )
  expect_error(
    ggboard(position, moves = board_moves(13, 8, die = 5)),
    "destination point 8 is blocked",
    class = "backgammon_move_blocked_destination"
  )
})

test_that("movement geometry that cannot be drawn is rejected", {
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L),
    player_0 = integer()
  )

  expect_error(
    ggboard(
      position,
      moves = board_moves(13, 8, die = 5),
      movement_style = movement_overlay_style(arrow_checker_gap = -1)
    ),
    "past the destination center"
  )
})
