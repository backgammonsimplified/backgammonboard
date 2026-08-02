test_that("board_moves constructs ordered structured movements only", {
  moves <- board_moves(
    from = c(13, 6, "bar", 2),
    to = c(8, 5, 24, "off"),
    die = c(5, 1, 1, 2),
    label = c("one", "two", "three", "four")
  )
  expect_s3_class(moves, "backgammon_board_moves")
  expect_identical(moves$step_id, 1:4)
  expect_identical(attr(moves, "die"), c(5L, 1L, 1L, 2L))
  expect_error(board_moves("13/8"), "`from` and `to` are required", fixed = TRUE)
  expect_error(board_moves("off", 1), "locations must be")
  expect_error(board_moves(1, "bar"), "locations must be")
})

test_that("player_0 and player_1 movements use mover-relative points", {
  player_0 <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("opening_white_roll"))
  )
  player_1 <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("opening_black_roll"))
  )
  moves <- board_moves(c(13, 6), c(8, 5), die = c(5, 1))

  result_0 <- backgammonboard:::apply_board_moves(player_0, moves)
  result_1 <- backgammonboard:::apply_board_moves(player_1, moves)
  expect_equal(result_0$points[[13]], player_0$points[[13]] - 1L)
  expect_equal(result_0$points[[8]], player_0$points[[8]] + 1L)
  expect_equal(result_1$points[[12]], player_1$points[[12]] + 1L)
  expect_equal(result_1$points[[17]], player_1$points[[17]] - 1L)
  expect_identical(result_0$die_validation_status, "checked")
})

test_that("application handles bar entry, hits, blocking, and bearing off", {
  hit_position <- custom_position(
    player_0 = c(`13` = 1L),
    player_1 = c(`8` = 1L)
  )
  hit <- backgammonboard:::apply_board_moves(
    backgammonboard:::as_render_position(hit_position),
    board_moves(13, 8, die = 5)
  )
  expect_true(hit$applied_steps$hit_confirmed[[1L]])
  expect_equal(hit$bar[["black"]], 1L)

  blocked <- custom_position(player_0 = c(`13` = 1L), player_1 = c(`8` = 2L))
  expect_error(
    backgammonboard:::apply_board_moves(
      backgammonboard:::as_render_position(blocked), board_moves(13, 8)
    ),
    "blocked"
  )

  entry <- custom_position(player_0_bar = 1L)
  entered <- backgammonboard:::apply_board_moves(
    backgammonboard:::as_render_position(entry), board_moves("bar", 24, die = 1)
  )
  expect_equal(entered$bar[["white"]], 0L)
  expect_equal(entered$points[[24]], 1L)

  bearoff <- custom_position(player_0 = c(`2` = 1L))
  borne <- backgammonboard:::apply_board_moves(
    backgammonboard:::as_render_position(bearoff), board_moves(2, "off", die = 2)
  )
  expect_equal(borne$off[["white"]], 15L)
})

test_that("die distances and after_xgid are checked without replacing before facts", {
  expect_error(
    ggboard(fixture_xgid("opening_white_roll"), moves = board_moves(13, 8, die = 4)),
    "die_distance_mismatch"
  )

  before <- sub(":00:", ":11:", fixture_xgid("white_bar"), fixed = TRUE)
  moves <- board_moves("bar", 24, die = 1)
  plot <- ggboard(before, moves = moves, after_xgid = fixture_xgid("opening_white_roll"))
  expect_identical(attr(plot, "backgammon_position")$bar[["player_0"]], 1L)
  expect_identical(attr(plot, "backgammon_display_position")$bar[["player_0"]], 0L)
  expect_true(attr(plot, "backgammon_move_validation")$after_xgid_checked)
  expect_error(
    ggboard(before, moves = moves, after_xgid = before),
    "after_xgid_mismatch"
  )
  expect_error(ggboard(before, after_xgid = before), "after_xgid_without_moves")
})
