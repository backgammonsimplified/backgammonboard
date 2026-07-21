application_test_position <- function(
    player = "white",
    white = integer(),
    black = integer(),
    white_bar = 0L,
    black_bar = 0L
) {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10"
  )

  points <- integer(24)

  if (length(white) > 0L) {
    stopifnot(!is.null(names(white)))
    points[as.integer(names(white))] <- as.integer(white)
  }

  if (length(black) > 0L) {
    stopifnot(!is.null(names(black)))
    indices <- as.integer(names(black))
    stopifnot(all(points[indices] == 0L))
    points[indices] <- -as.integer(black)
  }

  white_bar <- as.integer(white_bar)
  black_bar <- as.integer(black_bar)
  white_off <- as.integer(15L - sum(points[points > 0L]) - white_bar)
  black_off <- as.integer(15L - sum(abs(points[points < 0L])) - black_bar)

  stopifnot(white_off >= 0L, black_off >= 0L)

  position$points <- as.integer(points)
  position$bar <- c(white = white_bar, black = black_bar)
  position$off <- c(white = white_off, black = black_off)
  position$on_roll <- player
  position$dice <- integer()
  position$action_marker <- "00"
  position$dice_action <- "00"
  position
}


expect_application_error <- function(expr, text, class) {
  condition <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )

  expect_false(is.null(condition))
  expect_s3_class(condition, class)
  expect_true(grepl(text, conditionMessage(condition), fixed = TRUE))
  invisible(condition)
}


test_that("simple and independent White moves update checker state in order", {
  position <- application_test_position(
    white = c(`13` = 1L, `6` = 1L)
  )
  original <- position

  result <- backgammonboard:::apply_board_moves(
    position,
    board_moves("13/8 6/5")
  )

  expect_s3_class(result, "backgammon_applied_moves")
  expect_identical(result$player, "white")
  expect_identical(result$points[c(5, 6, 8, 13)], c(1L, 0L, 1L, 0L))
  expect_identical(result$bar, c(white = 0L, black = 0L))
  expect_identical(result$off, c(white = 13L, black = 15L))
  expect_identical(result$die_validation_status, "not_checked")
  expect_identical(result$full_play_validation_status, "not_performed")
  expect_identical(position, original)
})


test_that("compound chains reuse the moved checker", {
  position <- application_test_position(white = c(`24` = 1L))

  result <- backgammonboard:::apply_board_moves(
    position,
    board_moves("24/18/13")
  )

  expect_identical(result$points[c(13, 18, 24)], c(1L, 0L, 0L))
  expect_identical(result$applied_steps$chain_id, c(1L, 1L))
  expect_identical(result$applied_steps$hit_confirmed, c(FALSE, FALSE))
})


test_that("repetition and four-part doubles preserve atomic order", {
  repeated <- backgammonboard:::apply_board_moves(
    application_test_position(white = c(`13` = 2L)),
    board_moves("13/8(2)")
  )

  expect_identical(repeated$points[c(8, 13)], c(2L, 0L))
  expect_identical(repeated$applied_steps$repeat_group, c("r1", "r1"))

  doubled <- backgammonboard:::apply_board_moves(
    application_test_position(white = c(`13` = 2L, `8` = 2L)),
    board_moves("13/10(2) 8/5(2)")
  )

  expect_identical(doubled$points[c(5, 8, 10, 13)], c(2L, 0L, 2L, 0L))
  expect_identical(doubled$applied_steps$step_id, 1:4)
})


test_that("bar entry and bearing off update typed checker locations", {
  entered <- backgammonboard:::apply_board_moves(
    application_test_position(white_bar = 1L),
    board_moves("bar/24")
  )

  expect_identical(entered$bar, c(white = 0L, black = 0L))
  expect_identical(entered$points[[24L]], 1L)
  expect_true(entered$applied_steps$entered_from_bar[[1L]])

  borne_off <- backgammonboard:::apply_board_moves(
    application_test_position(white = c(`6` = 1L)),
    board_moves("6/off")
  )

  expect_identical(borne_off$points[[6L]], 0L)
  expect_identical(borne_off$off, c(white = 15L, black = 15L))
  expect_true(borne_off$applied_steps$borne_off[[1L]])
})


test_that("marked and unmarked hits are detected from factual state", {
  position <- application_test_position(
    white = c(`13` = 1L),
    black = c(`8` = 1L)
  )

  marked <- backgammonboard:::apply_board_moves(
    position,
    board_moves("13/8*")
  )

  expect_identical(marked$points[[8L]], 1L)
  expect_identical(marked$bar, c(white = 0L, black = 1L))
  expect_true(marked$applied_steps$hit_marked[[1L]])
  expect_true(marked$applied_steps$hit_confirmed[[1L]])
  expect_identical(marked$applied_steps$hit_player[[1L]], "black")

  unmarked <- backgammonboard:::apply_board_moves(
    position,
    board_moves("13/8")
  )

  expect_false(unmarked$applied_steps$hit_marked[[1L]])
  expect_true(unmarked$applied_steps$hit_confirmed[[1L]])
})


test_that("a marked hit must correspond to an opposing blot", {
  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(white = c(`13` = 1L)),
      board_moves("13/8*")
    ),
    "the notation marks a hit on point 8, but no opposing blot is present",
    "backgammon_move_marked_hit_not_confirmed"
  )
})


test_that("blocked destinations and invalid sources are rejected", {
  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(
        white = c(`13` = 1L),
        black = c(`8` = 2L)
      ),
      board_moves("13/8")
    ),
    "destination point 8 is blocked by 2 black checkers",
    "backgammon_move_blocked_destination"
  )

  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(),
      board_moves("13/8")
    ),
    "source point 13 is empty",
    "backgammon_move_empty_source"
  )

  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(black = c(`13` = 1L)),
      board_moves("13/8")
    ),
    "contains an opposing checker rather than a white checker",
    "backgammon_move_wrong_player"
  )
})


test_that("bar priority and movement direction are enforced", {
  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(
        white = c(`13` = 1L),
        white_bar = 1L
      ),
      board_moves("13/8")
    ),
    "must enter all checkers from the bar",
    "backgammon_move_bar_priority"
  )

  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(white = c(`8` = 1L)),
      board_moves("8/13")
    ),
    "white checkers must move in their semantic homeward direction",
    "backgammon_move_wrong_direction"
  )
})


test_that("bearing off requires every checker to be in the home board", {
  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(white = c(`6` = 1L, `13` = 1L)),
      board_moves("6/off")
    ),
    "may bear off only after all of that player's checkers are in the home board",
    "backgammon_move_invalid_bearoff"
  )
})


test_that("Black movement, bar entry, hits, and bearing off use semantic direction", {
  moved <- backgammonboard:::apply_board_moves(
    application_test_position(player = "black", black = c(`12` = 1L)),
    board_moves("12/17")
  )
  expect_identical(moved$points[c(12, 17)], c(0L, -1L))

  entered <- backgammonboard:::apply_board_moves(
    application_test_position(player = "black", black_bar = 1L),
    board_moves("bar/3")
  )
  expect_identical(entered$points[[3L]], -1L)
  expect_identical(entered$bar, c(white = 0L, black = 0L))

  hit <- backgammonboard:::apply_board_moves(
    application_test_position(
      player = "black",
      white = c(`17` = 1L),
      black = c(`12` = 1L)
    ),
    board_moves("12/17")
  )
  expect_identical(hit$points[[17L]], -1L)
  expect_identical(hit$bar, c(white = 1L, black = 0L))
  expect_identical(hit$applied_steps$hit_player[[1L]], "white")

  borne_off <- backgammonboard:::apply_board_moves(
    application_test_position(player = "black", black = c(`20` = 1L)),
    board_moves("20/off")
  )
  expect_identical(borne_off$points[[20L]], 0L)
  expect_identical(borne_off$off, c(white = 15L, black = 15L))
})


test_that("bar entry must land in the opponent home board", {
  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(white_bar = 1L),
      board_moves("bar/18")
    ),
    "white bar entry must land in the opponent's home board",
    "backgammon_move_invalid_bar_entry"
  )
})


test_that("application validates chain continuity and input objects", {
  moves <- board_moves("24/18/13")
  moves$from_point[[2L]] <- 17L

  expect_application_error(
    backgammonboard:::apply_board_moves(
      application_test_position(white = c(`24` = 1L)),
      moves
    ),
    "continues from 17 instead of the prior destination 18",
    "backgammon_move_invalid_chain"
  )

  expect_application_error(
    backgammonboard:::apply_board_moves(
      list(),
      board_moves("13/8")
    ),
    "must be a `backgammon_position` object",
    "backgammon_move_invalid_position"
  )
})
