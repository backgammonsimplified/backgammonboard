test_that("board_moves parses ordinary moves in supplied order", {
  moves <- board_moves("13/8 6/5")

  expect_s3_class(moves, "backgammon_board_moves")
  expect_s3_class(moves, "data.frame")
  expect_named(moves, backgammonboard:::move_step_columns())
  expect_identical(moves$step_id, 1:2)
  expect_identical(moves$chain_id, 1:2)
  expect_identical(moves$source_token, c("13/8", "6/5"))
  expect_identical(moves$from_point, c(13L, 6L))
  expect_identical(moves$to_point, c(8L, 5L))
  expect_identical(attr(moves, "notation"), "13/8 6/5")
})


test_that("compound notation preserves one checker chain", {
  moves <- board_moves("24/18/13")

  expect_identical(moves$step_id, 1:2)
  expect_identical(moves$chain_id, c(1L, 1L))
  expect_identical(moves$from_point, c(24L, 18L))
  expect_identical(moves$to_point, c(18L, 13L))
  expect_true(all(is.na(moves$repeat_group)))
})


test_that("repetition creates distinct chains and a shared repeat group", {
  moves <- board_moves("13/8(2)")

  expect_identical(moves$step_id, 1:2)
  expect_identical(moves$chain_id, 1:2)
  expect_identical(moves$from_point, c(13L, 13L))
  expect_identical(moves$to_point, c(8L, 8L))
  expect_identical(moves$repeat_group, c("r1", "r1"))

  compound <- board_moves("24/18/13(2)")
  expect_identical(compound$step_id, 1:4)
  expect_identical(compound$chain_id, c(1L, 1L, 2L, 2L))
  expect_identical(compound$repeat_group, rep("r1", 4L))
})


test_that("bar entry and bearing off retain typed locations", {
  entry <- board_moves("bar/24")
  expect_identical(entry$from_type, "bar")
  expect_true(is.na(entry$from_point))
  expect_identical(entry$to_type, "point")
  expect_identical(entry$to_point, 24L)

  bearoff <- board_moves("6/off")
  expect_identical(bearoff$from_type, "point")
  expect_identical(bearoff$from_point, 6L)
  expect_identical(bearoff$to_type, "off")
  expect_true(is.na(bearoff$to_point))
})


test_that("hit markers attach to their marked destinations", {
  single <- board_moves("13/8*")
  expect_identical(single$hit_marked, TRUE)

  compound <- board_moves("6/5*/3")
  expect_identical(compound$from_point, c(6L, 5L))
  expect_identical(compound$to_point, c(5L, 3L))
  expect_identical(compound$hit_marked, c(TRUE, FALSE))

  two_hits <- board_moves("13/8*/5*")
  expect_identical(two_hits$hit_marked, c(TRUE, TRUE))
})


test_that("parser normalizes case, commas, and whitespace", {
  moves <- board_moves("  BAR/24*,\t6/OFF  ")

  expect_identical(attr(moves, "notation"), "bar/24* 6/off")
  expect_identical(moves$source_token, c("bar/24*", "6/off"))
  expect_identical(moves$from_type, c("bar", "point"))
  expect_identical(moves$to_type, c("point", "off"))
})


test_that("four-part double notation preserves stable order and groups", {
  moves <- board_moves("13/10(2) 8/5(2)")

  expect_identical(moves$step_id, 1:4)
  expect_identical(moves$chain_id, 1:4)
  expect_identical(moves$from_point, c(13L, 13L, 8L, 8L))
  expect_identical(moves$to_point, c(10L, 10L, 5L, 5L))
  expect_identical(moves$repeat_group, c("r1", "r1", "r2", "r2"))
})


test_that("parser reports token-specific syntax errors", {
  expect_move_error <- function(expression, message, class) {
    condition <- tryCatch(
      {
        force(expression)
        NULL
      },
      error = identity
    )

    expect_false(is.null(condition))
    if (is.null(condition)) {
      return(invisible(NULL))
    }

    expect_true(inherits(condition, class))
    expect_true(grepl(message, conditionMessage(condition), fixed = TRUE))
    invisible(condition)
  }

  expect_move_error(
    board_moves(),
    "`notation` is required",
    "backgammon_move_invalid_input"
  )
  expect_move_error(
    board_moves(13),
    "must be a character value",
    "backgammon_move_invalid_input"
  )
  expect_move_error(
    board_moves(character()),
    "exactly one checker play",
    "backgammon_move_invalid_input"
  )
  expect_move_error(
    board_moves(""),
    "must not be empty",
    "backgammon_move_invalid_input"
  )
  expect_move_error(
    board_moves("13/8,,6/5"),
    "empty comma-separated token",
    "backgammon_move_invalid_separator"
  )
  expect_move_error(
    board_moves("13"),
    "Invalid move token 1 (`13`)",
    "backgammon_move_invalid_token"
  )
  expect_move_error(
    board_moves("25/20"),
    "Invalid location 1 in move token 1 (`25/20`)",
    "backgammon_move_invalid_location"
  )
  expect_move_error(
    board_moves("13/bar"),
    "`bar` may appear only as the first location",
    "backgammon_move_invalid_location"
  )
  expect_move_error(
    board_moves("off/6"),
    "`off` may appear only as the final destination",
    "backgammon_move_invalid_location"
  )
  expect_move_error(
    board_moves("6/off*"),
    "hit marker cannot be attached to `off`",
    "backgammon_move_invalid_hit_marker"
  )
  expect_move_error(
    board_moves("13*/8"),
    "hit marker `*` may appear once, after a point destination",
    "backgammon_move_invalid_hit_marker"
  )
  expect_move_error(
    board_moves("13/8(0)"),
    "repetition suffix",
    "backgammon_move_invalid_repeat"
  )
  expect_move_error(
    board_moves("13/8(1)"),
    "repetition count must be an integer of at least 2",
    "backgammon_move_invalid_repeat"
  )
})


test_that("parsing remains separate from move application and rendering", {
  expect_false(exists("apply_board_moves", mode = "function"))
  expect_false(exists("draw_move_overlay", mode = "function"))
  expect_false(exists("compare_after_xgid", mode = "function"))
})
