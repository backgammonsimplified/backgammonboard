fixture_xgid <- function(
    cube_exponent = 0L,
    cube_owner = 0L,
    turn = 1L,
    dice_action = "00",
    score_white = 0L,
    score_black = 0L,
    crawford = 0L,
    match_length = 0L,
    max_cube_exponent = 10L
) {
  paste0(
    "XGID=-b----E-C---eE---c-e----B-:",
    cube_exponent, ":",
    cube_owner, ":",
    turn, ":",
    dice_action, ":",
    score_white, ":",
    score_black, ":",
    crawford, ":",
    match_length, ":",
    max_cube_exponent
  )
}


test_that("factual position exposes one Crawford fact and cube facts", {
  position <- backgammon_position(
    fixture_xgid(
      cube_exponent = 3L,
      cube_owner = 1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  expect_identical(position$on_roll, "white")
  expect_identical(position$dice, integer())
  expect_identical(position$cube_value, 8L)
  expect_identical(position$cube_owner, "white")
  expect_identical(position$match_length, 7L)
  expect_identical(position$score_white, 2L)
  expect_identical(position$score_black, 2L)
  expect_false(position$is_crawford)
  expect_null(position$crawford_status)
  expect_false("action" %in% names(position))
  expect_null(position[["action"]])
  expect_identical(position$action_marker, "00")
})


test_that("Crawford is represented only by is_crawford", {
  position <- backgammon_position(
    fixture_xgid(
      score_white = 6L,
      score_black = 2L,
      crawford = 1L,
      match_length = 7L
    )
  )

  expect_true(position$is_crawford)
  expect_null(position$crawford_status)
})


test_that("package-supported cube limit is 64", {
  expect_identical(supported_cube_max(), 64L)

  validation <- validate_xgid(
    fixture_xgid(
      cube_exponent = 7L,
      cube_owner = 1L
    )
  )

  expect_false(validation$valid)
  expect_true("xgid_unsupported_cube_value" %in% validation$errors$code)
  expect_true(any(grepl("package_cube_limit", validation$errors$message)))
  expect_error(
    backgammon_position(
      fixture_xgid(cube_exponent = 7L, cube_owner = 1L)
    ),
    "package_cube_limit"
  )
})


test_that("unlimited centered and owned cube displays resolve", {
  centered <- backgammon_position(fixture_xgid())
  centered_display <- resolve_cube_display(centered)

  expect_true(centered_display$visible)
  expect_identical(centered_display$state, "centered")
  expect_identical(centered_display$value, 1L)
  expect_null(centered_display$owner)
  expect_identical(centered_display$placement, "outside_center")

  white_owned <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = 1L)
  )
  white_display <- resolve_cube_display(white_owned)

  expect_identical(white_display$state, "owned")
  expect_identical(white_display$value, 2L)
  expect_identical(white_display$owner, "white")
  expect_identical(white_display$placement, "white_side")

  black_owned <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = -1L)
  )
  black_display <- resolve_cube_display(black_owned)

  expect_identical(black_display$state, "owned")
  expect_identical(black_display$owner, "black")
  expect_identical(black_display$placement, "black_side")
})


test_that("ordinary match cube remains active when not Crawford", {
  position <- backgammon_position(
    fixture_xgid(
      cube_exponent = 2L,
      cube_owner = 1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  display <- resolve_cube_display(position)
  expect_true(display$visible)
  expect_identical(display$state, "owned")
  expect_identical(display$value, 4L)
})


test_that("Crawford hides the factual cube without rewriting it", {
  position <- backgammon_position(
    fixture_xgid(
      cube_exponent = 2L,
      cube_owner = 1L,
      score_white = 6L,
      score_black = 2L,
      crawford = 1L,
      match_length = 7L
    )
  )

  display <- resolve_cube_display(position)

  expect_false(display$visible)
  expect_identical(display$state, "hidden")
  expect_identical(display$value, 4L)
  expect_identical(position$cube_value, 4L)
  expect_identical(position$cube_owner, "white")
})


test_that("legal initial doubles and redoubles validate", {
  centered <- backgammon_position(fixture_xgid())
  initial <- validate_cube_offer_context(centered)

  expect_true(initial$valid)
  expect_identical(initial$action, "double")
  expect_identical(initial$current_value, 1L)
  expect_identical(initial$offered_value, 2L)

  owned <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = 1L)
  )
  redouble <- validate_cube_offer_context(owned)

  expect_true(redouble$valid)
  expect_identical(redouble$action, "redouble")
  expect_identical(redouble$offered_value, 4L)
})


test_that("opponent ownership and rolled dice reject offers", {
  opponent_owned <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = -1L)
  )

  expect_identical(
    validate_cube_offer_context(opponent_owned)$reason,
    "opponent_owns_cube"
  )

  rolled <- backgammon_position(
    fixture_xgid(dice_action = "51")
  )

  expect_identical(
    validate_cube_offer_context(rolled)$reason,
    "dice_already_rolled"
  )
})


test_that("pending offer stays separate and resolves at proposed value", {
  position <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = 1L)
  )

  offer <- cube_offer_context(
    offerer = "White",
    receiver = "Black",
    current_value = 2L,
    offered_value = 4L
  )

  validation <- validate_cube_offer_context(position, offer)
  display <- resolve_cube_display(position, offer)

  expect_true(validation$valid)
  expect_identical(display$state, "offered")
  expect_identical(display$value, 4L)
  expect_identical(display$offerer, "white")
  expect_identical(display$receiver, "black")
  expect_identical(position$cube_owner, "white")
  expect_identical(position$cube_value, 2L)
})


test_that("a cube-offer decision rejects a second pending offer", {
  position <- backgammon_position(
    fixture_xgid(cube_exponent = 1L, cube_owner = 1L)
  )

  offer <- cube_offer_context(
    offerer = "white",
    receiver = "black",
    current_value = 2L,
    offered_value = 4L
  )

  result <- validate_board_context(
    position,
    board_context(decision = "cube_offer", offer = offer)
  )

  expect_false(result$valid)
  expect_identical(result$reason, "offer_already_pending")
})


test_that("32 may be offered to 64 but 64 may not be offered to 128", {
  cube_32 <- backgammon_position(
    fixture_xgid(cube_exponent = 5L, cube_owner = 1L)
  )
  cube_64 <- backgammon_position(
    fixture_xgid(cube_exponent = 6L, cube_owner = 1L)
  )

  offer_64 <- validate_cube_offer_context(cube_32)
  offer_128 <- validate_cube_offer_context(cube_64)

  expect_true(offer_64$valid)
  expect_identical(offer_64$offered_value, 64L)
  expect_false(offer_128$valid)
  expect_identical(offer_128$reason, "package_cube_limit")
})


test_that("match cube limit uses the offerer's away score", {
  owns_4_at_5_away <- backgammon_position(
    fixture_xgid(
      cube_exponent = 2L,
      cube_owner = 1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  owns_8_at_5_away <- backgammon_position(
    fixture_xgid(
      cube_exponent = 3L,
      cube_owner = 1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  allowed <- validate_cube_offer_context(owns_4_at_5_away)
  unavailable <- validate_cube_offer_context(owns_8_at_5_away)

  expect_true(allowed$valid)
  expect_identical(allowed$offered_value, 8L)
  expect_false(unavailable$valid)
  expect_identical(unavailable$reason, "match_cube_limit")
  expect_identical(unavailable$offerer_away, 5L)

  display <- resolve_cube_display(owns_8_at_5_away)
  expect_true(display$visible)
  expect_identical(display$value, 8L)
})


test_that("offer parties and values return structured reasons", {
  position <- backgammon_position(fixture_xgid())

  missing_offerer <- cube_offer_context(
    offerer = NULL,
    receiver = "black",
    current_value = 1L,
    offered_value = 2L
  )
  missing_receiver <- cube_offer_context(
    offerer = "white",
    receiver = NULL,
    current_value = 1L,
    offered_value = 2L
  )
  same_parties <- cube_offer_context(
    offerer = "white",
    receiver = "white",
    current_value = 1L,
    offered_value = 2L
  )
  invalid_value <- cube_offer_context(
    offerer = "white",
    receiver = "black",
    current_value = 1L,
    offered_value = 4L
  )

  expect_identical(
    validate_cube_offer_context(position, missing_offerer)$reason,
    "missing_offerer"
  )
  expect_identical(
    validate_cube_offer_context(position, missing_receiver)$reason,
    "missing_receiver"
  )
  expect_identical(
    validate_cube_offer_context(position, same_parties)$reason,
    "same_offer_parties"
  )
  expect_identical(
    validate_cube_offer_context(position, invalid_value)$reason,
    "invalid_offer_value"
  )
})


test_that("Crawford rejects cube decision contexts", {
  position <- backgammon_position(
    fixture_xgid(
      score_white = 6L,
      score_black = 2L,
      crawford = 1L,
      match_length = 7L
    )
  )

  result <- validate_cube_offer_context(position)
  expect_false(result$valid)
  expect_identical(result$reason, "crawford")

  expect_error(
    position_status_label(
      position,
      board_context(decision = "cube_offer")
    ),
    "crawford"
  )
})


test_that("neutral and explicit decision status text are distinct", {
  position <- backgammon_position(fixture_xgid())

  expect_identical(position_status_label(position), "White on roll")
  expect_identical(
    position_status_label(
      position,
      board_context(decision = "cube_offer")
    ),
    "White to decide: Roll or Double?"
  )

  position$cube_owner <- "white"
  position$cube_value <- 2L

  expect_identical(
    position_status_label(
      position,
      board_context(decision = "cube_offer")
    ),
    "White to decide: Roll or Redouble?"
  )

  position$on_roll <- "black"
  position$cube_owner <- "black"

  offer <- cube_offer_context(
    offerer = "black",
    receiver = "white",
    current_value = 2L,
    offered_value = 4L
  )

  expect_identical(
    position_status_label(
      position,
      board_context(decision = "cube_response", offer = offer)
    ),
    "White to decide: Take or Pass?"
  )
})


test_that("renderer rejects invalid explicit cube decisions even without information", {
  position <- backgammon_position(
    fixture_xgid(
      score_white = 6L,
      score_black = 2L,
      crawford = 1L,
      match_length = 7L
    )
  )

  expect_error(
    render_board_preview(
      position,
      brand_text = NULL,
      show_information = FALSE,
      context = board_context("cube_offer")
    ),
    "crawford"
  )
})


test_that("match cube limit follows Black when Black is the offerer", {
  black_owns_4_at_5_away <- backgammon_position(
    fixture_xgid(
      cube_exponent = 2L,
      cube_owner = -1L,
      turn = -1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  black_owns_8_at_5_away <- backgammon_position(
    fixture_xgid(
      cube_exponent = 3L,
      cube_owner = -1L,
      turn = -1L,
      score_white = 2L,
      score_black = 2L,
      match_length = 7L
    )
  )

  allowed <- validate_cube_offer_context(black_owns_4_at_5_away)
  unavailable <- validate_cube_offer_context(black_owns_8_at_5_away)

  expect_true(allowed$valid)
  expect_identical(allowed$offerer, "black")
  expect_identical(allowed$offerer_away, 5L)
  expect_false(unavailable$valid)
  expect_identical(unavailable$reason, "match_cube_limit")
  expect_identical(unavailable$offerer, "black")
})


test_that("a pending offer must come from the player on roll", {
  position <- backgammon_position(fixture_xgid(turn = 1L))
  offer <- cube_offer_context(
    offerer = "black",
    receiver = "white",
    current_value = 1L,
    offered_value = 2L
  )

  result <- validate_cube_offer_context(position, offer)
  expect_false(result$valid)
  expect_identical(result$reason, "offerer_not_on_roll")
})

test_that("offered-cube rendering supports both semantic receivers", {
  offered_to_white_position <- backgammon_position(
    fixture_xgid(
      cube_exponent = 1L,
      cube_owner = -1L,
      turn = -1L
    )
  )

  offered_to_white <- cube_offer_context(
    offerer = "black",
    receiver = "white",
    current_value = 2L,
    offered_value = 4L
  )

  white_display <- resolve_cube_display(
    offered_to_white_position,
    offered_to_white
  )

  expect_identical(white_display$placement, "offered_to_white")
  expect_identical(cube_visual_state(white_display), "offered_white")

  offered_to_black_position <- backgammon_position(
    fixture_xgid(
      cube_exponent = 1L,
      cube_owner = 1L,
      turn = 1L
    )
  )

  offered_to_black <- cube_offer_context(
    offerer = "white",
    receiver = "black",
    current_value = 2L,
    offered_value = 4L
  )

  black_display <- resolve_cube_display(
    offered_to_black_position,
    offered_to_black
  )

  expect_identical(black_display$placement, "offered_to_black")
  expect_identical(cube_visual_state(black_display), "offered_black")
})
