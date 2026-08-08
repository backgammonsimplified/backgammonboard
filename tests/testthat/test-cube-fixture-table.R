test_that("cube-state fixture table covers the frozen review matrix", {
  fixture_path <- testthat::test_path("fixtures", "cube-state-fixtures.csv")
  fixture <- utils::read.csv(
    fixture_path,
    stringsAsFactors = FALSE,
    na.strings = c("")
  )

  expect_named(
    fixture,
    c(
      "fixture_id",
      "play_context",
      "cube_value",
      "cube_owner",
      "on_roll",
      "dice",
      "is_crawford",
      "decision",
      "offerer",
      "receiver",
      "offered_value",
      "expected_display",
      "expected_visible",
      "expected_reason",
      "expected_status"
    )
  )

  required_fixture_ids <- c(
    "unlimited_centered",
    "unlimited_white_owned",
    "unlimited_black_owned",
    "ordinary_match_centered",
    "ordinary_match_owned",
    "crawford_hidden",
    "non_crawford_active",
    "legal_initial_double",
    "legal_redouble",
    "opponent_owned",
    "dice_already_rolled",
    "offer_already_pending",
    "offer_32_to_64",
    "offer_64_to_128",
    "unsupported_factual_above_64",
    "five_away_owns_4",
    "five_away_owns_8",
    "missing_offerer",
    "missing_receiver",
    "same_offer_parties",
    "neutral_status_with_dice",
    "explicit_roll_double",
    "explicit_roll_redouble",
    "explicit_take_pass"
  )

  expect_true(all(required_fixture_ids %in% fixture$fixture_id))
  expect_true(all(fixture$cube_value %in% c(1L, 2L, 4L, 8L, 32L, 64L, 128L)))
  expect_true(all(fixture$expected_display %in% c(
    "hidden", "centered", "owned", "offered", "unsupported"
  )))
  expect_true("package_cube_limit" %in% fixture$expected_reason)
  expect_true("match_cube_limit" %in% fixture$expected_reason)
})
