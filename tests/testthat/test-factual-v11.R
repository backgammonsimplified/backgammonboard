test_that("complete XGID fixtures map to fixed player identities", {
  fixtures <- read_factual_fixtures()

  for (index in seq_len(nrow(fixtures))) {
    fixture <- fixtures[index, , drop = FALSE]
    position <- backgammon_position(fixture$xgid)

    expect_identical(position$points, semicolon_integers(fixture$points), info = fixture$fixture_id)
    expect_identical(
      position$bar,
      c(player_0 = as.integer(fixture$bar_white), player_1 = as.integer(fixture$bar_black)),
      info = fixture$fixture_id
    )
    expect_identical(
      position$off,
      c(player_0 = as.integer(fixture$off_white), player_1 = as.integer(fixture$off_black)),
      info = fixture$fixture_id
    )
    expect_identical(
      position$on_roll,
      if (fixture$on_roll == "white") "player_0" else "player_1",
      info = fixture$fixture_id
    )
    expect_identical(position$dice, semicolon_integers(fixture$dice), info = fixture$fixture_id)
    expect_identical(
      position$cube_owner,
      switch(fixture$cube_owner, white = "player_0", black = "player_1", center = "center"),
      info = fixture$fixture_id
    )
    expect_identical(
      position$score,
      c(player_0 = as.integer(fixture$score_white), player_1 = as.integer(fixture$score_black)),
      info = fixture$fixture_id
    )
    expect_equal(position$max_cube, fixture$xgid_max_cube, info = fixture$fixture_id)
    expect_equal(sum(pmax(position$points, 0L)) + position$bar[["player_0"]] + position$off[["player_0"]], 15L)
    expect_equal(sum(pmax(-position$points, 0L)) + position$bar[["player_1"]] + position$off[["player_1"]], 15L)
  }
})

test_that("turn-relative encodings preserve factual layout", {
  for (pair in list(
    c("opening_white_roll", "opening_black_roll"),
    c("asymmetric_white_roll", "asymmetric_black_roll")
  )) {
    first <- backgammon_position(fixture_xgid(pair[[1L]]))
    second <- backgammon_position(fixture_xgid(pair[[2L]]))
    expect_identical(first$points, second$points)
    expect_identical(first$bar, second$bar)
    expect_identical(first$off, second$off)
    expect_identical(first$on_roll, "player_0")
    expect_identical(second$on_roll, "player_1")
  }
})

test_that("factual state excludes display and compatibility fields", {
  position <- backgammon_position(fixture_xgid("ordinary_match"))
  expect_identical(position$crawford_status, "none")
  expect_identical(names(position$bar), c("player_0", "player_1"))
  expect_identical(names(position$score), c("player_0", "player_1"))
  expect_false(any(c(
    "player_labels", "learner_player", "renderer_view", "score_white",
    "score_black", "is_crawford", "action_marker", "position_payload"
  ) %in% names(position)))
})

test_that("Crawford and unlimited facts use the v1.1 vocabulary", {
  crawford <- backgammon_position(fixture_xgid("crawford"))
  unlimited <- backgammon_position(fixture_xgid("centered_cube"))
  expect_identical(crawford$crawford_status, "crawford")
  expect_identical(unlimited$crawford_status, "not_applicable")
  expect_true(is.na(unlimited$match_length))
})

test_that("the pinned invalid corpus keeps stable diagnostic codes", {
  fixtures <- utils::read.csv(
    testthat::test_path("fixtures", "xgid-invalid-fixtures.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  for (index in seq_len(nrow(fixtures))) {
    validation <- validate_xgid(fixtures$xgid[[index]])
    expect_false(validation$valid, info = fixtures$fixture_id[[index]])
    expect_true(
      fixtures$expected_code[[index]] %in% validation$errors$code,
      info = fixtures$fixture_id[[index]]
    )
  }
})
