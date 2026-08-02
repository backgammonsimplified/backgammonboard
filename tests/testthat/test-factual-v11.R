test_that("complete historical fixtures remain valid under v1.2 facts", {
  fixtures <- read_factual_fixtures()

  for (index in seq_len(nrow(fixtures))) {
    fixture <- fixtures[index, , drop = FALSE]
    position <- backgammon_position(fixture$xgid)

    fields <- strsplit(sub("^XGID=", "", fixture$xgid), ":", fixed = TRUE)[[1L]]
    expect_identical(position$on_roll, if (fields[[4L]] == "-1") "player_0" else "player_1")
    expect_identical(position$dice, semicolon_integers(fixture$dice), info = fixture$fixture_id)
    expect_identical(
      position$cube_owner,
      switch(fields[[3L]], `-1` = "player_0", `0` = "center", `1` = "player_1"),
      info = fixture$fixture_id
    )
    expect_identical(
      position$score,
      c(player_0 = as.integer(fields[[7L]]), player_1 = as.integer(fields[[6L]])),
      info = fixture$fixture_id
    )
    expect_equal(position$max_cube, fixture$xgid_max_cube, info = fixture$fixture_id)
    expect_equal(sum(pmax(-position$points, 0L)) + position$bar[["player_0"]] + position$off[["player_0"]], 15L)
    expect_equal(sum(pmax(position$points, 0L)) + position$bar[["player_1"]] + position$off[["player_1"]], 15L)
    expect_identical(position$point_occupancy, backgammonboard:::signed_points_to_occupancy(position$points))
  }
})

test_that("point order and ownership never change because of turn", {
  first <- backgammon_position(fixture_xgid("opening_white_roll"))
  second <- backgammon_position(fixture_xgid("opening_black_roll"))
  expect_identical(first$points, second$points)
  expect_identical(first$bar, second$bar)
  expect_identical(first$off, second$off)
  expect_identical(first$on_roll, "player_1")
  expect_identical(second$on_roll, "player_0")

  asymmetric_first <- backgammon_position(fixture_xgid("asymmetric_white_roll"))
  asymmetric_second <- backgammon_position(fixture_xgid("asymmetric_black_roll"))
  expect_false(identical(asymmetric_first$points, asymmetric_second$points))
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
