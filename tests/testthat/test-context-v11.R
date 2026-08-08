test_that("auto is conservative and gives moves first precedence", {
  dice <- backgammon_position(fixture_xgid("opening_white_roll"))
  no_dice <- backgammon_position(fixture_xgid("no_dice"))
  offer <- backgammon_position(fixture_xgid("offer_to_black"))

  expect_identical(attr(ggboard(dice), "backgammon_decision"), "checker_play")
  expect_identical(attr(ggboard(no_dice), "backgammon_decision"), "none")
  expect_identical(attr(ggboard(offer), "backgammon_decision"), "none")
  expect_error(
    ggboard(no_dice, moves = board_moves(13, 8)),
    "invalid_decision_state",
    fixed = TRUE
  )
})

test_that("Section 11 decision matrix is enforced", {
  dice <- fixture_xgid("opening_white_roll")
  no_dice <- fixture_xgid("no_dice")
  offer <- fixture_xgid("offer_to_black")

  expect_s3_class(ggboard(dice, decision = "checker_play"), "ggplot")
  expect_s3_class(ggboard(dice, decision = "none"), "ggplot")
  expect_error(ggboard(dice, decision = "roll_double"), "invalid_decision_state")
  expect_error(ggboard(dice, decision = "take_pass"), "invalid_decision_state")

  expect_s3_class(ggboard(no_dice, decision = "none"), "ggplot")
  expect_s3_class(ggboard(no_dice, decision = "roll_double"), "ggplot")
  expect_error(ggboard(no_dice, decision = "checker_play"), "invalid_decision_state")
  expect_error(ggboard(no_dice, decision = "take_pass"), "invalid_decision_state")

  expect_s3_class(ggboard(offer, decision = "take_pass"), "ggplot")
  expect_s3_class(ggboard(offer, decision = "none"), "ggplot")
  expect_error(ggboard(offer, decision = "roll_double"), "invalid_decision_state")
  expect_error(ggboard(offer, decision = "checker_play"), "invalid_decision_state")
})

test_that("decision maker and labels remain attached to factual players", {
  offer <- ggboard(
    fixture_xgid("offer_to_black"),
    decision = "take_pass",
    perspective = "decision_maker",
    player_labels = c(player_0 = "Foey", player_1 = "Homey")
  )
  context <- attr(offer, "backgammon_context")
  expect_identical(context$decision_maker, "player_0")
  expect_identical(context$near_player, "player_0")
  expect_identical(context$player_labels[["player_0"]], "Foey")
  expect_identical(context$player_labels[["player_1"]], "Homey")

  explicit <- ggboard(fixture_xgid("opening_white_roll"), perspective = "player_1")
  expect_identical(attr(explicit, "backgammon_perspective"), "player_1")
  expect_identical(attr(explicit, "backgammon_position")$on_roll, "player_1")
})

test_that("Crawford rejects cube decisions", {
  expect_error(ggboard(fixture_xgid("crawford"), decision = "roll_double"), "Crawford")
  expect_error(ggboard(fixture_xgid("crawford"), decision = "take_pass"), "invalid_decision_state")
})

test_that("cube display follows the decision matrix", {
  neutral <- attr(ggboard(fixture_xgid("no_dice"), decision = "none"), "backgammon_cube_display")
  roll <- attr(ggboard(fixture_xgid("no_dice"), decision = "roll_double"), "backgammon_cube_display")
  pending_none <- attr(ggboard(fixture_xgid("offer_to_black"), decision = "none"), "backgammon_cube_display")
  pending_take <- attr(ggboard(fixture_xgid("offer_to_black"), decision = "take_pass"), "backgammon_cube_display")
  crawford <- attr(ggboard(fixture_xgid("crawford"), decision = "none"), "backgammon_cube_display")

  expect_identical(neutral$state, "centered")
  expect_identical(roll$state, "centered")
  expect_identical(pending_none$state, "owned")
  expect_identical(pending_take$state, "offered")
  expect_identical(pending_take$offerer, "player_1")
  expect_identical(pending_take$receiver, "player_0")
  expect_identical(pending_take$value, 4L)
  expect_identical(crawford$state, "hidden")
})
