test_that("the public API is frozen to the contract-v1.2 functions", {
  expected <- c(
    "backgammon_position", "board_colors", "board_moves", "board_style",
    "ggboard", "normalize_xgid", "validate_xgid"
  )
  expect_identical(sort(getNamespaceExports("backgammonboard")), sort(expected))
  expect_identical(
    names(formals(ggboard)),
    c(
      "x", "colors", "style", "moves", "after_xgid", "decision",
      "perspective", "mirror_horizontal", "light_player", "player_labels",
      "score_format", "point_1_side", "player_name_style", "movement_style"
    )
  )
  expect_identical(names(formals(board_moves)), c("from", "to", "die", "label"))
})

test_that("ordinary complete-XGID cases return static ggplots", {
  cases <- c(
    "opening_white_roll", "opening_black_roll", "no_dice", "doubles",
    "white_bar", "borne_off", "centered_cube", "white_owned_cube",
    "black_owned_cube", "ordinary_match", "crawford"
  )
  for (case in cases) {
    plot <- ggboard(fixture_xgid(case), decision = if (case %in% c("opening_white_roll", "opening_black_roll", "doubles")) "checker_play" else "none")
    expect_true(inherits(plot, "ggplot"), info = case)
    expect_true(inherits(attr(plot, "backgammon_position"), "backgammon_position"), info = case)
  }
})

test_that("dice contain two distinct faces with correct pips", {
  for (value in 1:6) expect_equal(nrow(backgammonboard:::die_pip_pattern(value)), value)
  plot <- ggboard(fixture_xgid("doubles"))
  position <- backgammonboard:::as_render_position(attr(plot, "backgammon_position"))
  dice <- backgammonboard:::dice_layout(
    position,
    backgammonboard:::board_geometry(board_style(), perspective = "white"),
    board_style(),
    perspective = "white"
  )
  expect_identical(sort(unique(dice$faces$die)), 1:2)
  expect_equal(nrow(dice$pips), 8L)
})

test_that("one perspective resolution changes layout but not facts or labels", {
  position <- backgammon_position(fixture_xgid("asymmetric_white_roll"))
  player_0 <- ggboard(position, decision = "checker_play", perspective = "player_0")
  player_1 <- ggboard(position, decision = "checker_play", perspective = "player_1")
  expect_identical(attr(player_0, "backgammon_position"), position)
  expect_identical(attr(player_1, "backgammon_position"), position)
  expect_false(identical(player_0$layers[[6]]$data, player_1$layers[[6]]$data))
  expect_identical(attr(player_0, "backgammon_context")$player_labels, c(player_0 = "Foey", player_1 = "Homey"))
  expect_identical(attr(player_1, "backgammon_context")$player_labels, c(player_0 = "Foey", player_1 = "Homey"))
})

test_that("ordinary render inputs reject deferred compatibility objects", {
  expect_error(ggboard(list(xgid = fixture_xgid("no_dice"))), "`x` must be a character")
  expect_false("renderer_position" %in% getNamespaceExports("backgammonboard"))
})

test_that("prepared cube geometry preserves centered, owned, and offered states", {
  style <- board_style("bs")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  centered_position <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("centered_cube"))
  )
  centered <- backgammonboard:::cube_layout(centered_position, geometry, style)
  expect_identical(centered$state, "centered")
  expect_lt(centered$center$x[[1L]], geometry$frame$xmin[[1L]])
  expect_equal(centered$center$y[[1L]], style$board_height / 2)

  owned_position <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("white_owned_cube"))
  )
  owned <- backgammonboard:::cube_layout(owned_position, geometry, style)
  expect_identical(owned$state, "owned_white")
  expect_identical(owned$value, 2L)
  expect_lt(owned$center$y[[1L]], style$board_height / 2)

  offer_position <- backgammonboard:::as_render_position(
    backgammon_position(fixture_xgid("offer_to_black"))
  )
  offer <- backgammonboard:::proposed_cube_offer(offer_position)
  offered <- backgammonboard:::cube_layout(
    offer_position, geometry, style,
    cube_display = backgammonboard:::resolve_cube_display(offer_position, offer)
  )
  expect_identical(offered$state, "offered_black")
  expect_identical(offered$value, 4L)
  expect_identical(offered$center$x_mode[[1L]], "inside")
  expect_equal(offered$center$y[[1L]], style$board_height / 2)
})
