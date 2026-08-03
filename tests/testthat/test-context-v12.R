test_that("every public perspective resolves only the near player", {
  xgid <- "XGID=-b----E-C---eE---c-e----B-:1:1:1:D:0:0:0:0:10"
  expected <- c(
    player_0 = "player_0",
    player_1 = "player_1",
    on_roll = "player_1",
    decision_maker = "player_0"
  )
  for (perspective in names(expected)) {
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(
        xgid, decision = "take_pass", perspective = perspective,
        mirror_horizontal = mirror
      )
      context <- attr(plot, "backgammon_context")
      expect_identical(context$near_player, unname(expected[[perspective]]))
      expect_identical(context$mirror_horizontal, mirror)
    }
  }
})


test_that("default display is Homey near and not mirrored", {
  plot <- ggboard(fixture_xgid("no_dice"))
  context <- attr(plot, "backgammon_context")
  expect_identical(context$near_player, "player_1")
  expect_false(context$mirror_horizontal)
  expect_identical(context$light_player, "player_1")
  expect_identical(context$player_labels, c(player_0 = "Foey", player_1 = "Homey"))
})


test_that("light-player selection is independent of XGID facts", {
  position <- backgammon_position(fixture_xgid("asymmetric_white_roll"))
  original <- serialize(position, NULL, version = 3)

  near_light <- ggboard(
    position, perspective = "player_0", light_player = "near_player"
  )
  fixed_light <- ggboard(
    position, perspective = "player_1", light_player = "player_0"
  )

  expect_identical(attr(near_light, "backgammon_light_player"), "player_0")
  expect_identical(attr(fixed_light, "backgammon_light_player"), "player_0")
  expect_identical(attr(near_light, "backgammon_context")$near_player, "player_0")
  expect_identical(serialize(position, NULL, version = 3), original)
  expect_error(ggboard(position, light_player = "xgid"), "arg")
})


test_that("light-player palette swaps checker, die, and badge colors together", {
  colors <- board_colors("bms")
  swapped <- backgammonboard:::colors_for_light_player(colors, "black")

  expect_identical(swapped$black_checker_fill, colors$white_checker_fill)
  expect_identical(swapped$black_checker_ring, colors$white_checker_ring)
  expect_identical(swapped$black_checker_outer_ring, colors$black_checker_fill)
  expect_identical(swapped$black_checker_text, colors$white_checker_text)
  expect_identical(swapped$die_black_fill, colors$die_white_fill)
  expect_identical(swapped$die_black_pips, colors$die_white_pips)
  expect_identical(swapped$white_checker_fill, colors$black_checker_fill)
  badge <- backgammonboard:::information_name_palette(
    "black", swapped, "checker"
  )
  expect_identical(badge$fill, colors$white_checker_fill)
  expect_identical(badge$text, colors$white_checker_text)
  expect_s3_class(swapped, "backgammon_board_colors")
})
