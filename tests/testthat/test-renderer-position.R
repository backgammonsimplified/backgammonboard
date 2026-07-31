renderer_fixture <- function(name) {
  testthat::test_path(
    "fixtures",
    "renderer-position",
    paste0(name, ".json")
  )
}


read_renderer_fixture_text <- function(name) {
  paste(
    readLines(renderer_fixture(name), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}


semantic_renderer_fields <- function(position) {
  position[
    c(
      "play_context",
      "points",
      "bar",
      "off",
      "on_roll",
      "dice",
      "cube_value",
      "cube_owner",
      "score",
      "match_length",
      "is_crawford",
      "cube_enabled",
      "game_state",
      "phase",
      "decision_type",
      "checker_count",
      "renderer_semantic_state_hash"
    )
  ]
}


render_svg_bytes <- function(position) {
  path <- tempfile("renderer-position-", fileext = ".svg")
  grDevices::svg(path, width = 12, height = 9.1, onefile = TRUE)
  print(ggboard(
    position,
    colors = board_colors("bms"),
    style = board_style("bms"),
    decision = "none",
    brand_text = NULL
  ))
  grDevices::dev.off()
  readBin(path, what = "raw", n = file.info(path)$size)
}


test_that("accepted RendererPosition input forms parse successfully", {
  path <- renderer_fixture("opening-learner-right")
  text <- read_renderer_fixture_text("opening-learner-right")
  parsed <- backgammonboard:::parse_renderer_position_json(text)

  from_file <- renderer_position(path)
  from_text <- renderer_position(text)
  from_object <- renderer_position(parsed)

  expect_s3_class(from_file, "backgammon_renderer_position")
  expect_s3_class(from_file, "backgammon_position")
  expect_identical(from_file, from_text)
  expect_identical(from_file, from_object)
  expect_identical(
    from_file$renderer_semantic_state_hash,
    "dccc6114baf3653254eb10ef77b454849be8483227647f73518267035aaa3963"
  )
  expect_identical(
    from_file$renderer_view_hash,
    "1c6300e726bd901cf6988806942be4a94ae1ca65e9e807eb2979c48a7421b47e"
  )
  expect_identical(from_file$learner_slot, "player_1")
  expect_identical(from_file$opponent_slot, "player_0")
  expect_identical(from_file$renderer_display_policy, "learner-bottom-v1")
})


test_that("malformed envelopes and hashes fail clearly", {
  expect_error(
    renderer_position("{bad"),
    "Malformed RendererPosition JSON",
    fixed = TRUE
  )
  expect_error(
    renderer_position(structure(list(), names = character())),
    "missing required field",
    fixed = TRUE
  )

  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$semantic_state_hash <- "not-a-hash"
  expect_error(
    renderer_position(envelope),
    "64-character SHA-256",
    fixed = TRUE
  )

  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$machine_path <- "C:\\private"
  expect_error(
    renderer_position(envelope),
    "unsupported field",
    fixed = TRUE
  )

  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$view$point_labels_for <- "player_0"
  expect_error(
    renderer_position(envelope),
    "requires point labels for bottom_player `player_1`",
    fixed = TRUE
  )
})


test_that("unsupported contract versions fail clearly", {
  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$position$schema_version <- "universal-position-v2"
  expect_error(
    renderer_position(envelope),
    "unsupported contract version `universal-position-v2`",
    fixed = TRUE
  )

  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$view$schema_version <- "backgammon-view-v2"
  expect_error(
    renderer_position(envelope),
    "unsupported contract version `backgammon-view-v2`",
    fixed = TRUE
  )
})


test_that("missing required position or view fails clearly", {
  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$position <- NULL
  expect_error(
    renderer_position(envelope),
    "position",
    fixed = TRUE
  )

  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("opening-learner-right")
  )
  envelope$view <- NULL
  expect_error(
    renderer_position(envelope),
    "view",
    fixed = TRUE
  )
})


test_that("self-relative points map into the existing signed board model", {
  envelope <- backgammonboard:::parse_renderer_position_json(
    read_renderer_fixture_text("borne-off")
  )
  mapped <- renderer_position(envelope)
  player_0 <- as.integer(unlist(
    envelope$position$board$player_0$points,
    use.names = FALSE
  ))
  player_1 <- as.integer(unlist(
    envelope$position$board$player_1$points,
    use.names = FALSE
  ))

  expect_identical(mapped$points, player_0 - rev(player_1))
  expect_identical(
    mapped$bar,
    c(
      white = as.integer(envelope$position$board$player_0$bar),
      black = as.integer(envelope$position$board$player_1$bar)
    )
  )
  expect_identical(
    mapped$off,
    c(
      white = as.integer(envelope$position$board$player_0$off),
      black = as.integer(envelope$position$board$player_1$off)
    )
  )
})


test_that("bar, off, tall-stack, dice, cube, score, money, match, and Crawford facts map", {
  both_bars <- renderer_position(renderer_fixture("both-bars-and-off"))
  expect_identical(both_bars$bar, c(white = 1L, black = 2L))
  expect_identical(both_bars$off, c(white = 14L, black = 13L))

  one_bar <- renderer_position(renderer_fixture("one-bar"))
  expect_true(any(one_bar$bar > 0L))

  one_off <- renderer_position(renderer_fixture("one-player-off"))
  both_off <- renderer_position(renderer_fixture("both-fully-borne-off"))
  expect_true(sum(one_off$off > 0L) == 1L)
  expect_true(all(both_off$off > 0L))

  tall <- renderer_position(renderer_fixture("tall-stacks"))
  expect_identical(max(tall$points), 15L)
  expect_identical(min(tall$points), -15L)

  player_0_dice <- renderer_position(renderer_fixture("player0-dice"))
  player_1_dice <- renderer_position(renderer_fixture("player1-dice"))
  no_dice <- renderer_position(renderer_fixture("opening-learner-right"))
  expect_identical(player_0_dice$on_roll, "white")
  expect_identical(player_0_dice$dice, c(3L, 1L))
  expect_identical(player_1_dice$on_roll, "black")
  expect_identical(player_1_dice$dice, c(4L, 2L))
  expect_identical(no_dice$dice, integer())

  cube_0 <- renderer_position(renderer_fixture("cube-player0"))
  cube_1 <- renderer_position(renderer_fixture("cube-player1"))
  expect_identical(cube_0$cube_owner, "white")
  expect_identical(cube_1$cube_owner, "black")
  expect_identical(cube_0$cube_value, 2L)
  expect_identical(cube_1$cube_value, 2L)

  money <- renderer_position(renderer_fixture("money-nonzero"))
  expect_identical(money$play_context, "unlimited")
  expect_identical(money$score, c(white = 2L, black = 3L))
  expect_true(is.na(money$match_length))

  match <- renderer_position(renderer_fixture("match-score"))
  expect_identical(match$play_context, "match")
  expect_identical(match$score, c(white = 4L, black = 2L))
  expect_identical(match$match_length, 7L)
  expect_false(match$is_crawford)

  crawford <- renderer_position(renderer_fixture("crawford"))
  expect_identical(crawford$score, c(white = 6L, black = 2L))
  expect_true(crawford$is_crawford)
  expect_false(
    attr(ggboard(crawford, decision = "none"), "backgammon_cube_display")$visible
  )
})


test_that("policy-compliant mirror and cube-side changes do not alter semantic facts", {
  right <- renderer_position(renderer_fixture("opening-learner-right"))
  left <- renderer_position(renderer_fixture("opening-learner-left"))
  cube_right <- renderer_position(renderer_fixture("opening-cube-right"))

  expect_identical(
    semantic_renderer_fields(right),
    semantic_renderer_fields(left)
  )
  expect_identical(
    semantic_renderer_fields(right),
    semantic_renderer_fields(cube_right)
  )
  expect_identical(
    right$renderer_semantic_state_hash,
    left$renderer_semantic_state_hash
  )
  expect_false(identical(right$renderer_view_hash, left$renderer_view_hash))
  expect_false(identical(right$renderer_view_hash, cube_right$renderer_view_hash))
})


test_that("learner stays bottom with learner labels while home side mirrors independently", {
  style <- board_style("bms")
  right <- renderer_position(renderer_fixture("opening-learner-right"))
  left <- renderer_position(renderer_fixture("opening-learner-left"))
  cube_right <- renderer_position(renderer_fixture("opening-cube-right"))

  right_geometry <- backgammonboard:::board_geometry(
    style,
    perspective = "black",
    bottom_home_board_side = right$renderer_view$bottom_home_board_side,
    point_labels_for = "black"
  )
  left_geometry <- backgammonboard:::board_geometry(
    style,
    perspective = "black",
    bottom_home_board_side = left$renderer_view$bottom_home_board_side,
    point_labels_for = "black"
  )

  expect_identical(
    right_geometry$point_layout$point[
      right_geometry$point_layout$side == "bottom"
    ],
    c(13:18, 19:24)
  )
  expect_identical(
    left_geometry$point_layout$point[
      left_geometry$point_layout$side == "bottom"
    ],
    c(24:19, 18:13)
  )
  expect_identical(
    as.integer(right_geometry$point_labels$label),
    25L - right_geometry$point_labels$point
  )
  expect_identical(
    as.integer(left_geometry$point_labels$label),
    25L - left_geometry$point_labels$point
  )

  right_plot <- ggboard(right, decision = "none")
  left_plot <- ggboard(left, decision = "none")
  expect_identical(attr(right_plot, "backgammon_perspective"), "black")
  expect_identical(attr(left_plot, "backgammon_perspective"), "black")
  expect_error(
    ggboard(right, decision = "none", perspective = "white"),
    "learner-view policy",
    fixed = TRUE
  )
  expect_identical(
    cube_right$renderer_view$cube_display_side,
    "right"
  )

  cube <- backgammonboard:::cube_layout(
    position = cube_right,
    geometry = right_geometry,
    style = style,
    cube_display = backgammonboard:::resolve_cube_display(cube_right),
    perspective = "black",
    cube_display_side = "right"
  )
  expect_true(cube$center$x[[1L]] > right_geometry$frame$xmax[[1L]])
})


test_that("changing on-roll player never rotates or mirrors the learner view", {
  style <- board_style("bms")
  player_0 <- renderer_position(renderer_fixture("player0-dice"))
  player_1 <- renderer_position(renderer_fixture("player1-dice"))

  expect_identical(player_0$renderer_view, player_1$renderer_view)
  expect_identical(player_0$renderer_view_hash, player_1$renderer_view_hash)
  expect_identical(player_0$learner_slot, "player_1")
  expect_identical(player_1$learner_slot, "player_1")

  player_0_plot <- ggboard(player_0, decision = "none")
  player_1_plot <- ggboard(player_1, decision = "none")
  expect_identical(attr(player_0_plot, "backgammon_perspective"), "black")
  expect_identical(attr(player_1_plot, "backgammon_perspective"), "black")
  expect_error(
    ggboard(player_0, decision = "none", perspective = "on_roll"),
    "must not rotate the board",
    fixed = TRUE
  )

  geometry <- backgammonboard:::board_geometry(
    style,
    perspective = "black",
    bottom_home_board_side = "right",
    point_labels_for = "black"
  )
  dice_0 <- backgammonboard:::dice_layout(
    player_0,
    geometry,
    style,
    perspective = "black"
  )
  dice_1 <- backgammonboard:::dice_layout(
    player_1,
    geometry,
    style,
    perspective = "black"
  )
  expect_equal(
    mean(range(dice_0$faces$x)),
    mean(c(geometry$left_field$xmin, geometry$left_field$xmax))
  )
  expect_equal(
    mean(range(dice_1$faces$x)),
    mean(c(geometry$right_field$xmin, geometry$right_field$xmax))
  )

  information_0 <- backgammonboard:::board_information_layout(
    player_0,
    geometry,
    style,
    perspective = "black"
  )
  information_1 <- backgammonboard:::board_information_layout(
    player_1,
    geometry,
    style,
    perspective = "black"
  )
  expect_true(information_0$top$on_roll)
  expect_false(information_0$bottom$on_roll)
  expect_identical(information_0$top$on_roll_arrow, "\u2192")
  expect_true(information_1$bottom$on_roll)
  expect_false(information_1$top$on_roll)
  expect_identical(information_1$bottom$on_roll_arrow, "\u2192")
  expect_true(information_0$top$on_roll_arrow_x < information_0$top$player_x)
  expect_true(information_1$bottom$on_roll_arrow_x < information_1$bottom$player_x)
  expect_match(information_0$sentence$status, "player_0 on roll", fixed = TRUE)
  expect_match(information_1$sentence$status, "player_1 on roll", fixed = TRUE)
  expect_match(
    attr(player_0_plot, "backgammon_accessible_text"),
    "player_0 on roll",
    fixed = TRUE
  )
  expect_match(
    attr(player_1_plot, "backgammon_accessible_text"),
    "player_1 on roll",
    fixed = TRUE
  )
  expect_identical(board_colors("default")$on_roll_arrow, "#D9653B")
  expect_identical(board_colors("bms")$on_roll_arrow, "#D9653B")
})


test_that("canonical cube ownership remains independent of learner and on-roll identity", {
  style <- board_style("bms")
  cube_0 <- renderer_position(renderer_fixture("cube-player0"))
  cube_1 <- renderer_position(renderer_fixture("cube-player1"))
  geometry <- backgammonboard:::board_geometry(
    style,
    perspective = "black",
    bottom_home_board_side = "right",
    point_labels_for = "black"
  )
  layout_0 <- backgammonboard:::cube_layout(
    cube_0,
    geometry,
    style,
    cube_display = backgammonboard:::resolve_cube_display(cube_0),
    perspective = "black"
  )
  layout_1 <- backgammonboard:::cube_layout(
    cube_1,
    geometry,
    style,
    cube_display = backgammonboard:::resolve_cube_display(cube_1),
    perspective = "black"
  )

  expect_identical(cube_0$cube_owner, "white")
  expect_identical(cube_1$cube_owner, "black")
  expect_true(layout_0$center$y[[1L]] > style$board_height / 2)
  expect_true(layout_1$center$y[[1L]] < style$board_height / 2)
})


test_that("decision labels identify canonical perspective and themes do not define identity", {
  position <- renderer_position(renderer_fixture("opening-learner-right"))
  decision_plot <- ggboard(position, decision = "roll_double")
  decision_status <- backgammonboard:::position_status_label(
    position,
    attr(decision_plot, "backgammon_context")
  )
  expect_identical(
    decision_status,
    "player_1 to decide: Roll or Double?"
  )
  expect_match(
    attr(decision_plot, "backgammon_accessible_text"),
    "player_1 on roll",
    fixed = TRUE
  )
  expect_match(
    attr(decision_plot, "backgammon_accessible_text"),
    "player_1 to decide",
    fixed = TRUE
  )

  recolored <- ggboard(
    position,
    colors = board_colors(
      overrides = list(
        white_checker_fill = "#112233",
        black_checker_fill = "#DDEEFF"
      )
    ),
    decision = "none"
  )
  recolored_position <- attr(recolored, "backgammon_position")
  expect_identical(recolored_position$learner_slot, "player_1")
  expect_identical(recolored_position$opponent_slot, "player_0")
  expect_identical(recolored_position$on_roll, "black")
  expect_identical(recolored_position$cube_owner, "center")
  expect_identical(attr(recolored, "backgammon_perspective"), "black")
})


test_that("both learner home-side choices render byte-identically on repetition", {
  right <- renderer_position(renderer_fixture("opening-learner-right"))
  left <- renderer_position(renderer_fixture("opening-learner-left"))

  expect_identical(render_svg_bytes(right), render_svg_bytes(right))
  expect_identical(render_svg_bytes(left), render_svg_bytes(left))
  expect_false(identical(render_svg_bytes(right), render_svg_bytes(left)))
})


test_that("existing XGID public rendering remains compatible", {
  xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
  position <- backgammon_position(xgid)
  plot <- ggboard(xgid)

  expect_s3_class(plot, "ggplot")
  expect_identical(attr(plot, "backgammon_position"), position)
  expect_identical(attr(plot, "backgammon_perspective"), "white")
  expect_null(attr(plot, "backgammon_renderer_view"))
  expect_match(
    attr(plot, "backgammon_accessible_text"),
    "White on roll",
    fixed = TRUE
  )
})
