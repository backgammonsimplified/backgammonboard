component_fixture <- function() {
  position <- custom_position(
    on_roll = "player_0",
    player_0 = c(`21` = 2L),
    player_1 = c(`4` = 3L),
    player_0_bar = 2L,
    player_1_bar = 1L,
    dice = c(6L, 2L)
  )
  position$cube_value <- 2L
  position$cube_owner <- "player_0"
  position$score <- c(player_0 = 2L, player_1 = 5L)
  position
}


test_that("player-attached prepared components keep factual ownership", {
  position <- component_fixture()
  baseline <- NULL
  for (near in c("player_1", "player_0")) {
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(
        position, decision = "checker_play", perspective = near,
        mirror_horizontal = mirror, score_format = "both"
      )
      layout <- attr(plot, "backgammon_prepared_layout")
      checkers <- rbind(layout$checkers$points, layout$checkers$bar)
      expect_true(all(checkers$player %in% c("white", "black")))
      expect_true(all(layout$checkers$off$player %in% c("white", "black")))
      expect_true(all(layout$dice$faces$player == "black"))
      expect_identical(layout$cube$center$player[[1L]], "black")

      information <- rbind(layout$information$top, layout$information$bottom)
      foey <- information[information$player == "black", , drop = FALSE]
      homey <- information[information$player == "white", , drop = FALSE]
      expect_identical(foey$name, "Foey")
      expect_identical(homey$name, "Homey")
      expect_match(foey$pip_label, "^Pip count: ")
      expect_match(homey$pip_label, "^Pip count: ")

      side_for_y <- function(y) ifelse(y < board_style()$board_height / 2, "bottom", "top")
      expected_foey_side <- if (near == "player_0") "bottom" else "top"
      expected_homey_side <- if (near == "player_1") "bottom" else "top"
      foey_y <- c(
        checkers$y[checkers$player == "black"],
        layout$checkers$off$y[layout$checkers$off$player == "black"],
        foey$player_name_y,
        layout$cube$center$y
      )
      homey_y <- c(
        checkers$y[checkers$player == "white"],
        layout$checkers$off$y[layout$checkers$off$player == "white"],
        homey$player_name_y
      )
      expect_true(all(side_for_y(foey_y) == expected_foey_side))
      expect_true(all(side_for_y(homey_y) == expected_homey_side))

      snapshot <- list(
        checker_players = checkers$player,
        off_players = layout$checkers$off$player,
        dice_players = layout$dice$faces$player,
        cube_player = layout$cube$center$player,
        information = information[, c("player", "name", "secondary", "pip_label")]
      )
      if (is.null(baseline)) baseline <- snapshot else expect_identical(snapshot, baseline)
    }
  }
})


test_that("owned and offered cubes remain attached to factual players", {
  owned <- component_fixture()
  offer_xgids <- c(
    player_0 = "XGID=-b----E-C---eE---c-e----B-:1:-1:-1:D:0:0:0:0:10",
    player_1 = "XGID=-b----E-C---eE---c-e----B-:1:1:1:D:0:0:0:0:10"
  )
  for (near in c("player_1", "player_0")) {
    for (mirror in c(FALSE, TRUE)) {
      owned_layout <- attr(
        ggboard(owned, decision = "checker_play", perspective = near,
                mirror_horizontal = mirror),
        "backgammon_prepared_layout"
      )$cube
      expect_identical(
        backgammonboard:::render_player_to_project_player(owned_layout$center$player[[1L]]),
        "player_0"
      )
      for (offerer in names(offer_xgids)) {
        offered <- ggboard(
          offer_xgids[[offerer]], decision = "take_pass",
          perspective = near, mirror_horizontal = mirror
        )
        cube <- attr(offered, "backgammon_prepared_layout")$cube
        expect_identical(
          backgammonboard:::render_player_to_project_player(cube$center$player[[1L]]), offerer
        )
        expect_identical(attr(offered, "backgammon_cube_display")$offerer, offerer)
        expect_equal(cube$center$y, board_style()$board_height / 2)
      }
    }
  }
})


test_that("neutral and offered cubes center while owned cubes follow their owner", {
  cases <- list(
    centered = list(xgid = fixture_xgid("centered_cube"), decision = "none", owner = NA_character_),
    owned_player_0 = list(xgid = fixture_xgid("black_owned_cube"), decision = "none", owner = "player_0"),
    owned_player_1 = list(xgid = fixture_xgid("white_owned_cube"), decision = "none", owner = "player_1"),
    offered_to_player_0 = list(xgid = fixture_xgid("offer_to_white"), decision = "take_pass", owner = NA_character_),
    offered_to_player_1 = list(xgid = fixture_xgid("offer_to_black"), decision = "take_pass", owner = NA_character_)
  )
  midpoint <- board_style()$board_height / 2
  width <- board_style()$board_width

  for (case in cases) {
    cube_at <- function(near, mirror) {
      attr(
        ggboard(
          case$xgid, decision = case$decision, perspective = near,
          mirror_horizontal = mirror
        ),
        "backgammon_prepared_layout"
      )$cube$center
    }
    homey_near <- cube_at("player_1", FALSE)
    foey_near <- cube_at("player_0", FALSE)
    mirrored <- cube_at("player_1", TRUE)

    expect_equal(foey_near$x, homey_near$x)
    expect_equal(homey_near$x + mirrored$x, width)
    expect_equal(mirrored$y, homey_near$y)

    if (is.na(case$owner)) {
      expect_equal(homey_near$y, midpoint)
      expect_equal(foey_near$y, midpoint)
    } else {
      expect_equal(homey_near$y + foey_near$y, board_style()$board_height)
      if (identical(case$owner, "player_0")) {
        expect_gt(homey_near$y, midpoint)
        expect_lt(foey_near$y, midpoint)
      } else {
        expect_lt(homey_near$y, midpoint)
        expect_gt(foey_near$y, midpoint)
      }
    }
  }
})


test_that("every prepared component uses the shared coordinate transforms", {
  position <- component_fixture()
  prepare <- function(near, mirror) attr(
    ggboard(position, decision = "checker_play", perspective = near,
            mirror_horizontal = mirror),
    "backgammon_prepared_layout"
  )
  canonical <- prepare("player_1", FALSE)
  horizontal <- prepare("player_1", TRUE)
  vertical <- prepare("player_0", FALSE)
  width <- board_style()$board_width
  height <- board_style()$board_height
  paths <- list(
    c("geometry", "points"), c("geometry", "point_labels"),
    c("checkers", "points"), c("checkers", "bar"), c("checkers", "off"),
    c("dice", "faces"), c("dice", "pips"),
    c("cube", "center"), c("cube", "outer"), c("cube", "inner"),
    c("cube", "number"), c("information", "top"),
    c("information", "bottom")
  )
  at_path <- function(x, path) x[[path[[1L]]]][[path[[2L]]]]
  for (path in paths) {
    before <- at_path(canonical, path)
    mirrored <- at_path(horizontal, path)
    flipped <- at_path(vertical, path)
    if ("x" %in% names(before)) expect_equal(before$x + mirrored$x, rep(width, nrow(before)))
    if ("y" %in% names(before)) expect_equal(before$y + flipped$y, rep(height, nrow(before)))
  }
  expect_equal(
    canonical$information$sentence$y,
    vertical$information$sentence$y
  )
  expect_true(all(vertical$information$sentence$y < 0))
})
