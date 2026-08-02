test_that("text anchors transform while glyphs remain upright", {
  moves <- board_moves(13, 8, die = 5, label = "13/8")
  position <- custom_position(
    on_roll = "player_1", player_1 = c(`13` = 1L), dice = c(5L, 1L)
  )
  for (near in c("player_1", "player_0")) {
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(
        position, moves = moves, perspective = near,
        mirror_horizontal = mirror
      )
      text_layers <- Filter(function(layer) inherits(layer$geom, "GeomText"), plot$layers)
      angles <- unlist(lapply(text_layers, function(layer) {
        angle <- layer$aes_params$angle
        if (is.null(angle)) 0 else angle
      }))
      expect_true(all(angles == 0))
      labels <- unlist(lapply(text_layers, function(layer) {
        if ("label" %in% names(layer$data)) as.character(layer$data$label) else character()
      }))
      expect_true("13/8" %in% labels)
      expect_true("1" %in% labels)
      expect_true(any(grepl("Homey on roll", labels, fixed = TRUE)))
      expect_true(all(as.character(1:24) %in% labels))
      information <- attr(plot, "backgammon_prepared_layout")$information
      expect_setequal(c(information$top$name, information$bottom$name), c("Homey", "Foey"))
      expect_true(all(information$sentence$y < 0))
    }
  }
})


movement_cases_v12 <- function() {
  list(
    checker = list(
      position = custom_position(on_roll = "player_1", player_1 = c(`13` = 1L), dice = c(5L, 1L)),
      moves = board_moves(13, 8, die = 5)
    ),
    bar_entry = list(
      position = custom_position(on_roll = "player_1", player_1_bar = 1L, dice = c(1L, 2L)),
      moves = board_moves("bar", 24, die = 1)
    ),
    repeated = list(
      position = custom_position(on_roll = "player_1", player_1 = c(`13` = 2L), dice = c(5L, 5L)),
      moves = board_moves(c(13, 13), c(8, 8), die = c(5, 5))
    ),
    bearing_off = list(
      position = custom_position(on_roll = "player_1", player_1 = c(`2` = 1L), dice = c(2L, 1L)),
      moves = board_moves(2, "off", die = 2)
    )
  )
}


test_that("all retained movement geometries use the shared four-state transforms", {
  for (case_name in names(movement_cases_v12())) {
    case <- movement_cases_v12()[[case_name]]
    states <- list()
    for (near in c("player_1", "player_0")) {
      for (mirror in c(FALSE, TRUE)) {
        plot <- ggboard(
          case$position, moves = case$moves, perspective = near,
          mirror_horizontal = mirror
        )
        overlay <- attr(plot, "backgammon_prepared_layout")$selected_overlay
        key <- paste(near, mirror)
        states[[key]] <- overlay
        expect_identical(overlay$segments$source_token,
                         states[["player_1 FALSE"]]$segments$source_token)
        expect_identical(overlay$segments$role,
                         states[["player_1 FALSE"]]$segments$role)
        expect_identical(overlay$ghosts$player,
                         states[["player_1 FALSE"]]$ghosts$player)
      }
    }
    base <- states[["player_1 FALSE"]]
    horizontal <- states[["player_1 TRUE"]]
    vertical <- states[["player_0 FALSE"]]
    both <- states[["player_0 TRUE"]]
    width <- board_style()$board_width
    height <- board_style()$board_height
    for (column in c("source_x", "x", "ghost_x", "xend", "destination_x")) {
      expect_equal(base$segments[[column]] + horizontal$segments[[column]],
                   rep(width, nrow(base$segments)))
      expect_equal(vertical$segments[[column]] + both$segments[[column]],
                   rep(width, nrow(base$segments)))
    }
    for (column in c("source_y", "y", "ghost_y", "yend", "destination_y")) {
      expect_equal(base$segments[[column]] + vertical$segments[[column]],
                   rep(height, nrow(base$segments)))
      expect_equal(horizontal$segments[[column]] + both$segments[[column]],
                   rep(height, nrow(base$segments)))
    }
    expect_equal(horizontal$segments$curvature, -base$segments$curvature)
    expect_equal(vertical$segments$curvature, -base$segments$curvature)
    expect_equal(both$segments$curvature, base$segments$curvature)
  }
})


test_that("neutral no-dice and explicit cube decisions stay distinct", {
  neutral <- ggboard(fixture_xgid("no_dice"), decision = "none")
  roll_double <- ggboard(fixture_xgid("no_dice"), decision = "roll_double")
  take_pass <- ggboard(fixture_xgid("offer_to_black"), decision = "take_pass")
  expect_length(attr(neutral, "backgammon_prepared_layout")$dice$faces$die, 0L)
  expect_identical(attr(neutral, "backgammon_cube_display")$state, "centered")
  expect_identical(attr(roll_double, "backgammon_cube_display")$state, "centered")
  expect_false(identical(attr(roll_double, "backgammon_cube_display")$state, "offered"))
  expect_identical(attr(take_pass, "backgammon_cube_display")$state, "offered")
})
