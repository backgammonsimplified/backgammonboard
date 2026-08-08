review_xgids <- c(
  opening = "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10",
  offer_player_0 = "XGID=-b----E-C---eE---c-e----B-:1:-1:-1:D:0:0:0:0:10",
  offer_player_1 = "XGID=-b----E-C---eE---c-e----B-:1:1:1:D:0:0:0:0:10"
)


review_offer_layout <- function(xgid) {
  style <- board_style("bs")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  plot <- ggboard(
    xgid,
    colors = board_colors("bs"),
    style = style,
    decision = "take_pass",
    perspective = "player_0",
    score_format = "both"
  )
  render_position <- backgammonboard:::as_render_position(
    attr(plot, "backgammon_position")
  )
  offer <- backgammonboard:::pending_offer_from_position(render_position)
  layout <- backgammonboard:::cube_layout(
    render_position,
    geometry,
    style,
    cube_display = backgammonboard:::resolve_cube_display(render_position, offer),
    perspective = "white"
  )
  list(plot = plot, geometry = geometry, layout = layout)
}


test_that("BS labels and player_1 white styling preserve factual identity", {
  plot <- ggboard(
    review_xgids[["opening"]],
    colors = board_colors("bs"),
    style = board_style("bs"),
    decision = "checker_play",
    perspective = "player_0",
    player_labels = c(player_0 = "Foey", player_1 = "Homey")
  )
  factual <- attr(plot, "backgammon_position")
  context <- attr(plot, "backgammon_context")
  render <- backgammonboard:::as_render_position(factual, context$player_labels)
  geometry <- backgammonboard:::board_geometry(
    board_style("bs"), perspective = "white"
  )
  information <- backgammonboard:::board_information_layout(
    render,
    geometry,
    board_style("bs"),
    white_name = "Homey",
    black_name = "Foey",
    perspective = "white"
  )

  expect_identical(factual$on_roll, "player_1")
  expect_identical(names(factual$bar), c("player_0", "player_1"))
  expect_identical(names(factual$score), c("player_0", "player_1"))
  expect_false(any(c("white", "black") %in% names(factual$bar)))
  expect_identical(render$on_roll, "white")
  expect_identical(render$player_labels, c(white = "Homey", black = "Foey"))
  expect_identical(information$bottom$player, "white")
  expect_identical(information$bottom$name, "Homey")
  expect_identical(information$top$player, "black")
  expect_identical(information$top$name, "Foey")
  expect_identical(board_colors("bs")$white_checker_fill, "#F8EEDD")
})


test_that("Homey dice are screen-right and clear of information text", {
  style <- board_style("bs")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  factual <- backgammon_position(review_xgids[["opening"]])
  render <- backgammonboard:::as_render_position(factual)
  dice <- backgammonboard:::dice_layout(render, geometry, style, perspective = "white")
  information <- backgammonboard:::board_information_layout(
    render,
    geometry,
    style,
    white_name = "Homey",
    black_name = "Foey",
    perspective = "white"
  )
  board_center_x <- mean(c(geometry$frame$xmin, geometry$frame$xmax))
  dice_y <- range(dice$faces$y)
  information_y <- c(
    information$top$player_name_y,
    information$top$secondary_y,
    information$top$pip_y,
    information$bottom$player_name_y,
    information$bottom$secondary_y,
    information$bottom$pip_y,
    information$sentence$y
  )

  expect_identical(factual$on_roll, "player_1")
  expect_equal(factual$dice, c(5L, 2L))
  expect_gt(min(dice$faces$x), board_center_x)
  expect_true(all(information_y < dice_y[[1L]] | information_y > dice_y[[2L]]))
})


test_that("caller decoration reuses the dice and cube fields while ggboard stays neutral", {
  style <- board_style("bs")
  colors <- board_colors("bs")
  geometry <- backgammonboard:::board_geometry(style, perspective = "white")
  neutral <- ggboard(
    review_xgids[["opening"]],
    colors = colors,
    style = style,
    decision = "checker_play",
    perspective = "player_0"
  )
  single_text <- "Backgammon Simplified"
  two_line_text <- "Backgammon\nSimplified"
  expect_identical(
    eval(formals(backgammonboard:::render_board_preview)$brand_text),
    two_line_text
  )
  single <- backgammonboard:::add_board_brand(
    neutral, geometry, single_text, side = "left",
    color = colors$brand_text
  )
  two_line <- backgammonboard:::add_board_brand(
    neutral, geometry, two_line_text, side = "right",
    color = colors$brand_text
  )
  source_code <- backgammonboard:::add_board_brand(
    neutral, geometry, single_text, side = "left",
    color = colors$brand_text, family = "Source Code Pro BS 700",
    fontface = "plain"
  )
  layer_labels <- function(plot) {
    unlist(lapply(plot$layers, function(layer) {
      c(
        if ("label" %in% names(layer$data)) as.character(layer$data$label) else character(),
        if ("label" %in% names(layer$aes_params)) as.character(layer$aes_params$label) else character()
      )
    }), use.names = FALSE)
  }

  expect_identical(strsplit(two_line_text, "\n", fixed = TRUE)[[1L]], c("Backgammon", "Simplified"))
  expect_false(any(c(single_text, two_line_text) %in% layer_labels(neutral)))
  expect_true(single_text %in% layer_labels(single))
  expect_true(two_line_text %in% layer_labels(two_line))
  expect_equal(tail(single$layers, 1L)[[1L]]$data$x, mean(c(
    geometry$left_field$xmin, geometry$left_field$xmax
  )))
  expect_equal(tail(two_line$layers, 1L)[[1L]]$data$x, mean(c(
    geometry$right_field$xmin, geometry$right_field$xmax
  )))
  expect_equal(tail(single$layers, 1L)[[1L]]$data$y, style$board_height / 2)
  expect_identical(tail(single$layers, 1L)[[1L]]$aes_params$family, "Source Sans 3")
  expect_identical(tail(single$layers, 1L)[[1L]]$aes_params$colour, colors$brand_text)
  expect_identical(tail(single$layers, 1L)[[1L]]$aes_params$fontface, "bold")
  expect_identical(tail(source_code$layers, 1L)[[1L]]$aes_params$family, "Source Code Pro BS 700")
  expect_identical(tail(source_code$layers, 1L)[[1L]]$aes_params$fontface, "plain")
  expect_identical(colors$brand_text, colors$bar_fill)
  expect_identical(attr(neutral, "backgammon_brand_side"), "right")
  offered <- ggboard(
    review_xgids[["offer_player_1"]], decision = "take_pass",
    perspective = "player_0"
  )
  expect_identical(attr(offered, "backgammon_brand_side"), "left")
  offered_by_player_0 <- ggboard(
    review_xgids[["offer_player_0"]], decision = "take_pass",
    perspective = "player_0"
  )
  expect_identical(attr(offered_by_player_0, "backgammon_brand_side"), "right")

  dice_right <- ggboard(
    "XGID=-b----E-C---eE---c-e----B-:0:0:-1:31:0:0:0:0:10",
    decision = "checker_play", perspective = "player_0"
  )
  expect_identical(attr(dice_right, "backgammon_brand_side"), "left")

  no_field_object <- ggboard(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10",
    decision = "none", perspective = "player_0"
  )
  expect_identical(attr(no_field_object, "backgammon_brand_side"), "left")
})


test_that("pending offers share the midline with opposite semantic-right fields", {
  player_0 <- review_offer_layout(review_xgids[["offer_player_0"]])
  player_1 <- review_offer_layout(review_xgids[["offer_player_1"]])
  display_0 <- attr(player_0$plot, "backgammon_cube_display")
  display_1 <- attr(player_1$plot, "backgammon_cube_display")
  context_0 <- attr(player_0$plot, "backgammon_context")
  context_1 <- attr(player_1$plot, "backgammon_context")
  board_y_center <- mean(c(
    player_0$geometry$frame$ymin,
    player_0$geometry$frame$ymax
  ))
  left_field_center <- mean(c(
    player_0$geometry$left_field$xmin,
    player_0$geometry$left_field$xmax
  ))
  right_field_center <- mean(c(
    player_1$geometry$right_field$xmin,
    player_1$geometry$right_field$xmax
  ))

  expect_identical(display_0$state, "offered")
  expect_identical(display_0$offerer, "player_0")
  expect_identical(display_0$receiver, "player_1")
  expect_identical(display_0$value, 4L)
  expect_identical(display_1$state, "offered")
  expect_identical(display_1$offerer, "player_1")
  expect_identical(display_1$receiver, "player_0")
  expect_identical(display_1$value, 4L)
  expect_equal(player_0$layout$center$x, left_field_center)
  expect_equal(player_1$layout$center$x, right_field_center)
  expect_equal(player_0$layout$center$y, board_y_center)
  expect_equal(player_1$layout$center$y, board_y_center)
  for (context in list(context_0, context_1)) {
    expect_false(context$show_dice)
    expect_false(context$show_normal_cube)
    expect_true(context$show_offered_cube)
  }
})


test_that("only explicit take_pass displays a pending offered cube", {
  for (xgid in review_xgids[c("offer_player_0", "offer_player_1")]) {
    for (decision in c("auto", "none")) {
      display <- attr(
        ggboard(xgid, decision = decision, perspective = "player_0"),
        "backgammon_cube_display"
      )
      expect_false(identical(display$state, "offered"))
    }
    expect_error(
      ggboard(xgid, decision = "roll_double", perspective = "player_0"),
      "invalid_decision_state"
    )
  }
  roll_double <- attr(
    ggboard(fixture_xgid("no_dice"), decision = "roll_double"),
    "backgammon_cube_display"
  )
  expect_false(identical(roll_double$state, "offered"))
})
