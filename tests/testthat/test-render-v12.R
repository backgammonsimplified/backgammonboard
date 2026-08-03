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
      expect_false("13/8" %in% labels)
      expect_true("1" %in% labels)
      expect_true(any(grepl("Homey on roll", labels, fixed = TRUE)))
      expect_true(all(as.character(1:24) %in% labels))
      information <- attr(plot, "backgammon_prepared_layout")$information
      prepared <- attr(plot, "backgammon_prepared_layout")
      expect_true(any(prepared$checkers$points$point == 13L))
      expect_false(any(prepared$checkers$points$point == 8L))
      expect_equal(nrow(prepared$selected_overlay$markers), 0L)
      expect_true(all(prepared$selected_overlay$segments$curvature == 0))
      expect_setequal(c(information$top$name, information$bottom$name), c("Homey", "Foey"))
      expect_true(all(information$sentence$y < 0))
    }
  }
})


test_that("movement review overlays preserve sources and handle hits and off trays", {
  style <- board_style("bms")
  hit_position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L),
    player_0 = c(`10` = 1L),
    dice = c(3L, 1L)
  )
  hit_plot <- ggboard(
    hit_position,
    moves = board_moves(13, 10, die = 3),
    style = style
  )
  prepared <- attr(hit_plot, "backgammon_prepared_layout")
  overlay <- prepared$selected_overlay
  blot <- prepared$checkers$points[
    prepared$checkers$points$point == 10L &
      prepared$checkers$points$player == "black",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(blot), 1L)
  expect_equal(overlay$ghosts$y, blot$y + style$checker_stack_step)
  expect_true(overlay$segments$ghost_visible)
  source_clearance <- sqrt(
    (overlay$segments$x - overlay$segments$source_x)^2 +
      (overlay$segments$y - overlay$segments$source_y)^2
  )
  destination_clearance <- sqrt(
    (overlay$segments$xend - overlay$segments$destination_x)^2 +
      (overlay$segments$yend - overlay$segments$destination_y)^2
  )
  expect_equal(
    source_clearance,
    style$checker_outer_radius + style$checker_outer_ring_width
  )
  expect_equal(
    destination_clearance,
    style$checker_outer_radius + style$checker_outer_ring_width
  )
  expect_false(any(vapply(
    hit_plot$layers,
    function(layer) inherits(layer$geom, "GeomCurve"),
    logical(1)
  )))
  movement_start <- attr(hit_plot, "backgammon_movement_layer_start")
  ghost_layers <- seq.int(movement_start, movement_start + 2L)
  arrow_layers <- seq.int(movement_start + 3L, movement_start + 6L)
  expect_equal(length(arrow_layers), 4L)
  expect_equal(
    sum(vapply(
      hit_plot$layers[arrow_layers],
      function(layer) inherits(layer$geom, "GeomPolygon"),
      logical(1)
    )),
    2L
  )
  expect_lt(max(ghost_layers), min(arrow_layers))
  checker_layers <- which(vapply(
    hit_plot$layers,
    function(layer) inherits(layer$geom, "GeomCircle"),
    logical(1)
  ))
  factual_checker_layers <- checker_layers[checker_layers < movement_start]
  expect_lt(max(factual_checker_layers), min(arrow_layers))
  expect_true(any(vapply(
    hit_plot$layers[ghost_layers],
    function(layer) inherits(layer$geom, "GeomPoint"), logical(1)
  )))
  expect_true(any(vapply(
    hit_plot$layers[ghost_layers],
    function(layer) {
      inherits(layer$geom, "GeomCircle") &&
        identical(layer$aes_params$linetype, "solid")
    },
    logical(1)
  )))
  expect_false(any(vapply(
    hit_plot$layers[ghost_layers],
    function(layer) inherits(layer$geom, "GeomPath"), logical(1)
  )))

  existing_off <- custom_position(
    on_roll = "player_1", player_1 = c(`2` = 1L), dice = c(2L, 1L)
  )
  off_plot <- ggboard(
    existing_off, moves = board_moves(2, "off", die = 2), style = style
  )
  off_overlay <- attr(off_plot, "backgammon_prepared_layout")$selected_overlay
  expect_true(off_overlay$segments$ghost_visible)
  expect_equal(nrow(off_overlay$ghosts), 1L)
  expect_equal(
    off_overlay$ghosts$y,
    style$off_marker_bottom_y + style$checker_stack_step
  )

  empty_off <- custom_position(
    on_roll = "player_1", player_1 = c(`4` = 15L), dice = c(4L, 4L)
  )
  first_off_plot <- ggboard(
    empty_off, moves = board_moves(4, "off", die = 4), style = style
  )
  first_off_overlay <- attr(first_off_plot, "backgammon_prepared_layout")$selected_overlay
  expect_true(first_off_overlay$segments$ghost_visible)
  expect_equal(nrow(first_off_overlay$ghosts), 1L)
  expect_equal(first_off_overlay$ghosts$y, style$off_marker_bottom_y)

  split_off <- custom_position(
    on_roll = "player_1",
    player_1 = c(`4` = 1L, `3` = 1L, `2` = 1L, `1` = 1L),
    dice = c(4L, 4L)
  )
  split_plot <- ggboard(
    split_off,
    moves = board_moves(c(4, 3, 2, 1), rep("off", 4)),
    style = style
  )
  split_overlay <- attr(split_plot, "backgammon_prepared_layout")$selected_overlay
  nearest_source <- which.min(abs(
    split_overlay$segments$source_x - split_overlay$segments$destination_x
  ))
  expect_equal(
    split_overlay$segments$ghost_y[[nearest_source]],
    min(split_overlay$segments$ghost_y)
  )

  stacked_off <- custom_position(
    on_roll = "player_1", player_1 = c(`4` = 4L), dice = c(4L, 4L)
  )
  stacked_plot <- ggboard(
    stacked_off,
    moves = board_moves(rep(4, 4), rep("off", 4), die = rep(4, 4)),
    style = style
  )
  stacked_segments <- attr(
    stacked_plot, "backgammon_prepared_layout"
  )$selected_overlay$segments
  expect_identical(
    order(stacked_segments$source_y),
    order(stacked_segments$ghost_y)
  )

  repeated_stack <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 4L, `7` = 3L),
    dice = c(6L, 6L)
  )
  repeated_plot <- ggboard(
    repeated_stack,
    moves = board_moves(rep(13, 4), rep(7, 4), die = rep(6, 4)),
    style = style
  )
  repeated_prepared <- attr(
    repeated_plot, "backgammon_prepared_layout"
  )
  repeated_segments <- repeated_prepared$selected_overlay$segments
  point_7 <- repeated_prepared$geometry$point_layout[
    repeated_prepared$geometry$point_layout$point == 7L,
    ,
    drop = FALSE
  ]
  base_y <- repeated_prepared$geometry$layout$field_ymin +
    style$checker_margin + style$checker_outer_radius
  expect_equal(
    repeated_segments$ghost_y,
    base_y + (c(4L, 5L, 6L, 6L) - 1L) * style$checker_stack_step
  )
  expect_equal(repeated_segments$ghost_x, rep(point_7$x[[1L]], 4L))

  opposite_sources <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L, `11` = 1L, `10` = 2L),
    dice = c(3L, 1L)
  )
  opposite_plot <- ggboard(
    opposite_sources,
    moves = board_moves(c(11, 13), c(10, 10), die = c(1, 3)),
    style = style
  )
  opposite_segments <- attr(
    opposite_plot, "backgammon_prepared_layout"
  )$selected_overlay$segments
  expect_lt(opposite_segments$ghost_y[[1L]], opposite_segments$ghost_y[[2L]])

  departing_destination <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 2L, `11` = 2L, `9` = 2L),
    dice = c(2L, 2L)
  )
  departing_plot <- ggboard(
    departing_destination,
    moves = board_moves(
      c(13, 13, 11, 11), c(11, 11, 9, 9), die = rep(2, 4)
    ),
    style = style
  )
  departing_segments <- attr(
    departing_plot, "backgammon_prepared_layout"
  )$selected_overlay$segments
  arrivals_to_11 <- departing_segments[
    departing_segments$destination_point == 11L, , drop = FALSE
  ]
  departing_prepared <- attr(departing_plot, "backgammon_prepared_layout")
  point_11 <- departing_prepared$geometry$point_layout[
    departing_prepared$geometry$point_layout$point == 11L,
    ,
    drop = FALSE
  ]
  direction_11 <- if (point_11$side[[1L]] == "bottom") 1 else -1
  base_11 <- if (direction_11 == 1) {
    departing_prepared$geometry$layout$field_ymin +
      style$checker_margin + style$checker_outer_radius
  } else {
    departing_prepared$geometry$layout$field_ymax -
      style$checker_margin - style$checker_outer_radius
  }
  expect_equal(
    arrivals_to_11$ghost_y,
    base_11 + direction_11 * (c(3L, 4L) - 1L) *
      style$checker_stack_step
  )
  departures_from_11 <- departing_segments[
    departing_segments$source_token == "11/9", , drop = FALSE
  ]
  expect_setequal(departures_from_11$source_y, arrivals_to_11$ghost_y)
  expect_equal(departures_from_11$source_y, departures_from_11$ghost_y)

  independent_rows <- custom_position(
    on_roll = "player_1",
    player_1 = c(`18` = 2L, `11` = 2L, `9` = 2L),
    dice = c(2L, 2L)
  )
  independent_plot <- ggboard(
    independent_rows,
    moves = board_moves(
      c(18, 18, 11, 11), c(16, 16, 9, 9), die = rep(2, 4)
    ),
    style = style
  )
  independent_segments <- attr(
    independent_plot, "backgammon_prepared_layout"
  )$selected_overlay$segments
  home_pair <- independent_segments[
    independent_segments$source_token == "11/9", , drop = FALSE
  ]
  independent_geometry <- attr(
    independent_plot, "backgammon_prepared_layout"
  )$geometry
  expect_identical(
    order(backgammonboard:::move_overlay_edge_depth(
      home_pair$source_y, independent_geometry
    )),
    order(backgammonboard:::move_overlay_edge_depth(
      home_pair$ghost_y, independent_geometry
    ))
  )
  far_pair <- independent_segments[
    independent_segments$source_token == "18/16", , drop = FALSE
  ]
  expect_identical(
    order(backgammonboard:::move_overlay_edge_depth(
      far_pair$source_y, independent_geometry
    )),
    order(backgammonboard:::move_overlay_edge_depth(
      far_pair$ghost_y, independent_geometry
    ))
  )

  foey_rows <- custom_position(
    on_roll = "player_0",
    player_0 = c(`7` = 2L, `14` = 2L, `16` = 2L),
    dice = c(2L, 2L)
  )
  foey_plot <- ggboard(
    foey_rows,
    moves = board_moves(
      c(18, 18, 11, 11), c(16, 16, 9, 9), die = rep(2, 4)
    ),
    style = style
  )
  foey_segments <- attr(
    foey_plot, "backgammon_prepared_layout"
  )$selected_overlay$segments
  foey_home_pair <- foey_segments[
    foey_segments$source_token == "14/16", , drop = FALSE
  ]
  foey_geometry <- attr(
    foey_plot, "backgammon_prepared_layout"
  )$geometry
  expect_identical(
    order(backgammonboard:::move_overlay_edge_depth(
      foey_home_pair$source_y, foey_geometry
    )),
    order(backgammonboard:::move_overlay_edge_depth(
      foey_home_pair$ghost_y, foey_geometry
    ))
  )
  foey_far_pair <- foey_segments[
    foey_segments$source_token == "7/9", , drop = FALSE
  ]
  expect_identical(
    order(backgammonboard:::move_overlay_edge_depth(
      foey_far_pair$source_y, foey_geometry
    )),
    order(backgammonboard:::move_overlay_edge_depth(
      foey_far_pair$ghost_y, foey_geometry
    ))
  )
})


test_that("ghosts use one neutral palette over both point colors", {
  colors <- board_colors("bms")
  style <- board_style("bms")
  position <- custom_position(
    on_roll = "player_1",
    player_1 = c(`13` = 1L, `6` = 1L),
    dice = c(3L, 1L)
  )
  moves <- board_moves(c(13, 6), c(10, 5), die = c(3, 1))

  for (light_player in c("player_1", "player_0")) {
    plot <- ggboard(
      position,
      moves = moves,
      colors = colors,
      style = style,
      light_player = light_player
    )
    prepared <- attr(plot, "backgammon_prepared_layout")
    destination_roles <- unique(prepared$geometry$points$point_role[
      prepared$geometry$points$point %in% c(5L, 10L)
    ])
    expect_setequal(destination_roles, c("light", "dark"))

    ghost_layers <- plot$layers[vapply(
      plot$layers,
      function(layer) inherits(layer$geom, c("GeomPoint", "GeomCircle")),
      logical(1)
    )]
    expected_fill <- grDevices::adjustcolor(
      colors$movement_ghost_fill,
      alpha.f = 0.88
    )
    layer_parameter <- function(layer, name) layer$aes_params[[name]]

    expect_true(any(vapply(
      ghost_layers,
      function(layer) identical(layer_parameter(layer, "fill"), expected_fill),
      logical(1)
    )))
    expect_true(any(vapply(
      ghost_layers,
      function(layer) {
        inherits(layer$geom, "GeomPoint") &&
          identical(
            layer_parameter(layer, "colour"),
            colors$movement_ghost_pattern
          )
      },
      logical(1)
    )))
    expect_true(any(vapply(
      ghost_layers,
      function(layer) {
        inherits(layer$geom, "GeomCircle") &&
          identical(
            layer_parameter(layer, "colour"),
            colors$movement_ghost_outline
          )
      },
      logical(1)
    )))
  }
})


test_that("custom movement styles control ghost, arrow, and edge gap", {
  custom <- movement_overlay_style(
    ghost_fill = NA,
    ghost_outline = "#010203",
    ghost_outline_width = 1.7,
    ghost_dot_colour = "#405060",
    ghost_dot_alpha = 0.7,
    ghost_grid_rows = 9L,
    ghost_grid_cols = 5L,
    ghost_dot_size = 1.2,
    ghost_grid_inset = 0.08,
    arrow_colour = "#D95F32",
    arrow_alpha = 0.8,
    arrow_width = 1.6,
    arrow_outline_colour = "#000000",
    arrow_outline_width = 0.3,
    arrowhead_length = 0.21,
    arrowhead_width = 0.15,
    arrow_checker_gap = 0.07
  )
  style <- board_style("bms")
  position <- custom_position(
    on_roll = "player_1", player_1 = c(`13` = 1L), dice = c(3L, 1L)
  )
  plot <- ggboard(
    position,
    moves = board_moves(13, 10, die = 3),
    style = style,
    movement_style = custom
  )
  overlay <- attr(plot, "backgammon_prepared_layout")$selected_overlay
  destination_clearance <- sqrt(
    (overlay$segments$xend - overlay$segments$destination_x)^2 +
      (overlay$segments$yend - overlay$segments$destination_y)^2
  )
  expect_equal(
    destination_clearance,
    style$checker_outer_radius + style$checker_outer_ring_width + 0.07
  )
  source_clearance <- sqrt(
    (overlay$segments$x - overlay$segments$source_x)^2 +
      (overlay$segments$y - overlay$segments$source_y)^2
  )
  expect_equal(
    source_clearance,
    style$checker_outer_radius + style$checker_outer_ring_width + 0.07
  )
  expect_identical(attr(plot, "backgammon_movement_style"), custom)

  movement_start <- attr(plot, "backgammon_movement_layer_start")
  overlay_layers <- plot$layers[seq.int(movement_start, movement_start + 2L)]
  expect_true(any(vapply(
    overlay_layers,
    function(layer) inherits(layer$geom, "GeomCircle") &&
      is.na(layer$aes_params$fill),
    logical(1)
  )))
  ghost_circle <- overlay_layers[[which(vapply(
    overlay_layers,
    function(layer) inherits(layer$geom, "GeomCircle") &&
      !is.null(layer$data$radius),
    logical(1)
  ))[[1L]]]]
  expect_equal(unique(ghost_circle$data$radius), style$checker_outer_radius)
  expect_true(any(vapply(
    overlay_layers,
    function(layer) inherits(layer$geom, "GeomPoint") &&
      identical(layer$aes_params$size, 1.2) &&
      identical(layer$aes_params$alpha, 0.7),
    logical(1)
  )))
  arrow_layers <- plot$layers[seq.int(movement_start + 3L, movement_start + 6L)]
  expect_length(arrow_layers, 4L)
  shaft_layers <- arrow_layers[vapply(
    arrow_layers,
    function(layer) inherits(layer$geom, "GeomPath"),
    logical(1)
  )]
  head_layers <- arrow_layers[vapply(
    arrow_layers,
    function(layer) inherits(layer$geom, "GeomPolygon") &&
      !inherits(layer$geom, "GeomCircle"),
    logical(1)
  )]
  expect_equal(
    unname(vapply(shaft_layers, function(layer) nrow(layer$data), integer(1))),
    c(2L, 2L)
  )
  expect_equal(
    unname(vapply(head_layers, function(layer) nrow(layer$data), integer(1))),
    c(3L, 3L)
  )
  expect_identical(shaft_layers[[2L]]$aes_params$linewidth, 1.6)
  expect_identical(shaft_layers[[2L]]$aes_params$alpha, 0.8)
  expect_identical(head_layers[[2L]]$geom_params$linejoin, "round")
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
