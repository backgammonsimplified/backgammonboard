test_that("board color presets are complete and validated", {
  default <- board_colors("default")
  bms <- board_colors("bms")
  
  expect_s3_class(default, "backgammon_board_colors")
  expect_s3_class(bms, "backgammon_board_colors")
  
  expect_identical(attr(default, "preset_id"), "default")
  expect_identical(attr(bms, "preset_id"), "bms")
  
  expect_identical(names(default), names(bms))
  expect_gt(length(default), 0L)
  
  valid_default_values <- vapply(
    default,
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    },
    logical(1)
  )
  
  valid_bms_values <- vapply(
    bms,
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    },
    logical(1)
  )
  
  expect_true(all(valid_default_values))
  expect_true(all(valid_bms_values))
})

test_that("board color overrides are explicit", {
  colors <- board_colors(
    "bms",
    overrides = list(
      cube_face = "white"
    )
  )
  
  expect_identical(colors$cube_face, "white")
  expect_identical(attr(colors, "preset_id"), "bms")
  
  expect_error(
    board_colors(
      "bms",
      overrides = list(
        not_a_key = "white"
      )
    ),
    "Unknown color override"
  )
  
  expect_error(
    board_colors(
      "bms",
      overrides = list(
        cube_face = "not-a-real-color"
      )
    ),
    "Invalid color value"
  )
  
  expect_error(
    board_colors(
      "bms",
      overrides = list("white")
    ),
    "must be a named list"
  )
})

test_that("board color preset names are validated", {
  expect_error(
    board_colors("not-a-preset"),
    "Unknown board color preset"
  )
  
  expect_error(
    board_colors(c("default", "bms")),
    "length-1 character"
  )
  
  expect_error(
    board_colors(NA_character_),
    "length-1 character"
  )
  
  expect_error(
    board_colors(1),
    "length-1 character"
  )
})

test_that("board style presets are complete and validated", {
  default <- board_style("default")
  bms <- board_style("bms")
  
  expect_s3_class(default, "backgammon_board_style")
  expect_s3_class(bms, "backgammon_board_style")
  
  expect_identical(attr(default, "preset_id"), "default")
  expect_identical(attr(bms, "preset_id"), "bms")
  
  expect_identical(names(default), names(bms))
  expect_gt(length(default), 0L)
  expect_identical(default$count_badge_size, 3.0)
  expect_identical(bms$count_badge_size, 6.0)
  expect_identical(default$arrow_linewidth, 1.15)
  expect_identical(default$arrow_head_length_mm, 3.4)
  expect_identical(bms$arrow_linewidth, 1.8)
  expect_identical(bms$arrow_head_length_mm, 4.4)
  
  expect_true(
    all(
      vapply(
        default,
        function(value) {
          is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value)
        },
        logical(1)
      )
    )
  )
  
  expect_true(
    all(
      vapply(
        bms,
        function(value) {
          is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value)
        },
        logical(1)
      )
    )
  )
})

test_that("board style overrides are explicit", {
  style <- board_style(
    "bms",
    overrides = list(
      checker_margin = 0.05
    )
  )
  
  expect_identical(style$checker_margin, 0.05)
  expect_identical(attr(style, "preset_id"), "bms")
  
  expect_error(
    board_style(
      "bms",
      overrides = list(
        not_a_key = 1
      )
    ),
    "Unknown style override"
  )
  
  expect_error(
    board_style(
      "bms",
      overrides = list(
        checker_margin = 0
      )
    ),
    "checker_margin.*positive numeric scalar"
  )
  
  expect_error(
    board_style(
      "bms",
      overrides = list(
        max_stack_visible = 2.5
      )
    ),
    "max_stack_visible.*positive integer scalar"
  )
  
  expect_error(
    board_style(
      "bms",
      overrides = list(
        arrow_curvature = 2
      )
    ),
    "arrow_curvature.*between -1 and 1"
  )
  
  expect_error(
    board_style(
      "bms",
      overrides = list(0.05)
    ),
    "must be a named list"
  )
})

test_that("board style preset names are validated", {
  expect_error(
    board_style("not-a-preset"),
    "Unknown board style preset"
  )
  
  expect_error(
    board_style(c("default", "bms")),
    "length-1 character"
  )
  
  expect_error(
    board_style(NA_character_),
    "length-1 character"
  )
  
  expect_error(
    board_style(1),
    "length-1 character"
  )
})

test_that("BMS preset uses the approved checker and board values", {
  style <- board_style("bms")
  colors <- board_colors("bms")
  
  expect_identical(style$checker_outer_radius, 0.37)
  expect_identical(style$checker_face_radius, 0.31)
  
  expect_identical(style$off_marker_outer_radius, 0.37)
  expect_identical(style$off_marker_face_radius, 0.31)
  
  expect_identical(style$checker_margin, 0.025)
  expect_identical(style$checker_stack_step, 0.78)
  expect_identical(style$max_stack_visible, 5L)
  
  expect_identical(colors$black_checker_ring, "#111B35")
  expect_identical(colors$white_checker_outer_ring, colors$black_checker_fill)
  expect_identical(colors$bar_fill, "#D8C5A5")
  expect_identical(colors$frame_fill, "#D8C5A5")
})

test_that("BMS move-overlay tokens match the accepted guide", {
  colors <- board_colors("bms")
  style <- board_style("bms")

  expect_identical(colors$arrow_primary, "#C94F2C")
  expect_identical(colors$arrow_secondary, "#6E557A")
  expect_identical(colors$arrow_hit, "#923B45")
  expect_identical(colors$arrow_order_label, "#111B35")
  expect_identical(colors$arrow_marker_fill, "#FFFFFF")
  expect_identical(colors$arrow_marker_border, "#111B35")
  expect_identical(colors$arrow_halo_light, "#FFFDF8")
  expect_identical(colors$arrow_halo_dark, "#081126")
  expect_identical(colors$movement_ghost_fill, "#D4D8DC")
  expect_identical(colors$movement_ghost_pattern, "#65707A")
  expect_identical(colors$movement_ghost_outline, "#27313B")

  expect_identical(style$arrow_halo_dark_ratio, 1.45)
  expect_identical(style$arrow_halo_light_ratio, 1.20)
  expect_gt(style$arrow_halo_dark_ratio, style$arrow_halo_light_ratio)
  expect_gt(style$arrow_halo_light_ratio, 1)
})

test_that("BMS point outlines retain a direct-render minimum", {
  style <- board_style("bms")

  expect_true(style$point_border_width >= 0.38)
})


test_that("movement overlay styles expose validated iteration controls", {
  expected <- c(
    "ghost_fill", "ghost_fill_alpha", "ghost_outline",
    "ghost_outline_width", "ghost_dot_colour", "ghost_dot_alpha",
    "ghost_grid_rows", "ghost_grid_cols", "ghost_dot_size",
    "ghost_grid_inset", "arrow_colour", "arrow_alpha", "arrow_width",
    "arrow_lineend", "arrow_outline_colour", "arrow_outline_width", "arrowhead_length",
    "arrowhead_width", "arrow_checker_gap", "arrow_curve_enabled",
    "arrow_curve_offset", "arrow_curve_step", "arrow_curve_max",
    "arrow_curve_length_cap",
    "arrow_chain_full_angle", "arrow_chain_moderate_angle",
    "arrow_chain_max_angle", "arrow_chain_full_multiplier",
    "arrow_chain_moderate_multiplier", "arrow_chain_shallow_multiplier",
    "arrow_chain_short_length_radii", "arrow_chain_short_curve_max",
    "arrow_collinear_angle_tolerance", "arrow_overlap_threshold",
    "arrow_curve_two_path_split"
  )
  overlay_style <- movement_overlay_style()

  expect_s3_class(overlay_style, "backgammon_movement_overlay_style")
  expect_identical(names(overlay_style), expected)
  expect_true(is.na(overlay_style$ghost_fill))
  expect_identical(overlay_style$ghost_outline, "#000000")
  expect_identical(overlay_style$ghost_grid_rows, 7L)
  expect_identical(overlay_style$ghost_grid_cols, 7L)
  expect_error(movement_overlay_style(ghost_fill_alpha = 1.1), "between 0 and 1")
  expect_error(movement_overlay_style(ghost_grid_rows = 1L), "at least 2")
  expect_error(movement_overlay_style(arrow_width = 0), "greater than zero")
  expect_error(movement_overlay_style(arrow_lineend = "curved"), "arrow_lineend")
  expect_error(
    movement_overlay_style(arrow_curve_enabled = NA), "arrow_curve_enabled"
  )
  expect_error(
    movement_overlay_style(arrow_overlap_threshold = 1.1), "between 0 and 1"
  )
  expect_identical(
    movement_overlay_style(arrow_checker_gap = -0.2)$arrow_checker_gap,
    -0.2
  )
})
