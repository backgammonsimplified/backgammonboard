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
  expect_identical(colors$bar_fill, "#D8C5A5")
  expect_identical(colors$frame_fill, "#D8C5A5")
})