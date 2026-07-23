overlay_test_position <- function(
    player = "white",
    white = integer(),
    black = integer(),
    white_bar = 0L,
    black_bar = 0L
) {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10"
  )

  points <- integer(24)

  if (length(white) > 0L) {
    points[as.integer(names(white))] <- as.integer(white)
  }

  if (length(black) > 0L) {
    indices <- as.integer(names(black))
    points[indices] <- -as.integer(black)
  }

  white_bar <- as.integer(white_bar)
  black_bar <- as.integer(black_bar)

  position$points <- as.integer(points)
  position$bar <- c(white = white_bar, black = black_bar)
  position$off <- c(
    white = as.integer(15L - sum(points[points > 0L]) - white_bar),
    black = as.integer(15L - sum(abs(points[points < 0L])) - black_bar)
  )
  position$on_roll <- player
  position$dice <- integer()
  position$action_marker <- "00"
  position$dice_action <- "00"
  position
}


test_that("move overlay input accepts notation and parsed moves", {
  parsed <- board_moves("13/8")

  expect_identical(
    backgammonboard:::normalize_move_overlay_input(parsed),
    parsed
  )
  expect_s3_class(
    backgammonboard:::normalize_move_overlay_input("13/8"),
    "backgammon_board_moves"
  )
  expect_null(backgammonboard:::normalize_move_overlay_input(NULL))

  expect_error(
    backgammonboard:::normalize_move_overlay_input(13),
    "must be NULL"
  )
})


test_that("simple selected move receives finite perspective-aware geometry", {
  overlay <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`13` = 1L)),
    moves = board_moves("13/8"),
    style = board_style("bms"),
    perspective = "white",
    role = "selected"
  )

  expect_s3_class(overlay, "backgammon_move_overlay")
  expect_identical(overlay$role, "selected")
  expect_identical(overlay$segments$linetype, "solid")
  expect_equal(nrow(overlay$segments), 1L)
  expect_true(all(is.finite(unlist(
    overlay$segments[c("x", "y", "xend", "yend", "curvature")]
  ))))
  expect_equal(nrow(overlay$markers), 0L)
  expect_equal(nrow(overlay$hits), 0L)
})


test_that("compound and repeated moves preserve structural order", {
  compound <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`24` = 1L)),
    moves = board_moves("24/18/13"),
    perspective = "white"
  )

  expect_identical(compound$segments$step_id, 1:2)
  expect_identical(compound$markers$label, c("1", "2"))
  expect_identical(compound$segments$chain_id, c(1L, 1L))

  repeated <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`13` = 2L)),
    moves = board_moves("13/8(2)"),
    perspective = "white"
  )

  expect_equal(nrow(repeated$segments), 2L)
  expect_identical(repeated$markers$label, c("1", "2"))
  expect_true(
    any(repeated$segments$x != repeated$segments$x[[1L]]) ||
      any(repeated$segments$y != repeated$segments$y[[1L]]) ||
      any(repeated$segments$xend != repeated$segments$xend[[1L]]) ||
      any(repeated$segments$yend != repeated$segments$yend[[1L]]) ||
      any(repeated$segments$curvature != repeated$segments$curvature[[1L]])
  )
})


test_that("bar entry, bearing off, and confirmed hits receive explicit geometry", {
  entered <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white_bar = 1L),
    moves = board_moves("bar/24"),
    perspective = "white"
  )
  expect_equal(nrow(entered$segments), 1L)
  expect_true(all(is.finite(unlist(
    entered$segments[c("x", "y", "xend", "yend")]
  ))))

  borne_off <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`6` = 1L)),
    moves = board_moves("6/off"),
    perspective = "white"
  )
  expect_gt(borne_off$segments$xend[[1L]], borne_off$segments$x[[1L]])

  hit <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(
      white = c(`13` = 1L),
      black = c(`8` = 1L)
    ),
    moves = board_moves("13/8*"),
    perspective = "white"
  )
  expect_true(hit$segments$hit_confirmed[[1L]])
  expect_equal(nrow(hit$hits), 1L)
  expect_identical(hit$hits$label, "\u00d7")
})


test_that("alternative overlays use dashed structural styling", {
  overlay <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`13` = 1L)),
    moves = board_moves("13/8"),
    perspective = "white",
    role = "alternative"
  )

  expect_identical(overlay$role, "alternative")
  expect_identical(overlay$segments$linetype, "dashed")
})


test_that("layer construction uses both halos above the board", {
  colors <- board_colors("bms")
  style <- board_style("bms")
  overlay <- backgammonboard:::move_overlay_geometry(
    position = overlay_test_position(white = c(`13` = 1L)),
    moves = board_moves("13/8"),
    style = style,
    perspective = "white"
  )

  plot <- backgammonboard:::add_move_overlay_layers(
    ggplot2::ggplot(),
    overlay,
    colors,
    style
  )

  expect_equal(length(plot$layers), 3L)
  expect_gt(style$arrow_halo_dark_ratio, style$arrow_halo_light_ratio)
  expect_gt(style$arrow_halo_light_ratio, 1)
  expect_identical(colors$arrow_halo_dark, "#081126")
  expect_identical(colors$arrow_halo_light, "#FFFDF8")
})


test_that("Black perspective changes geometry without changing factual state", {
  position <- overlay_test_position(
    player = "black",
    black = c(`12` = 1L)
  )
  original <- position

  white_view <- backgammonboard:::move_overlay_geometry(
    position,
    board_moves("12/17"),
    perspective = "white"
  )
  black_view <- backgammonboard:::move_overlay_geometry(
    position,
    board_moves("12/17"),
    perspective = "black"
  )

  expect_false(identical(white_view$segments, black_view$segments))
  expect_identical(position, original)
})
