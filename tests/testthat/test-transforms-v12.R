test_that("horizontal and vertical transforms are involutions and commute", {
  bounds <- list(x_min = 0, x_max = 17, y_min = 0, y_max = 12)
  data <- data.frame(
    x = c(1.2, 15.4), y = c(2.5, 9.1),
    xend = c(5.7, 13.2), yend = c(7.8, 1.6),
    source_x = c(2, 3), source_y = c(4, 5),
    destination_x = c(6, 7), destination_y = c(8, 9),
    curvature = c(0.2, -0.1), side = c("bottom", "top")
  )
  horizontal <- function(x) backgammonboard:::transform_coordinate_frame(x, bounds, TRUE, FALSE)
  vertical <- function(x) backgammonboard:::transform_coordinate_frame(x, bounds, FALSE, TRUE)

  expect_equal(horizontal(horizontal(data)), data)
  expect_equal(vertical(vertical(data)), data)
  expect_equal(horizontal(vertical(data)), vertical(horizontal(data)))
})


test_that("layout preparation and rendering never mutate factual positions", {
  position <- backgammon_position(fixture_xgid("asymmetric_white_roll"))
  original <- serialize(position, NULL, version = 3)
  for (near in c("player_1", "player_0")) {
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(
        position, decision = "checker_play",
        perspective = near, mirror_horizontal = mirror
      )
      expect_identical(serialize(position, NULL, version = 3), original)
      expect_identical(attr(plot, "backgammon_position"), position)
    }
  }
})


test_that("point values depend only on near player", {
  xgid <- fixture_xgid("asymmetric_white_roll")
  for (near in c("player_1", "player_0")) {
    labels <- list()
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(xgid, perspective = near, mirror_horizontal = mirror)
      prepared <- attr(plot, "backgammon_prepared_layout")
      labels[[as.character(mirror)]] <- prepared$geometry$point_labels[
        order(prepared$geometry$point_labels$point), c("point", "label")
      ]
    }
    expect_identical(labels$`FALSE`$label, labels$`TRUE`$label)
    expected <- if (near == "player_1") 1:24 else 24:1
    expect_identical(as.integer(labels$`FALSE`$label), expected)
  }
})
