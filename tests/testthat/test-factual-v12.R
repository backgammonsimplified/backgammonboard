test_that("v1.2 decodes source roles through one fixed mapping", {
  entries <- rep("-", 26L)
  entries[[1L]] <- "b"
  entries[[2L]] <- "C"
  entries[[8L]] <- "a"
  entries[[26L]] <- "D"
  payload <- paste(entries, collapse = "")
  xgid <- paste0("XGID=", payload, ":0:-1:-1:00:3:5:0:0:8")
  position <- backgammon_position(xgid)

  expect_identical(position$player_mapping, c(top = "player_0", bottom = "player_1"))
  expect_identical(position$on_roll, "player_0")
  expect_identical(position$cube_owner, "player_0")
  expect_identical(position$score, c(player_0 = 5L, player_1 = 3L))
  expect_identical(position$bar, c(player_0 = 2L, player_1 = 4L))
  expect_identical(position$points[[1L]], 3L)
  expect_identical(position$points[[7L]], -1L)
  expect_identical(position$point_occupancy$owner[[1L]], "player_1")
  expect_identical(position$point_occupancy$owner[[7L]], "player_0")
})


test_that("turn changes only on-roll identity, never XGID point order", {
  payload <- "-b----E-C---eE---c-e----B-"
  player_0_roll <- backgammon_position(
    paste0("XGID=", payload, ":0:0:-1:00:0:0:0:0:8")
  )
  player_1_roll <- backgammon_position(
    paste0("XGID=", payload, ":0:0:1:00:0:0:0:0:8")
  )
  expect_identical(player_0_roll$points, player_1_roll$points)
  expect_identical(player_0_roll$bar, player_1_roll$bar)
  expect_identical(player_0_roll$on_roll, "player_0")
  expect_identical(player_1_roll$on_roll, "player_1")
})


test_that("mandatory case 18 has the exact v1.2 factual interpretation", {
  xgid <- "XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8"
  position <- backgammon_position(xgid)
  expect_identical(position$player_mapping, c(top = "player_0", bottom = "player_1"))
  expect_identical(position$on_roll, "player_0")
  expect_length(position$dice, 0L)
  expect_identical(position$cube_value, 1L)
  expect_identical(position$cube_owner, "center")

  for (near in c("player_1", "player_0")) {
    for (mirror in c(FALSE, TRUE)) {
      plot <- ggboard(xgid, perspective = near, mirror_horizontal = mirror)
      expect_identical(attr(plot, "backgammon_near_player"), near)
      expect_identical(attr(plot, "backgammon_mirror_horizontal"), mirror)
      expect_identical(attr(plot, "backgammon_decision"), "none")
      expect_identical(attr(plot, "backgammon_cube_display")$state, "centered")
      expect_length(attr(plot, "backgammon_prepared_layout")$dice$faces$die, 0L)
    }
  }
})
