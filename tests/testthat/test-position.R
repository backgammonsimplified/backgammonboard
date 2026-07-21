test_that("opening position decodes into White-relative canonical points", {
  xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10"
  position <- backgammon_position(xgid)

  expected <- integer(24)
  expected[c(1, 12, 17, 19)] <- c(-2L, -5L, -3L, -5L)
  expected[c(6, 8, 13, 24)] <- c(5L, 3L, 5L, 2L)

  expect_s3_class(position, "backgammon_position")
  expect_identical(position$points, expected)
  expect_identical(position$bar, c(white = 0L, black = 0L))
  expect_identical(position$off, c(white = 0L, black = 0L))
  expect_identical(position$on_roll, "white")
  expect_identical(position$dice, c(5L, 2L))
  expect_identical(position$cube_value, 1L)
  expect_identical(position$cube_owner, "center")
  expect_identical(position$play_context, "unlimited")
  expect_false(position$is_crawford)
})

test_that("asymmetric fixture decodes bar and borne-off checkers", {
  xgid <- "XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10"
  position <- backgammon_position(xgid)

  expected <- integer(24)
  expected[c(1, 2, 4, 21)] <- c(6L, 4L, 1L, 1L)
  expected[c(3, 19, 22, 23, 24)] <- c(-1L, -1L, -3L, -2L, -2L)

  expect_identical(position$points, expected)
  expect_identical(position$bar, c(white = 0L, black = 1L))
  expect_identical(position$off, c(white = 3L, black = 5L))
  expect_identical(position$cube_value, 2L)
  expect_identical(position$cube_owner, "black")
  expect_identical(position$score, c(white = 3L, black = 0L))
  expect_identical(position$match_length, 7L)
  expect_false(position$is_crawford)
})

test_that("Crawford flag describes the current game only", {
  crawford <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:21:9:10:1:11:10"
  )
  expect_true(crawford$is_crawford)

  post <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:21:10:8:0:11:10"
  )
  expect_false(post$is_crawford)
})
