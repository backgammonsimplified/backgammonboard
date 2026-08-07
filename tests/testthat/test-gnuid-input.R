testthat::test_that("ggboard GNU input delegates to backgammoncalculator", {
  testthat::skip_if_not_installed("backgammoncalculator")

  position_id <- "4HPwATDgc/ABMA"
  match_id <- "8IhuACAACAAE"
  gnuid <- paste(position_id, match_id, sep = ":")

  expected <- normalize_xgid(
    backgammoncalculator::gnuid_to_xgid(
      position_id = position_id,
      match_id = match_id
    )
  )

  testthat::expect_identical(
    .resolve_ggboard_input(gnuid),
    expected
  )

  testthat::expect_identical(
    .resolve_ggboard_input(position_id, match_id),
    expected
  )
})

testthat::test_that("release XGID path is unchanged", {
  xgid <- paste0(
    "XGID=-b----E-C---eE---c-e----B-:",
    "0:0:1:53:1:2:1:3:10"
  )
  testthat::expect_identical(.resolve_ggboard_input(xgid), xgid)
})

testthat::test_that("all three stages of each round trip render equivalently", {
  testthat::skip_if_not_installed("backgammoncalculator")

  original_gnuid <- "4HPwATDgc/ABMA:8IhuACAACAAE"
  xgid_from_gnu <- .gnuid_to_xgid(original_gnuid)
  gnuid_roundtrip <- .xgid_to_gnuid(xgid_from_gnu)

  original_xgid <- xgid_from_gnu
  gnuid_from_xg <- .xgid_to_gnuid(original_xgid)
  xgid_roundtrip <- .gnuid_to_xgid(gnuid_from_xg)

  common <- list(
    perspective = "player_1",
    mirror_horizontal = FALSE
  )

  gnu_a <- do.call(ggboard, c(list(x = original_gnuid), common))
  gnu_b <- do.call(ggboard, c(list(x = xgid_from_gnu), common))
  gnu_c <- do.call(ggboard, c(list(x = gnuid_roundtrip), common))

  xg_a <- do.call(ggboard, c(list(x = original_xgid), common))
  xg_b <- do.call(ggboard, c(list(x = gnuid_from_xg), common))
  xg_c <- do.call(ggboard, c(list(x = xgid_roundtrip), common))

  build <- function(plot) ggplot2::ggplot_build(plot)$data

  testthat::expect_equal(build(gnu_a), build(gnu_b))
  testthat::expect_equal(build(gnu_b), build(gnu_c))
  testthat::expect_equal(build(xg_a), build(xg_b))
  testthat::expect_equal(build(xg_b), build(xg_c))
})
