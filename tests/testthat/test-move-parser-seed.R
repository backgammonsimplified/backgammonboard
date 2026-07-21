test_that("move-notation seed fixture has the frozen normalized schema", {
  fixture_path <- testthat::test_path("fixtures", "move-notation-seed.csv")
  fixture <- utils::read.csv(
    fixture_path,
    stringsAsFactors = FALSE,
    na.strings = c("")
  )

  expect_named(
    fixture,
    c(
      "case_id",
      "notation",
      "step_id",
      "chain_id",
      "source_token",
      "from_type",
      "from_point",
      "to_type",
      "to_point",
      "hit_marked",
      "repeat_group",
      "notes"
    )
  )

  required_notation <- c(
    "13/8",
    "13/8 6/5",
    "24/18/13",
    "13/8(2)",
    "bar/24",
    "6/off",
    "13/8*",
    "13/10(2) 8/5(2)"
  )

  expect_true(all(required_notation %in% fixture$notation))
  expect_equal(sum(fixture$case_id == "four_part_double"), 4L)
  hit_marked <- tolower(trimws(as.character(fixture$hit_marked)))
  expect_true(all(hit_marked %in% c("true", "false")))
  expect_true(any(hit_marked == "true"))
  expect_true(any(fixture$from_type == "bar"))
  expect_true(any(fixture$to_type == "off"))
})


test_that("move parser and overlay are intentionally absent from this seed", {
  expect_false(exists("parse_board_moves", mode = "function"))
  expect_false(exists("apply_board_moves", mode = "function"))
  expect_false(exists("draw_move_overlay", mode = "function"))
})
