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


test_that("board_moves reproduces every normalized seed fixture", {
  fixture_path <- testthat::test_path("fixtures", "move-notation-seed.csv")
  fixture <- utils::read.csv(
    fixture_path,
    stringsAsFactors = FALSE,
    na.strings = c("")
  )

  schema <- backgammonboard:::move_step_columns()

  for (case_id in unique(fixture$case_id)) {
    expected <- fixture[fixture$case_id == case_id, schema, drop = FALSE]
    notation <- unique(fixture$notation[fixture$case_id == case_id])

    expect_length(notation, 1L)

    expected$step_id <- as.integer(expected$step_id)
    expected$chain_id <- as.integer(expected$chain_id)
    expected$from_point <- as.integer(expected$from_point)
    expected$to_point <- as.integer(expected$to_point)
    expected$hit_marked <-
      tolower(trimws(as.character(expected$hit_marked))) == "true"
    rownames(expected) <- NULL

    actual_moves <- board_moves(notation)
    actual <- data.frame(
      step_id = actual_moves$step_id,
      chain_id = actual_moves$chain_id,
      source_token = actual_moves$source_token,
      from_type = actual_moves$from_type,
      from_point = actual_moves$from_point,
      to_type = actual_moves$to_type,
      to_point = actual_moves$to_point,
      hit_marked = actual_moves$hit_marked,
      repeat_group = actual_moves$repeat_group,
      stringsAsFactors = FALSE
    )

    expect_identical(actual, expected)
  }
})
