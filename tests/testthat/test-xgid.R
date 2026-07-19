test_that("normalize_xgid accepts both complete supported textual forms", {
  full <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10"
  without_prefix <- sub("^XGID=", "", full)

  expect_identical(normalize_xgid(full), full)
  expect_identical(normalize_xgid(without_prefix), full)
  expect_identical(normalize_xgid(paste0("  ", full, "  ")), full)
})

test_that("normalize_xgid rejects incomplete or malformed identifiers", {
  expect_error(
    normalize_xgid("-b----E-C---eE---c-e----B-"),
    "xgid_missing_fields",
    fixed = TRUE
  )
  expect_error(
    normalize_xgid("XGID=-b----E-C---eE---c-e----Q-:0:0:1:52:0:0:3:0:10"),
    "xgid_invalid_position_character",
    fixed = TRUE
  )
})

test_that("validate_xgid returns inspectable invalid results", {
  missing_value <- validate_xgid(NA_character_)
  expect_s3_class(missing_value, "backgammon_xgid_validation")
  expect_false(missing_value$valid)
  expect_identical(missing_value$errors$code, "xgid_missing_value")

  incomplete <- validate_xgid("-b----E-C---eE---c-e----B-")
  expect_false(incomplete$valid)
  expect_true("xgid_missing_fields" %in% incomplete$errors$code)
})

test_that("validate_xgid treats programmer misuse as an error", {
  expect_error(validate_xgid(c("a", "b")), "exactly one")
  expect_error(validate_xgid(list("a")), "character vector")
})

test_that("validation catches factual-state failures", {
  wrong_bar_owner <- validate_xgid(
    "XGID=A-------------------------:0:0:1:00:0:0:0:0:10"
  )
  expect_false(wrong_bar_owner$valid)
  expect_true("xgid_invalid_bar_owner" %in% wrong_bar_owner$errors$code)

  too_many <- validate_xgid(
    "XGID=-P------------------------:0:0:1:00:0:0:0:0:10"
  )
  expect_false(too_many$valid)
  expect_true("xgid_impossible_checker_total" %in% too_many$errors$code)
})
