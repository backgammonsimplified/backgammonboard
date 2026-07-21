read_xgid_factual_fixtures <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "xgid-factual-fixtures.csv"),
    stringsAsFactors = FALSE,
    na.strings = c("")
  )
}

read_xgid_invalid_fixtures <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "xgid-invalid-fixtures.csv"),
    stringsAsFactors = FALSE,
    na.strings = c("")
  )
}

semicolon_integers <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(integer())
  }
  as.integer(strsplit(value, ";", fixed = TRUE)[[1L]])
}

fixture_by_id <- function(fixtures, fixture_id) {
  row <- fixtures[fixtures$fixture_id == fixture_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Fixture id must identify exactly one row: ", fixture_id, call. = FALSE)
  }
  row
}


test_that("pinned factual XGID fixtures decode into canonical semantic facts", {
  fixtures <- read_xgid_factual_fixtures()

  for (index in seq_len(nrow(fixtures))) {
    fixture <- fixtures[index, , drop = FALSE]
    validation <- validate_xgid(fixture$xgid)

    expect_true(validation$valid, info = fixture$fixture_id)
    expect_identical(validation$action_marker, fixture$action_marker)

    position <- backgammon_position(fixture$xgid)

    expect_identical(
      position$points,
      semicolon_integers(fixture$points),
      info = fixture$fixture_id
    )
    expect_identical(
      position$bar,
      c(
        white = as.integer(fixture$bar_white),
        black = as.integer(fixture$bar_black)
      ),
      info = fixture$fixture_id
    )
    expect_identical(
      position$off,
      c(
        white = as.integer(fixture$off_white),
        black = as.integer(fixture$off_black)
      ),
      info = fixture$fixture_id
    )
    expect_identical(position$on_roll, fixture$on_roll, info = fixture$fixture_id)
    expect_identical(
      position$dice,
      semicolon_integers(fixture$dice),
      info = fixture$fixture_id
    )
    expect_identical(position$action_marker, fixture$action_marker, info = fixture$fixture_id)
    expect_identical(position$cube_value, as.integer(fixture$cube_value), info = fixture$fixture_id)
    expect_identical(position$cube_owner, fixture$cube_owner, info = fixture$fixture_id)
    expect_identical(position$score_white, as.integer(fixture$score_white), info = fixture$fixture_id)
    expect_identical(position$score_black, as.integer(fixture$score_black), info = fixture$fixture_id)

    expected_match_length <- if (is.na(fixture$match_length)) {
      NA_integer_
    } else {
      as.integer(fixture$match_length)
    }
    expect_identical(position$match_length, expected_match_length, info = fixture$fixture_id)
    expect_identical(
      position$is_crawford,
      identical(tolower(as.character(fixture$is_crawford)), "true"),
      info = fixture$fixture_id
    )
    expect_equal(position$xgid_max_cube, as.numeric(fixture$xgid_max_cube), info = fixture$fixture_id)

    expect_equal(
      sum(pmax(position$points, 0L)) + position$bar[["white"]] + position$off[["white"]],
      15L,
      info = fixture$fixture_id
    )
    expect_equal(
      sum(pmax(-position$points, 0L)) + position$bar[["black"]] + position$off[["black"]],
      15L,
      info = fixture$fixture_id
    )
  }
})


test_that("turn-relative encodings preserve stable semantic position facts", {
  fixtures <- read_xgid_factual_fixtures()

  for (pair in list(
    c("opening_white_roll", "opening_black_roll"),
    c("asymmetric_white_roll", "asymmetric_black_roll")
  )) {
    white_turn <- backgammon_position(fixture_by_id(fixtures, pair[[1L]])$xgid)
    black_turn <- backgammon_position(fixture_by_id(fixtures, pair[[2L]])$xgid)

    expect_identical(white_turn$points, black_turn$points)
    expect_identical(white_turn$bar, black_turn$bar)
    expect_identical(white_turn$off, black_turn$off)
    expect_identical(white_turn$cube_owner, black_turn$cube_owner)
    expect_identical(white_turn$score, black_turn$score)
    expect_identical(white_turn$on_roll, "white")
    expect_identical(black_turn$on_roll, "black")
  }
})


test_that("prefix normalization is metamorphic", {
  fixture <- fixture_by_id(read_xgid_factual_fixtures(), "asymmetric_black_roll")
  without_prefix <- sub("^XGID=", "", fixture$xgid)

  expect_identical(normalize_xgid(without_prefix), fixture$xgid)
  expect_identical(backgammon_position(without_prefix), backgammon_position(fixture$xgid))
})


test_that("invalid XGID fixture corpus returns stable diagnostic codes", {
  fixtures <- read_xgid_invalid_fixtures()

  for (index in seq_len(nrow(fixtures))) {
    fixture <- fixtures[index, , drop = FALSE]
    result <- validate_xgid(fixture$xgid)

    expect_false(result$valid, info = fixture$fixture_id)
    expect_true(
      fixture$expected_code %in% result$errors$code,
      info = paste(fixture$fixture_id, paste(result$errors$code, collapse = ", "))
    )
  }
})


test_that("unsupported B and R markers remain inspectable diagnostics", {
  fixtures <- read_xgid_invalid_fixtures()

  for (fixture_id in c("unsupported_beaver", "unsupported_raccoon")) {
    fixture <- fixture_by_id(fixtures, fixture_id)
    result <- validate_xgid(fixture$xgid)

    expect_false(result$valid)
    expect_identical(result$action_marker, if (fixture_id == "unsupported_beaver") "B" else "R")
    expect_true("unsupported_action_marker" %in% result$errors$code)
    expect_error(backgammon_position(fixture$xgid), "unsupported_action_marker", fixed = TRUE)
  }
})
