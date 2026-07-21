#' Construct a factual backgammon position
#'
#' `backgammon_position()` converts a supported complete XGID into a factual
#' position object. White and Black are semantic identities; display perspective
#' and instructional wording are deliberately absent from this object.
#'
#' The package supports factual cube values through 64. The XGID maximum-cube
#' metadata is preserved separately and does not expand the package rendering
#' limit.
#'
#' @param x A complete XGID string, with or without the `XGID=` prefix.
#'
#' @return An object of class `backgammon_position`.
#' @export
backgammon_position <- function(x) {
  xgid <- normalize_xgid(x)
  fields <- strsplit(substring(xgid, 6L), ":", fixed = TRUE)[[1L]]
  names(fields) <- xgid_field_names()

  parsed <- parse_validated_xgid_fields(fields)
  payload <- decode_xgid_payload(
    fields[["position"]],
    parsed$turn_code
  )
  play_context <- if (parsed$match_length == 0L) "unlimited" else "match"

  score <- c(
    white = parsed$score_white,
    black = parsed$score_black
  )

  dice <- if (grepl("^[1-6][1-6]$", parsed$dice_action)) {
    as.integer(strsplit(parsed$dice_action, "", fixed = TRUE)[[1L]])
  } else {
    integer()
  }

  cube_owner <- switch(
    as.character(parsed$cube_owner_code),
    `-1` = "black",
    `0` = "center",
    `1` = "white"
  )

  unlimited_rules <- if (play_context == "unlimited") {
    list(
      jacoby = parsed$crawford_jacoby %in% c(1L, 3L),
      beavers = parsed$crawford_jacoby %in% c(2L, 3L)
    )
  } else {
    list(jacoby = FALSE, beavers = FALSE)
  }

  structure(
    list(
      xgid = xgid,
      play_context = play_context,
      points = as.integer(payload$points),
      bar = stats::setNames(as.integer(payload$bar), names(payload$bar)),
      off = stats::setNames(as.integer(payload$off), names(payload$off)),
      on_roll = if (parsed$turn_code == 1L) "white" else "black",
      dice = dice,
      cube_value = as.integer(2^parsed$cube_exponent),
      cube_owner = cube_owner,
      score_white = as.integer(parsed$score_white),
      score_black = as.integer(parsed$score_black),
      score = stats::setNames(as.integer(score), names(score)),
      match_length = if (play_context == "match") {
        as.integer(parsed$match_length)
      } else {
        NA_integer_
      },
      is_crawford = identical(play_context, "match") &&
        identical(parsed$crawford_jacoby, 1L),
      position_payload = fields[["position"]],
      action_marker = parsed$dice_action,
      dice_action = parsed$dice_action,
      cube_exponent = as.integer(parsed$cube_exponent),
      xgid_max_cube = 2^parsed$max_cube_exponent,
      encoded_max_cube = 2^parsed$max_cube_exponent,
      max_cube = 2^parsed$max_cube_exponent,
      max_cube_exponent = as.integer(parsed$max_cube_exponent),
      max_supported_cube_value = supported_cube_max(),
      jacoby = unlimited_rules$jacoby,
      beavers = unlimited_rules$beavers
    ),
    class = "backgammon_position"
  )
}

#' @export
print.backgammon_position <- function(x, ...) {
  cat("<backgammon_position>\n")
  cat("  Context: ", x$play_context, "\n", sep = "")
  cat("  On roll: ", x$on_roll, "\n", sep = "")

  if (length(x$dice) == 2L) {
    cat("  Dice: ", paste(x$dice, collapse = "-"), "\n", sep = "")
  } else {
    cat("  Dice: not rolled\n")
  }

  cat("  Cube: ", x$cube_value, " (", x$cube_owner, ")\n", sep = "")

  if (identical(x$play_context, "match")) {
    away <- x$match_length - c(
      white = x$score_white,
      black = x$score_black
    )

    cat(
      "  Score: White ", x$score_white,
      " / Black ", x$score_black,
      " - ", away[["white"]], "-away / ", away[["black"]], "-away\n",
      sep = ""
    )
    cat("  Crawford game: ", if (isTRUE(x$is_crawford)) "yes" else "no", "\n", sep = "")
  } else {
    cat("  Unlimited play\n")
  }

  invisible(x)
}
