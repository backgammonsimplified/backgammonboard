#' Construct a factual backgammon position
#'
#' `backgammon_position()` converts a supported complete XGID into a factual
#' position object. White and Black are semantic identities; display perspective
#' is deliberately absent from this object.
#'
#' @param x A complete XGID string, with or without the `XGID=` prefix.
#'
#' @return An object of class `backgammon_position`.
#' @export
backgammon_position <- function(x) {
  xgid <- normalize_xgid(x)
  fields <- strsplit(substring(xgid, 6L), ":", fixed = TRUE)[[1L]]
  names(fields) <- c(
    "position", "cube_exponent", "cube_owner", "turn", "dice_action",
    "score_white", "score_black", "crawford_jacoby", "match_length",
    "max_cube_exponent"
  )

  parsed <- parse_validated_xgid_fields(fields)
  payload <- decode_xgid_payload(fields[["position"]])
  play_context <- if (parsed$match_length == 0L) "unlimited" else "match"

  score <- c(
    white = parsed$score_white,
    black = parsed$score_black
  )

  crawford_status <- resolve_crawford_status(
    play_context = play_context,
    crawford_code = parsed$crawford_jacoby,
    score = score,
    match_length = parsed$match_length
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

  action <- switch(
    parsed$dice_action,
    `00` = "roll_double",
    D = "double",
    B = "beaver",
    R = "raccoon",
    "checker_play"
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
      cube_value = 2^parsed$cube_exponent,
      cube_owner = cube_owner,
      score = stats::setNames(as.integer(score), names(score)),
      match_length = if (play_context == "match") parsed$match_length else NA_integer_,
      crawford_status = crawford_status,
      max_cube = 2^parsed$max_cube_exponent,
      position_payload = fields[["position"]],
      dice_action = parsed$dice_action,
      action = action,
      cube_exponent = parsed$cube_exponent,
      max_cube_exponent = parsed$max_cube_exponent,
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
    cat("  Action: ", x$dice_action, "\n", sep = "")
  }
  cat("  Cube: ", x$cube_value, " (", x$cube_owner, ")\n", sep = "")
  if (x$play_context == "match") {
    away <- x$match_length - x$score
    cat(
      "  Score: White ", x$score[["white"]], " / Black ", x$score[["black"]],
      "  - ", away[["white"]], "-away / ", away[["black"]], "-away\n",
      sep = ""
    )
    cat("  Crawford: ", x$crawford_status, "\n", sep = "")
  } else {
    cat("  Unlimited play\n")
  }
  invisible(x)
}

resolve_crawford_status <- function(
    play_context,
    crawford_code,
    score,
    match_length) {
  if (play_context == "unlimited") {
    return("not_applicable")
  }
  if (crawford_code == 1L) {
    return("crawford")
  }
  if (match_length > 1L && any(match_length - score == 1L)) {
    return("post_crawford")
  }
  "none"
}
