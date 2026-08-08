#' Construct a factual backgammon position
#'
#' `backgammon_position()` converts one complete XGID into factual state. The
#' fixed identities are `player_0` (the XGID top player) and `player_1` (the
#' XGID bottom player); labels and screen placement are display context, not facts.
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
  payload <- decode_xgid_payload(fields[["position"]], parsed$turn_code)
  play_context <- if (parsed$match_length == 0L) "unlimited" else "match"

  dice <- if (grepl("^[1-6][1-6]$", parsed$dice_action)) {
    as.integer(strsplit(parsed$dice_action, "", fixed = TRUE)[[1L]])
  } else {
    integer()
  }

  cube_owner <- xgid_cube_owner_player(parsed$cube_owner_code)
  cube_action <- switch(
    parsed$dice_action,
    D = "double",
    B = "beaver",
    R = "raccoon",
    "none"
  )

  unlimited_rules <- if (identical(play_context, "unlimited")) {
    list(
      jacoby = parsed$crawford_jacoby %in% c(1L, 3L),
      beavers_allowed = parsed$crawford_jacoby %in% c(2L, 3L)
    )
  } else {
    list(jacoby = FALSE, beavers_allowed = FALSE)
  }

  structure(
    list(
      xgid = xgid,
      source_roles = c(top = "top", bottom = "bottom"),
      player_mapping = xgid_source_player_map(),
      play_context = play_context,
      points = as.integer(payload$points),
      point_occupancy = payload$point_occupancy,
      bar = stats::setNames(as.integer(payload$bar), c("player_0", "player_1")),
      off = stats::setNames(as.integer(payload$off), c("player_0", "player_1")),
      on_roll = xgid_turn_player(parsed$turn_code),
      dice = dice,
      cube_value = as.integer(2^parsed$cube_exponent),
      cube_owner = cube_owner,
      cube_action = cube_action,
      score = c(
        player_0 = as.integer(parsed$score_second),
        player_1 = as.integer(parsed$score_first)
      ),
      match_length = if (identical(play_context, "match")) {
        as.integer(parsed$match_length)
      } else {
        NA_integer_
      },
      crawford_status = if (identical(play_context, "unlimited")) {
        "not_applicable"
      } else if (identical(parsed$crawford_jacoby, 1L)) {
        "crawford"
      } else {
        "none"
      },
      jacoby = unlimited_rules$jacoby,
      beavers_allowed = unlimited_rules$beavers_allowed,
      max_cube = 2^parsed$max_cube_exponent
    ),
    class = "backgammon_position"
  )
}


# Internal adapter for the established visual layer. It is deliberately not an
# accepted input type and never escapes as the factual plot attribute.
as_render_position <- function(
    position,
    player_labels = c(player_0 = "Foey", player_1 = "Homey")) {
  assert_backgammon_position(position)
  labels <- validate_player_labels(player_labels)

  marker <- if (length(position$dice) == 2L) {
    paste0(position$dice, collapse = "")
  } else if (identical(position$cube_action, "double")) {
    "D"
  } else {
    "00"
  }

  render <- position
  render$bar <- c(
    white = unname(position$bar[[render_player_to_project_player("white")]]),
    black = unname(position$bar[[render_player_to_project_player("black")]])
  )
  render$off <- c(
    white = unname(position$off[[render_player_to_project_player("white")]]),
    black = unname(position$off[[render_player_to_project_player("black")]])
  )
  render$on_roll <- project_player_to_render_player(position$on_roll)
  render$cube_owner <- switch(
    position$cube_owner,
    player_0 = project_player_to_render_player("player_0"),
    player_1 = project_player_to_render_player("player_1"),
    center = "center"
  )
  render$score <- c(
    white = unname(position$score[[render_player_to_project_player("white")]]),
    black = unname(position$score[[render_player_to_project_player("black")]])
  )
  render$score_white <- unname(render$score[["white"]])
  render$score_black <- unname(render$score[["black"]])
  render$is_crawford <- identical(position$crawford_status, "crawford")
  render$action_marker <- marker
  render$dice_action <- marker
  render$xgid_max_cube <- position$max_cube
  render$encoded_max_cube <- position$max_cube
  render$max_supported_cube_value <- supported_cube_max()
  render$beavers <- position$beavers_allowed
  render$player_labels <- c(
    white = unname(labels[[render_player_to_project_player("white")]]),
    black = unname(labels[[render_player_to_project_player("black")]])
  )
  class(render) <- c("backgammon_render_position", "backgammon_position")
  render
}


#' @export
print.backgammon_position <- function(x, ...) {
  cat("<backgammon_position>\n")
  cat("  Context: ", x$play_context, "\n", sep = "")
  cat("  On roll: ", x$on_roll, "\n", sep = "")
  cat(
    "  Dice: ",
    if (length(x$dice) == 2L) paste(x$dice, collapse = "-") else "not rolled",
    "\n",
    sep = ""
  )
  cat("  Cube: ", x$cube_value, " (", x$cube_owner, ")\n", sep = "")

  if (identical(x$play_context, "match")) {
    away <- x$match_length - x$score
    cat(
      "  Score: player_0 ", x$score[["player_0"]],
      " / player_1 ", x$score[["player_1"]],
      " - ", away[["player_0"]], "-away / ", away[["player_1"]], "-away\n",
      sep = ""
    )
    cat("  Crawford: ", x$crawford_status, "\n", sep = "")
  } else {
    cat("  Unlimited play\n")
  }

  invisible(x)
}
