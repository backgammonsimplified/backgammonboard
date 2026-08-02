# Resolve the public display contract once, before geometry is built.
resolve_display_context <- function(
    position,
    moves,
    decision,
    perspective,
    player_labels,
    score_format) {
  assert_backgammon_position(position)
  labels <- validate_player_labels(player_labels)
  decision <- match.arg(
    decision,
    c("auto", "checker_play", "roll_double", "take_pass", "none")
  )
  perspective <- match.arg(
    perspective,
    c("decision_maker", "on_roll", "player_0", "player_1")
  )
  score_format <- match.arg(score_format, c("away", "raw", "both"))

  resolved_decision <- if (identical(decision, "auto")) {
    if (!is.null(moves)) {
      "checker_play"
    } else if (length(position$dice) == 2L) {
      "checker_play"
    } else {
      "none"
    }
  } else {
    decision
  }
  state <- if (length(position$dice) == 2L) {
    "dice"
  } else if (identical(position$cube_action, "double")) {
    "double"
  } else {
    "no_dice"
  }
  valid <- switch(
    state,
    dice = resolved_decision %in% c("checker_play", "none"),
    no_dice = resolved_decision %in% c("none", "roll_double"),
    double = resolved_decision %in% c("take_pass", "none"),
    FALSE
  )
  if (!valid) {
    stop(
      paste0(
        "Invalid decision/factual-state combination: `", state,
        "` with decision `", resolved_decision, "` [invalid_decision_state]"
      ),
      call. = FALSE
    )
  }
  if (!is.null(moves) && !identical(resolved_decision, "checker_play")) {
    stop("`moves` requires decision `checker_play` [moves_without_checker_play]", call. = FALSE)
  }
  if (identical(resolved_decision, "checker_play") && is.null(moves) &&
      length(position$dice) != 2L) {
    stop("Checker play requires factual dice or structured movements [missing_checker_play]", call. = FALSE)
  }
  if (identical(position$crawford_status, "crawford") &&
      resolved_decision %in% c("roll_double", "take_pass")) {
    stop("Cube decisions are unavailable during Crawford [crawford_cube_suppressed]", call. = FALSE)
  }
  if (resolved_decision %in% c("roll_double", "take_pass") &&
      2 * position$cube_value > min(position$max_cube, supported_cube_max())) {
    stop("The offered cube exceeds the factual maximum cube [maximum_cube]", call. = FALSE)
  }
  if (identical(resolved_decision, "roll_double") &&
      !position$cube_owner %in% c("center", position$on_roll)) {
    stop("The player on roll does not own the cube [cube_not_available]", call. = FALSE)
  }

  decision_maker <- switch(
    resolved_decision,
    checker_play = position$on_roll,
    roll_double = position$on_roll,
    take_pass = other_player(position$on_roll),
    none = NA_character_
  )
  resolved_perspective <- switch(
    perspective,
    player_0 = "player_0",
    player_1 = "player_1",
    on_roll = position$on_roll,
    decision_maker = if (is.na(decision_maker)) position$on_roll else decision_maker
  )

  structure(
    list(
      decision = resolved_decision,
      decision_maker = decision_maker,
      perspective = resolved_perspective,
      player_labels = labels,
      score_format = score_format,
      show_dice = length(position$dice) == 2L &&
        resolved_decision %in% c("checker_play", "none"),
      show_normal_cube = !identical(position$crawford_status, "crawford") &&
        !identical(resolved_decision, "take_pass"),
      show_offered_cube = identical(resolved_decision, "take_pass")
    ),
    class = "backgammon_display_context"
  )
}


validate_player_labels <- function(player_labels) {
  if (!is.character(player_labels) || length(player_labels) != 2L ||
      is.null(names(player_labels)) ||
      !identical(sort(names(player_labels)), c("player_0", "player_1")) ||
      anyNA(player_labels) || any(!nzchar(trimws(player_labels)))) {
    stop(
      "`player_labels` must name one non-empty label for `player_0` and `player_1`.",
      call. = FALSE
    )
  }
  player_labels[c("player_0", "player_1")]
}


other_player <- function(player) {
  if (!player %in% c("player_0", "player_1")) {
    stop("Player identity must be `player_0` or `player_1`.", call. = FALSE)
  }
  if (identical(player, "player_0")) "player_1" else "player_0"
}


to_render_player <- function(player) {
  switch(
    player,
    player_0 = "white",
    player_1 = "black",
    stop("Player identity must be `player_0` or `player_1`.", call. = FALSE)
  )
}


# Small visual-layer context. It is not an accepted public input.
board_context <- function(
    decision = c("none", "cube_offer", "cube_response"),
    offer = NULL) {
  decision <- match.arg(decision)
  if (!is.null(offer) && !inherits(offer, "backgammon_cube_offer_context")) {
    stop("`offer` must be an internal cube offer.", call. = FALSE)
  }
  structure(list(decision = decision, offer = offer), class = "backgammon_board_context")
}


normalize_board_context <- function(context) {
  if (is.null(context)) return(board_context("none"))
  if (!inherits(context, "backgammon_board_context")) {
    stop("Invalid internal board context.", call. = FALSE)
  }
  context
}


position_player_label <- function(position, player) {
  assert_backgammon_position(position)
  player <- normalize_semantic_player(player)
  labels <- position$player_labels
  if (!is.null(labels) && player %in% names(labels)) return(labels[[player]])
  if (identical(player, "white")) "White" else "Black"
}


position_status_label <- function(position, context = NULL, ...) {
  assert_backgammon_position(position)
  context <- normalize_board_context(context)
  roller <- position_player_label(position, position$on_roll)
  if (identical(context$decision, "none")) {
    if (length(position$dice) == 2L) {
      return(paste0(roller, " on roll, to play: ", paste(position$dice, collapse = "-")))
    }
    return(paste0(roller, " on roll"))
  }
  if (identical(context$decision, "cube_offer")) {
    action <- if (position$cube_value == 1L) "Roll or Double?" else "Roll or Redouble?"
    return(paste0(roller, " to decide: ", action))
  }
  if (is.null(context$offer)) stop("Cube response requires a factual offer.", call. = FALSE)
  paste0(
    position_player_label(position, context$offer$receiver),
    " to decide: Take or Pass?"
  )
}
