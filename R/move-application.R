applied_move_step_columns <- function() {
  c(
    move_step_columns(),
    "player",
    "hit_confirmed",
    "hit_player",
    "entered_from_bar",
    "borne_off"
  )
}


apply_board_moves <- function(position, moves) {
  validate_move_application_position(position)
  validate_board_moves(moves)

  player <- position[["on_roll"]]
  supplied_die <- attr(moves, "die")
  if (isTRUE(attr(moves, "mover_relative"))) {
    validate_move_die_distances(moves, supplied_die)
    moves <- moves_for_render_player(moves, player)
  }
  opponent <- if (identical(player, "white")) "black" else "white"
  player_sign <- if (identical(player, "white")) 1L else -1L

  points <- as.integer(position[["points"]])
  bar <- stats::setNames(
    as.integer(position[["bar"]][c("white", "black")]),
    c("white", "black")
  )
  off <- stats::setNames(
    as.integer(position[["off"]][c("white", "black")]),
    c("white", "black")
  )

  applied_steps <- as.data.frame(moves, stringsAsFactors = FALSE)
  applied_steps[["player"]] <- rep(player, nrow(applied_steps))
  applied_steps[["hit_confirmed"]] <- rep(FALSE, nrow(applied_steps))
  applied_steps[["hit_player"]] <- rep(NA_character_, nrow(applied_steps))
  applied_steps[["entered_from_bar"]] <-
    applied_steps[["from_type"]] == "bar"
  applied_steps[["borne_off"]] <- applied_steps[["to_type"]] == "off"

  for (row_index in seq_len(nrow(applied_steps))) {
    step <- applied_steps[row_index, , drop = FALSE]
    validate_move_step_structure(
      step = step,
      player = player,
      points = points,
      bar = bar
    )

    if (identical(step[["from_type"]][[1L]], "bar")) {
      bar[[player]] <- bar[[player]] - 1L
    } else {
      from_point <- step[["from_point"]][[1L]]
      points[[from_point]] <- points[[from_point]] - player_sign
    }

    hit_confirmed <- FALSE

    if (identical(step[["to_type"]][[1L]], "off")) {
      off[[player]] <- off[[player]] + 1L
    } else {
      to_point <- step[["to_point"]][[1L]]
      destination_value <- points[[to_point]]
      opposing_count <- opponent_checker_count(
        destination_value = destination_value,
        player = player
      )

      if (opposing_count >= 2L) {
        stop_move_application_step_error(
          step,
          paste0(
            "destination point ", to_point,
            " is blocked by ", opposing_count, " ", opponent,
            " checkers"
          ),
          subclass = "backgammon_move_blocked_destination",
          player = player
        )
      }

      hit_confirmed <- opposing_count == 1L

      if (hit_confirmed) {
        points[[to_point]] <- 0L
        bar[[opponent]] <- bar[[opponent]] + 1L
        applied_steps[["hit_confirmed"]][[row_index]] <- TRUE
        applied_steps[["hit_player"]][[row_index]] <- opponent
      }

      points[[to_point]] <- points[[to_point]] + player_sign
    }

  }

  validate_resulting_checker_state(points, bar, off)

  checker_state <- list(
    points = as.integer(points),
    bar = stats::setNames(as.integer(bar), names(bar)),
    off = stats::setNames(as.integer(off), names(off))
  )

  structure(
    list(
      starting_xgid = position[["xgid"]],
      player = player,
      moves = moves,
      applied_steps = applied_steps[, applied_move_step_columns(), drop = FALSE],
      points = checker_state$points,
      bar = checker_state$bar,
      off = checker_state$off,
      checker_state = checker_state,
      die_validation_status = if (any(!is.na(supplied_die))) "checked" else "not_checked",
      full_play_validation_status = "not_performed"
    ),
    class = "backgammon_applied_moves"
  )
}


moves_for_render_player <- function(moves, player) {
  transformed <- moves
  if (identical(player, "black")) {
    from_points <- transformed$from_type == "point"
    to_points <- transformed$to_type == "point"
    transformed$from_point[from_points] <- 25L - transformed$from_point[from_points]
    transformed$to_point[to_points] <- 25L - transformed$to_point[to_points]
  }
  attr(transformed, "die") <- attr(moves, "die")
  attr(transformed, "label") <- attr(moves, "label")
  attr(transformed, "mover_relative") <- FALSE
  transformed
}


validate_move_die_distances <- function(moves, die) {
  checked <- which(!is.na(die))
  for (index in checked) {
    distance <- if (identical(moves$from_type[[index]], "bar")) {
      25L - moves$to_point[[index]]
    } else if (identical(moves$to_type[[index]], "off")) {
      moves$from_point[[index]]
    } else {
      moves$from_point[[index]] - moves$to_point[[index]]
    }
    if (!identical(as.integer(distance), die[[index]])) {
      stop_move_application_error(
        paste0(
          "Movement ", index, " has distance ", distance,
          " but specifies die ", die[[index]], " [die_distance_mismatch]"
        ),
        subclass = "backgammon_move_die_mismatch",
        step_id = index
      )
    }
  }
  invisible(moves)
}


validate_move_application_position <- function(position) {
  if (!inherits(position, "backgammon_position") || !is.list(position)) {
    stop_move_application_error(
      "`position` must be a `backgammon_position` object",
      subclass = "backgammon_move_invalid_position"
    )
  }

  points <- position[["points"]]
  if (!is.integer(points) || length(points) != 24L || anyNA(points)) {
    stop_move_application_error(
      "`position$points` must contain 24 non-missing integer occupancies",
      subclass = "backgammon_move_invalid_position"
    )
  }

  for (component in c("bar", "off")) {
    value <- position[[component]]
    if (!is.integer(value) ||
        !identical(names(value), c("white", "black")) ||
        anyNA(value) ||
        any(value < 0L)) {
      stop_move_application_error(
        paste0(
          "`position$", component,
          "` must be a named non-negative integer vector for White and Black"
        ),
        subclass = "backgammon_move_invalid_position"
      )
    }
  }

  on_roll <- position[["on_roll"]]
  if (!is.character(on_roll) ||
      length(on_roll) != 1L ||
      is.na(on_roll) ||
      !on_roll %in% c("white", "black")) {
    stop_move_application_error(
      "`position$on_roll` must be `white` or `black`",
      subclass = "backgammon_move_invalid_position"
    )
  }

  validate_resulting_checker_state(
    points = points,
    bar = position[["bar"]],
    off = position[["off"]],
    subclass = "backgammon_move_invalid_position"
  )

  invisible(position)
}


validate_move_step_structure <- function(step, player, points, bar) {
  from_type <- step[["from_type"]][[1L]]
  from_point <- step[["from_point"]][[1L]]
  to_type <- step[["to_type"]][[1L]]
  to_point <- step[["to_point"]][[1L]]

  if (bar[[player]] > 0L && !identical(from_type, "bar")) {
    stop_move_application_step_error(
      step,
      paste0(
        player,
        " must enter all checkers from the bar before moving a point checker"
      ),
      subclass = "backgammon_move_bar_priority",
      player = player
    )
  }

  if (identical(from_type, "bar")) {
    if (bar[[player]] < 1L) {
      stop_move_application_step_error(
        step,
        paste0(player, " has no checker on the bar"),
        subclass = "backgammon_move_empty_source",
        player = player
      )
    }

    valid_entry <- if (identical(player, "white")) {
      to_type == "point" && to_point %in% 19:24
    } else {
      to_type == "point" && to_point %in% 1:6
    }

    if (!valid_entry) {
      stop_move_application_step_error(
        step,
        paste0(
          player,
          " bar entry must land in the opponent's home board"
        ),
        subclass = "backgammon_move_invalid_bar_entry",
        player = player
      )
    }

    return(invisible(step))
  }

  source_value <- points[[from_point]]
  if (source_value == 0L) {
    stop_move_application_step_error(
      step,
      paste0("source point ", from_point, " is empty"),
      subclass = "backgammon_move_empty_source",
      player = player
    )
  }

  owns_source <- if (identical(player, "white")) {
    source_value > 0L
  } else {
    source_value < 0L
  }

  if (!owns_source) {
    stop_move_application_step_error(
      step,
      paste0(
        "source point ", from_point,
        " contains an opposing checker rather than a ", player, " checker"
      ),
      subclass = "backgammon_move_wrong_player",
      player = player
    )
  }

  if (identical(to_type, "point")) {
    correct_direction <- if (identical(player, "white")) {
      to_point < from_point
    } else {
      to_point > from_point
    }

    if (!correct_direction) {
      stop_move_application_step_error(
        step,
        paste0(
          player,
          " checkers must move in their semantic homeward direction"
        ),
        subclass = "backgammon_move_wrong_direction",
        player = player
      )
    }

    return(invisible(step))
  }

  source_in_home <- if (identical(player, "white")) {
    from_point %in% 1:6
  } else {
    from_point %in% 19:24
  }

  if (!source_in_home || player_has_checker_outside_home(points, player)) {
    stop_move_application_step_error(
      step,
      paste0(
        player,
        " may bear off only after all of that player's checkers are in the home board"
      ),
      subclass = "backgammon_move_invalid_bearoff",
      player = player
    )
  }

  invisible(step)
}


player_has_checker_outside_home <- function(points, player) {
  if (identical(player, "white")) {
    any(points[7:24] > 0L)
  } else {
    any(points[1:18] < 0L)
  }
}


opponent_checker_count <- function(destination_value, player) {
  if (identical(player, "white") && destination_value < 0L) {
    return(as.integer(abs(destination_value)))
  }

  if (identical(player, "black") && destination_value > 0L) {
    return(as.integer(destination_value))
  }

  0L
}


validate_resulting_checker_state <- function(
    points,
    bar,
    off,
    subclass = "backgammon_move_invalid_result"
) {
  white_total <- sum(points[points > 0L]) + bar[["white"]] + off[["white"]]
  black_total <- sum(abs(points[points < 0L])) + bar[["black"]] + off[["black"]]

  if (!identical(as.integer(white_total), 15L) ||
      !identical(as.integer(black_total), 15L)) {
    stop_move_application_error(
      paste0(
        "checker totals must remain exactly 15 for each player; found White ",
        white_total, " and Black ", black_total
      ),
      subclass = subclass
    )
  }

  invisible(list(points = points, bar = bar, off = off))
}


stop_move_application_step_error <- function(
    step,
    detail,
    subclass,
    player = NULL
) {
  stop_move_application_error(
    paste0(
      "Cannot apply atomic step ", step[["step_id"]][[1L]],
      " (", format_applied_location(step$from_type[[1L]], step$from_point[[1L]]),
      "/", format_applied_location(step$to_type[[1L]], step$to_point[[1L]]),
      "): ", detail
    ),
    subclass = subclass,
    step_id = step[["step_id"]][[1L]],
    player = player
  )
}


format_applied_location <- function(type, point) {
  if (identical(type, "point")) as.character(point) else type
}


stop_move_application_error <- function(
    message,
    subclass = "backgammon_move_application_error",
    step_id = NULL,
    player = NULL
) {
  condition <- structure(
    list(
      message = message,
      call = NULL,
      step_id = step_id,
      player = player
    ),
    class = unique(c(
      subclass,
      "backgammon_move_application_error",
      "backgammon_move_error",
      "error",
      "condition"
    ))
  )

  stop(condition)
}


validate_after_xgid <- function(applied, after_xgid) {
  if (is.null(after_xgid)) return(invisible(applied))
  if (!inherits(applied, "backgammon_applied_moves")) {
    stop("`applied` must be a movement application result.", call. = FALSE)
  }
  after <- backgammon_position(after_xgid)
  same_points <- identical(as.integer(applied$points), as.integer(after$points))
  same_bar <- identical(
    unname(as.integer(applied$bar[c("white", "black")])),
    unname(as.integer(after$bar[c(
      render_player_to_project_player("white"),
      render_player_to_project_player("black")
    )]))
  )
  same_off <- identical(
    unname(as.integer(applied$off[c("white", "black")])),
    unname(as.integer(after$off[c(
      render_player_to_project_player("white"),
      render_player_to_project_player("black")
    )]))
  )
  if (!same_points || !same_bar || !same_off) {
    stop(
      "Applied movements do not match `after_xgid` checker layout [after_xgid_mismatch]",
      call. = FALSE
    )
  }
  invisible(applied)
}
