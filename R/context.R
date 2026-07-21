# Internal constructor for explicit instructional board context.
board_context <- function(
    decision = c("none", "cube_offer", "cube_response"),
    offer = NULL
) {
  decision <- match.arg(decision)

  if (!is.null(offer) && !inherits(offer, "backgammon_cube_offer_context")) {
    stop("`offer` must be NULL or created by cube_offer_context().", call. = FALSE)
  }

  structure(
    list(
      decision = decision,
      offer = offer
    ),
    class = "backgammon_board_context"
  )
}


normalize_board_context <- function(context) {
  if (is.null(context)) {
    return(board_context("none"))
  }

  if (!inherits(context, "backgammon_board_context")) {
    stop("`context` must be NULL or created by board_context().", call. = FALSE)
  }

  context
}


new_board_context_validation <- function(
    valid,
    decision,
    reason,
    cube = NULL
) {
  structure(
    list(
      valid = isTRUE(valid),
      decision = decision,
      reason = reason,
      cube = cube
    ),
    class = "backgammon_board_context_validation"
  )
}


validate_board_context <- function(
    position,
    context = NULL,
    max_cube_value = supported_cube_max()
) {
  assert_backgammon_position(position)
  context <- normalize_board_context(context)
  max_cube_value <- validate_max_cube_value_argument(max_cube_value)

  if (identical(context$decision, "none")) {
    return(new_board_context_validation(
      valid = TRUE,
      decision = "none",
      reason = "valid"
    ))
  }

  if (identical(context$decision, "cube_offer")) {
    if (!is.null(context$offer)) {
      cube <- validate_cube_offer_context(
        position = position,
        offer = context$offer,
        max_cube_value = max_cube_value
      )
      cube$valid <- FALSE
      cube$reason <- "offer_already_pending"

      return(new_board_context_validation(
        valid = FALSE,
        decision = context$decision,
        reason = "offer_already_pending",
        cube = cube
      ))
    }

    cube <- validate_cube_offer_context(
      position = position,
      offer = NULL,
      max_cube_value = max_cube_value
    )

    return(new_board_context_validation(
      valid = cube$valid,
      decision = context$decision,
      reason = cube$reason,
      cube = cube
    ))
  }

  if (is.null(context$offer)) {
    return(new_board_context_validation(
      valid = FALSE,
      decision = context$decision,
      reason = "missing_offer"
    ))
  }

  cube <- validate_cube_offer_context(
    position = position,
    offer = context$offer,
    max_cube_value = max_cube_value
  )

  new_board_context_validation(
    valid = cube$valid,
    decision = context$decision,
    reason = cube$reason,
    cube = cube
  )
}


stop_for_board_context_validation <- function(validation) {
  if (!inherits(validation, "backgammon_board_context_validation")) {
    stop("`validation` must be a board-context validation result.", call. = FALSE)
  }

  stop(
    paste0("Invalid board context [", validation$reason, "]"),
    call. = FALSE
  )
}


sentence_case_player <- function(player) {
  player <- normalize_semantic_player(player)
  if (is.null(player) || is.na(player)) {
    stop("Unsupported semantic player.", call. = FALSE)
  }

  if (identical(player, "white")) "White" else "Black"
}


# Derive factual or explicitly instructional board status text.
position_status_label <- function(
    position,
    context = NULL,
    max_cube_value = supported_cube_max()
) {
  assert_backgammon_position(position)
  context <- normalize_board_context(context)
  roller <- sentence_case_player(position$on_roll)

  if (identical(context$decision, "none")) {
    if (length(position$dice) == 2L) {
      return(paste0(
        roller,
        " on roll, to play: ",
        position$dice[[1L]],
        "-",
        position$dice[[2L]]
      ))
    }

    return(paste0(roller, " on roll"))
  }

  validation <- validate_board_context(
    position = position,
    context = context,
    max_cube_value = max_cube_value
  )

  if (!isTRUE(validation$valid)) {
    stop_for_board_context_validation(validation)
  }

  if (identical(context$decision, "cube_offer")) {
    question <- if (identical(validation$cube$action, "double")) {
      "Roll or Double?"
    } else {
      "Roll or Redouble?"
    }

    return(paste0(
      sentence_case_player(validation$cube$offerer),
      " to decide: ",
      question
    ))
  }

  paste0(
    sentence_case_player(validation$cube$receiver),
    " to decide: Take or Pass?"
  )
}
