.BACKGAMMONBOARD_MAX_CUBE_VALUE <- 64L


supported_cube_max <- function() {
  .BACKGAMMONBOARD_MAX_CUBE_VALUE
}


is_single_whole_number <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= -.Machine$integer.max &&
    x <= .Machine$integer.max &&
    x == floor(x)
}


as_integer_or_na <- function(x) {
  if (is_single_whole_number(x)) as.integer(x) else NA_integer_
}


is_power_of_two <- function(x) {
  if (!is_single_whole_number(x) || x < 1L) {
    return(FALSE)
  }

  exponent <- log2(as.integer(x))
  isTRUE(all.equal(exponent, round(exponent), tolerance = 1e-12))
}


normalize_semantic_player <- function(player) {
  if (is.null(player)) {
    return(NULL)
  }

  if (
    !is.character(player) ||
    length(player) != 1L ||
    is.na(player) ||
    !nzchar(trimws(player))
  ) {
    return(NA_character_)
  }

  resolved <- tolower(trimws(player))
  if (!resolved %in% c("white", "black")) {
    return(NA_character_)
  }

  resolved
}


other_semantic_player <- function(player) {
  player <- normalize_semantic_player(player)
  if (is.null(player) || is.na(player)) {
    return(NULL)
  }

  if (identical(player, "white")) "black" else "white"
}


assert_backgammon_position <- function(position) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  invisible(position)
}


validate_max_cube_value_argument <- function(max_cube_value) {
  if (
    !is_single_whole_number(max_cube_value) ||
    as.integer(max_cube_value) != supported_cube_max()
  ) {
    stop(
      paste0(
        "`max_cube_value` must be the package-supported value ",
        supported_cube_max(),
        "."
      ),
      call. = FALSE
    )
  }

  supported_cube_max()
}


# Internal constructor for a pending cube offer.
#
# This object is display and decision context. It is not factual cube ownership.
cube_offer_context <- function(
    offerer = NULL,
    receiver = NULL,
    current_value = NULL,
    offered_value = NULL
) {
  normalized_offerer <- normalize_semantic_player(offerer)
  normalized_receiver <- normalize_semantic_player(receiver)

  current_value <- if (is.null(current_value)) {
    NULL
  } else if (is_single_whole_number(current_value)) {
    as.integer(current_value)
  } else {
    current_value
  }

  if (is.null(offered_value) && is_single_whole_number(current_value)) {
    offered_value <- as.integer(current_value) * 2L
  } else if (is_single_whole_number(offered_value)) {
    offered_value <- as.integer(offered_value)
  }

  structure(
    list(
      offerer = normalized_offerer,
      receiver = normalized_receiver,
      current_value = current_value,
      offered_value = offered_value
    ),
    class = "backgammon_cube_offer_context"
  )
}


new_cube_context_validation <- function(
    valid,
    action,
    current_value,
    offered_value,
    reason,
    offerer = NULL,
    receiver = NULL,
    offerer_away = NA_integer_
) {
  structure(
    list(
      valid = isTRUE(valid),
      action = action,
      current_value = as_integer_or_na(current_value),
      offered_value = as_integer_or_na(offered_value),
      reason = as.character(reason),
      offerer = offerer,
      receiver = receiver,
      offerer_away = as.integer(offerer_away)
    ),
    class = "backgammon_cube_context_validation"
  )
}


cube_validation_result <- function(
    valid,
    position,
    offerer,
    receiver,
    current_value,
    offered_value,
    reason,
    offerer_away = NA_integer_
) {
  action <- if (
    identical(position$cube_owner, "center") ||
    (is_single_whole_number(current_value) && as.integer(current_value) == 1L)
  ) {
    "double"
  } else {
    "redouble"
  }

  new_cube_context_validation(
    valid = valid,
    action = action,
    current_value = current_value,
    offered_value = offered_value,
    reason = reason,
    offerer = offerer,
    receiver = receiver,
    offerer_away = offerer_away
  )
}


position_raw_score <- function(position, player = c("white", "black")) {
  assert_backgammon_position(position)
  player <- match.arg(player)

  direct_name <- paste0("score_", player)
  if (!is.null(position[[direct_name]])) {
    return(as.integer(position[[direct_name]]))
  }

  if (!is.null(position$score) && !is.null(position$score[[player]])) {
    return(as.integer(position$score[[player]]))
  }

  NA_integer_
}


position_away_score <- function(position, player = c("white", "black")) {
  assert_backgammon_position(position)
  player <- match.arg(player)

  if (!identical(position$play_context, "match")) {
    return(NA_integer_)
  }

  raw_score <- position_raw_score(position, player)
  if (is.na(raw_score) || is.na(position$match_length)) {
    return(NA_integer_)
  }

  as.integer(position$match_length - raw_score)
}


proposed_cube_offer <- function(position) {
  assert_backgammon_position(position)

  current_value <- as.integer(position$cube_value)
  offerer <- normalize_semantic_player(position$on_roll)

  cube_offer_context(
    offerer = offerer,
    receiver = other_semantic_player(offerer),
    current_value = current_value,
    offered_value = current_value * 2L
  )
}


# Validate one proposed or pending cube offer against factual position state.
validate_cube_offer_context <- function(
    position,
    offer = NULL,
    max_cube_value = supported_cube_max()
) {
  assert_backgammon_position(position)
  max_cube_value <- validate_max_cube_value_argument(max_cube_value)

  factual_cube <- position$cube_value
  if (
    !is_single_whole_number(factual_cube) ||
    !is_power_of_two(factual_cube) ||
    factual_cube < 1L ||
    factual_cube > max_cube_value
  ) {
    return(cube_validation_result(
      valid = FALSE,
      position = position,
      offerer = NULL,
      receiver = NULL,
      current_value = factual_cube,
      offered_value = if (is_single_whole_number(factual_cube)) factual_cube * 2L else NULL,
      reason = "package_cube_limit"
    ))
  }

  if (is.null(offer)) {
    offer <- proposed_cube_offer(position)
  }

  if (!inherits(offer, "backgammon_cube_offer_context")) {
    stop("`offer` must be NULL or created by cube_offer_context().", call. = FALSE)
  }

  offerer <- normalize_semantic_player(offer$offerer)
  receiver <- normalize_semantic_player(offer$receiver)
  current_value <- offer$current_value
  offered_value <- offer$offered_value

  if (is.null(offerer) || is.na(offerer)) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "missing_offerer"
    ))
  }

  if (is.null(receiver) || is.na(receiver)) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "missing_receiver"
    ))
  }

  if (identical(offerer, receiver)) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "same_offer_parties"
    ))
  }

  if (!identical(offerer, normalize_semantic_player(position$on_roll))) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "offerer_not_on_roll"
    ))
  }

  if (
    !is_single_whole_number(current_value) ||
    !is_power_of_two(current_value) ||
    !is_single_whole_number(offered_value) ||
    !is_power_of_two(offered_value) ||
    as.integer(current_value) != as.integer(factual_cube) ||
    as.integer(offered_value) != as.integer(current_value) * 2L
  ) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "invalid_offer_value"
    ))
  }

  current_value <- as.integer(current_value)
  offered_value <- as.integer(offered_value)

  if (isTRUE(position$is_crawford)) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "crawford"
    ))
  }

  if (length(position$dice) > 0L) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "dice_already_rolled"
    ))
  }

  cube_owner <- normalize_semantic_player(position$cube_owner)
  cube_is_centered <- identical(position$cube_owner, "center")

  if (!cube_is_centered && !identical(cube_owner, offerer)) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "opponent_owns_cube"
    ))
  }

  if (offered_value > max_cube_value) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "package_cube_limit"
    ))
  }

  offerer_away <- position_away_score(position, offerer)
  if (
    identical(position$play_context, "match") &&
    !is.na(offerer_away) &&
    current_value >= offerer_away
  ) {
    return(cube_validation_result(
      FALSE, position, offerer, receiver, current_value, offered_value,
      "match_cube_limit",
      offerer_away = offerer_away
    ))
  }

  cube_validation_result(
    TRUE,
    position,
    offerer,
    receiver,
    current_value,
    offered_value,
    "valid",
    offerer_away = offerer_away
  )
}


stop_for_cube_validation <- function(validation, prefix = "Invalid cube context") {
  if (!inherits(validation, "backgammon_cube_context_validation")) {
    stop("`validation` must be a cube-context validation result.", call. = FALSE)
  }

  stop(
    paste0(prefix, " [", validation$reason, "]"),
    call. = FALSE
  )
}


new_cube_display <- function(
    visible,
    state,
    value,
    owner = NULL,
    offerer = NULL,
    receiver = NULL,
    placement
) {
  structure(
    list(
      visible = isTRUE(visible),
      state = state,
      value = if (is.null(value)) NA_integer_ else as.integer(value),
      owner = owner,
      offerer = offerer,
      receiver = receiver,
      placement = placement
    ),
    class = "backgammon_cube_display"
  )
}


# Resolve factual cube state plus an optional pending offer into drawing state.
resolve_cube_display <- function(
    position,
    offer = NULL,
    max_cube_value = supported_cube_max()
) {
  assert_backgammon_position(position)
  max_cube_value <- validate_max_cube_value_argument(max_cube_value)

  factual_cube <- position$cube_value
  if (
    !is_single_whole_number(factual_cube) ||
    !is_power_of_two(factual_cube) ||
    factual_cube < 1L ||
    factual_cube > max_cube_value
  ) {
    validation <- new_cube_context_validation(
      valid = FALSE,
      action = NA_character_,
      current_value = factual_cube,
      offered_value = NA_integer_,
      reason = "package_cube_limit"
    )
    stop_for_cube_validation(validation, "Unsupported factual cube value")
  }

  if (isTRUE(position$is_crawford)) {
    if (!is.null(offer)) {
      validation <- validate_cube_offer_context(
        position = position,
        offer = offer,
        max_cube_value = max_cube_value
      )
      stop_for_cube_validation(validation)
    }

    return(new_cube_display(
      visible = FALSE,
      state = "hidden",
      value = factual_cube,
      owner = if (identical(position$cube_owner, "center")) NULL else position$cube_owner,
      placement = "hidden"
    ))
  }

  if (!is.null(offer)) {
    validation <- validate_cube_offer_context(
      position = position,
      offer = offer,
      max_cube_value = max_cube_value
    )

    if (!isTRUE(validation$valid)) {
      stop_for_cube_validation(validation)
    }

    return(new_cube_display(
      visible = TRUE,
      state = "offered",
      value = validation$offered_value,
      owner = if (identical(position$cube_owner, "center")) NULL else position$cube_owner,
      offerer = validation$offerer,
      receiver = validation$receiver,
      placement = paste0("offered_to_", validation$receiver)
    ))
  }

  if (identical(position$cube_owner, "center")) {
    return(new_cube_display(
      visible = TRUE,
      state = "centered",
      value = 1L,
      owner = NULL,
      placement = "outside_center"
    ))
  }

  owner <- normalize_semantic_player(position$cube_owner)
  if (is.na(owner) || is.null(owner)) {
    stop("Unsupported cube owner in `position`.", call. = FALSE)
  }

  new_cube_display(
    visible = TRUE,
    state = "owned",
    value = factual_cube,
    owner = owner,
    placement = paste0(owner, "_side")
  )
}
