supported_cube_max <- function() 64L


normalize_semantic_player <- function(player) {
  if (!is.character(player) || length(player) != 1L || is.na(player)) return(NA_character_)
  resolved <- tolower(player)
  if (!resolved %in% c("white", "black")) return(NA_character_)
  resolved
}


other_semantic_player <- function(player) {
  player <- normalize_semantic_player(player)
  if (is.na(player)) stop("Unsupported internal render player.", call. = FALSE)
  if (identical(player, "white")) "black" else "white"
}


assert_backgammon_position <- function(position) {
  if (!inherits(position, "backgammon_position") || !is.list(position)) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }
  invisible(position)
}


position_raw_score <- function(position, player = c("white", "black")) {
  assert_backgammon_position(position)
  player <- match.arg(player)
  as.integer(position$score[[player]])
}


proposed_cube_offer <- function(position) {
  assert_backgammon_position(position)
  offerer <- normalize_semantic_player(position$on_roll)
  if (is.na(offerer)) stop("A cube offer requires an internal render position.", call. = FALSE)
  receiver <- other_semantic_player(offerer)
  if (!position$cube_owner %in% c("center", offerer)) {
    stop("The offerer does not own the cube [cube_not_available]", call. = FALSE)
  }
  offered_value <- as.integer(2L * position$cube_value)
  if (offered_value > min(position$max_cube, supported_cube_max())) {
    stop("The offered cube exceeds the factual maximum cube [maximum_cube]", call. = FALSE)
  }
  structure(
    list(
      offerer = offerer,
      receiver = receiver,
      current_value = as.integer(position$cube_value),
      offered_value = offered_value
    ),
    class = "backgammon_cube_offer_context"
  )
}


new_cube_display <- function(
    visible,
    state,
    value,
    owner = NULL,
    offerer = NULL,
    receiver = NULL,
    placement = NULL) {
  structure(
    list(
      visible = isTRUE(visible), state = state, value = as.integer(value),
      owner = owner, offerer = offerer, receiver = receiver, placement = placement
    ),
    class = "backgammon_cube_display"
  )
}


resolve_cube_display <- function(position, offer = NULL) {
  assert_backgammon_position(position)
  if (isTRUE(position$is_crawford) || identical(position$crawford_status, "crawford")) {
    return(new_cube_display(FALSE, "hidden", position$cube_value, placement = "hidden"))
  }
  if (!is.null(offer)) {
    if (!inherits(offer, "backgammon_cube_offer_context")) {
      stop("`offer` must be a factual internal cube offer.", call. = FALSE)
    }
    return(new_cube_display(
      TRUE, "offered", offer$offered_value,
      owner = if (identical(position$cube_owner, "center")) NULL else position$cube_owner,
      offerer = offer$offerer, receiver = offer$receiver,
      placement = paste0("offered_to_", offer$receiver)
    ))
  }
  if (identical(position$cube_owner, "center")) {
    return(new_cube_display(TRUE, "centered", 1L, placement = "outside_center"))
  }
  owner <- normalize_semantic_player(position$cube_owner)
  if (is.na(owner)) stop("Unsupported cube owner in render position.", call. = FALSE)
  new_cube_display(
    TRUE, "owned", position$cube_value, owner = owner,
    placement = paste0(owner, "_side")
  )
}
