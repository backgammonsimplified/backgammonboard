normalize_board_perspective <- function(perspective) {
  player <- normalize_semantic_player(perspective)
  if (is.null(player) || is.na(player)) {
    stop("`perspective` must resolve to `white` or `black`.", call. = FALSE)
  }
  player
}

visual_side_for_player <- function(player, perspective = "white") {
  player <- normalize_semantic_player(player)
  perspective <- normalize_board_perspective(perspective)
  if (is.null(player) || is.na(player)) {
    stop("Unsupported semantic player.", call. = FALSE)
  }
  if (identical(player, perspective)) "bottom" else "top"
}


offered_cube_field_side <- function(receiver, perspective = "white") {
  receiver <- normalize_semantic_player(receiver)
  perspective <- normalize_board_perspective(perspective)

  if (is.null(receiver) || is.na(receiver)) {
    stop("An offered cube requires a semantic receiver.", call. = FALSE)
  }

  if (identical(receiver, perspective)) "left" else "right"
}
