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
