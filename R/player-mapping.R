# Authoritative v1.2 bridge from XGID source roles to stable project players.
xgid_source_player_map <- function() {
  c(top = "player_0", bottom = "player_1")
}


xgid_source_role_to_player <- function(role) {
  mapping <- xgid_source_player_map()
  if (!is.character(role) || anyNA(role) || any(!role %in% names(mapping))) {
    stop("XGID source role must be `top` or `bottom`.", call. = FALSE)
  }
  unname(mapping[role])
}


xgid_checker_source_role <- function(entry) {
  if (entry %in% letters[1:16]) return("top")
  if (entry %in% LETTERS[1:16]) return("bottom")
  if (identical(entry, "-")) return(NA_character_)
  stop("Unsupported XGID checker character.", call. = FALSE)
}


xgid_turn_player <- function(turn_code) {
  switch(
    as.character(as.integer(turn_code)),
    `-1` = xgid_source_role_to_player("top"),
    `1` = xgid_source_role_to_player("bottom"),
    stop("XGID turn must be -1 or 1.", call. = FALSE)
  )
}


xgid_cube_owner_player <- function(owner_code) {
  switch(
    as.character(as.integer(owner_code)),
    `-1` = xgid_source_role_to_player("top"),
    `0` = "center",
    `1` = xgid_source_role_to_player("bottom"),
    stop("XGID cube owner must be -1, 0, or 1.", call. = FALSE)
  )
}


# The retained visual layer calls player_1 `white` and player_0 `black` as
# stable compatibility tokens. Light/dark presentation is selected separately
# at render time and never changes this factual adapter.
project_player_to_render_player <- function(player) {
  switch(
    player,
    player_0 = "black",
    player_1 = "white",
    stop("Player identity must be `player_0` or `player_1`.", call. = FALSE)
  )
}


render_player_to_project_player <- function(player) {
  player <- normalize_semantic_player(player)
  if (is.na(player)) stop("Internal render player must be `white` or `black`.", call. = FALSE)
  if (identical(player, "white")) "player_1" else "player_0"
}


# Signed factual storage is documented and subordinate to point_occupancy:
# positive = player_1, negative = player_0.
project_player_sign <- function(player) {
  if (identical(player, "player_1")) return(1L)
  if (identical(player, "player_0")) return(-1L)
  stop("Player identity must be `player_0` or `player_1`.", call. = FALSE)
}


signed_points_to_occupancy <- function(points) {
  points <- as.integer(points)
  if (length(points) != 24L || anyNA(points)) {
    stop("Signed point storage must contain 24 integers.", call. = FALSE)
  }
  data.frame(
    point_id = seq_len(24L),
    owner = ifelse(points > 0L, "player_1", ifelse(points < 0L, "player_0", "empty")),
    count = abs(points),
    stringsAsFactors = FALSE
  )
}
