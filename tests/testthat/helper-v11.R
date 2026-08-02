read_factual_fixtures <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "xgid-factual-fixtures.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
}

fixture_xgid <- function(id) {
  fixtures <- read_factual_fixtures()
  value <- fixtures$xgid[fixtures$fixture_id == id]
  if (length(value) != 1L) stop("Unknown fixture: ", id, call. = FALSE)
  value
}

semicolon_integers <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(value)) return(integer())
  as.integer(strsplit(value, ";", fixed = TRUE)[[1L]])
}

custom_position <- function(
    on_roll = "player_0",
    player_0 = integer(),
    player_1 = integer(),
    player_0_bar = 0L,
    player_1_bar = 0L,
    dice = c(6L, 1L)) {
  position <- backgammon_position(fixture_xgid("opening_white_roll"))
  points <- integer(24)
  if (length(player_0)) points[as.integer(names(player_0))] <- -as.integer(player_0)
  if (length(player_1)) points[as.integer(names(player_1))] <- as.integer(player_1)
  position$points <- points
  position$point_occupancy <- backgammonboard:::signed_points_to_occupancy(points)
  position$bar <- c(player_0 = as.integer(player_0_bar), player_1 = as.integer(player_1_bar))
  position$off <- c(
    player_0 = 15L - sum(pmax(-points, 0L)) - as.integer(player_0_bar),
    player_1 = 15L - sum(pmax(points, 0L)) - as.integer(player_1_bar)
  )
  position$on_roll <- on_roll
  position$dice <- as.integer(dice)
  position
}
