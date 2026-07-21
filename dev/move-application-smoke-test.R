devtools::load_all(".", reset = TRUE)

make_smoke_position <- function(
    player = "white",
    white = integer(),
    black = integer(),
    white_bar = 0L,
    black_bar = 0L
) {
  position <- backgammon_position(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10"
  )
  points <- integer(24)

  if (length(white) > 0L) {
    points[as.integer(names(white))] <- as.integer(white)
  }
  if (length(black) > 0L) {
    points[as.integer(names(black))] <- -as.integer(black)
  }

  position$points <- points
  position$bar <- c(white = as.integer(white_bar), black = as.integer(black_bar))
  position$off <- c(
    white = as.integer(15L - sum(points[points > 0L]) - as.integer(white_bar)),
    black = as.integer(15L - sum(abs(points[points < 0L])) - as.integer(black_bar))
  )
  position$on_roll <- player
  position$dice <- integer()
  position
}

cases <- list(
  simple = list(
    position = make_smoke_position(white = c(`13` = 1L)),
    notation = "13/8"
  ),
  compound = list(
    position = make_smoke_position(white = c(`24` = 1L)),
    notation = "24/18/13"
  ),
  bar_entry = list(
    position = make_smoke_position(white_bar = 1L),
    notation = "bar/24"
  ),
  bearoff = list(
    position = make_smoke_position(white = c(`6` = 1L)),
    notation = "6/off"
  ),
  hit = list(
    position = make_smoke_position(
      white = c(`13` = 1L),
      black = c(`8` = 1L)
    ),
    notation = "13/8*"
  ),
  black_move = list(
    position = make_smoke_position(
      player = "black",
      black = c(`12` = 1L)
    ),
    notation = "12/17"
  )
)

for (name in names(cases)) {
  case <- cases[[name]]
  result <- backgammonboard:::apply_board_moves(
    case$position,
    board_moves(case$notation)
  )

  cat("\n", strrep("=", 64L), "\n", sep = "")
  cat(name, ": ", case$notation, "\n", sep = "")
  cat(strrep("-", 64L), "\n", sep = "")
  print(result$applied_steps)
  cat("Bar: ", paste(names(result$bar), result$bar, collapse = ", "), "\n", sep = "")
  cat("Off: ", paste(names(result$off), result$off, collapse = ", "), "\n", sep = "")
}

cat("\nPASS: deterministic move-application smoke examples completed.\n")
