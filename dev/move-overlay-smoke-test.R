devtools::load_all(".")

preview_position <- function(
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

  position$points <- as.integer(points)
  position$bar <- c(
    white = as.integer(white_bar),
    black = as.integer(black_bar)
  )
  position$off <- c(
    white = as.integer(15L - sum(points[points > 0L]) - white_bar),
    black = as.integer(15L - sum(abs(points[points < 0L])) - black_bar)
  )
  position$on_roll <- player
  position$dice <- integer()
  position$action_marker <- "00"
  position$dice_action <- "00"
  position
}

colors <- board_colors("bms")
style <- board_style("bms")
output_dir <- file.path(tempdir(), "backgammonboard-move-overlays")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cases <- list(
  selected_alternative = list(
    position = preview_position(white = c(`13` = 1L, `6` = 1L)),
    moves = "13/8",
    alternative_moves = "6/5"
  ),
  compound = list(
    position = preview_position(white = c(`24` = 1L)),
    moves = "24/18/13",
    alternative_moves = NULL
  ),
  repeated = list(
    position = preview_position(white = c(`13` = 2L)),
    moves = "13/8(2)",
    alternative_moves = NULL
  ),
  hit = list(
    position = preview_position(
      white = c(`13` = 1L),
      black = c(`8` = 1L)
    ),
    moves = "13/8*",
    alternative_moves = NULL
  ),
  bar_entry = list(
    position = preview_position(white_bar = 1L),
    moves = "bar/24",
    alternative_moves = NULL
  ),
  bearoff = list(
    position = preview_position(white = c(`6` = 1L)),
    moves = "6/off",
    alternative_moves = NULL
  )
)

for (case_name in names(cases)) {
  case <- cases[[case_name]]
  plot <- ggboard(
    case$position,
    colors = colors,
    style = style,
    decision = "none",
    perspective = "white",
    show_information = FALSE,
    brand_text = "Backgammon\nMade Simple",
    moves = case$moves,
    alternative_moves = case$alternative_moves
  )

  widths <- if (identical(case_name, "selected_alternative")) {
    c(1200, 768, 480, 320)
  } else {
    768
  }

  for (width_px in widths) {
    output <- file.path(
      output_dir,
      paste0(case_name, "-", width_px, "px.png")
    )

    ggplot2::ggsave(
      filename = output,
      plot = plot,
      width = width_px / 96,
      height = width_px / 96 * 12 / 17,
      units = "in",
      dpi = 96,
      bg = "white"
    )

    stopifnot(file.exists(output), file.info(output)$size > 0)
    message(output)
  }
}

message("PASS: move-overlay smoke images rendered.")
