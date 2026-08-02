arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) {
  stop(
    "Supply zero or one output directory path.",
    call. = FALSE
  )
}

output_dir <- if (length(arguments) == 1L) {
  arguments[[1L]]
} else {
  file.path("inst", "gallery", "move-illustration")
}

if (file.exists(output_dir) && !dir.exists(output_dir)) {
  stop(
    paste0("Output path exists but is not a directory: ", output_dir),
    call. = FALSE
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_dir)) {
  stop(
    paste0("Unable to create output directory: ", output_dir),
    call. = FALSE
  )
}

devtools::load_all(".", quiet = TRUE)

gallery_position <- function(
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
  position$bar <- c(white = as.integer(white_bar), black = as.integer(black_bar))
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

cases <- list(
  ordinary = list(
    position = gallery_position(white = c(`13` = 1L, `6` = 1L)),
    move = "13/8",
    perspective = "white",
    brand_text = "Backgammon\nMade Simple"
  ),
  hit = list(
    position = gallery_position(white = c(`13` = 1L), black = c(`8` = 1L)),
    move = "13/8*",
    perspective = "white",
    brand_text = NULL
  ),
  bar_entry = list(
    position = gallery_position(white_bar = 1L),
    move = "bar/24",
    perspective = "white",
    brand_text = NULL
  ),
  bearing_off = list(
    position = gallery_position(white = c(`6` = 1L)),
    move = "6/off",
    perspective = "white",
    brand_text = NULL
  ),
  reversed = list(
    position = gallery_position(player = "black", black = c(`12` = 1L)),
    move = "12/17",
    perspective = "black",
    brand_text = NULL
  )
)

for (case_name in names(cases)) {
  case <- cases[[case_name]]
  plot <- ggboard(
    case$position,
    colors = board_colors("bms"),
    style = board_style("bms"),
    decision = "none",
    perspective = case$perspective,
    show_information = FALSE,
    brand_text = case$brand_text,
    moves = case$move
  )
  output <- file.path(output_dir, paste0(case_name, ".svg"))
  grDevices::svg(output, width = 10.625, height = 7.5, bg = "white")
  print(plot)
  grDevices::dev.off()
  stopifnot(file.exists(output), file.info(output)$size > 0)
  message(normalizePath(output, winslash = "/"))
}

message("PASS: move-illustration gallery rendered.")
