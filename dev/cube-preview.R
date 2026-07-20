devtools::load_all(reset = TRUE)

CUBE_STATE <- "offered_white"
CUBE_X_MODE <- "outside"
CUBE_VALUE <- 2L
SHOW_CUBE_CROSSHAIR <- TRUE
SHOW_BOARD_GUIDES <- TRUE

xgid <- paste0(
  "XGID=-b----E-C---eE---c-e----B-",
  ":0:0:1:52:0:0:3:0:10"
)

position <- backgammon_position(xgid)
position$dice <- integer()

plot <- render_board_preview(
  x = position,
  brand_text = NULL,
  cube_state = CUBE_STATE,
  cube_x_mode = CUBE_X_MODE,
  cube_value = CUBE_VALUE,
  show_cube_crosshair = SHOW_CUBE_CROSSHAIR,
  show_guides = SHOW_BOARD_GUIDES
)

print(plot)

output_directory <- file.path("dev", "preview-output")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

resolved_x_mode <- if (identical(CUBE_STATE, "offered_white")) {
  "inside-left-playing-field"
} else {
  CUBE_X_MODE
}

output_file <- file.path(
  output_directory,
  paste0(
    "cube-",
    CUBE_STATE,
    "-",
    resolved_x_mode,
    "-",
    if (identical(CUBE_STATE, "centered")) 1L else CUBE_VALUE,
    ".png"
  )
)

ggplot2::ggsave(
  filename = output_file,
  plot = plot,
  width = 12,
  height = 8.5,
  units = "in",
  dpi = 200,
  bg = board_colors("bms")$outside_fill
)

output_file <- normalizePath(output_file, winslash = "/", mustWork = TRUE)
cat("\nCube preview saved to:\n", output_file, "\n", sep = "")

if (.Platform$OS.type == "windows") {
  shell.exec(output_file)
} else {
  browseURL(output_file)
}
