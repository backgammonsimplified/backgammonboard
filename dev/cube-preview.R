devtools::load_all(reset = TRUE)

# centered, white_owned, black_owned, offered, or crawford
DISPLAY_STATE <- "offered"
SHOW_CUBE_CROSSHAIR <- TRUE
SHOW_BOARD_GUIDES <- TRUE

payload <- "-b----E-C---eE---c-e----B-"

xgid <- switch(
  DISPLAY_STATE,
  centered = paste0("XGID=", payload, ":0:0:1:00:0:0:0:0:10"),
  white_owned = paste0("XGID=", payload, ":2:1:1:00:0:0:0:0:10"),
  black_owned = paste0("XGID=", payload, ":3:-1:1:00:0:0:0:0:10"),
  offered = paste0("XGID=", payload, ":1:-1:-1:00:0:0:0:0:10"),
  crawford = paste0("XGID=", payload, ":2:1:1:00:6:2:1:7:10"),
  stop("Unsupported DISPLAY_STATE", call. = FALSE)
)

position <- backgammon_position(xgid)

offer <- if (identical(DISPLAY_STATE, "offered")) {
  cube_offer_context(
    offerer = "black",
    receiver = "white",
    current_value = 2L,
    offered_value = 4L
  )
} else {
  NULL
}

plot <- render_board_preview(
  x = position,
  brand_text = NULL,
  cube_offer = offer,
  show_cube_crosshair = SHOW_CUBE_CROSSHAIR,
  show_guides = SHOW_BOARD_GUIDES
)

print(plot)

output_directory <- file.path("dev", "preview-output")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(
  output_directory,
  paste0("cube-state-", DISPLAY_STATE, ".png")
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
