# Run from the package root after installing development dependencies.

# Change these two values while developing the board.
BRAND_TEXT <- "Backgammon\nSimplified"
SHOW_GUIDES <- TRUE

# Other useful text choices:
# BRAND_TEXT <- "Backgammon Simplified"
# BRAND_TEXT <- NULL

devtools::load_all(reset = TRUE)

xgid <- paste0(
  "XGID=-b----E-C---eE---c-e----B-",
  ":0:0:1:52:0:0:3:0:10"
)

plot <- render_board_preview(
  x = xgid,
  brand_text = BRAND_TEXT,
  show_guides = SHOW_GUIDES
)

output_dir <- file.path("dev", "preview-output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "board-default.png")

ggplot2::ggsave(
  filename = output_file,
  plot = plot,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 180,
  bg = board_colors("bs")$outside_fill,
  limitsize = FALSE
)

message("Saved preview to: ", normalizePath(output_file, mustWork = TRUE))
