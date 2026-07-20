devtools::load_all(reset = TRUE)


# ============================================================
# PREVIEW CONFIGURATION
# ============================================================

# "match" or "money"
DISPLAY_EXAMPLE <- "match"

WHITE_PLAYER_NAME <- "White"
BLACK_PLAYER_NAME <- "Black"

# Used for the secondary labels in money games.
WHITE_WINS <- 6L
BLACK_WINS <- 2L

SHOW_BRANDING <- FALSE


# ============================================================
# TEST POSITIONS
# ============================================================

position_payload <- "-b----E-C---eE---c-e----B-"

match_xgid <- paste0(
  "XGID=",
  position_payload,
  ":0:0:1:51:6:2:1:7:10"
)

money_xgid <- paste0(
  "XGID=",
  position_payload,
  ":0:0:1:51:0:0:0:0:10"
)

selected_xgid <- if (identical(DISPLAY_EXAMPLE, "match")) {
  match_xgid
} else {
  money_xgid
}

position <- backgammonboard::backgammon_position(selected_xgid)


# ============================================================
# RENDER
# ============================================================

plot <- backgammonboard:::render_board_preview(
  x = position,
  colors = backgammonboard::board_colors("bms"),
  style = backgammonboard::board_style("bms"),
  point_1_side = "right",
  brand_text = if (isTRUE(SHOW_BRANDING)) {
    "Backgammon\nMade Simple"
  } else {
    NULL
  },
  cube_x_mode = "outside",
  show_information = TRUE,
  white_name = WHITE_PLAYER_NAME,
  black_name = BLACK_PLAYER_NAME,
  white_wins = if (identical(DISPLAY_EXAMPLE, "money")) {
    WHITE_WINS
  } else {
    NULL
  },
  black_wins = if (identical(DISPLAY_EXAMPLE, "money")) {
    BLACK_WINS
  } else {
    NULL
  }
)

print(plot)


# ============================================================
# SAVE
# ============================================================

output_directory <- file.path(
  getwd(),
  "dev",
  "preview-output"
)

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_directory,
  paste0(
    "match-properties-",
    DISPLAY_EXAMPLE,
    "-centered.png"
  )
)

ggplot2::ggsave(
  filename = output_file,
  plot = plot,
  device = "png",
  width = 12,
  height = 9.1,
  units = "in",
  dpi = 200,
  bg = backgammonboard::board_colors("bms")$outside_fill
)

output_file <- normalizePath(
  output_file,
  winslash = "/",
  mustWork = TRUE
)

cat(
  "\nPreview saved to:\n",
  output_file,
  "\n",
  sep = ""
)

if (.Platform$OS.type == "windows") {
  shell.exec(output_file)
} else {
  browseURL(output_file)
}
