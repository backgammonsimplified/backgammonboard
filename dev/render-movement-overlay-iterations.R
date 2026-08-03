# Rapid RStudio iteration for movement-overlay styling.
# From the repository root, run:
#   source("dev/render-movement-overlay-iterations.R")
# Or from a terminal:
#   Rscript dev/render-movement-overlay-iterations.R

arguments <- commandArgs(trailingOnly = TRUE)
output <- if (length(arguments) >= 1L && nzchar(arguments[[1L]])) {
  arguments[[1L]]
} else {
  file.path("artifacts", "movement-overlay-iterations")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(reset = TRUE, quiet = TRUE)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:31:0:0:0:0:10"
moves <- board_moves(c(13, 6), c(10, 5), die = c(3, 1))

shared <- list(
  ghost_outline = "#000000",
  ghost_outline_width = 1.5,
  ghost_dot_colour = "#65707A",
  ghost_dot_alpha = 1,
  ghost_grid_rows = 7L,
  ghost_grid_cols = 7L,
  ghost_dot_size = 1,
  ghost_grid_inset = 0.08,
  arrow_colour = "#D95F32",
  arrow_alpha = 1,
  arrow_width = 2,
  arrow_lineend = "round",
  arrow_outline_colour = "#000000",
  arrow_outline_width = 0.35,
  arrowhead_length = 0.18,
  arrowhead_width = 0.12,
  arrow_checker_gap = 0.05
)

variants <- list(
  no_fill = c(list(ghost_fill = NA, ghost_fill_alpha = 0), shared),
  fill_15 = c(list(ghost_fill = "#D4D8DC", ghost_fill_alpha = 0.15), shared),
  fill_30 = c(list(ghost_fill = "#D4D8DC", ghost_fill_alpha = 0.30), shared)
)

for (name in names(variants)) {
  overlay_style <- do.call(movement_overlay_style, variants[[name]])
  plot <- ggboard(
    xgid,
    colors = board_colors("bms"),
    style = board_style("bms"),
    moves = moves,
    decision = "checker_play",
    perspective = "player_1",
    movement_style = overlay_style
  )
  ggplot2::ggsave(
    file.path(output, paste0(name, ".svg")),
    plot = plot, width = 12, height = 9.1, units = "in"
  )
  ggplot2::ggsave(
    file.path(output, paste0(name, ".png")),
    plot = plot, width = 12, height = 9.1, units = "in", dpi = 144
  )
}

cards <- vapply(names(variants), function(name) {
  sprintf(
    '<article><h2>%s</h2><img src="%s.png" alt="%s movement overlay"></article>',
    gsub("_", " ", name), name, name
  )
}, character(1))
html <- c(
  "<!doctype html>",
  '<html><head><meta charset="utf-8"><title>Movement overlay iterations</title>',
  '<style>body{font-family:sans-serif;margin:24px;background:#eee}main{display:grid;grid-template-columns:repeat(3,minmax(440px,1fr));gap:18px}article{background:white;padding:12px;border:1px solid #aaa}img{width:100%;height:auto}h2{margin:0 0 8px;text-transform:capitalize}</style>',
  "</head><body><h1>Movement overlay fill iterations</h1><main>",
  cards,
  "</main></body></html>"
)
writeLines(html, file.path(output, "index.html"), useBytes = TRUE)

message("Rendered movement-overlay iterations to ", normalizePath(output))
