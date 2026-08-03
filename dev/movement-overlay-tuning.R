# Fast, development-only movement-overlay tuning.
# RStudio: source("dev/movement-overlay-tuning.R")
# Terminal: Rscript dev/movement-overlay-tuning.R

devtools::load_all(reset = TRUE, quiet = TRUE)

# Change only this block during visual iteration.
tuning_mode <- Sys.getenv(
  "BACKGAMMONBOARD_MOVEMENT_TUNING_MODE", unset = "manual"
) # "manual" or "grid"
output_dir <- file.path("dev", "preview-output", "movement-overlay-tuning")
write_html_gallery <- TRUE

manual_parameters <- list(
  ghost_fill = "#D4D8DC",
  ghost_fill_alpha = 0.20,
  ghost_outline = "#000000",
  ghost_outline_width = 1.5,
  ghost_dot_colour = "#65707A",
  ghost_dot_alpha = 1,
  ghost_grid_rows = 7L,
  ghost_grid_cols = 7L,
  ghost_dot_size = 1,
  ghost_grid_inset = 0.04,
  arrow_colour = "#D95F32",
  arrow_alpha = 1,
  arrow_width = 2,
  arrow_lineend = "round",
  arrow_outline_colour = "#000000",
  arrow_outline_width = 0,
  arrowhead_length = 0.08,
  arrowhead_width = 0.05,
  arrow_checker_gap = -0.20,
  arrow_curve_enabled = TRUE,
  arrow_curve_offset = 0.22,
  arrow_curve_step = 0.18,
  arrow_curve_max = 0.65,
  arrow_curve_length_cap = 0.08,
  arrow_chain_full_angle = 6,
  arrow_chain_moderate_angle = 12,
  arrow_chain_max_angle = 20,
  arrow_chain_full_multiplier = 1.00,
  arrow_chain_moderate_multiplier = 0.60,
  arrow_chain_shallow_multiplier = 0.25,
  arrow_chain_short_length_radii = 2.75,
  arrow_chain_short_curve_max = 0.15,
  arrow_collinear_angle_tolerance = 8,
  arrow_overlap_threshold = 0.40,
  arrow_curve_two_path_split = 0.5
)

# These are curated nearby combinations, not a full Cartesian product.
# Switch tuning_mode to "grid" to render them side by side.
parameter_grid <- data.frame(
  id = c("G01", "G02", "G03", "G04"),
  arrow_curve_offset = c(0.16, 0.22, 0.28, 0.22),
  arrow_curve_step = c(0.14, 0.18, 0.22, 0.18),
  arrow_curve_max = c(0.50, 0.65, 0.65, 0.80),
  arrow_checker_gap = c(-0.16, -0.20, -0.24, -0.20),
  arrow_width = c(1.7, 2.0, 2.2, 2.0),
  arrowhead_length = c(0.07, 0.08, 0.09, 0.08),
  arrowhead_width = c(0.045, 0.050, 0.060, 0.050),
  ghost_fill_alpha = c(0.15, 0.20, 0.30, 0.20),
  ghost_dot_size = c(0.85, 1.00, 1.15, 1.00),
  ghost_grid_inset = c(0.02, 0.04, 0.06, 0.04),
  stringsAsFactors = FALSE
)

if (!tuning_mode %in% c("manual", "grid")) {
  stop("`tuning_mode` must be `manual` or `grid`.", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

make_position <- function(
    player_1 = integer(), player_0 = integer(),
    player_1_bar = 0L, player_0_bar = 0L,
    on_roll = "player_1", dice = c(3L, 1L)
) {
  opening <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:31:0:0:0:0:10"
  position <- backgammon_position(opening)
  points <- integer(24L)
  if (length(player_1) > 0L) {
    points[as.integer(names(player_1))] <- as.integer(player_1)
  }
  if (length(player_0) > 0L) {
    points[as.integer(names(player_0))] <- -as.integer(player_0)
  }
  position$points <- points
  position$point_occupancy <-
    backgammonboard:::signed_points_to_occupancy(points)
  position$bar <- c(
    player_0 = as.integer(player_0_bar),
    player_1 = as.integer(player_1_bar)
  )
  position$off <- c(
    player_0 = 15L - sum(pmax(-points, 0L)) - as.integer(player_0_bar),
    player_1 = 15L - sum(pmax(points, 0L)) - as.integer(player_1_bar)
  )
  position$on_roll <- on_roll
  position$dice <- as.integer(dice)
  position
}

cases <- list(
  list(
    id = "T01", title = "Ordinary separated arrows",
    position = make_position(
      player_1 = c(`13` = 1L, `6` = 1L), dice = c(3L, 1L)
    ),
    moves = board_moves(c(13, 6), c(10, 5), die = c(3, 1))
  ),
  list(
    id = "T02", title = "Five nearly identical repeated paths",
    position = make_position(
      player_1 = c(`13` = 9L, `10` = 5L), dice = c(3L, 3L)
    ),
    moves = board_moves(rep(13, 5), rep(10, 5), die = rep(3, 5))
  ),
  list(
    id = "T03", title = "Repeated destination from different sources",
    position = make_position(
      player_1 = c(`13` = 1L, `11` = 1L, `10` = 4L),
      dice = c(3L, 1L)
    ),
    moves = board_moves(c(11, 13), c(10, 10), die = c(1, 3))
  ),
  list(
    id = "T04", title = "M17 chained arrivals and departures",
    position = make_position(
      player_1 = c(`13` = 2L, `11` = 2L, `9` = 2L),
      dice = c(2L, 2L)
    ),
    moves = board_moves(
      c(13, 13, 11, 11), c(11, 11, 9, 9), die = rep(2, 4)
    )
  ),
  list(
    id = "T05", title = "M18 mixed chain",
    position = make_position(
      player_1 = c(`20` = 1L, `18` = 1L, `17` = 1L),
      dice = c(4L, 4L)
    ),
    moves = board_moves(
      c(17, 18, 20, 16), c(13, 14, 16, 12), die = rep(4, 4)
    )
  ),
  list(
    id = "T06", title = "Stacked pairs on both rows",
    position = make_position(
      player_1 = c(`18` = 2L, `11` = 2L, `9` = 2L),
      dice = c(2L, 2L)
    ),
    moves = board_moves(
      c(18, 18, 11, 11), c(16, 16, 9, 9), die = rep(2, 4)
    )
  )
)

style_sets <- if (tuning_mode == "manual") {
  list(MANUAL = manual_parameters)
} else {
  result <- vector("list", nrow(parameter_grid))
  names(result) <- parameter_grid$id
  for (index in seq_len(nrow(parameter_grid))) {
    values <- manual_parameters
    row <- parameter_grid[index, , drop = FALSE]
    for (name in setdiff(names(row), "id")) {
      values[[name]] <- row[[name]][[1L]]
    }
    result[[index]] <- values
  }
  result
}

render_svg <- function(plot, path, background) {
  grDevices::svg(path, width = 12, height = 9.1, bg = background, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
}

render_png <- function(plot, path, background) {
  grDevices::png(
    path, width = 1200, height = 910, res = 100, bg = background
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
}

colors <- board_colors("bms")
board_style_values <- board_style("bms")
manifest <- list()
cards <- character()
manifest_index <- 1L
card_index <- 1L

for (style_id in names(style_sets)) {
  parameters <- style_sets[[style_id]]
  movement_style_values <- do.call(movement_overlay_style, parameters)
  style_dir <- file.path(output_dir, style_id)
  dir.create(style_dir, recursive = TRUE, showWarnings = FALSE)

  for (item in cases) {
    plot <- ggboard(
      item$position,
      moves = item$moves,
      colors = colors,
      style = board_style_values,
      movement_style = movement_style_values,
      perspective = "player_1",
      mirror_horizontal = FALSE,
      score_format = "both",
      player_name_style = "checker"
    )
    stem <- paste0(item$id, "-", style_id)
    svg_path <- file.path(style_dir, paste0(stem, ".svg"))
    png_path <- file.path(style_dir, paste0(stem, ".png"))
    render_svg(plot, svg_path, colors$outside_fill)
    render_png(plot, png_path, colors$outside_fill)

    overlay <- attr(plot, "backgammon_prepared_layout")$selected_overlay
    manifest[[manifest_index]] <- data.frame(
      style_id = style_id,
      case_id = item$id,
      title = item$title,
      curved_arrows = sum(overlay$segments$curve_offset != 0),
      svg = file.path(style_id, basename(svg_path)),
      png = file.path(style_id, basename(png_path)),
      stringsAsFactors = FALSE
    )
    manifest_index <- manifest_index + 1L
    cards[[card_index]] <- paste0(
      '<article><h2>', item$id, ' · ', item$title, '</h2>',
      '<p><strong>', style_id, '</strong> · curved arrows: ',
      sum(overlay$segments$curve_offset != 0), '</p>',
      '<img src="', file.path(style_id, basename(svg_path)), '"></article>'
    )
    card_index <- card_index + 1L
  }
}

manifest <- do.call(rbind, manifest)
utils::write.csv(manifest, file.path(output_dir, "manifest.csv"), row.names = FALSE)

active_parameters <- do.call(rbind, lapply(names(style_sets), function(id) {
  data.frame(style_id = id, as.list(style_sets[[id]]), check.names = FALSE)
}))
utils::write.csv(
  active_parameters, file.path(output_dir, "active-parameters.csv"),
  row.names = FALSE
)

if (isTRUE(write_html_gallery)) {
  html <- c(
    '<!doctype html><html><head><meta charset="utf-8">',
    '<title>Movement overlay tuning</title>',
    '<style>body{margin:0;background:#e9e5dc;color:#172039;font:15px system-ui}',
    'main{max-width:2400px;margin:auto;padding:20px}.grid{display:grid;',
    'grid-template-columns:repeat(2,minmax(600px,1fr));gap:14px}',
    'article{background:white;border-radius:10px;padding:10px}',
    'h2{font-size:18px;margin:0 0 5px}p{margin:0 0 8px}img{width:100%;height:auto}',
    '@media(max-width:1300px){.grid{grid-template-columns:1fr}}</style>',
    '</head><body><main><h1>Movement overlay tuning</h1><section class="grid">',
    cards,
    '</section></main></body></html>'
  )
  writeLines(html, file.path(output_dir, "index.html"), useBytes = TRUE)
}

cat("Movement overlay tuning complete.\n")
cat("Mode:", tuning_mode, "\n")
cat("Output:", normalizePath(output_dir, winslash = "/"), "\n")
cat("Gallery:", normalizePath(
  file.path(output_dir, "index.html"), winslash = "/", mustWork = FALSE
), "\n")
cat("Active parameter values:\n")
print(active_parameters)
