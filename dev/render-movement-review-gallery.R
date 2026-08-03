# Render a focused movement-overlay gallery in setup-right and setup-left views.
# Usage in RStudio: source("dev/render-movement-review-gallery.R")
# Terminal usage: Rscript dev/render-movement-review-gallery.R [output-directory]

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) {
  stop("Supply at most one output directory.", call. = FALSE)
}
output <- if (length(arguments) == 1L && nzchar(arguments[[1L]])) {
  arguments[[1L]]
} else {
  file.path("artifacts", "movement-review-gallery")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(reset = TRUE, quiet = TRUE)

# Edit this one block in RStudio to restyle every movement overlay in the
# gallery. All panels below receive this same object through ggboard().
movement_style <- movement_overlay_style(
  ghost_fill = NA,
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
  
  arrow_curve_offset = 2.0,
  arrow_curve_step = 0.75,
  arrow_curve_max = 3.0,
  arrow_curve_length_cap = 0.40,
  arrow_chain_full_angle = 6,
  arrow_chain_moderate_angle = 12,
  arrow_chain_max_angle = 20,
  arrow_chain_full_multiplier = 1.00,
  arrow_chain_moderate_multiplier = 0.60,
  arrow_chain_shallow_multiplier = 0.25,
  arrow_chain_short_length_radii = 2.75,
  arrow_chain_short_curve_max = 0.15,
  arrow_collinear_angle_tolerance = 6,
  arrow_overlap_threshold = 0.25
)

encode_count <- function(count, player) {
  if (count < 1L || count > 15L) stop("Checker counts must be 1 through 15.")
  value <- intToUtf8(utf8ToInt("A") + count - 1L)
  if (identical(player, "player_0")) tolower(value) else value
}

movement_xgid <- function(player_1 = integer(), player_0 = integer(),
                          player_1_bar = 0L, player_0_bar = 0L,
                          turn = 1L, dice = "00") {
  payload <- rep("-", 26L)
  add_points <- function(values, player) {
    if (length(values) == 0L) return()
    points <- as.integer(names(values))
    if (anyNA(points) || any(!points %in% 1:24)) stop("Invalid point map.")
    for (index in seq_along(points)) {
      slot <- points[[index]] + 1L
      if (!identical(payload[[slot]], "-")) stop("Both players occupy one point.")
      payload[[slot]] <<- encode_count(as.integer(values[[index]]), player)
    }
  }
  add_points(player_1, "player_1")
  add_points(player_0, "player_0")
  if (player_0_bar > 0L) payload[[1L]] <- encode_count(player_0_bar, "player_0")
  if (player_1_bar > 0L) payload[[26L]] <- encode_count(player_1_bar, "player_1")
  paste0(
    "XGID=", paste(payload, collapse = ""),
    ":0:0:", turn, ":", dice, ":0:0:0:0:10"
  )
}

case <- function(
    id, title, xgid, moves, review, perspective = "player_1"
) {
  list(
    id = id, title = title, xgid = xgid, moves = moves, review = review,
    perspective = perspective
  )
}

opening <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:31:0:0:0:0:10"
cases <- list(
  case(
    "M01", "Complete 3-1 from the opening position", opening,
    board_moves(c(13, 6), c(10, 5), die = c(3, 1)),
    "The five factual checkers remain on point 13. One ghost lands alone on point 10 and one lands alone on point 5."
  ),
  case(
    "M02", "Move onto an existing friendly stack",
    movement_xgid(c(`13` = 2L, `10` = 3L, `6` = 1L), c(`24` = 2L), dice = "31"),
    board_moves(c(13, 6), c(10, 5), die = c(3, 1)),
    "The destination ghost sits immediately above the three factual checkers already on the destination point."
  ),
  case(
    "M03", "Hit an opposing blot",
    movement_xgid(c(`13` = 1L, `6` = 1L), c(`10` = 1L), dice = "31"),
    board_moves(c(13, 6), c(10, 5), die = c(3, 1)),
    "The ghost is drawn directly over the opposing blot; no separate hit symbol is drawn."
  ),
  case(
    "M04", "Enter from the bar onto an empty point",
    movement_xgid(c(`13` = 2L), c(`1` = 2L), player_1_bar = 1L, dice = "12"),
    board_moves(c("bar", 13), c(24, 11), die = c(1, 2)),
    "The factual checker remains visibly on the bar while its destination ghost appears on the entry point."
  ),
  case(
    "M05", "Enter from the bar and hit",
    movement_xgid(c(`13` = 2L), c(`24` = 1L), player_1_bar = 1L, dice = "12"),
    board_moves(c("bar", 13), c(24, 11), die = c(1, 2)),
    "The bar checker remains in place and the entry ghost overlays the opponent blot."
  ),
  case(
    "M06", "Bear four checkers off a tall stack",
    movement_xgid(c(`4` = 5L, `3` = 5L, `2` = 5L), c(`19` = 15L), dice = "44"),
    board_moves(rep(4, 4), rep("off", 4), die = rep(4, 4)),
    "All five factual source checkers remain. Four rounded-tail arrows originate from the successively exposed checker positions and land on ordered off-tray ghosts above the factual marker."
  ),
  case(
    "M07", "Move four checkers onto a tall stack",
    movement_xgid(c(`13` = 5L, `10` = 3L), c(`24` = 2L), dice = "33"),
    board_moves(rep(13, 4), rep(10, 4), die = rep(3, 4)),
    "Move order maps the exposed point-13 checker to the first destination ghost. Arrivals use slots 4, 5, 6, and 6, so overflow overlaps at the sixth visible level."
  ),
  case(
    "M08", "Two sources land on one friendly stack",
    movement_xgid(c(`13` = 1L, `11` = 1L, `10` = 2L), c(`24` = 2L), dice = "31"),
    board_moves(c(11, 13), c(10, 10), die = c(1, 3)),
    "The lower checker from point 11 lands first in the lower ghost slot; the checker from point 13 lands second in the upper slot."
  ),
  case(
    "M09", "One checker moves twice",
    movement_xgid(c(`13` = 1L), c(`24` = 1L), dice = "31"),
    board_moves(c(13, 10), c(10, 9), die = c(3, 1)),
    "The second arrow starts at the intermediate ghost while the factual checker remains at its original point."
  ),
  case(
    "M10", "Long diagonal followed by a short move",
    movement_xgid(c(`13` = 1L, `8` = 2L), c(`24` = 2L), dice = "51"),
    board_moves(c(13, 8), c(8, 7), die = c(5, 1)),
    "Check straight-arrow clearance across a long diagonal and a crowded intermediate destination."
  ),
  case(
    "M11", "Two checkers bear off",
    movement_xgid(c(`2` = 1L, `1` = 1L), c(`19` = 2L), dice = "21"),
    board_moves(c(2, 1), c("off", "off"), die = c(2, 1)),
    "Both factual home-board checkers remain while the off-tray destination receives the ghost overlays."
  ),
  case(
    "M12", "Foey moves and hits in the source orientation",
    movement_xgid(c(`15` = 1L), c(`12` = 1L, `19` = 1L), turn = -1L, dice = "31"),
    board_moves(c(13, 6), c(10, 5), die = c(3, 1)),
    "Foey moves across the top half in the canonical XGID orientation and the ghost overlays Homey's blot."
  ),
  case(
    "M13", "Bear four different points off with double 4",
    movement_xgid(c(`4` = 1L, `3` = 1L, `2` = 1L, `1` = 1L),
                  c(`19` = 4L), dice = "44"),
    board_moves(c(4, 3, 2, 1), rep("off", 4)),
    "The rightmost source receives the nearest available off-tray ghost; progressively farther sources receive progressively farther ghosts so the arrows do not cross."
  ),
  case(
    "M14", "Bear four from one stack into an empty off tray",
    movement_xgid(c(`4` = 4L, `3` = 5L, `2` = 6L),
                  c(`19` = 15L), dice = "44"),
    board_moves(rep(4, 4), rep("off", 4), die = rep(4, 4)),
    "With no factual checker off, the first ghost occupies the tray marker and the remaining ghosts stack inward. Top source checkers map to farther destination slots."
  ),
  case(
    "M15", "Bear four onto an occupied numbered off tray",
    movement_xgid(c(`1` = 4L), c(`19` = 4L), dice = "11"),
    board_moves(rep(1, 4), rep("off", 4), die = rep(1, 4)),
    "The numbered factual off marker remains visible and four neutral ghosts stack above it rather than placing arrowheads on the marker."
  ),
  case(
    "M16", "Move four checkers onto one stack with double 6",
    movement_xgid(c(`13` = 4L, `7` = 3L), c(`24` = 2L), dice = "66"),
    board_moves(rep(13, 4), rep(7, 4), die = rep(6, 4)),
    "The exposed point-13 checker maps to the first destination ghost. Arrivals use slots 4, 5, 6, and 6 above the factual point-7 stack."
  ),
  case(
    "M17", "Chained double 2 uses newly landed ghosts",
    movement_xgid(c(`13` = 2L, `11` = 2L, `9` = 2L),
                  c(`24` = 2L), dice = "22"),
    board_moves(c(13, 13, 11, 11), c(11, 11, 9, 9), die = rep(2, 4)),
    "The two 13-to-11 arrivals are drawn first. The later 11-to-9 arrows start from those two ghost checkers, then align lower-to-lower and upper-to-upper on the mover's home row."
  ),
  case(
    "M18", "Same-side priority and an across-board double-4 chain",
    movement_xgid(c(`20` = 1L, `18` = 1L, `17` = 1L),
                  c(`24` = 2L), dice = "44"),
    board_moves(c(17, 18, 20, 16), c(13, 14, 16, 12), die = rep(4, 4)),
    "The closer checker moves 17/13. The checker on 20 legally reaches 12 as 20/16/12, keeping each atomic arrow tied to one die."
  ),
  case(
    "M19", "Foey moves four checkers with double 6",
    movement_xgid(c(`1` = 2L), c(`12` = 4L), turn = -1L, dice = "66"),
    board_moves(rep(13, 4), rep(7, 4), die = rep(6, 4)),
    "Foey uses the same move-order assignment: the exposed source checker maps to the first destination ghost, with overflow capped at level six."
  ),
  case(
    "M20", "One checker advances four times with double 3",
    movement_xgid(c(`13` = 1L), c(`24` = 1L), dice = "33"),
    board_moves(c(13, 10, 7, 4), c(10, 7, 4, 1), die = rep(3, 4)),
    "A four-step chain checks repeated ghost-to-arrow continuation over a long route."
  ),
  case(
    "M21", "Opposite-side sources land on one tall stack",
    movement_xgid(c(`13` = 1L, `11` = 1L, `10` = 4L),
                  c(`24` = 2L), dice = "31"),
    board_moves(c(11, 13), c(10, 10), die = c(1, 3)),
    "Move order puts point 11 into slot 5 and point 13 into slot 6 above the four factual destination checkers."
  ),
  case(
    "M22", "Four checkers cross to an empty point with double 6",
    movement_xgid(c(`13` = 4L), c(`24` = 2L), dice = "66"),
    board_moves(rep(13, 4), rep(7, 4), die = rep(6, 4)),
    "The exposed source checker maps to the bottom destination ghost, followed by the remaining source levels in move order."
  ),
  case(
    "M23", "Two same-side sources land on one stack",
    movement_xgid(c(`11` = 1L, `9` = 1L, `8` = 3L),
                  c(`24` = 2L), dice = "31"),
    board_moves(c(11, 9), c(8, 8), die = c(3, 1)),
    "With both sources on the destination side, horizontal proximity assigns point 9 to the nearer slot and point 11 to the farther slot."
  ),
  case(
    "M24", "Foey bears four different points off with double 4",
    movement_xgid(c(`1` = 4L),
                  c(`21` = 1L, `22` = 1L, `23` = 1L, `24` = 1L),
                  turn = -1L, dice = "44"),
    board_moves(c(4, 3, 2, 1), rep("off", 4)),
    "Foey's top off tray uses the same rightmost-source and inward-stacking rules after the canonical player transform."
  ),
  case(
    "M25", "Two independent double-2 stacks on opposite rows",
    movement_xgid(c(`18` = 2L, `11` = 2L, `9` = 2L),
                  c(`24` = 2L), dice = "22"),
    board_moves(c(18, 18, 11, 11), c(16, 16, 9, 9), die = rep(2, 4)),
    "Both same-row pairs align stack depth: 18-to-16 on the top row and 11-to-9 on the bottom row each map lower-to-lower and upper-to-upper without crossing."
  ),
  case(
    "M26", "Two crowded home-row stacks move with double 2",
    movement_xgid(c(`11` = 3L, `9` = 2L, `6` = 3L, `4` = 2L),
                  c(`24` = 2L), dice = "22"),
    board_moves(c(11, 11, 6, 6), c(9, 9, 4, 4), die = rep(2, 4)),
    "Both same-row pairs use the top two movable source levels while matching their relative lower and upper depths at the crowded destinations."
  ),
  case(
    "M27", "Foey verifies the same-row double-2 rule",
    movement_xgid(c(`1` = 2L),
                  c(`7` = 2L, `14` = 2L, `16` = 2L),
                  turn = -1L, dice = "22"),
    board_moves(c(18, 18, 11, 11), c(16, 16, 9, 9), die = rep(2, 4)),
    "With Foey on roll, both same-row pairs use lower-to-lower and upper-to-upper assignment, including the visual-bottom 7-to-9 pair."
  ),
  case(
    "M28", "Two chained checkers share the bottom movement line",
    movement_xgid(c(`10` = 1L, `9` = 1L), c(`24` = 2L), dice = "33"),
    board_moves(c(10, 9, 7, 6), c(7, 6, 4, 3), die = rep(3, 4)),
    "Each checker moves twice. The overlapping middle paths receive only the later-segment inward bow; the first path remains straight."
  ),
  case(
    "M29", "Two different top moves share most of one line",
    movement_xgid(c(`18` = 1L, `17` = 1L), c(`24` = 2L), dice = "33"),
    board_moves(c(17, 18), c(14, 15), die = c(3, 3)),
    "The first top-row segment remains straight and the later substantially overlapping segment bows minimally toward the board centre."
  ),
  case(
    "M30", "Top-half same-line overlap",
    movement_xgid(c(`20` = 1L, `18` = 1L), c(`24` = 2L), dice = "44"),
    board_moves(c(18, 20), c(14, 16), die = c(4, 4)),
    "A longer top-half overlap verifies that the later arrow bows downward, never outward toward the information rail."
  ),
  case(
    "M31", "Bottom-half same-line overlap",
    movement_xgid(c(`11` = 1L, `10` = 1L), c(`24` = 2L), dice = "33"),
    board_moves(c(10, 11), c(7, 8), die = c(3, 3)),
    "The matching bottom-half case verifies that the later arrow bows upward toward the board centre."
  ),
  case(
    "M32", "Near-vertical shared-source overlap",
    movement_xgid(c(`15` = 2L), c(`24` = 2L), dice = "54"),
    board_moves(c(15, 15), c(10, 11), die = c(5, 4)),
    "Two near-vertical paths share a long initial run but have different destinations. Only the later path bows toward the horizontal centre of its board half."
  ),
  case(
    "M33", "Isolated crossing remains straight",
    movement_xgid(c(`15` = 1L, `14` = 1L), c(`24` = 2L), dice = "44"),
    board_moves(c(14, 15), c(10, 11), die = c(4, 4)),
    "The arrows cross once near the board centre but do not share a line for meaningful distance, so both remain straight."
  ),
  case(
    "M34", "Legacy two-independent-checker example",
    opening,
    board_moves(c(13, 6), c(8, 5), die = c(5, 1)),
    "Restores the earlier 13/8 and 6/5 edge-case render using the current overlay style and all four gallery views."
  ),
  case(
    "M35", "Legacy one-checker-twice example",
    sub(":31:", ":65:", opening, fixed = TRUE),
    board_moves(c(24, 18), c(18, 13), die = c(6, 5)),
    "Restores the earlier 24/18/13 chained-move example. The factual checker remains on point 24 and the second arrow starts from the intermediate ghost."
  ),
  case(
    "M36", "Legacy repeated 13/8 pair",
    sub(":31:", ":55:", opening, fixed = TRUE),
    board_moves(c(13, 13), c(8, 8), die = c(5, 5)),
    "Restores the earlier 13/8(2) repeated-path example without changing shared-destination arrow treatment."
  ),
  case(
    "M37", "Legacy hit on point 8",
    movement_xgid(c(`13` = 1L), c(`8` = 1L, `24` = 1L), dice = "51"),
    board_moves(13, 8, die = 5),
    "Restores the earlier 13/8 hit example. The destination ghost overlays the opposing blot."
  ),
  case(
    "M38", "Legacy five-path overstack onto point 8",
    movement_xgid(c(`13` = 9L, `8` = 5L), c(`24` = 2L), dice = "55"),
    board_moves(rep(13, 5), rep(8, 5), die = rep(5, 5)),
    "Restores the earlier 13/8(5) geometry stress case: a nine-checker source stack sends five paths onto an existing five-checker destination stack."
  ),
  case(
    "M39", "Legacy alternate-view 13/8",
    opening,
    board_moves(13, 8, die = 5),
    "Restores the earlier rotated-perspective example with Foey/player_0 near while preserving the same factual move.",
    perspective = "player_0"
  ),
  case(
    "M40", "Five-path tall-stack tuning stress case",
    movement_xgid(c(`13` = 9L, `10` = 5L), c(`24` = 2L), dice = "33"),
    board_moves(rep(13, 5), rep(10, 5), die = rep(3, 5)),
    "Carries forward tuning case T02: five nearly identical paths from a nine-checker source stack onto an existing five-checker destination stack."
  ),
  case(
    "M41", "Four-hop 4-4 chain with collinear top-row hops",
    movement_xgid(c(`24` = 1L), c(`1` = 1L), dice = "44"),
    board_moves(
      c(24, 20, 16, 12), c(20, 16, 12, 8), die = rep(4, 4)
    ),
    "One checker uses all four 4s. The consecutive 24-to-20 and 20-to-16 top-row hops have zero projected overlap but form one straight chain, so both receive separate shallow downward arches and retain separate arrowheads."
  ),
  case(
    "C01", "Ghost colour identity on tan points",
    movement_xgid(c(`6` = 2L), c(`24` = 1L), dice = "31"),
    board_moves(c(6, 6), c(3, 5), die = c(3, 1)),
    "Both neutral destination ghosts sit over tan points. Confirm that their appearance is identical across the paired checker palettes."
  ),
  case(
    "C02", "Ghost colour identity on blue points",
    movement_xgid(c(`13` = 2L), c(`24` = 1L), dice = "31"),
    board_moves(c(13, 13), c(10, 12), die = c(3, 1)),
    "Both neutral destination ghosts sit over blue points. Confirm that their appearance is identical across the paired checker palettes."
  )
)

colors <- board_colors("bms")
style <- board_style("bms")
cards <- character(length(cases))
manifest <- vector("list", length(cases))

for (index in seq_along(cases)) {
  item <- cases[[index]]
  variants <- data.frame(
    suffix = c(
      "light-bottom", "dark-bottom",
      "light-bottom-left", "dark-bottom-left"
    ),
    panel_id = paste0(
      item$id, c("-LIGHT-R", "-DARK-R", "-LIGHT-L", "-DARK-L")
    ),
    label = c(
      "Light bottom — setup right", "Dark bottom — setup right",
      "Light bottom — setup left", "Dark bottom — setup left"
    ),
    light_player = c("near_player", "player_0", "near_player", "player_0"),
    mirror_horizontal = c(FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  panels <- character(nrow(variants))
  variant_manifest <- vector("list", nrow(variants))

  for (variant_index in seq_len(nrow(variants))) {
    variant <- variants[variant_index, , drop = FALSE]
    plot <- ggboard(
      item$xgid,
      colors = colors,
      style = style,
      movement_style = movement_style,
      moves = item$moves,
      decision = "checker_play",
      perspective = item$perspective,
      mirror_horizontal = variant$mirror_horizontal[[1L]],
      light_player = variant$light_player[[1L]],
      score_format = "both",
      player_name_style = "checker"
    )
    plot <- backgammonboard:::add_board_brand(
      plot,
      attr(plot, "backgammon_prepared_layout")$geometry,
      "Backgammon\nSimplified",
      side = attr(plot, "backgammon_brand_side"),
      color = colors$brand_text,
      family = "Source Sans 3",
      fontface = "bold"
    )
    movement_layer_start <- attr(plot, "backgammon_movement_layer_start")
    brand_layer <- plot$layers[[length(plot$layers)]]
    plot$layers <- plot$layers[-length(plot$layers)]
    plot$layers <- append(
      plot$layers,
      list(brand_layer),
      after = movement_layer_start - 1L
    )
    filename <- paste0(item$id, "-", variant$suffix[[1L]], ".svg")
    grDevices::svg(file.path(output, filename), width = 12, height = 9.1,
                   bg = colors$outside_fill, onefile = TRUE)
    print(plot)
    grDevices::dev.off()

    overlay <- attr(plot, "backgammon_prepared_layout")$selected_overlay
    if (nrow(overlay$markers) != 0L) {
      stop("Movement overlay review invariant failed for ", item$id, ".")
    }
    variant_manifest[[variant_index]] <- data.frame(
      id = item$id,
      panel_id = variant$panel_id[[1L]],
      palette = variant$label[[1L]],
      light_player = variant$light_player[[1L]],
      output_path = filename,
      title = item$title,
      xgid = item$xgid,
      moves = nrow(item$moves),
      hits = sum(overlay$segments$hit_confirmed),
      curved_steps = paste(
        overlay$segments$step_id[overlay$segments$curve_offset != 0],
        collapse = ","
      ),
      viewpoint = if (variant$mirror_horizontal[[1L]]) {
        paste0(item$perspective, " near; horizontally mirrored; setup left")
      } else {
        paste0(item$perspective, " near; unmirrored; setup right")
      },
      stringsAsFactors = FALSE
    )
    panels[[variant_index]] <- paste0(
      '<figure><p class="panel-id">', variant$panel_id[[1L]], '</p>',
      '<figcaption>', variant$label[[1L]], '</figcaption>',
      '<img src="', filename, '" alt="', item$title, ' — ',
      variant$label[[1L]], '"></figure>'
    )
  }
  manifest[[index]] <- do.call(rbind, variant_manifest)
  cards[[index]] <- paste0(
    '<article id="', item$id, '"><h2><span>', item$id, '</span> ',
    item$title, '</h2>',
    '<div class="variants">', paste0(panels, collapse = ""), '</div>',
    '<p>', item$review, '</p>',
    '<details><summary>XGID</summary><code>', item$xgid, '</code></details>',
    '</article>'
  )
}

manifest <- do.call(rbind, manifest)
utils::write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
case_index <- paste0(
  vapply(
    cases,
    function(item) paste0(
      '<a href="#', item$id, '" title="', item$title, '">', item$id, '</a>'
    ),
    character(1)
  ),
  collapse = ""
)
html <- c(
  '<!doctype html><html><head><meta charset="utf-8"><title>Movement overlay review</title>',
  '<style>body{margin:0;background:#e9e5dc;color:#172039;font:16px system-ui}main{max-width:2600px;margin:auto;padding:24px}header{background:#fff;padding:18px;border-radius:12px;margin-bottom:20px}.case-index,.priority-links{display:flex;flex-wrap:wrap;gap:7px;margin:10px 0}.case-index a,.priority-links a{display:inline-block;padding:5px 9px;border-radius:999px;background:#172039;color:#fff;text-decoration:none;font:800 13px ui-monospace,monospace}.priority-links a{background:#d95f32}.grid{display:grid;grid-template-columns:repeat(2,minmax(780px,1fr));gap:18px}article{background:#fff;border:1px solid #c8cdd5;border-radius:10px;padding:12px;scroll-margin-top:12px}.variants{display:grid;grid-template-columns:1fr 1fr;gap:10px}.variants figure{margin:0;border:1px solid #d9dde3;border-radius:8px;padding:8px}.panel-id{display:inline-block;margin:0 0 5px;padding:3px 7px;border-radius:999px;background:#172039;color:#fff;font:800 13px ui-monospace,monospace}.variants figcaption{font-weight:800;margin-bottom:6px}h2{font-size:20px;margin:0 0 10px}h2 span{display:inline-block;background:#172039;color:#fff;border-radius:999px;padding:3px 8px;margin-right:5px;font:800 14px ui-monospace,monospace}img{display:block;width:100%;height:auto}p{line-height:1.45}code{overflow-wrap:anywhere}details{background:#f4f5f7;padding:8px;border-radius:6px}@media(max-width:1700px){.grid{grid-template-columns:1fr}}@media(max-width:900px){.variants{grid-template-columns:1fr}}</style>',
  '</head><body><main><header><h1>Movement overlay edge-case review</h1>',
  '<h2>Stack-to-stack stress cases</h2><div class="priority-links"><a href="#M07">M07 — five onto three</a><a href="#M16">M16 — four onto three</a><a href="#M22">M22 — four onto empty</a><a href="#M38">M38 — nine onto five, five paths</a><a href="#M40">M40 — tuning stack, five paths</a></div>',
  '<h2>Same-line ambiguity cases</h2><div class="priority-links"><a href="#M18">M18</a><a href="#M28">M28</a><a href="#M29">M29</a><a href="#M30">M30</a><a href="#M31">M31</a><a href="#M32">M32</a><a href="#M33">M33 control</a><a href="#M41">M41 4-4 chain</a></div>',
  '<h2>All ', length(cases), ' cases</h2><div class="case-index">', case_index, '</div>',
  '<p>Every case shows setup-right and horizontally mirrored setup-left views, each with light-bottom and dark-bottom checker palettes. Homey/player_1 remains near except M39, which explicitly restores the earlier Foey/player_0-near view. Every board remains the factual starting position. Neutral light-grey checkers with dense dark dots show destinations; orange arrows have rounded tails and pointed convex heads placed inside the destination markers. Arrows remain straight unless a later segment substantially overlaps an earlier, nearly collinear segment or consecutive chain hops would read as one continuous line. Chain curvature scales down with the turn angle and stays straight for sharp turns; genuinely collinear chains retain the configured full inward bow. Shared destinations and isolated crossings remain straight. Point arrivals begin above the factual destination stack and overlap at visible level six when necessary. Repeated moves within either visual row align source and destination depths; moves between rows preserve atomic order. Chained arrows can originate from ghosts created by earlier atomic moves. Bearing-off destinations retain their edge-aware ordering. Move notation is intentionally absent from the images.</p>',
  '</header><section class="grid">', cards, '</section></main></body></html>'
)
writeLines(html, file.path(output, "index.html"), useBytes = TRUE)
message(
  "Rendered ", length(cases), " movement cases and ", nrow(manifest),
  " panels to ", normalizePath(output, winslash = "/")
)
