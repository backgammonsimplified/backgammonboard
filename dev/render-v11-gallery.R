# Render the broad contract-v1.1 XGID gallery.
# Usage: Rscript dev/render-v11-gallery.R <staging-directory> <output-directory>

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Supply exactly two paths: staging and output.", call. = FALSE)
}
staging <- arguments[[1L]]
output <- arguments[[2L]]
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(reset = TRUE, quiet = TRUE)

cases <- data.frame(
  case_id = sprintf("%02d", 1:22),
  slug = c(
    "accepted-opening-learner-right", "opening-learner-left",
    "player0-on-roll-dice", "player1-on-roll-dice", "dice-absent",
    "one-player-on-bar", "both-players-on-bar", "one-player-borne-off",
    "both-players-borne-off", "tall-stacks", "centered-cube",
    "cube-owned-player0", "cube-owned-player1", "money-game",
    "match-score", "crawford", "on-roll-view-invariance",
    "late-bearoff", "asymmetric-borne-off",
    "bms-homey-on-roll", "pending-offer-player0", "pending-offer-player1"
  ),
  title = c(
    "Opening — player_1 perspective (historical case 01)",
    "Opening — player_1 perspective (historical mirror deferred)",
    "Opening — player_1 on roll, dice 3-1",
    "Opening — player_0 on roll, dice 4-2",
    "Neutral position without dice",
    "One player on the bar", "Both players on the bar",
    "One player borne off", "Both players borne off", "Tall stacks",
    "Centered cube", "Cube owned by player_1 (historical case 12)",
    "Cube owned by player_0 (historical case 13)",
    "Unlimited game", "Match score", "Crawford",
    "Same factual view as case 03", "Late bearoff", "Asymmetric borne-off",
    "BMS review - Homey on roll", "BMS review - player_0 pending offer",
    "BMS review - player_1 pending offer"
  ),
  xgid = c(
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:-1:31:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:42:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----AA:0:0:1:00:0:0:0:0:10",
    "XGID=a------------------------B:0:0:1:00:0:0:0:0:10",
    "XGID=-E---EE-----------------a-:0:0:1:00:0:0:0:0:10",
    "XGID=--------------------------:0:0:1:00:0:0:0:0:10",
    "XGID=-O----------------------o-:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:1:-1:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:1:1:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:3:2:3:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:2:4:0:7:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:2:6:1:7:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:-1:31:0:0:0:0:10",
    "XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8",
    "XGID=-FDaA--------------a-Acbb-:0:0:1:00:3:0:0:7:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:1:1:1:D:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:1:-1:-1:D:0:0:0:0:10"
  ),
  decision = c(
    "none", "none", "checker_play", "checker_play", "none", "none",
    "none", "none", "none", "none", "none", "none", "none", "none",
    "none", "none", "checker_play", "none", "none", "checker_play",
    "take_pass", "take_pass"
  ),
  perspective = c(rep("player_1", 19L), rep("player_0", 3L)),
  stringsAsFactors = FALSE
)

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

render_svg <- function(plot, path, background) {
  grDevices::svg(path, width = 12, height = 9.1, bg = background, onefile = TRUE)
  print(plot)
  grDevices::dev.off()
}

colors <- board_colors("bms")
style <- board_style("bms")
manifest <- vector("list", nrow(cases))

for (index in seq_len(nrow(cases))) {
  case <- cases[index, , drop = FALSE]
  validation <- validate_xgid(case$xgid[[1L]])
  if (!validation$valid) stop("Invalid gallery XGID: ", case$case_id[[1L]], call. = FALSE)
  plot <- ggboard(
    case$xgid[[1L]], colors = colors, style = style,
    decision = case$decision[[1L]], perspective = case$perspective[[1L]],
    score_format = "both"
  )
  filename <- paste0(case$case_id[[1L]], "-", case$slug[[1L]], ".svg")
  render_svg(plot, file.path(staging, filename), colors$outside_fill)
  position <- attr(plot, "backgammon_position")
  cube <- attr(plot, "backgammon_cube_display")
  manifest[[index]] <- data.frame(
    case_id = case$case_id[[1L]], title = case$title[[1L]],
    xgid = position$xgid, decision = attr(plot, "backgammon_decision"),
    perspective = attr(plot, "backgammon_perspective"),
    on_roll = position$on_roll,
    dice = if (length(position$dice)) paste(position$dice, collapse = "-") else "absent",
    cube_state = cube$state, cube_value = if (is.na(cube$value)) "" else cube$value,
    output_path = filename, stringsAsFactors = FALSE
  )
}
manifest <- do.call(rbind, manifest)
utils::write.csv(manifest, file.path(staging, "manifest.csv"), row.names = FALSE, na = "")

cards <- vapply(seq_len(nrow(manifest)), function(index) {
  row <- manifest[index, , drop = FALSE]
  paste0(
    '<article><h2>', escape_html(row$case_id), " — ", escape_html(row$title),
    '</h2><img src="', escape_html(row$output_path), '" alt="',
    escape_html(row$title), '"><dl><dt>XGID</dt><dd><code>',
    escape_html(row$xgid), '</code></dd><dt>Context</dt><dd>',
    escape_html(row$decision), " / ", escape_html(row$perspective),
    '</dd><dt>Facts</dt><dd>on roll: ', escape_html(row$on_roll),
    '; dice: ', escape_html(row$dice), '; cube: ', escape_html(row$cube_state),
    " ", escape_html(row$cube_value), '</dd></dl></article>'
  )
}, character(1))

html <- c(
  '<!doctype html><html><head><meta charset="utf-8"><title>backgammonboard v1.1 gallery</title>',
  '<style>body{font:15px system-ui;background:#f4f1e8;color:#172039;margin:0}main{max-width:1200px;margin:auto;padding:24px}article{background:white;border:1px solid #ccd1d8;border-radius:10px;padding:16px;margin:20px 0}img{display:block;width:100%;height:auto}code{overflow-wrap:anywhere}dt{font-weight:700;margin-top:.5rem}</style>',
  '</head><body><main><h1>backgammonboard contract-v1.1 gallery</h1>',
  '<p>Complete XGID is the only source input. All cases use explicit display context and the BMS preset.</p>',
  cards, '</main></body></html>'
)
writeLines(html, file.path(staging, "index.html"), useBytes = TRUE)
writeLines(
  c(
    "# Contract-v1.1 gallery", "",
    "Generated exclusively from complete XGID through `ggboard()`.",
    "The historical independent home-board mirror is deferred with RendererPosition input."
  ),
  file.path(staging, "README.md"), useBytes = TRUE
)

files <- c(manifest$output_path, "manifest.csv", "index.html", "README.md")
copied <- file.copy(file.path(staging, files), file.path(output, files), overwrite = TRUE)
if (!all(copied)) stop("Gallery copy failed.", call. = FALSE)
message("Rendered ", nrow(manifest), " v1.1 cases to ", normalizePath(output, winslash = "/"))
