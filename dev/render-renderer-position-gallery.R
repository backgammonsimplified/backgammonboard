# Render the accepted RendererPosition edge-case gallery.
#
# Usage from the repository root:
#   Rscript dev/render-renderer-position-gallery.R \
#     task-work/B-BR-01/runtime/gallery \
#     task-work/B-BR-01/output/gallery

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop(
    "Supply exactly two paths: runtime staging directory and final gallery directory.",
    call. = FALSE
  )
}

runtime_directory <- arguments[[1L]]
output_directory <- arguments[[2L]]
dir.create(runtime_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(reset = TRUE, quiet = TRUE)

fixture_directory <- file.path(
  "tests",
  "testthat",
  "fixtures",
  "renderer-position"
)

cases <- data.frame(
  case_id = sprintf("%02d", 1:19),
  slug = c(
    "accepted-opening-learner-right",
    "opening-learner-left",
    "player0-on-roll-dice",
    "player1-on-roll-dice",
    "dice-absent",
    "one-player-on-bar",
    "both-players-on-bar",
    "one-player-borne-off",
    "both-players-borne-off",
    "tall-stacks",
    "centered-cube",
    "cube-owned-player0",
    "cube-owned-player1",
    "money-game",
    "match-score",
    "crawford",
    "on-roll-view-invariance",
    "late-bearoff",
    "asymmetric-borne-off"
  ),
  title = c(
    "Accepted opening - learner bottom - home right",
    "Same opening semantics - learner bottom - home left",
    "Opponent player_0 on roll at the top - dice 3-1",
    "Learner player_1 on roll at the bottom - dice 4-2",
    "Dice absent",
    "One player with a checker on the bar",
    "Both players on the bar",
    "Borne-off checkers for one player",
    "Borne-off checkers for both players",
    "Tall stacks with count treatment",
    "Centered cube",
    "Cube owned by player_0",
    "Cube owned by player_1",
    "Money-game context with nonzero score",
    "Seven-point match with nonzero scores",
    "Crawford match state",
    "On-roll changes without rotating the learner view",
    "Representative late bear-off",
    "Asymmetric borne-off counts"
  ),
  fixture = c(
    "opening-learner-right",
    "opening-learner-left",
    "player0-dice",
    "player1-dice",
    "opening-learner-right",
    "one-bar",
    "both-bars-and-off",
    "one-player-off",
    "both-fully-borne-off",
    "tall-stacks",
    "opening-learner-right",
    "cube-player0",
    "cube-player1",
    "money-nonzero",
    "match-score",
    "crawford",
    "player0-dice",
    "late-bearoff",
    "borne-off"
  ),
  purpose = c(
    "Baseline learner policy: player_1 remains bottom, player_0 remains top, and point labels are for player_1.",
    "Verify home-side mirroring is independent: learner remains bottom and learner-relative labels remain active.",
    "Verify the actual opponent on roll stays at the top, receives the orange arrow, and owns the displayed dice.",
    "Verify the actual learner on roll stays at the bottom, receives the orange arrow, and owns the displayed dice.",
    "Verify no placeholder dice are drawn.",
    "Verify bar placement for one stable player slot.",
    "Verify simultaneous bar stacks plus off-tray legibility.",
    "Verify a single occupied off tray and player-slot placement.",
    "Verify both off trays when all checkers are borne off.",
    "Verify five-visible checker treatment and stack-count labels.",
    "Verify centered cube value and left-side accepted display choice.",
    "Verify cube ownership maps to stable player_0 and follows its vertical side.",
    "Verify cube ownership maps to stable player_1 and follows its vertical side.",
    "Verify unlimited-play status, stable player labels, and nonzero win counts.",
    "Verify match length, raw scores, away scores, and stable labels.",
    "Verify Crawford status and hidden cube.",
    "Compare with case 04: the view hash, learner placement, home side, and point numbering stay fixed when on-roll identity changes.",
    "Inspect spacing and legibility in a sparse late bear-off race.",
    "Inspect both off trays with unequal counts and remaining board contact."
  ),
  stringsAsFactors = FALSE
)

html_escape <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

format_pair <- function(value) {
  if (length(value) == 0L) "absent" else paste(value, collapse = "-")
}

format_counts <- function(value) {
  paste0("player_0=", value[["white"]], ", player_1=", value[["black"]])
}

render_svg <- function(plot, path, background) {
  grDevices::svg(
    filename = path,
    width = 12,
    height = 9.1,
    bg = background,
    onefile = TRUE
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
}

colors <- board_colors("bms")
style <- board_style("bms")
manifest_rows <- vector("list", nrow(cases))

for (index in seq_len(nrow(cases))) {
  item <- cases[index, , drop = FALSE]
  fixture_path <- file.path(
    fixture_directory,
    paste0(item$fixture[[1L]], ".json")
  )
  position <- renderer_position(fixture_path)
  plot <- ggboard(
    position,
    colors = colors,
    style = style,
    decision = "none",
    score_format = "both",
    brand_text = NULL
  )

  output_name <- paste0(
    item$case_id[[1L]],
    "-",
    item$slug[[1L]],
    ".svg"
  )
  runtime_path <- file.path(runtime_directory, output_name)
  render_svg(plot, runtime_path, colors$outside_fill)

  cube <- attr(plot, "backgammon_cube_display")
  view <- position$renderer_view
  manifest_rows[[index]] <- data.frame(
    case_id = item$case_id[[1L]],
    title = item$title[[1L]],
    source_fixture = paste0(item$fixture[[1L]], ".json"),
    semantic_state_hash = position$renderer_semantic_state_hash,
    view_hash = position$renderer_view_hash,
    output_path = output_name,
    purpose = item$purpose[[1L]],
    top_player = view$top_player,
    bottom_player = view$bottom_player,
    bottom_home_board_side = view$bottom_home_board_side,
    point_labels_for = view$point_labels_for,
    cube_display_side = view$cube_display_side,
    rotation = view$rotation,
    view_origin = view$view_origin,
    on_roll = position$on_roll,
    dice = format_pair(position$dice),
    bar = format_counts(position$bar),
    off = format_counts(position$off),
    cube_state = cube$state,
    cube_value = if (is.na(cube$value)) "" else as.character(cube$value),
    cube_owner = if (is.null(cube$owner)) "" else cube$owner,
    learner_player = position$learner_slot,
    opponent_player = position$opponent_slot,
    on_roll_player = unname(position$player_labels[[position$on_roll]]),
    on_roll_display_side = backgammonboard:::visual_side_for_player(
      position$on_roll,
      perspective = position$learner_player
    ),
    visible_status = backgammonboard:::position_status_label(position),
    display_policy = position$renderer_display_policy,
    play_context = position$play_context,
    score = format_counts(position$score),
    match_length = if (is.na(position$match_length)) {
      ""
    } else {
      as.character(position$match_length)
    },
    crawford = isTRUE(position$is_crawford),
    stringsAsFactors = FALSE
  )
}

manifest <- do.call(rbind, manifest_rows)
utils::write.csv(
  manifest,
  file.path(runtime_directory, "manifest.csv"),
  row.names = FALSE,
  na = ""
)

card_html <- function(row) {
  paste0(
    '<article class="card">',
    '<div class="case-number">', html_escape(row$case_id), "</div>",
    "<h2>", html_escape(row$title), "</h2>",
    '<p class="purpose">', html_escape(row$purpose), "</p>",
    '<img src="', html_escape(row$output_path),
    '" alt="', html_escape(row$title), '">',
    '<dl>',
    '<dt>Fixture</dt><dd><code>', html_escape(row$source_fixture), "</code></dd>",
    '<dt>Semantic hash</dt><dd><code>', html_escape(row$semantic_state_hash), "</code></dd>",
    '<dt>View hash</dt><dd><code>', html_escape(row$view_hash), "</code></dd>",
    '<dt>View</dt><dd>',
    html_escape(paste0(
      "learner ",
      row$learner_player,
      " bottom - opponent ",
      row$opponent_player,
      " top - home ",
      row$bottom_home_board_side,
      " - labels ",
      row$point_labels_for,
      " - cube ",
      row$cube_display_side,
      " - ",
      row$rotation
    )),
    "</dd>",
    '<dt>Facts</dt><dd>',
    html_escape(paste0(
      "on roll ",
      row$on_roll_player,
      " at ",
      row$on_roll_display_side,
      " - dice ",
      row$dice,
      " - bar ",
      row$bar,
      " - off ",
      row$off
    )),
    "</dd>",
    "</dl>",
    "</article>"
  )
}

write_gallery <- function(path, narrow = FALSE) {
  cards <- vapply(
    seq_len(nrow(manifest)),
    function(index) card_html(manifest[index, , drop = FALSE]),
    character(1)
  )
  layout_class <- if (narrow) "narrow" else "responsive"
  alternate <- if (narrow) {
    '<a href="index.html">Open responsive review</a>'
  } else {
    '<a href="narrow.html">Open forced-narrow review</a>'
  }
  html <- c(
    "<!doctype html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>B-BR-01 RendererPosition Gallery</title>",
    "<style>",
    ":root{color-scheme:light}",
    "*{box-sizing:border-box}",
    "body{margin:0;padding:24px;background:#f4f1eb;color:#111b35;font-family:Arial,sans-serif}",
    "main{max-width:1600px;margin:0 auto}",
    "header{margin-bottom:24px}",
    "header p{max-width:1000px;line-height:1.5}",
    "a{color:#8b351f}",
    ".grid{display:grid;gap:22px;align-items:start}",
    ".grid.responsive{grid-template-columns:repeat(auto-fit,minmax(min(100%,520px),1fr))}",
    ".grid.narrow{grid-template-columns:minmax(0,520px);justify-content:center}",
    ".card{position:relative;background:#fff;border:1px solid #d8c5a5;border-radius:10px;padding:16px;overflow:hidden}",
    ".case-number{position:absolute;right:14px;top:12px;color:#8b351f;font-weight:bold}",
    ".card h2{margin:0 36px 8px 0;font-size:20px}",
    ".purpose{min-height:44px;line-height:1.4;color:#44506a}",
    ".card img{display:block;width:100%;height:auto;border:1px solid #eee4d3;background:#f4f1eb}",
    "dl{display:grid;grid-template-columns:max-content minmax(0,1fr);gap:6px 10px;margin:14px 0 0;font-size:12px}",
    "dt{font-weight:bold}",
    "dd{margin:0;min-width:0;overflow-wrap:anywhere}",
    "code{font-size:11px}",
    "@media(max-width:560px){body{padding:10px}.card{padding:10px}.purpose{min-height:0}dl{grid-template-columns:1fr}dt{margin-top:4px}}",
    "</style>",
    "</head>",
    "<body>",
    "<main>",
    "<header>",
    "<h1>B-BR-01 RendererPosition edge-case gallery</h1>",
    "<p>The learner must remain at the bottom, the opponent at the top, and point labels in the learner's perspective. Compare the independent home-side mirror and verify that changing the on-roll player moves only the orange arrow and dice, not the board orientation.</p>",
    "<p>", alternate, ' - <a href="manifest.csv">Open manifest</a>',
    ' - <a href="README.md">Review instructions</a></p>',
    "</header>",
    '<section class="grid ', layout_class, '">',
    cards,
    "</section>",
    "</main>",
    "</body>",
    "</html>"
  )
  writeLines(html, path, useBytes = TRUE)
}

write_gallery(file.path(runtime_directory, "index.html"), narrow = FALSE)
write_gallery(file.path(runtime_directory, "narrow.html"), narrow = TRUE)

readme <- c(
  "# B-BR-01 manual RendererPosition review",
  "",
  "Open `index.html` in a local browser for the responsive comparison.",
  "Open `narrow.html` for a forced 520-pixel-wide single-column review.",
  "",
  "Review cases 01 and 02 together first:",
  "",
  "- both must keep player_1 (the learner) at the bottom and player_0 at the top;",
  "- both must label points for player_1;",
  "- only the independent left/right home-board mirror changes.",
  "",
  "Then compare cases 03, 04, and 17:",
  "",
  "- cases 03 and 04 use the same view hash while the on-roll player changes;",
  "- the board must not rotate or mirror;",
  "- the orange arrow must sit immediately left of the actual on-roll player's name;",
  "- dice must follow the actual on-roll player;",
  "- visible status text must explicitly state who is on roll.",
  "",
  "Then inspect every case for bar/off placement, stack counts, dice and cube",
  "placement, money/match/Crawford information, clipping, overlap, spacing, and",
  "legibility. Record any failed case ID before approving the checkpoint.",
  "",
  "The exact source fixture and both hashes are in `manifest.csv`.",
  "Fixture generation provenance is in `fixture-provenance.json`."
)
writeLines(
  readme,
  file.path(runtime_directory, "README.md"),
  useBytes = TRUE
)
invisible(file.copy(
  file.path(fixture_directory, "provenance.json"),
  file.path(runtime_directory, "fixture-provenance.json"),
  overwrite = TRUE
))

generated_files <- c(
  manifest$output_path,
  "manifest.csv",
  "index.html",
  "narrow.html",
  "README.md",
  "fixture-provenance.json"
)
copied <- file.copy(
  file.path(runtime_directory, generated_files),
  file.path(output_directory, generated_files),
  overwrite = TRUE
)
if (!all(copied)) {
  stop("One or more staged gallery files could not be copied.", call. = FALSE)
}

message(
  "Rendered ",
  nrow(manifest),
  " cases to ",
  normalizePath(output_directory, winslash = "/", mustWork = TRUE)
)
message(
  "Responsive gallery: ",
  normalizePath(
    file.path(output_directory, "index.html"),
    winslash = "/",
    mustWork = TRUE
  )
)
message(
  "Narrow gallery: ",
  normalizePath(
    file.path(output_directory, "narrow.html"),
    winslash = "/",
    mustWork = TRUE
  )
)
