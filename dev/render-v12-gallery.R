# Render the contract-v1.2 semantic and transform review gallery.
# Usage: Rscript dev/render-v12-gallery.R <staging-directory> <output-directory>

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Supply exactly two paths: staging and output.", call. = FALSE)
}
staging <- arguments[[1L]]
output <- arguments[[2L]]
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(reset = TRUE, quiet = TRUE)

make_payload <- function(entries) {
  payload <- rep("-", 26L)
  for (name in names(entries)) payload[[as.integer(name)]] <- entries[[name]]
  paste(payload, collapse = "")
}

complete_xgid <- function(payload, cube = 0, owner = 0, turn = 1,
                          dice = "00", first_score = 0, second_score = 0,
                          crawford = 0, match_length = 0, max_cube = 10) {
  paste0(
    "XGID=", payload, ":", cube, ":", owner, ":", turn, ":", dice,
    ":", first_score, ":", second_score, ":", crawford, ":",
    match_length, ":", max_cube
  )
}

opening <- "-b----E-C---eE---c-e----B-"
asymmetric <- "-FDaA--------------a-Acbb-"
both_bars <- make_payload(c(`1` = "a", `26` = "A"))
bearing <- make_payload(c(`3` = "A"))

scenarios <- list(
  list(id = "case-18", title = "Mandatory case 18", xgid =
         "XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8",
       decision = "none", moves = NULL,
       inspect = "Confirm Foey is on roll, Homey is near by default, no dice, centered 1-cube, and no offer."),
  list(id = "asymmetric-checkers", title = "Asymmetric player-owned checkers", xgid =
         complete_xgid(asymmetric, owner = -1, turn = 1, dice = "42", first_score = 3,
                       second_score = 0, match_length = 7),
       decision = "checker_play", moves = NULL,
       inspect = "Trace lowercase checkers to Foey and uppercase checkers to Homey on fixed point IDs."),
  list(id = "bars", title = "Both factual bars", xgid = complete_xgid(both_bars),
       decision = "none", moves = NULL,
       inspect = "Confirm character 0 belongs to Foey's bar and character 25 to Homey's bar."),
  list(id = "borne-off", title = "Borne-off counts", xgid =
         complete_xgid("--------------------------"),
       decision = "none", moves = NULL,
       inspect = "Confirm both off markers retain player styling and follow the shared transforms."),
  list(id = "player-0-dice", title = "Foey on roll with dice", xgid =
         complete_xgid(opening, turn = -1, dice = "31"),
       decision = "checker_play", moves = NULL,
       inspect = "Confirm dice and on-roll status remain attached to player_0/Foey."),
  list(id = "player-1-dice", title = "Homey on roll with dice", xgid =
         complete_xgid(opening, turn = 1, dice = "42"),
       decision = "checker_play", moves = NULL,
       inspect = "Confirm dice and on-roll status remain attached to player_1/Homey."),
  list(id = "player-0-cube", title = "Foey owns cube", xgid =
         complete_xgid(opening, cube = 2, owner = -1, turn = -1),
       decision = "none", moves = NULL,
       inspect = "Confirm Foey owns the 4-cube, it stays outside on Foey's vertical side, and its number stays upright."),
  list(id = "player-1-cube", title = "Homey owns cube", xgid =
         complete_xgid(opening, cube = 2, owner = 1, turn = 1),
       decision = "none", moves = NULL,
       inspect = "Confirm Homey owns the 4-cube, it stays outside on Homey's vertical side, and its number stays upright."),
  list(id = "player-0-offer", title = "Pending offer by Foey", xgid =
         complete_xgid(opening, cube = 1, owner = -1, turn = -1, dice = "D"),
       decision = "take_pass", moves = NULL,
       inspect = "Confirm offered value 4 belongs to Foey, stays vertically centered inside, and normal cube/dice are hidden."),
  list(id = "player-1-offer", title = "Pending offer by Homey", xgid =
         complete_xgid(opening, cube = 1, owner = 1, turn = 1, dice = "D"),
       decision = "take_pass", moves = NULL,
       inspect = "Confirm offered value 4 belongs to Homey, stays vertically centered inside, and normal cube/dice are hidden."),
  list(id = "asymmetric-score", title = "Asymmetric scores", xgid =
         complete_xgid(opening, turn = 1, first_score = 5, second_score = 2,
                       match_length = 7),
       decision = "none", moves = NULL,
       inspect = "Confirm first XGID score 5 stays with Homey and second score 2 with Foey."),
  list(id = "checker-movement", title = "Checker movement", xgid =
         complete_xgid(opening, turn = 1, dice = "31"),
       decision = "checker_play", moves = board_moves(13, 10, die = 3, label = "13/10"),
       inspect = "Confirm arrow, destination ghost, and movement label transform without shape changes."),
  list(id = "bar-entry", title = "Bar entry", xgid =
         complete_xgid("-b----E-C---eE---c-e----AA", turn = 1, dice = "11"),
       decision = "checker_play", moves = board_moves("bar", 24, die = 1, label = "bar/24"),
       inspect = "Confirm Homey's bar-entry arrow and ghost follow both transforms."),
  list(id = "repeated-movement", title = "Repeated movement", xgid =
         complete_xgid(opening, turn = 1, dice = "55"),
       decision = "checker_play", moves = board_moves(c(13, 13), c(8, 8),
                                                       die = c(5, 5),
                                                       label = c("first", "second")),
       inspect = "Confirm repeated arrows retain routing, parallel curvature, ghosts, and upright labels."),
  list(id = "bearing-off", title = "Bearing off", xgid =
         complete_xgid(bearing, turn = 1, dice = "22"),
       decision = "checker_play", moves = board_moves(2, "off", die = 2, label = "2/off"),
       inspect = "Confirm the bearing-off arrow and destination ghost use the common transforms.")
)

states <- expand.grid(
  near_player = c("player_1", "player_0"),
  mirror_horizontal = c(FALSE, TRUE),
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

embedded_svg <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  start <- which(grepl("<svg", lines, fixed = TRUE))[[1L]]
  paste(lines[start:length(lines)], collapse = "\n")
}

repository_commit <- trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
colors <- board_colors("bms")
style <- board_style("bms")
manifest <- list()
cards <- list()
index <- 1L

for (scenario in scenarios) {
  for (state_index in seq_len(nrow(states))) {
    state <- states[state_index, , drop = FALSE]
    plot <- ggboard(
      scenario$xgid,
      colors = colors,
      style = style,
      moves = scenario$moves,
      decision = scenario$decision,
      perspective = state$near_player[[1L]],
      mirror_horizontal = state$mirror_horizontal[[1L]],
      light_player = "near_player",
      score_format = "both",
      player_name_style = "checker"
    )
    brand_text <- "Backgammon\nSimplified"
    plot <- backgammonboard:::add_board_brand(
      plot,
      attr(plot, "backgammon_prepared_layout")$geometry,
      brand_text,
      side = attr(plot, "backgammon_brand_side"),
      color = colors$brand_text,
      family = "Source Sans 3",
      fontface = "bold"
    )
    state_slug <- paste0(
      if (state$near_player[[1L]] == "player_1") "homey-near" else "foey-near",
      if (state$mirror_horizontal[[1L]]) "-mirrored" else "-not-mirrored"
    )
    filename <- paste0(sprintf("%02d", index), "-", scenario$id, "-", state_slug, ".svg")
    svg_path <- file.path(staging, filename)
    render_svg(plot, svg_path, colors$outside_fill)

    position <- attr(plot, "backgammon_position")
    cube <- attr(plot, "backgammon_cube_display")
    owner_or_offerer <- if (identical(cube$state, "offered")) {
      paste0("offerer=", cube$offerer)
    } else if (!is.null(cube$owner)) {
      paste0("owner=", cube$owner)
    } else {
      "centered/none"
    }
    manifest[[index]] <- data.frame(
      case_id = sprintf("%02d", index),
      scenario = scenario$id,
      title = scenario$title,
      xgid = position$xgid,
      decoded_mapping = "top=player_0/Foey; bottom=player_1/Homey",
      on_roll = position$on_roll,
      near_player = state$near_player[[1L]],
      mirror_horizontal = state$mirror_horizontal[[1L]],
      light_player = attr(plot, "backgammon_light_player"),
      cube_owner_or_offerer = owner_or_offerer,
      decision = attr(plot, "backgammon_decision"),
      inspection_instruction = scenario$inspect,
      repository_commit = repository_commit,
      review_status = "Ready for Marty review",
      output_path = filename,
      stringsAsFactors = FALSE
    )
    row <- manifest[[index]]
    cards[[index]] <- paste0(
      '<article><h2>', escape_html(row$case_id), " - ", escape_html(row$title),
      " - ", escape_html(state_slug), '</h2><div class="svg">', embedded_svg(svg_path),
      '</div><dl><dt>XGID</dt><dd><code>', escape_html(row$xgid),
      '</code></dd><dt>Decoded mapping</dt><dd>', escape_html(row$decoded_mapping),
      '</dd><dt>Display</dt><dd>on roll: ', escape_html(row$on_roll),
      '; near: ', escape_html(row$near_player), '; mirror: ',
      tolower(as.character(row$mirror_horizontal)), '; light: ',
      escape_html(row$light_player), '</dd><dt>Cube</dt><dd>',
      escape_html(row$cube_owner_or_offerer), '</dd><dt>Decision</dt><dd>',
      escape_html(row$decision), '</dd><dt>Inspect</dt><dd>',
      escape_html(row$inspection_instruction), '</dd><dt>Repository commit</dt><dd><code>',
      escape_html(row$repository_commit), '</code></dd><dt>Review status</dt><dd>',
      escape_html(row$review_status), '</dd></dl></article>'
    )
    index <- index + 1L
  }
}

manifest <- do.call(rbind, manifest)
utils::write.csv(manifest, file.path(staging, "manifest.csv"), row.names = FALSE, na = "")
html <- c(
  '<!doctype html><html><head><meta charset="utf-8"><title>backgammonboard v1.2 review gallery</title>',
  '<style>body{font:15px system-ui;background:#f4f1e8;color:#172039;margin:0}main{max-width:1280px;margin:auto;padding:24px}article{background:white;border:1px solid #ccd1d8;border-radius:10px;padding:16px;margin:20px 0}.svg svg{display:block;width:100%;height:auto}code{overflow-wrap:anywhere}dt{font-weight:700;margin-top:.5rem}</style>',
  '</head><body><main><h1>backgammonboard contract-v1.2 review gallery</h1>',
  '<p>Every scenario is rendered in all four independent display states. SVG is embedded for wrapper-free review.</p>',
  unlist(cards, use.names = FALSE), '</main></body></html>'
)
writeLines(html, file.path(staging, "index.html"), useBytes = TRUE)
writeLines(c(
  "# Contract-v1.2 review gallery", "",
  paste0(nrow(manifest), " embedded-SVG cases generated at repository commit `", repository_commit, "`."),
  "Every scenario is shown Homey-near/Foey-near with and without independent horizontal mirroring."
), file.path(staging, "README.md"), useBytes = TRUE)
file.copy(file.path("dev", "fonts", "OFL-1.1.txt"),
          file.path(staging, "OFL-SourceCodePro.txt"), overwrite = TRUE)

files <- c(manifest$output_path, "manifest.csv", "index.html", "README.md", "OFL-SourceCodePro.txt")
copied <- file.copy(file.path(staging, files), file.path(output, files), overwrite = TRUE)
if (!all(copied)) stop("Gallery copy failed.", call. = FALSE)
message("Rendered ", nrow(manifest), " v1.2 cases to ", normalizePath(output, winslash = "/"))
