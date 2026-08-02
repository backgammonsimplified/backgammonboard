# Render the Marty review gallery from prepared real GNU fixtures.
#
# Run `python dev/generate-real-gnu-fixtures.py` first when the accepted Engine
# Kit checkout is available, then:
#   Rscript dev/render-move-illustration-gallery.R artifacts/gallery

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) stop("Supply zero or one output directory path.", call. = FALSE)
output_dir <- if (length(arguments) == 1L) arguments[[1L]] else file.path("artifacts", "gallery")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(".", quiet = TRUE)

fixture_dir <- file.path("tests", "testthat", "fixtures", "real-gnu-gallery")
manifest_path <- file.path(fixture_dir, "manifest.csv")
if (!file.exists(manifest_path)) {
  stop("Prepared real GNU fixtures are missing; run dev/generate-real-gnu-fixtures.py.", call. = FALSE)
}
cases <- utils::read.csv(manifest_path, stringsAsFactors = FALSE, na.strings = "")

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

inline_svg <- function(path) {
  svg <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # The standalone XML declaration is invalid when SVG is embedded in HTML.
  sub("^\\s*<\\?xml[^>]*>\\s*", "", svg)
}

# GNU notation numbers points from the mover's own perspective.  The package
# move overlay uses its stable White/Black coordinate system, so this bounded
# gallery-only preparation mirrors numbered endpoints for a Black mover.
prepared_gnu_moves <- function(notation, position) {
  if (is.na(notation) || !nzchar(notation)) return(NULL)
  moves <- board_moves(notation)
  if (identical(position$on_roll, "black")) {
    for (column in c("from_point", "to_point")) {
      numbered <- !is.na(moves[[column]])
      moves[[column]][numbered] <- 25L - moves[[column]][numbered]
    }
  }
  # GNU's `6/4*(2)` annotates the repeated play group; only the first arrival
  # can hit the original blot.  Keep the raw notation in metadata while
  # preparing factual atomic hit flags for the overlay.
  hit_targets <- which(moves$hit_marked & !is.na(moves$to_point))
  for (target in unique(moves$to_point[hit_targets])) {
    duplicates <- hit_targets[moves$to_point[hit_targets] == target]
    if (length(duplicates) > 1L) moves$hit_marked[duplicates[-1L]] <- FALSE
  }
  validate_board_moves(moves)
  moves
}

case_html <- character(nrow(cases))
for (index in seq_len(nrow(cases))) {
  item <- cases[index, , drop = FALSE]
  position <- renderer_position(file.path(fixture_dir, item$fixture[[1L]]))
  brand_text <- if (identical(item$id[[1L]], "opening-branded")) "Backgammon\nSimplified" else NULL
  moves <- prepared_gnu_moves(item$notation[[1L]], position)
  plot <- ggboard(
    position,
    colors = board_colors("bms"),
    style = board_style("bms"),
    decision = item$decision[[1L]],
    show_information = TRUE,
    brand_text = brand_text,
    moves = moves
  )
  svg_path <- file.path(output_dir, paste0(item$id[[1L]], ".svg"))
  grDevices::svg(svg_path, width = 10.625, height = 7.5, bg = "white")
  print(plot)
  grDevices::dev.off()
  svg <- inline_svg(svg_path)
  unlink(svg_path)
  metadata <- paste0(
    "<dl><dt>Source</dt><dd>", escape_html(item$source[[1L]]), " move ", escape_html(item$move_number[[1L]]),
    "</dd><dt>GNU IDs</dt><dd><code>", escape_html(item$position_id[[1L]]), "</code> / <code>", escape_html(item$match_id[[1L]]),
    "</code></dd><dt>Original event</dt><dd>", escape_html(item$original_event[[1L]]),
    "</dd><dt>Decision / dice</dt><dd>", escape_html(item$decision[[1L]]), " / ", escape_html(item$dice[[1L]]),
    "</dd><dt>Cube</dt><dd>", escape_html(item$cube_facts[[1L]]),
    "</dd><dt>Score / Crawford</dt><dd>", escape_html(item$score_context[[1L]]),
    "</dd><dt>Perspective</dt><dd>", escape_html(item$perspective[[1L]]),
    "</dd></dl>"
  )
  case_html[[index]] <- paste0(
    "<figure id=\"", escape_html(item$id[[1L]]), "\"><figcaption><h2>",
    escape_html(item$title[[1L]]), "</h2>", metadata,
    "<p>", escape_html(item$inspect[[1L]]), "</p></figcaption>", svg, "</figure>"
  )
}

page <- c(
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Marty real GNU review gallery</title>",
  "<style>body{font:16px system-ui;max-width:1100px;margin:2rem auto;padding:0 1rem}figure{margin:3rem 0;border-bottom:1px solid #ddd;padding-bottom:2rem}h2{margin-bottom:.4rem}dl{display:grid;grid-template-columns:max-content 1fr;gap:.25rem .75rem;margin:.75rem 0}dt{font-weight:650}dd{margin:0}svg{display:block;width:100%;height:auto;border:1px solid #ddd}code{overflow-wrap:anywhere}</style></head><body>",
  "<h1>Marty review gallery: real GNU cases</h1><p>Every board below is an inline SVG generated from the prepared real GNU fixture named in its metadata.</p>",
  case_html,
  "</body></html>"
)
writeLines(page, file.path(output_dir, "index.html"), useBytes = TRUE)
message("PASS: real GNU gallery rendered as one embedded HTML page.")
