# Build the historical 24-case side-by-side comparison index.
# Usage: Rscript dev/render-v12-comparison.R <comparison-dir> <corrected-commit>

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Supply the comparison directory and corrected commit.", call. = FALSE)
}
comparison <- arguments[[1L]]
corrected_commit <- arguments[[2L]]
before <- utils::read.csv(
  file.path(comparison, "before", "manifest.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
after <- utils::read.csv(
  file.path(comparison, "after", "manifest.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (!identical(before$case_id, after$case_id) ||
    !identical(before$output_path, after$output_path)) {
  stop("Before and after manifests do not describe the same 24 cases.", call. = FALSE)
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

cards <- vapply(seq_len(nrow(before)), function(index) {
  row <- before[index, , drop = FALSE]
  xgid <- escape_html(row$xgid)
  filename <- escape_html(row$output_path)
  title <- escape_html(row$title)
  paste0(
    '<article><h2>', escape_html(row$case_id), " - ", title,
    '</h2><div class="pair"><figure><figcaption>Accepted starting render (efdbdb6)</figcaption>',
    '<img src="before/', filename, '" alt="Starting ', title, '">',
    '<div class="xgid"><strong>XGID</strong><code>', xgid, '</code></div></figure>',
    '<figure><figcaption>Corrected render (', escape_html(substr(corrected_commit, 1L, 8L)), ')</figcaption>',
    '<img src="after/', filename, '" alt="Corrected ', title, '">',
    '<div class="xgid"><strong>XGID</strong><code>', xgid, '</code></div></figure></div>',
    '<dl><dt>Required semantic correction under v1.2</dt><dd>',
    'Top/player_0/Foey and bottom/player_1/Homey facts, turn, cube, scores, labels, and player attachment use the corrected mapping.</dd>',
    '<dt>Coordinate movement caused by independent transforms</dt><dd>',
    'The historical request for player_0 near resolves to Foey near; horizontal mirroring remains independent. ',
    'The status sentence stays in the bottom band, and the near player uses the light palette through the explicit display property.</dd>',
    '<dt>Unintended regression</dt><dd>None detected by automated structure/style tests; visual acceptance remains Marty&rsquo;s manual gate.</dd></dl></article>'
  )
}, character(1))

html <- c(
  '<!doctype html><html><head><meta charset="utf-8"><title>backgammonboard v1.2 before/after comparison</title>',
  '<style>body{font:15px system-ui;background:#f4f1e8;color:#172039;margin:0}main{max-width:1500px;margin:auto;padding:24px}article{background:#fff;border:1px solid #ccd1d8;border-radius:10px;padding:16px;margin:22px 0}.pair{display:grid;grid-template-columns:1fr 1fr;gap:16px}figure{margin:0}figcaption{font-weight:700;margin-bottom:8px}img{display:block;width:100%;height:auto;border:1px solid #ddd}.xgid{margin-top:8px}.xgid strong{display:block}.xgid code{display:block;overflow-wrap:anywhere;background:#f6f7f9;padding:8px;border-radius:4px}dt{font-weight:700;margin-top:.55rem}@media(max-width:900px){.pair{grid-template-columns:1fr}}</style>',
  '</head><body><main><h1>Accepted v1.1 baseline vs corrected v1.2 semantics</h1>',
  paste0('<p>The left column is the exact 24-case gallery generated before editing at efdbdb6. The right column is the same historical case list rendered from implementation commit ', escape_html(substr(corrected_commit, 1L, 8L)), '. The XGID is repeated immediately below each image.</p>'),
  cards,
  '</main></body></html>'
)
writeLines(html, file.path(comparison, "index.html"), useBytes = TRUE)
message("Wrote ", nrow(before), " comparison pairs with XGIDs below both images.")
