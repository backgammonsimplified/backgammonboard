if (!requireNamespace("backgammoncalculator", quietly = TRUE)) {
  stop("Install local `backgammoncalculator` before running this gallery.")
}

if (!requireNamespace("backgammonboard", quietly = TRUE)) {
  stop("Install `backgammonboard` before running this gallery.")
}

original_gnuid <- "4HPwATDgc/ABMA:8IhuACAACAAE"

# Round trip 1: GNU -> XG -> GNU
gnu_to_xg <- backgammoncalculator::gnuid_to_xgid(original_gnuid)
xg_to_gnu <- backgammoncalculator::gnuid_from_position(
  backgammoncalculator::position_from_xgid(gnu_to_xg)
)

# Round trip 2: XG -> GNU -> XG
original_xgid <- gnu_to_xg
xg_to_gnu_2 <- backgammoncalculator::gnuid_from_position(
  backgammoncalculator::position_from_xgid(original_xgid)
)
gnu_to_xg_2 <- backgammoncalculator::gnuid_to_xgid(xg_to_gnu_2)

gnu_exact <- identical(xg_to_gnu, original_gnuid)
xg_exact <- identical(
  backgammonboard::normalize_xgid(gnu_to_xg_2),
  backgammonboard::normalize_xgid(original_xgid)
)

rows <- list(
  list(
    title = "GNU → XG → GNU",
    pass = gnu_exact,
    stages = list(
      list(label = "1. Original GNUID", id = original_gnuid),
      list(label = "2. Converted XGID", id = gnu_to_xg),
      list(label = "3. Round-tripped GNUID", id = xg_to_gnu)
    )
  ),
  list(
    title = "XG → GNU → XG",
    pass = xg_exact,
    stages = list(
      list(label = "1. Original XGID", id = original_xgid),
      list(label = "2. Converted GNUID", id = xg_to_gnu_2),
      list(label = "3. Round-tripped XGID", id = gnu_to_xg_2)
    )
  )
)

out_dir <- file.path(
  path.expand("~/Documents/scratch"),
  "backgammonboard-gnuid-roundtrip-gallery"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

save_svg <- function(plot, path) {
  grDevices::svg(path, width = 10, height = 7, onefile = TRUE)
  print(plot)
  grDevices::dev.off()
}

read_svg <- function(path) {
  x <- paste(readLines(path, warn = FALSE), collapse = "\n")
  x <- sub("^\\s*<\\?xml[^>]*>\\s*", "", x, perl = TRUE)
  sub("^\\s*<!DOCTYPE[^>]*>\\s*", "", x, perl = TRUE)
}

render_stage <- function(stage, row_index, col_index) {
  plot <- backgammonboard::ggboard(
    stage$id,
    perspective = "player_1",
    mirror_horizontal = FALSE
  )

  path <- file.path(
    out_dir,
    sprintf("roundtrip-%d-stage-%d.svg", row_index, col_index)
  )
  save_svg(plot, path)

  paste0(
    "<article class='stage'>",
    "<h3>", html_escape(stage$label), "</h3>",
    read_svg(path),
    "<p><code>", html_escape(stage$id), "</code></p>",
    "</article>"
  )
}

render_row <- function(row, row_index) {
  stages <- vapply(
    seq_along(row$stages),
    function(i) render_stage(row$stages[[i]], row_index, i),
    character(1)
  )

  status <- if (row$pass) "IDENTICAL ID" else "NORMALIZED ID"

  paste0(
    "<section class='trip'>",
    "<header><h2>", html_escape(row$title), "</h2>",
    "<strong class='", if (row$pass) "pass" else "normalized", "'>",
    status, "</strong></header>",
    "<div class='stages'>", paste(stages, collapse = "\n"), "</div>",
    "</section>"
  )
}

body <- paste(
  vapply(seq_along(rows), function(i) render_row(rows[[i]], i), character(1)),
  collapse = "\n"
)

doc <- paste0(
  "<!doctype html><html><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>GNUID / XGID round trips</title>",
  "<style>",
  "body{font-family:system-ui,sans-serif;background:#f4f4f4;color:#222;margin:0}",
  "main{max-width:1700px;margin:auto;padding:24px}",
  ".trip{background:#fff;border:1px solid #ddd;border-radius:10px;padding:18px;margin-bottom:24px}",
  ".trip header{display:flex;justify-content:space-between;align-items:center;gap:16px}",
  ".stages{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px}",
  ".stage{border:1px solid #ddd;border-radius:8px;padding:10px;min-width:0}",
  ".stage svg{width:100%;height:auto;display:block}",
  "code{overflow-wrap:anywhere}",
  ".pass{background:#e8f5e9;padding:6px 10px;border-radius:6px}",
  ".normalized{background:#fff3cd;padding:6px 10px;border-radius:6px}",
  "@media(max-width:1000px){.stages{grid-template-columns:1fr}}",
  "</style></head><body><main>",
  "<h1>GNUID ↔ XGID round-trip visualization</h1>",
  "<p>All panels use <code>perspective = \"player_1\"</code> and ",
  "<code>mirror_horizontal = FALSE</code>. Display orientation is held constant.</p>",
  body,
  "</main></body></html>"
)

index <- file.path(out_dir, "index.html")
writeLines(doc, index, useBytes = TRUE)

cat("GNU -> XG -> GNU:", if (gnu_exact) "IDENTICAL ID" else "NORMALIZED ID", "\n")
cat("XG -> GNU -> XG:", if (xg_exact) "IDENTICAL ID" else "NORMALIZED ID", "\n")
cat(normalizePath(index, winslash = "/"), "\n")
