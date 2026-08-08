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
    title = "GNUID → XGID → GNUID",
    pass = gnu_exact,
    stages = list(
      list(role = "Original", kind = "GNUID", id = original_gnuid),
      list(role = "Converted", kind = "XGID", id = gnu_to_xg),
      list(role = "Round trip", kind = "GNUID", id = xg_to_gnu)
    )
  ),
  list(
    title = "XGID → GNUID → XGID",
    pass = xg_exact,
    stages = list(
      list(role = "Original", kind = "XGID", id = original_xgid),
      list(role = "Converted", kind = "GNUID", id = xg_to_gnu_2),
      list(role = "Round trip", kind = "XGID", id = gnu_to_xg_2)
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
    "<article class='stage kind-", tolower(stage$kind), "'>",
    "<div class='stage-heading'>",
    "<div>",
    "<span class='step'>Stage ", col_index, "</span>",
    "<h3>", html_escape(stage$role), "</h3>",
    "</div>",
    "<span class='kind-badge'>", html_escape(stage$kind), "</span>",
    "</div>",
    "<div class='board-frame'>", read_svg(path), "</div>",
    "<div class='identifier'>",
    "<span class='identifier-label'>", html_escape(stage$kind), "</span>",
    "<code>", html_escape(stage$id), "</code>",
    "</div>",
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
  status_class <- if (row$pass) "pass" else "normalized"
  status_note <- if (row$pass) {
    "The round-trip identifier is byte-identical to the original."
  } else {
    paste0(
      "The identifier changed during normalization. This is informational: ",
      "compare the rendered factual state across all three boards."
    )
  }

  paste0(
    "<section class='trip'>",
    "<header class='trip-heading'>",
    "<div><span class='eyebrow'>Round trip ", row_index, "</span>",
    "<h2>", html_escape(row$title), "</h2></div>",
    "<div class='status-wrap'>",
    "<strong class='status ", status_class, "'>", status, "</strong>",
    "<span>", html_escape(status_note), "</span>",
    "</div>",
    "</header>",
    "<div class='flow-labels'><span>source</span><span>conversion</span><span>result</span></div>",
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
  ":root{",
  "--page:#f3efe7;--paper:#fffdf8;--ink:#16251f;--muted:#68756f;",
  "--line:#d8d4ca;--homey:#0f6b58;--homey-soft:#e3f2ed;",
  "--foey:#b56f22;--foey-soft:#fbeddc;--xgid:#315e8a;--xgid-soft:#e8f0f7;",
  "--normalized:#8b6514;--normalized-soft:#fff3cf;--shadow:0 10px 30px rgba(25,39,33,.08)",
  "}",
  "*{box-sizing:border-box}",
  "body{font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:var(--page);color:var(--ink);margin:0;line-height:1.45}",
  "main{max-width:1780px;margin:auto;padding:34px 28px 56px}",
  ".hero{background:linear-gradient(135deg,#173f36 0%,#0f6b58 62%,#315e8a 100%);color:#fff;border-radius:18px;padding:28px 32px;margin-bottom:24px;box-shadow:var(--shadow)}",
  ".hero h1{font-size:clamp(28px,3vw,44px);letter-spacing:-.03em;margin:0 0 8px}",
  ".hero p{max-width:1050px;margin:7px 0;color:rgba(255,255,255,.88);font-size:16px}",
  ".hero code{background:rgba(255,255,255,.12);border:1px solid rgba(255,255,255,.16);border-radius:5px;padding:1px 5px;color:#fff}",
  ".legend{display:flex;flex-wrap:wrap;gap:9px;margin-top:17px}",
  ".legend span{display:inline-flex;align-items:center;gap:7px;background:rgba(255,255,255,.1);border:1px solid rgba(255,255,255,.18);border-radius:999px;padding:6px 10px;font-size:13px}",
  ".dot{display:inline-block;width:9px;height:9px;border-radius:50%}",
  ".dot.homey{background:#7bd6bd}.dot.foey{background:#f1b76f}.dot.xgid{background:#9cc6ee}",
  ".trip{background:var(--paper);border:1px solid var(--line);border-radius:16px;padding:21px;margin-bottom:24px;box-shadow:var(--shadow)}",
  ".trip-heading{display:flex;justify-content:space-between;align-items:flex-start;gap:24px;margin-bottom:15px}",
  ".trip-heading h2{font-size:25px;letter-spacing:-.02em;margin:2px 0 0}",
  ".eyebrow,.step{display:block;text-transform:uppercase;letter-spacing:.11em;font-size:11px;font-weight:800;color:var(--muted)}",
  ".status-wrap{display:flex;flex-direction:column;align-items:flex-end;gap:5px;max-width:520px;text-align:right;color:var(--muted);font-size:13px}",
  ".status{display:inline-block;border-radius:999px;padding:6px 10px;font-size:12px;letter-spacing:.04em}",
  ".status.pass{background:var(--homey-soft);color:var(--homey)}",
  ".status.normalized{background:var(--normalized-soft);color:var(--normalized)}",
  ".flow-labels{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:0 0 6px}",
  ".flow-labels span{text-align:center;text-transform:uppercase;letter-spacing:.12em;font-size:10px;font-weight:800;color:#89928e}",
  ".stages{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px}",
  ".stage{background:#fff;border:1px solid var(--line);border-top:5px solid var(--homey);border-radius:12px;padding:11px;min-width:0;overflow:hidden}",
  ".stage.kind-xgid{border-top-color:var(--xgid)}",
  ".stage.kind-gnuid{border-top-color:var(--foey)}",
  ".stage-heading{display:flex;align-items:flex-start;justify-content:space-between;gap:10px;padding:1px 2px 9px}",
  ".stage-heading h3{font-size:19px;margin:1px 0 0}",
  ".kind-badge{border-radius:999px;padding:5px 9px;font-size:11px;font-weight:800;letter-spacing:.05em}",
  ".kind-xgid .kind-badge{background:var(--xgid-soft);color:var(--xgid)}",
  ".kind-gnuid .kind-badge{background:var(--foey-soft);color:var(--foey)}",
  ".board-frame{background:#f7f4ed;border:1px solid #e4e0d7;border-radius:9px;padding:5px;overflow:hidden}",
  ".stage svg{width:100%;height:auto;display:block}",
  ".identifier{margin-top:9px;background:#f7f5f0;border-radius:8px;padding:9px 10px}",
  ".identifier-label{display:block;text-transform:uppercase;letter-spacing:.1em;color:var(--muted);font-size:10px;font-weight:800;margin-bottom:3px}",
  "code{font-family:'SFMono-Regular',Consolas,'Liberation Mono',monospace;overflow-wrap:anywhere;word-break:break-word}",
  ".identifier code{font-size:12px;color:#26342f}",
  ".footer-note{color:var(--muted);font-size:13px;margin:15px 3px 0}",
  "@media(max-width:1050px){",
  ".trip-heading{flex-direction:column}.status-wrap{align-items:flex-start;text-align:left}",
  ".flow-labels{display:none}.stages{grid-template-columns:1fr}",
  "}",
  "</style></head><body><main>",
  "<section class='hero'>",
  "<h1>GNUID ↔ XGID round-trip review</h1>",
  "<p><strong>Visual gate:</strong> the three boards in each row should preserve the same renderable factual state. A GNUID can normalize information during conversion, so a changed identifier is not automatically a failure.</p>",
  "<p>Every board uses <code>perspective = \"player_1\"</code> with Homey near and <code>mirror_horizontal = FALSE</code>. Display controls are held constant so conversion differences cannot hide behind viewpoint changes.</p>",
  "<div class='legend'>",
  "<span><i class='dot homey'></i>Homey / player_1 near</span>",
  "<span><i class='dot foey'></i>GNUID stage</span>",
  "<span><i class='dot xgid'></i>XGID stage</span>",
  "</div></section>",
  body,
  "<p class='footer-note'>Identifier status is informational. The release check is factual/render equivalence across each round trip.</p>",
  "</main></body></html>"
)

index <- file.path(out_dir, "index.html")
writeLines(doc, index, useBytes = TRUE)

cat("GNU -> XG -> GNU:", if (gnu_exact) "IDENTICAL ID" else "NORMALIZED ID", "\n")
cat("XG -> GNU -> XG:", if (xg_exact) "IDENTICAL ID" else "NORMALIZED ID", "\n")
cat(normalizePath(index, winslash = "/"), "\n")
