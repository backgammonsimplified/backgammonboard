if (!requireNamespace("backgammoncalculator", quietly = TRUE)) {
  stop("Install `backgammoncalculator` before running this gallery.")
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
  # Match the board's native 17:12 aspect ratio and give board text enough room.
  grDevices::svg(path, width = 12.75, height = 9, onefile = TRUE)
  print(plot)
  grDevices::dev.off()
}

read_svg <- function(path) {
  x <- paste(readLines(path, warn = FALSE), collapse = "\n")
  x <- sub("^\\s*<\\?xml[^>]*>\\s*", "", x, perl = TRUE)
  sub("^\\s*<!DOCTYPE[^>]*>\\s*", "", x, perl = TRUE)
}

# Use the accepted Backgammon Simplified presentation explicitly. The gallery
# should review the same colorful release presentation, not the neutral default.
gallery_colors <- backgammonboard::board_colors("bs")
gallery_style <- backgammonboard::board_style("bs")
gallery_geometry <- backgammonboard:::board_geometry(
  gallery_style,
  perspective = "white"
)

render_stage <- function(stage, row_index, col_index) {
  plot <- backgammonboard::ggboard(
    stage$id,
    colors = gallery_colors,
    style = gallery_style,
    perspective = "player_1",
    mirror_horizontal = FALSE,
    player_name_style = "checker"
  )

  plot <- backgammonboard:::add_board_brand(
    plot,
    gallery_geometry,
    "Backgammon\nSimplified",
    side = attr(plot, "backgammon_brand_side"),
    color = gallery_colors$brand_text
  )

  path <- file.path(
    out_dir,
    sprintf("roundtrip-%d-stage-%d.svg", row_index, col_index)
  )
  save_svg(plot, path)

  paste0(
    "<article class='stage kind-", tolower(stage$kind), "'>",
    "<header class='stage-heading'>",
    "<div class='stage-title'>",
    "<span class='step'>Stage ", col_index, "</span>",
    "<h3>", html_escape(stage$role), "</h3>",
    "</div>",
    "<span class='kind-badge'>", html_escape(stage$kind), "</span>",
    "</header>",
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
      "The identifier changed during normalization. This is informational. ",
      "Use the boards to review factual/render equivalence."
    )
  }

  paste0(
    "<section class='trip'>",
    "<header class='trip-heading'>",
    "<span class='eyebrow'>Round trip ", row_index, "</span>",
    "<h2>", html_escape(row$title), "</h2>",
    "<div class='status-line'>",
    "<strong class='status ", status_class, "'>", status, "</strong>",
    "<span>", html_escape(status_note), "</span>",
    "</div>",
    "</header>",
    "<div class='stage-scroller'><div class='stages'>",
    paste(stages, collapse = "\n"),
    "</div></div>",
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
  "--page:#eee7dc;--paper:#fffdf9;--ink:#111b35;--muted:#5f6877;",
  "--line:#d8c5a5;--navy:#111b35;--cream:#f6ebdd;--tan:#c29a6b;",
  "--blue:#9eb2d2;--xgid:#315e8a;--xgid-soft:#e8f0f7;",
  "--gnu:#a66520;--gnu-soft:#fbeddc;--ok:#0f6b58;--ok-soft:#e3f2ed;",
  "--normalized:#8b6514;--normalized-soft:#fff3cf;",
  "--shadow:0 12px 30px rgba(17,27,53,.09)",
  "}",
  "*{box-sizing:border-box}",
  "html{background:var(--page)}",
  "body{font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:var(--page);color:var(--ink);margin:0;line-height:1.45}",
  "main{max-width:1900px;margin:0 auto;padding:30px 28px 56px}",
  ".hero{background:var(--navy);color:#fff;border:6px solid var(--tan);border-radius:18px;padding:28px 32px;margin-bottom:26px;box-shadow:var(--shadow)}",
  ".hero h1{font-size:clamp(30px,3vw,46px);letter-spacing:-.035em;margin:0 0 10px;line-height:1.08}",
  ".hero p{max-width:1200px;margin:7px 0;color:#f8eedd;font-size:16px}",
  ".hero code{background:rgba(255,255,255,.12);border:1px solid rgba(255,255,255,.18);border-radius:5px;padding:1px 5px;color:#fff}",
  ".legend{display:flex;flex-wrap:wrap;gap:10px;margin-top:18px}",
  ".legend span{display:inline-flex;align-items:center;gap:7px;background:#f8eedd;color:var(--navy);border-radius:999px;padding:7px 11px;font-size:13px;font-weight:700}",
  ".dot{display:inline-block;width:10px;height:10px;border-radius:50%}",
  ".dot.homey{background:var(--blue)}.dot.foey{background:var(--tan)}.dot.xgid{background:var(--xgid)}",
  ".trip{background:var(--paper);border:1px solid var(--line);border-radius:16px;padding:22px;margin-bottom:26px;box-shadow:var(--shadow)}",
  ".trip-heading{margin-bottom:16px}",
  ".trip-heading h2{font-size:27px;letter-spacing:-.02em;line-height:1.15;margin:3px 0 10px}",
  ".eyebrow,.step{display:block;text-transform:uppercase;letter-spacing:.11em;font-size:11px;font-weight:800;color:var(--muted)}",
  ".status-line{display:flex;align-items:center;gap:10px;flex-wrap:wrap;color:var(--muted);font-size:13px}",
  ".status{display:inline-block;white-space:nowrap;border-radius:999px;padding:6px 10px;font-size:12px;letter-spacing:.04em}",
  ".status.pass{background:var(--ok-soft);color:var(--ok)}",
  ".status.normalized{background:var(--normalized-soft);color:var(--normalized)}",
  ".stage-scroller{width:100%;overflow-x:auto;padding:1px 1px 10px}",
  ".stages{display:grid;grid-template-columns:repeat(3,minmax(460px,1fr));gap:16px;min-width:1420px}",
  ".stage{background:#fff;border:1px solid var(--line);border-top:6px solid var(--gnu);border-radius:12px;padding:12px;min-width:0;overflow:hidden}",
  ".stage.kind-xgid{border-top-color:var(--xgid)}",
  ".stage-heading{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;padding:2px 2px 10px;min-height:58px}",
  ".stage-title{min-width:0}",
  ".stage-heading h3{font-size:20px;line-height:1.1;margin:2px 0 0}",
  ".kind-badge{flex:0 0 auto;border-radius:999px;padding:5px 9px;font-size:11px;font-weight:800;letter-spacing:.05em}",
  ".kind-xgid .kind-badge{background:var(--xgid-soft);color:var(--xgid)}",
  ".kind-gnuid .kind-badge{background:var(--gnu-soft);color:var(--gnu)}",
  ".board-frame{background:var(--cream);border:1px solid var(--line);border-radius:9px;padding:7px;overflow:hidden}",
  ".stage svg{display:block;width:100%;height:auto;max-width:100%}",
  ".identifier{margin-top:10px;background:#f7f5f0;border-radius:8px;padding:10px 11px;min-height:66px;overflow:hidden}",
  ".identifier-label{display:block;text-transform:uppercase;letter-spacing:.1em;color:var(--muted);font-size:10px;font-weight:800;margin-bottom:4px}",
  "code{font-family:'SFMono-Regular',Consolas,'Liberation Mono',monospace}",
  ".identifier code{display:block;font-size:12px;line-height:1.45;color:#26342f;white-space:normal;overflow-wrap:anywhere;word-break:break-word}",
  ".footer-note{color:var(--muted);font-size:13px;margin:16px 3px 0}",
  "@media(max-width:900px){main{padding:18px 12px 40px}.hero{padding:22px 20px}.trip{padding:16px}.stages{display:block;min-width:0}.stage{margin-bottom:16px}.stage-scroller{overflow:visible}}",
  "</style></head><body><main>",
  "<section class='hero'>",
  "<h1>GNUID ↔ XGID round-trip review</h1>",
  "<p><strong>Visual gate:</strong> all three boards in each round trip should preserve the same renderable factual state. GNU identifiers can normalize information during conversion, so a changed identifier is not automatically a failure.</p>",
  "<p>Boards use the released <strong>Backgammon Simplified (BS) color/style presets</strong>, <code>perspective = \"player_1\"</code> with Homey near, and <code>mirror_horizontal = FALSE</code>. The display is held constant so conversion differences cannot hide behind viewpoint changes.</p>",
  "<div class='legend'>",
  "<span><i class='dot homey'></i>BS / Homey near</span>",
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
