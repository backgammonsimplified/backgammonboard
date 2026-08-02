# Development helper for rendering a named collection of complete XGIDs.
#
# This is intentionally a development utility rather than a public package
# export. Load the package first, then source this file.

render_xgid_batch <- function(
    xgids,
    output_dir = "dev/preview-output/xgid-batch",
    colors = board_colors("bms"),
    style = board_style("bms"),
    brand_text = "Backgammon\nSimplified",
    width = 12,
    height = 8,
    dpi = 160
) {
  cases <- normalize_xgid_batch_input(xgids)

  if (!inherits(colors, "backgammon_board_colors")) {
    stop("`colors` must be created by board_colors().", call. = FALSE)
  }

  if (!inherits(style, "backgammon_board_style")) {
    stop("`style` must be created by board_style().", call. = FALSE)
  }

  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` must be a non-empty character scalar.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  manifest_rows <- vector("list", nrow(cases))

  for (i in seq_len(nrow(cases))) {
    case <- cases[i, , drop = FALSE]
    slug <- make_xgid_batch_slug(case$name[[1L]], i)
    filename <- paste0(slug, ".png")
    filepath <- file.path(output_dir, filename)

    rendered <- tryCatch(
      {
        plot <- ggboard(
          x = case$xgid[[1L]],
          colors = colors,
          style = style,
          decision = case$decision[[1L]],
          perspective = case$perspective[[1L]],
          score_format = case$score_format[[1L]],
          brand_text = brand_text
        )

        ggplot2::ggsave(
          filename = filepath,
          plot = plot,
          width = width,
          height = height,
          units = "in",
          dpi = dpi
        )

        position <- attr(plot, "backgammon_position")
        cube_display <- attr(plot, "backgammon_cube_display")

        list(
          valid = TRUE,
          error = "",
          png = filename,
          canonical_xgid = position$xgid,
          on_roll = position$on_roll,
          cube_state = cube_display$state,
          cube_value = cube_display$value,
          cube_owner = if (is.null(cube_display$owner)) "" else cube_display$owner,
          receiver = if (is.null(cube_display$receiver)) "" else cube_display$receiver,
          resolved_perspective = attr(plot, "backgammon_perspective")
        )
      },
      error = function(error) {
        list(
          valid = FALSE,
          error = conditionMessage(error),
          png = "",
          canonical_xgid = "",
          on_roll = "",
          cube_state = "",
          cube_value = NA_integer_,
          cube_owner = "",
          receiver = "",
          resolved_perspective = ""
        )
      }
    )

    manifest_rows[[i]] <- data.frame(
      name = case$name[[1L]],
      xgid = case$xgid[[1L]],
      valid = rendered$valid,
      error = rendered$error,
      png = rendered$png,
      canonical_xgid = rendered$canonical_xgid,
      on_roll = rendered$on_roll,
      cube_state = rendered$cube_state,
      cube_value = rendered$cube_value,
      cube_owner = rendered$cube_owner,
      receiver = rendered$receiver,
      requested_perspective = case$perspective[[1L]],
      resolved_perspective = rendered$resolved_perspective,
      decision = case$decision[[1L]],
      score_format = case$score_format[[1L]],
      stringsAsFactors = FALSE
    )
  }

  manifest <- do.call(rbind, manifest_rows)
  manifest_path <- file.path(output_dir, "manifest.csv")
  html_path <- file.path(output_dir, "index.html")

  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  write_xgid_batch_gallery(manifest, html_path)

  message("Rendered XGID batch to: ", normalizePath(output_dir, winslash = "/"))
  message("Manifest: ", normalizePath(manifest_path, winslash = "/"))
  message("Gallery: ", normalizePath(html_path, winslash = "/"))

  invisible(manifest)
}


normalize_xgid_batch_input <- function(xgids) {
  if (is.character(xgids)) {
    if (length(xgids) == 0L) {
      stop("`xgids` must contain at least one XGID.", call. = FALSE)
    }

    case_names <- names(xgids)
    if (is.null(case_names)) {
      case_names <- rep("", length(xgids))
    }

    missing_names <- is.na(case_names) | !nzchar(trimws(case_names))
    case_names[missing_names] <- sprintf(
      "position-%03d",
      which(missing_names)
    )

    cases <- data.frame(
      name = case_names,
      xgid = unname(xgids),
      perspective = "decision_maker",
      decision = "auto",
      score_format = "away",
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(xgids)) {
    if (!"xgid" %in% names(xgids)) {
      stop("A batch data frame must contain an `xgid` column.", call. = FALSE)
    }

    cases <- xgids
    row_count <- nrow(cases)

    if (!"name" %in% names(cases)) {
      cases$name <- sprintf("position-%03d", seq_len(row_count))
    }
    if (!"perspective" %in% names(cases)) {
      cases$perspective <- "decision_maker"
    }
    if (!"decision" %in% names(cases)) {
      cases$decision <- "auto"
    }
    if (!"score_format" %in% names(cases)) {
      cases$score_format <- "away"
    }

    cases <- cases[
      ,
      c("name", "xgid", "perspective", "decision", "score_format"),
      drop = FALSE
    ]
  } else {
    stop(
      "`xgids` must be a character vector or data frame.",
      call. = FALSE
    )
  }

  character_columns <- c(
    "name",
    "xgid",
    "perspective",
    "decision",
    "score_format"
  )

  for (column in character_columns) {
    if (!is.character(cases[[column]])) {
      cases[[column]] <- as.character(cases[[column]])
    }

    if (anyNA(cases[[column]])) {
      stop(
        paste0("Batch column `", column, "` must not contain missing values."),
        call. = FALSE
      )
    }
  }

  if (any(!nzchar(trimws(cases$xgid)))) {
    stop("Batch XGIDs must not be empty.", call. = FALSE)
  }

  cases
}


make_xgid_batch_slug <- function(name, index) {
  slug <- tolower(trimws(name))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)

  if (!nzchar(slug)) {
    slug <- sprintf("position-%03d", index)
  }

  sprintf("%02d-%s", index, slug)
}


escape_xgid_batch_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}


write_xgid_batch_gallery <- function(manifest, path) {
  cards <- character(nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, , drop = FALSE]
    name <- escape_xgid_batch_html(row$name[[1L]])
    xgid <- escape_xgid_batch_html(row$xgid[[1L]])

    if (isTRUE(row$valid[[1L]])) {
      image <- paste0(
        '<img src="',
        escape_xgid_batch_html(row$png[[1L]]),
        '" alt="Rendered backgammon board for ',
        name,
        '">'
      )

      details <- paste0(
        "<p><strong>On roll:</strong> ",
        escape_xgid_batch_html(row$on_roll[[1L]]),
        " &nbsp; <strong>Cube:</strong> ",
        escape_xgid_batch_html(row$cube_state[[1L]]),
        if (!is.na(row$cube_value[[1L]])) {
          paste0(" ", row$cube_value[[1L]])
        } else {
          ""
        },
        " &nbsp; <strong>Perspective:</strong> ",
        escape_xgid_batch_html(row$resolved_perspective[[1L]]),
        "</p>"
      )
    } else {
      image <- ""
      details <- paste0(
        '<p class="error"><strong>Render failed:</strong> ',
        escape_xgid_batch_html(row$error[[1L]]),
        "</p>"
      )
    }

    cards[[i]] <- paste0(
      '<article class="card">',
      "<h2>", name, "</h2>",
      '<code class="xgid">', xgid, "</code>",
      details,
      image,
      "</article>"
    )
  }

  html <- c(
    "<!doctype html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>Backgammon XGID Gallery</title>",
    "<style>",
    "body{margin:0;padding:24px;font-family:Arial,sans-serif;background:#f4f1eb;color:#111b35}",
    "main{max-width:1500px;margin:0 auto}",
    ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:24px}",
    ".card{background:white;border:1px solid #d8c5a5;border-radius:8px;padding:16px}",
    ".card h2{margin:0 0 8px}",
    ".xgid{display:block;overflow-wrap:anywhere;margin-bottom:12px;font-size:12px}",
    ".card img{display:block;width:100%;height:auto;margin-top:12px}",
    ".error{color:#9b1c1c}",
    "</style>",
    "</head>",
    "<body>",
    "<main>",
    "<h1>Backgammon XGID Gallery</h1>",
    '<div class="grid">',
    cards,
    "</div>",
    "</main>",
    "</body>",
    "</html>"
  )

  writeLines(html, path, useBytes = TRUE)
}
