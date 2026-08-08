# Render the contract-v1.2 GNU-oriented semantic and transform review gallery.
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

gallery_scenario <- function(id, title, xgid, decision, moves, inspect,
                             include_alternate = FALSE,
                             include_dark_bottom = FALSE,
                             score_review = FALSE) {
  list(
    id = id,
    title = title,
    xgid = xgid,
    decision = decision,
    moves = moves,
    inspect = inspect,
    include_alternate = include_alternate,
    include_dark_bottom = include_dark_bottom,
    score_review = score_review
  )
}

opening <- "-b----E-C---eE---c-e----B-"
asymmetric <- "-FDaA--------------a-Acbb-"
both_bars <- make_payload(c(`1` = "a", `26` = "A"))
bearing <- make_payload(c(`3` = "A"))

scenarios <- list(
  gallery_scenario(
    "case-18", "Mandatory case 18",
    "XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8",
    "none", NULL,
    "Confirm Foey is on roll, Homey is near by default, no dice, centered 1-cube, and no offer.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "asymmetric-checkers", "Asymmetric player-owned checkers",
    complete_xgid(asymmetric, owner = -1, turn = 1, dice = "42"),
    "checker_play", NULL,
    "Trace lowercase checkers to Foey and uppercase checkers to Homey on fixed point IDs.",
    include_alternate = TRUE,
    include_dark_bottom = TRUE
  ),
  gallery_scenario(
    "bars", "Both factual bars", complete_xgid(both_bars),
    "none", NULL,
    "Confirm character 0 belongs to Foey's bar and character 25 to Homey's bar.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "borne-off", "Borne-off counts", complete_xgid("--------------------------"),
    "none", NULL,
    "Confirm both off markers retain player styling and follow the shared transforms."
  ),
  gallery_scenario(
    "player-0-dice", "Foey on roll with dice",
    complete_xgid(opening, turn = -1, dice = "31"),
    "checker_play", NULL,
    "Confirm dice and on-roll status remain attached to player_0/Foey.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "player-1-dice", "Homey on roll with dice",
    complete_xgid(opening, turn = 1, dice = "42"),
    "checker_play", NULL,
    "Confirm dice and on-roll status remain attached to player_1/Homey."
  ),
  gallery_scenario(
    "player-0-cube", "Foey owns cube",
    complete_xgid(opening, cube = 2, owner = -1, turn = -1),
    "none", NULL,
    "Confirm Foey owns the 4-cube and it follows Foey under display transforms.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "player-1-cube", "Homey owns cube",
    complete_xgid(opening, cube = 2, owner = 1, turn = 1),
    "none", NULL,
    "Confirm Homey owns the 4-cube and it follows Homey under horizontal mirroring."
  ),
  gallery_scenario(
    "player-0-offer", "Pending offer by Foey",
    complete_xgid(opening, cube = 1, owner = -1, turn = -1, dice = "D"),
    "take_pass", NULL,
    "Confirm the offered 4-cube belongs to Foey and normal cube/dice are hidden.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "player-1-offer", "Pending offer by Homey",
    complete_xgid(opening, cube = 1, owner = 1, turn = 1, dice = "D"),
    "take_pass", NULL,
    "Confirm the offered 4-cube belongs to Homey and normal cube/dice are hidden."
  ),
  gallery_scenario(
    "score-7-4-2", "Ordinary 7-point match: Homey 4, Foey 2",
    complete_xgid(opening, first_score = 4, second_score = 2, match_length = 7),
    "none", NULL,
    "Verify both XGID score fields, factual player attachment, away calculation, and displayed match text.",
    include_alternate = TRUE, score_review = TRUE
  ),
  gallery_scenario(
    "score-7-3-0", "Ordinary 7-point match: Homey 3, Foey 0",
    complete_xgid(asymmetric, first_score = 3, second_score = 0, match_length = 7),
    "none", NULL,
    "Verify a second asymmetric ordinary score against GNU.",
    score_review = TRUE
  ),
  gallery_scenario(
    "score-9-6-2", "Ordinary 9-point match: Homey 6, Foey 2",
    complete_xgid(opening, first_score = 6, second_score = 2, match_length = 9),
    "none", NULL,
    "Verify score decoding and away calculation at a different match length.",
    score_review = TRUE
  ),
  gallery_scenario(
    "score-7-crawford", "Crawford 7-point match: Homey 2, Foey 6",
    complete_xgid(opening, first_score = 2, second_score = 6,
                  crawford = 1, match_length = 7),
    "none", NULL,
    "Verify explicit Crawford input, score decoding, cube suppression, and displayed match text.",
    score_review = TRUE
  ),
  gallery_scenario(
    "checker-movement", "Checker movement",
    complete_xgid(opening, turn = 1, dice = "31"),
    "checker_play", board_moves(c(13, 6), c(10, 5), die = c(3, 1)),
    "Confirm both dice are represented by straight arrows and destination ghosts while the starting checkers remain in place.",
    include_alternate = TRUE
  ),
  gallery_scenario(
    "bar-entry", "Bar entry",
    complete_xgid("-b----E-C---eE---c-e----AA", turn = 1, dice = "11"),
    "checker_play", board_moves("bar", 24, die = 1, label = "bar/24"),
    "Confirm Homey's bar-entry arrow and ghost follow horizontal mirroring."
  ),
  gallery_scenario(
    "repeated-movement", "Repeated movement",
    complete_xgid(opening, turn = 1, dice = "55"),
    "checker_play",
    board_moves(c(13, 13), c(8, 8), die = c(5, 5),
                label = c("first", "second")),
    "Confirm repeated arrows retain routing, parallel curvature, ghosts, and upright labels."
  ),
  gallery_scenario(
    "bearing-off", "Bearing off",
    complete_xgid(bearing, turn = 1, dice = "22"),
    "checker_play", board_moves(2, "off", die = 2, label = "2/off"),
    "Confirm the bearing-off arrow and destination ghost follow horizontal mirroring."
  )
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

scenario_states <- function(include_alternate, include_dark_bottom) {
  normal <- data.frame(
    setup = c("right", "left"),
    near_player = "player_1",
    perspective = "player_1",
    mirror_horizontal = c(FALSE, TRUE),
    light_player = "near_player",
    alternate = FALSE,
    dark_bottom = FALSE,
    stringsAsFactors = FALSE
  )
  result <- normal
  if (isTRUE(include_alternate)) {
    result <- rbind(result, data.frame(
      setup = c("right", "left"),
      near_player = "player_0",
      perspective = "player_0",
      mirror_horizontal = c(FALSE, TRUE),
      light_player = "near_player",
      alternate = TRUE,
      dark_bottom = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  if (isTRUE(include_dark_bottom)) {
    result <- rbind(result, data.frame(
      setup = "right",
      near_player = "player_1",
      perspective = "player_1",
      mirror_horizontal = FALSE,
      light_player = "player_0",
      alternate = FALSE,
      dark_bottom = TRUE,
      stringsAsFactors = FALSE
    ))
  }
  result[order(
    match(result$setup, c("right", "left")),
    result$dark_bottom,
    result$alternate
  ), , drop = FALSE]
}

xgid_score_fields <- function(xgid) {
  fields <- strsplit(sub("^XGID=", "", xgid), ":", fixed = TRUE)[[1L]]
  c(first = as.integer(fields[[6L]]), second = as.integer(fields[[7L]]))
}

cube_fact_label <- function(position) {
  if (identical(position$cube_action, "double")) {
    return(paste0(
      "offerer=", position$on_roll,
      "; offered value=", 2L * position$cube_value
    ))
  }
  paste0("owner=", position$cube_owner, "; value=", position$cube_value)
}

score_fact_label <- function(position) {
  context <- if (identical(position$play_context, "match")) {
    paste0("; match length=", position$match_length)
  } else {
    "; unlimited game win fields"
  }
  paste0(
    "player_1/Homey=", position$score[["player_1"]],
    "; player_0/Foey=", position$score[["player_0"]], context
  )
}

displayed_match_text <- function(plot, position) {
  if (!identical(position$play_context, "match")) return("Not applicable")
  information <- attr(plot, "backgammon_prepared_layout")$information
  rows <- rbind(information$top, information$bottom)
  players <- vapply(
    rows$player,
    backgammonboard:::render_player_to_project_player,
    character(1)
  )
  labels <- c(player_0 = "Foey", player_1 = "Homey")
  values <- paste0(labels[players], ": ", rows$secondary)
  paste(values[order(players)], collapse = "; ")
}

viewpoint_note <- function(near_player, mirror_horizontal, dark_bottom = FALSE) {
  if (isTRUE(dark_bottom)) {
    return(paste0(
      "Expected to differ from GNU only in display palette assignment: ",
      "Homey/player_1 remains near/bottom but uses the dark checker palette. ",
      "The factual position and viewpoint are unchanged."
    ))
  }
  if (identical(near_player, "player_1") && !isTRUE(mirror_horizontal)) {
    return(paste0(
      "Default control state: this should align directly with the source/GNU ",
      "orientation; the factual position is unchanged."
    ))
  }
  transforms <- c(
    if (identical(near_player, "player_0")) "Foey/player_0-near vertical transform",
    if (isTRUE(mirror_horizontal)) "horizontal mirror"
  )
  paste0(
    "Expected to look different from GNU because of the ",
    paste(transforms, collapse = " and "),
    ". This is display-only; the factual position is unchanged."
  )
}

make_plot <- function(scenario, perspective, mirror_horizontal, colors, style,
                      light_player = "near_player") {
  plot <- ggboard(
    scenario$xgid,
    colors = colors,
    style = style,
    moves = scenario$moves,
    decision = scenario$decision,
    perspective = perspective,
    mirror_horizontal = mirror_horizontal,
    light_player = light_player,
    score_format = "both",
    player_name_style = "checker"
  )
  brand_text <- "Backgammon\nSimplified"
  backgammonboard:::add_board_brand(
    plot,
    attr(plot, "backgammon_prepared_layout")$geometry,
    brand_text,
    side = attr(plot, "backgammon_brand_side"),
    color = colors$brand_text,
    family = "Source Sans 3",
    fontface = "bold"
  )
}

repository_commit <- trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
colors <- board_colors("bs")
style <- board_style("bs")
manifest <- list()
scenario_cards <- list()
source_files <- character()
changed_files <- character()
row_index <- 1L

for (scenario_index in seq_along(scenarios)) {
  scenario <- scenarios[[scenario_index]]
  source_plot <- make_plot(scenario, "player_1", FALSE, colors, style)
  position <- attr(source_plot, "backgammon_position")
  source_context <- attr(source_plot, "backgammon_context")
  source_file <- paste0("source-", scenario$id, ".svg")
  render_svg(source_plot, file.path(staging, source_file), colors$outside_fill)
  source_files <- c(source_files, source_file)

  score_fields <- xgid_score_fields(position$xgid)
  match_text <- displayed_match_text(source_plot, position)
  states <- scenario_states(
    scenario$include_alternate,
    scenario$include_dark_bottom
  )
  comparison_rows <- list()

  for (state_index in seq_len(nrow(states))) {
    state <- states[state_index, , drop = FALSE]
    changed_plot <- make_plot(
      scenario,
      state$perspective[[1L]],
      state$mirror_horizontal[[1L]],
      colors,
      style,
      state$light_player[[1L]]
    )
    resolved_near <- attr(changed_plot, "backgammon_near_player")
    changed_file <- paste0(
      "view-", scenario$id, "-", state$setup[[1L]], "-",
      if (state$dark_bottom[[1L]]) {
        "homey-near-dark"
      } else if (state$alternate[[1L]]) {
        "foey-near"
      } else {
        "homey-near"
      }, ".svg"
    )
    render_svg(changed_plot, file.path(staging, changed_file), colors$outside_fill)
    changed_files <- c(changed_files, changed_file)
    note <- viewpoint_note(
      resolved_near,
      state$mirror_horizontal[[1L]],
      state$dark_bottom[[1L]]
    )
    alternate_label <- if (state$dark_bottom[[1L]]) {
      "Palette demonstration: Homey / player_1 near and dark"
    } else if (state$alternate[[1L]]) {
      "Alternate view: Foey / player_0 near"
    } else {
      "Normal default: Homey / player_1 near"
    }
    setup_code <- if (identical(state$setup[[1L]], "right")) "R" else "L"
    view_code <- if (state$dark_bottom[[1L]]) {
      "D"
    } else if (state$alternate[[1L]]) {
      "A"
    } else {
      "N"
    }
    source_panel_id <- paste(
      sprintf("%02d", scenario_index), setup_code, view_code, "GNU", sep = "-"
    )
    changed_panel_id <- paste(
      sprintf("%02d", scenario_index), setup_code, view_code, "VIEW", sep = "-"
    )

    manifest[[row_index]] <- data.frame(
      comparison_id = sprintf("%03d", row_index),
      scenario_order = scenario_index,
      scenario = scenario$id,
      title = scenario$title,
      xgid = position$xgid,
      setup = state$setup[[1L]],
      source_panel_id = source_panel_id,
      changed_panel_id = changed_panel_id,
      source_output_path = source_file,
      changed_output_path = changed_file,
      xgid_top_player = "player_0/Foey",
      xgid_bottom_player = "player_1/Homey",
      on_roll_player = position$on_roll,
      cube_owner_or_offerer = cube_fact_label(position),
      score = score_fact_label(position),
      decision = source_context$decision,
      near_player = resolved_near,
      mirror_horizontal = state$mirror_horizontal[[1L]],
      perspective = state$perspective[[1L]],
      light_player = state$light_player[[1L]],
      alternate_view = state$alternate[[1L]],
      dark_bottom_demo = state$dark_bottom[[1L]],
      view_label = alternate_label,
      transform_note = note,
      first_xgid_score_field = score_fields[["first"]],
      second_xgid_score_field = score_fields[["second"]],
      decoded_player_1_score = position$score[["player_1"]],
      decoded_player_0_score = position$score[["player_0"]],
      displayed_match_text = match_text,
      inspection_instruction = scenario$inspect,
      repository_commit = repository_commit,
      stringsAsFactors = FALSE
    )

    comparison_rows[[state_index]] <- paste0(
      '<div class="comparison-row ', state$setup[[1L]], '">',
      '<figure><p class="panel-id">', escape_html(source_panel_id), '</p>',
      '<figcaption>Default XGID / GNU comparison</figcaption>',
      '<img src="', escape_html(source_file), '" alt="Default XGID source rendering for ',
      escape_html(scenario$title), '">',
      '<p class="controls"><code>near_player=player_1</code> · ',
      '<code>perspective=player_1</code> · <code>mirror_horizontal=false</code></p>',
      '</figure><figure><p class="panel-id">', escape_html(changed_panel_id), '</p>',
      '<figcaption>Changed viewpoint</figcaption>',
      '<img src="', escape_html(changed_file), '" alt="Changed viewpoint rendering for ',
      escape_html(scenario$title), '">',
      '<p class="view-label',
      if (state$alternate[[1L]]) " alternate" else "",
      if (state$dark_bottom[[1L]]) " palette-demo" else "", '">',
      escape_html(alternate_label), '</p>',
      '<p class="controls"><code>near_player=', escape_html(resolved_near),
      '</code> · <code>perspective=', escape_html(state$perspective[[1L]]),
      '</code> · <code>mirror_horizontal=',
      tolower(as.character(state$mirror_horizontal[[1L]])),
      '</code> · <code>light_player=',
      escape_html(state$light_player[[1L]]), '</code></p>',
      '<p class="transform-note">', escape_html(note), '</p>',
      '</figure></div>'
    )
    row_index <- row_index + 1L
  }

  right_rows <- comparison_rows[states$setup == "right"]
  left_rows <- comparison_rows[states$setup == "left"]
  score_details <- if (scenario$score_review) {
    paste0(
      '<div class="score-review"><h3>GNU score decoding check</h3><dl>',
      '<dt>First XGID score field</dt><dd>', score_fields[["first"]], '</dd>',
      '<dt>Second XGID score field</dt><dd>', score_fields[["second"]], '</dd>',
      '<dt>Decoded player_1 / Homey score</dt><dd>', position$score[["player_1"]], '</dd>',
      '<dt>Decoded player_0 / Foey score</dt><dd>', position$score[["player_0"]], '</dd>',
      '<dt>Displayed match text</dt><dd>', escape_html(match_text), '</dd>',
      '</dl></div>'
    )
  } else {
    ""
  }

  scenario_cards[[scenario_index]] <- paste0(
    '<article class="scenario"><h2>', sprintf("%02d", scenario_index), " — ",
    escape_html(scenario$title), '</h2><p class="xgid"><code>',
    escape_html(position$xgid), '</code></p>',
    '<section class="facts"><h3>Factual summary</h3><dl>',
    '<dt>XGID top player</dt><dd>player_0 / Foey</dd>',
    '<dt>XGID bottom player</dt><dd>player_1 / Homey</dd>',
    '<dt>On-roll player</dt><dd>', escape_html(position$on_roll), '</dd>',
    '<dt>Cube owner or offerer</dt><dd>', escape_html(cube_fact_label(position)), '</dd>',
    '<dt>Score</dt><dd>', escape_html(score_fact_label(position)), '</dd>',
    '<dt>Decision</dt><dd>', escape_html(source_context$decision), '</dd>',
    '<dt>Inspection instruction</dt><dd>', escape_html(scenario$inspect), '</dd>',
    '</dl></section>', score_details,
    '<section class="setup"><h3>Right-side setup comparisons</h3>',
    paste0(unlist(right_rows, use.names = FALSE), collapse = "\n"), '</section>',
    '<section class="setup"><h3>Left-side setup comparisons</h3>',
    paste0(unlist(left_rows, use.names = FALSE), collapse = "\n"), '</section>',
    '</article>'
  )
}

manifest <- do.call(rbind, manifest)
if (sum(manifest$near_player == "player_1") <= sum(manifest$near_player == "player_0")) {
  stop("Most changed-view comparisons must keep player_1/Homey near.", call. = FALSE)
}
if (!all(manifest$xgid_top_player == "player_0/Foey") ||
    !all(manifest$xgid_bottom_player == "player_1/Homey")) {
  stop("Gallery factual mapping metadata is inconsistent.", call. = FALSE)
}
dark_bottom_rows <- manifest[manifest$dark_bottom_demo, , drop = FALSE]
if (nrow(dark_bottom_rows) != 1L ||
    dark_bottom_rows$near_player[[1L]] != "player_1" ||
    dark_bottom_rows$light_player[[1L]] != "player_0") {
  stop("Gallery must contain exactly one explicit dark-bottom demonstration.", call. = FALSE)
}
score_manifest <- manifest[manifest$scenario %in% vapply(
  scenarios[vapply(scenarios, `[[`, logical(1), "score_review")],
  `[[`, character(1), "id"
), , drop = FALSE]
if (length(unique(score_manifest$scenario)) != 4L ||
    !all(score_manifest$first_xgid_score_field == score_manifest$decoded_player_1_score) ||
    !all(score_manifest$second_xgid_score_field == score_manifest$decoded_player_0_score)) {
  stop("Score-review metadata does not prove the v1.2 score mapping.", call. = FALSE)
}

utils::write.csv(manifest, file.path(staging, "manifest.csv"), row.names = FALSE, na = "")
html <- c(
  '<!doctype html><html><head><meta charset="utf-8"><title>backgammonboard GNU comparison review</title>',
  '<style>body{font:15px system-ui;background:#f4f1e8;color:#172039;margin:0}main{max-width:1500px;margin:auto;padding:24px}.intro,.scenario{background:#fff;border:1px solid #ccd1d8;border-radius:12px;padding:18px;margin:22px 0}.xgid code{display:block;background:#f5f6f8;padding:10px;border-radius:6px;overflow-wrap:anywhere}.facts dl,.score-review dl{display:grid;grid-template-columns:minmax(190px,280px) 1fr;gap:5px 14px}.facts dt,.score-review dt{font-weight:700}.facts dd,.score-review dd{margin:0}.setup{border-top:2px solid #d8dde5;margin-top:20px;padding-top:8px}.comparison-row{display:grid;grid-template-columns:1fr 1fr;gap:18px;margin:14px 0 26px}.comparison-row figure{margin:0;border:1px solid #d8dde5;border-radius:8px;padding:10px;background:#fbfbfc}.panel-id{display:inline-block;margin:0 0 7px;padding:4px 8px;border-radius:999px;background:#172039;color:#fff;font:800 13px ui-monospace,monospace;letter-spacing:.03em}.comparison-row figcaption{font-size:1.05rem;font-weight:800;margin-bottom:8px}.comparison-row img{display:block;width:100%;height:auto}.controls{line-height:1.7}.controls code{background:#eef1f5;padding:2px 4px;border-radius:3px}.view-label{font-weight:800;color:#265c43}.view-label.alternate{color:#9a3f1f}.transform-note{background:#f3f5f8;border-left:4px solid #61728a;padding:8px}.score-review{background:#f7f2df;border:1px solid #dfd19b;border-radius:8px;padding:10px 14px;margin-top:14px}@media(max-width:900px){.comparison-row{grid-template-columns:1fr}.facts dl,.score-review dl{grid-template-columns:1fr}.facts dd,.score-review dd{margin-bottom:6px}}</style>',
  '</head><body><main><section class="intro"><h1>GNU-oriented v1.2 review gallery</h1>',
  '<p>Paste each XGID into GNU once. Start with the left panel, which always uses the canonical default source layout: Homey/player_1 near, no horizontal mirror. Compare the right-side setup rows first, then flip GNU once and continue with the left-side setup rows.</p>',
  '<p>Every right panel lists its exact display controls. A changed appearance caused by <code>near_player</code> or <code>mirror_horizontal</code> is a display transform only and never changes the factual XGID position.</p>',
  '<p>Panel <code>02-R-D-VIEW</code> deliberately keeps Homey/player_1 near while assigning the dark checker palette, demonstrating that near/bottom player and checker color are independent display controls.</p>',
  '<p><strong>Post-Crawford is intentionally omitted:</strong> the supported complete-XGID input distinguishes an explicit Crawford game from <code>crawford_status=none</code>, but does not explicitly distinguish pre-Crawford from post-Crawford without inference.</p>',
  '<p>Generated from repository commit <code>', escape_html(repository_commit), '</code>.</p></section>',
  unlist(scenario_cards, use.names = FALSE), '</main></body></html>'
)
writeLines(html, file.path(staging, "index.html"), useBytes = TRUE)
writeLines(c(
  "# GNU-oriented contract-v1.2 review gallery", "",
  paste0(nrow(manifest), " paired comparison rows generated at repository commit `", repository_commit, "`."),
  "Each XGID is grouped once with right-side setup rows before left-side setup rows.",
  "The source panel is always player_1/Homey near and unmirrored.",
  "Post-Crawford is omitted because supported complete-XGID input does not explicitly distinguish it without inference."
), file.path(staging, "README.md"), useBytes = TRUE)
file.copy(
  file.path("dev", "fonts", "OFL-1.1.txt"),
  file.path(staging, "OFL-SourceCodePro.txt"),
  overwrite = TRUE
)

files <- c(
  unique(source_files), unique(changed_files), "manifest.csv", "index.html",
  "README.md", "OFL-SourceCodePro.txt"
)
copied <- file.copy(file.path(staging, files), file.path(output, files), overwrite = TRUE)
if (!all(copied)) stop("Gallery copy failed.", call. = FALSE)
message(
  "Rendered ", length(scenarios), " grouped XGIDs and ", nrow(manifest),
  " paired comparisons to ", normalizePath(output, winslash = "/")
)
