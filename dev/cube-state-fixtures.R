devtools::load_all(reset = TRUE)

payload <- "-b----E-C---eE---c-e----B-"

fixture_specs <- list(
  unlimited_centered = list(
    xgid = paste0("XGID=", payload, ":0:0:1:00:0:0:0:0:10"),
    context = NULL,
    offer = NULL
  ),
  unlimited_white_owned = list(
    xgid = paste0("XGID=", payload, ":1:1:1:00:0:0:0:0:10"),
    context = NULL,
    offer = NULL
  ),
  unlimited_black_owned = list(
    xgid = paste0("XGID=", payload, ":1:-1:1:00:0:0:0:0:10"),
    context = NULL,
    offer = NULL
  ),
  ordinary_match_centered = list(
    xgid = paste0("XGID=", payload, ":0:0:1:00:2:2:0:7:10"),
    context = NULL,
    offer = NULL
  ),
  ordinary_match_owned = list(
    xgid = paste0("XGID=", payload, ":2:1:1:00:2:2:0:7:10"),
    context = NULL,
    offer = NULL
  ),
  crawford_hidden = list(
    xgid = paste0("XGID=", payload, ":0:0:1:00:6:2:1:7:10"),
    context = NULL,
    offer = NULL
  ),
  explicit_roll_double = list(
    xgid = paste0("XGID=", payload, ":0:0:1:00:0:0:0:0:10"),
    context = board_context("cube_offer"),
    offer = NULL
  ),
  explicit_roll_redouble = list(
    xgid = paste0("XGID=", payload, ":1:1:1:00:0:0:0:0:10"),
    context = board_context("cube_offer"),
    offer = NULL
  ),
  explicit_take_pass = list(
    xgid = paste0("XGID=", payload, ":1:-1:-1:00:0:0:0:0:10"),
    context = board_context(
      "cube_response",
      offer = cube_offer_context(
        offerer = "black",
        receiver = "white",
        current_value = 2L,
        offered_value = 4L
      )
    ),
    offer = NULL
  ),
  offer_32_to_64 = list(
    xgid = paste0("XGID=", payload, ":5:-1:-1:00:0:0:0:0:10"),
    context = board_context(
      "cube_response",
      offer = cube_offer_context(
        offerer = "black",
        receiver = "white",
        current_value = 32L,
        offered_value = 64L
      )
    ),
    offer = NULL
  ),
  five_away_owns_8_no_further_offer = list(
    xgid = paste0("XGID=", payload, ":3:1:1:00:2:2:0:7:10"),
    context = NULL,
    offer = NULL
  )
)

output_directory <- file.path("dev", "preview-output", "cube-state-fixtures")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

manifest <- vector("list", length(fixture_specs))

for (index in seq_along(fixture_specs)) {
  fixture_id <- names(fixture_specs)[[index]]
  spec <- fixture_specs[[index]]
  position <- backgammon_position(spec$xgid)

  resolved_offer <- if (!is.null(spec$offer)) {
    spec$offer
  } else if (!is.null(spec$context)) {
    spec$context$offer
  } else {
    NULL
  }

  display <- resolve_cube_display(position, offer = resolved_offer)
  status <- position_status_label(position, context = spec$context)

  plot <- render_board_preview(
    x = position,
    brand_text = NULL,
    context = spec$context,
    cube_offer = spec$offer,
    show_information = TRUE
  )

  output_file <- file.path(output_directory, paste0(fixture_id, ".png"))

  ggplot2::ggsave(
    filename = output_file,
    plot = plot,
    width = 12,
    height = 9.1,
    units = "in",
    dpi = 200,
    bg = board_colors("bms")$outside_fill
  )

  manifest[[index]] <- data.frame(
    fixture_id = fixture_id,
    cube_state = display$state,
    cube_value = display$value,
    cube_visible = display$visible,
    status = status,
    output = normalizePath(output_file, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )

  cat(fixture_id, " -> ", manifest[[index]]$output, "\n", sep = "")
}

manifest <- do.call(rbind, manifest)
manifest_file <- file.path(output_directory, "manifest.csv")
utils::write.csv(manifest, manifest_file, row.names = FALSE, na = "")

cat(
  "\nFixture manifest:\n",
  normalizePath(manifest_file, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)
