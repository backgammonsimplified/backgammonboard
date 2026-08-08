devtools::load_all(reset = TRUE)

fixture_path <- file.path("inst", "fixtures", "xgid-factual-fixtures.csv")
fixtures <- utils::read.csv(
  fixture_path,
  stringsAsFactors = FALSE,
  na.strings = c("")
)

xgid_for <- function(fixture_id) {
  value <- fixtures$xgid[fixtures$fixture_id == fixture_id]
  if (length(value) != 1L) {
    stop("Expected exactly one XGID fixture: ", fixture_id, call. = FALSE)
  }
  value
}

specs <- list(
  `01-opening-white-on-roll` = list(id = "opening_white_roll"),
  `02-opening-black-on-roll` = list(id = "opening_black_roll"),
  `03-asymmetric-white-on-roll` = list(id = "asymmetric_white_roll"),
  `04-asymmetric-black-on-roll` = list(id = "asymmetric_black_roll"),
  `05-white-bar` = list(id = "white_bar"),
  `06-black-bar` = list(id = "black_bar"),
  `07-borne-off` = list(id = "borne_off"),
  `08-centered-cube` = list(id = "centered_cube"),
  `09-white-owned-cube` = list(id = "white_owned_cube"),
  `10-black-owned-cube` = list(id = "black_owned_cube"),
  `11-offered-to-white` = list(id = "offer_to_white", perspective = "white"),
  `12-offered-to-black` = list(id = "offer_to_black", perspective = "white"),
  `13-ordinary-match` = list(id = "ordinary_match", score_format = "both"),
  `14-crawford` = list(id = "crawford", score_format = "both"),
  `15-cube-64` = list(id = "cube_64"),
  `16-white-perspective` = list(id = "asymmetric_white_roll", perspective = "white"),
  `17-black-perspective` = list(id = "asymmetric_white_roll", perspective = "black")
)

output_directory <- file.path(
  "dev", "preview-output", "xgid-render-fixtures"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

colors <- board_colors("bs")
style <- board_style("bs")
manifest <- vector("list", length(specs))

for (index in seq_along(specs)) {
  name <- names(specs)[[index]]
  spec <- specs[[index]]
  xgid <- xgid_for(spec$id)
  perspective <- if (is.null(spec$perspective)) "decision_maker" else spec$perspective
  score_format <- if (is.null(spec$score_format)) "away" else spec$score_format

  plot <- ggboard(
    x = xgid,
    colors = colors,
    style = style,
    decision = "none",
    perspective = perspective,
    score_format = score_format,
    brand_text = NULL
  )

  output_file <- file.path(output_directory, paste0(name, ".png"))
  ggplot2::ggsave(
    filename = output_file,
    plot = plot,
    width = 12,
    height = 9.1,
    units = "in",
    dpi = 200,
    bg = colors$outside_fill
  )

  position <- attr(plot, "backgammon_position")
  cube <- attr(plot, "backgammon_cube_display")

  manifest[[index]] <- data.frame(
    fixture = name,
    source_fixture = spec$id,
    output = normalizePath(output_file, winslash = "/", mustWork = TRUE),
    perspective = attr(plot, "backgammon_perspective"),
    on_roll = position$on_roll,
    action_marker = position$action_marker,
    cube_state = cube$state,
    cube_value = cube$value,
    cube_owner = if (is.null(cube$owner)) "" else cube$owner,
    cube_receiver = if (is.null(cube$receiver)) "" else cube$receiver,
    cube_placement = cube$placement,
    is_crawford = position$is_crawford,
    stringsAsFactors = FALSE
  )

  message(name, " -> ", normalizePath(output_file, winslash = "/"))
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
