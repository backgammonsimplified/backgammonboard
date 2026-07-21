# Render the comprehensive Backgammon Made Simple XGID edge-case suite.
#
# Copy both downloaded files into dev/ before running:
#   bms-xgid-edge-cases.csv
#   run-bms-xgid-edge-cases.R
#
# The development batch renderer must already exist at:
#   dev/render-xgid-batch.R

devtools::load_all(reset = TRUE)
source("dev/render-xgid-batch.R")

case_file <- "dev/bms-xgid-edge-cases.csv"
output_dir <- "dev/preview-output/bms-xgid-edge-cases"

cases <- utils::read.csv(
  case_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "category",
  "name",
  "description",
  "xgid",
  "perspective",
  "decision",
  "score_format",
  "expected_on_roll",
  "expected_action_marker",
  "expected_cube_value",
  "expected_cube_owner",
  "expected_bar_white",
  "expected_bar_black",
  "expected_off_white",
  "expected_off_black",
  "expected_match_length",
  "expected_is_crawford"
)

missing_columns <- setdiff(required_columns, names(cases))
if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "Missing required case column(s): ",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

validate_case <- function(row) {
  validation <- validate_xgid(row$xgid[[1L]])

  if (!isTRUE(validation$valid)) {
    errors <- paste(
      paste0(validation$errors$code, ": ", validation$errors$message),
      collapse = " | "
    )

    stop(
      paste0("Invalid fixture `", row$name[[1L]], "`: ", errors),
      call. = FALSE
    )
  }

  position <- backgammon_position(row$xgid[[1L]])

  checks <- c(
    on_roll = identical(
      position$on_roll,
      row$expected_on_roll[[1L]]
    ),
    action_marker = identical(
      position$action_marker,
      row$expected_action_marker[[1L]]
    ),
    cube_value = identical(
      position$cube_value,
      as.integer(row$expected_cube_value[[1L]])
    ),
    cube_owner = identical(
      position$cube_owner,
      row$expected_cube_owner[[1L]]
    ),
    bar_white = identical(
      unname(position$bar[["white"]]),
      as.integer(row$expected_bar_white[[1L]])
    ),
    bar_black = identical(
      unname(position$bar[["black"]]),
      as.integer(row$expected_bar_black[[1L]])
    ),
    off_white = identical(
      unname(position$off[["white"]]),
      as.integer(row$expected_off_white[[1L]])
    ),
    off_black = identical(
      unname(position$off[["black"]]),
      as.integer(row$expected_off_black[[1L]])
    ),
    is_crawford = identical(
      isTRUE(position$is_crawford),
      identical(tolower(row$expected_is_crawford[[1L]]), "true")
    )
  )

  if (!all(checks)) {
    stop(
      paste0(
        "Fixture expectation mismatch for `",
        row$name[[1L]],
        "`: ",
        paste(names(checks)[!checks], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(position)
}

for (i in seq_len(nrow(cases))) {
  validate_case(cases[i, , drop = FALSE])
}

message("Validated ", nrow(cases), " XGID edge cases.")

render_input <- cases[
  ,
  c("name", "xgid", "perspective", "decision", "score_format"),
  drop = FALSE
]

manifest <- render_xgid_batch(
  render_input,
  output_dir = output_dir,
  colors = board_colors("bms"),
  style = board_style("bms"),
  brand_text = "Backgammon\nMade Simple"
)

details <- merge(
  cases[
    ,
    c(
      "category",
      "name",
      "description",
      "expected_bar_white",
      "expected_bar_black",
      "expected_off_white",
      "expected_off_black"
    ),
    drop = FALSE
  ],
  manifest,
  by = "name",
  all.x = TRUE,
  sort = FALSE
)

details <- details[
  match(cases$name, details$name),
  ,
  drop = FALSE
]

utils::write.csv(
  details,
  file.path(output_dir, "detailed-manifest.csv"),
  row.names = FALSE,
  na = ""
)

category_summary <- aggregate(
  valid ~ category,
  data = details,
  FUN = function(x) {
    paste0(sum(x, na.rm = TRUE), "/", length(x))
  }
)

utils::write.csv(
  category_summary,
  file.path(output_dir, "category-summary.csv"),
  row.names = FALSE,
  na = ""
)

failed <- details[is.na(details$valid) | !details$valid, , drop = FALSE]
if (nrow(failed) > 0L) {
  print(failed[, c("category", "name", "error"), drop = FALSE])
  stop(
    paste0(nrow(failed), " fixture(s) failed to render."),
    call. = FALSE
  )
}

message("All ", nrow(details), " fixtures rendered successfully.")
message(
  "Open gallery: ",
  normalizePath(
    file.path(output_dir, "index.html"),
    winslash = "/",
    mustWork = TRUE
  )
)

browseURL(
  normalizePath(
    file.path(output_dir, "index.html"),
    winslash = "/",
    mustWork = TRUE
  )
)

invisible(details)
