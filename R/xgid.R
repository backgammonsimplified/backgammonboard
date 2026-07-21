#' Validate a complete eXtreme Gammon identifier
#'
#' `validate_xgid()` inspects one complete XGID and returns structured
#' diagnostics. Invalid XGID text returns an invalid validation object;
#' programmer misuse, such as passing a vector or unsupported object type,
#' throws an error.
#'
#' @param x A length-one character value containing a complete XGID, with or
#'   without the `XGID=` prefix.
#'
#' @return An object of class `backgammon_xgid_validation`.
#' @export
validate_xgid <- function(x) {
  if (missing(x)) {
    stop("`x` is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("`x` must be a character vector", call. = FALSE)
  }
  if (length(x) != 1L) {
    stop("`x` must contain exactly one XGID", call. = FALSE)
  }

  errors <- list()
  warnings <- list()
  action_marker <- NA_character_

  add_error <- function(code, category, field, message) {
    errors[[length(errors) + 1L]] <<- xgid_diagnostic(
      code = code,
      category = category,
      field = field,
      message = message
    )
  }

  add_warning <- function(code, category, field, message) {
    warnings[[length(warnings) + 1L]] <<- xgid_diagnostic(
      code = code,
      category = category,
      field = field,
      message = message
    )
  }

  invalid_result <- function() {
    new_xgid_validation(
      errors = errors,
      warnings = warnings,
      action_marker = action_marker
    )
  }

  if (is.na(x)) {
    add_error(
      "xgid_missing_value", "text", "xgid",
      "XGID text is missing"
    )
    return(invalid_result())
  }

  text <- trimws(x)
  if (!nzchar(text)) {
    add_error(
      "xgid_missing_value", "text", "xgid",
      "XGID text is empty"
    )
    return(invalid_result())
  }

  if (grepl("[\r\n]", text)) {
    add_error(
      "xgid_multiple_lines", "text", "xgid",
      "A complete XGID must be supplied on one line"
    )
    return(invalid_result())
  }

  if (grepl("^[A-Za-z][A-Za-z0-9_]*=", text) &&
      !startsWith(text, "XGID=")) {
    add_error(
      "xgid_invalid_prefix", "text", "prefix",
      "The only supported identifier prefix is `XGID=`"
    )
    return(invalid_result())
  }

  body <- if (startsWith(text, "XGID=")) {
    substring(text, 6L)
  } else {
    text
  }

  colon_count <- nchar(gsub("[^:]", "", body))
  if (colon_count < 9L) {
    add_error(
      "xgid_missing_fields", "text", "fields",
      "A complete XGID must contain one position field and nine metadata fields"
    )
    return(invalid_result())
  }
  if (colon_count > 9L) {
    add_error(
      "xgid_extra_fields", "text", "fields",
      "A complete XGID must contain exactly ten fields"
    )
    return(invalid_result())
  }

  fields <- strsplit(body, ":", fixed = TRUE)[[1L]]
  if (length(fields) != 10L || any(!nzchar(fields))) {
    add_error(
      "xgid_missing_fields", "text", "fields",
      "A complete XGID must contain ten non-empty fields"
    )
    return(invalid_result())
  }

  names(fields) <- xgid_field_names()
  action_marker <- fields[["dice_action"]]

  payload <- fields[["position"]]
  if (nchar(payload, type = "chars") != 26L) {
    add_error(
      "xgid_invalid_position_length", "position", "position",
      "The XGID position field must contain exactly 26 characters"
    )
  } else if (grepl("[^A-Pa-p-]", payload)) {
    add_error(
      "xgid_invalid_position_character", "position", "position",
      "The position field may contain only `-`, `A` through `P`, and `a` through `p`"
    )
  }

  integer_fields <- c(
    "cube_exponent", "score_white", "score_black", "crawford_jacoby",
    "match_length", "max_cube_exponent"
  )
  for (field in integer_fields) {
    if (!is_unsigned_integer_text(fields[[field]])) {
      add_error(
        paste0("xgid_invalid_", field), "metadata", field,
        paste0("`", field, "` must be a non-negative integer")
      )
    }
  }

  if (!fields[["cube_owner"]] %in% c("-1", "0", "1")) {
    add_error(
      "xgid_invalid_cube_owner", "metadata", "cube_owner",
      "Cube ownership must be -1, 0, or 1"
    )
  }

  if (!fields[["turn"]] %in% c("-1", "1")) {
    add_error(
      "xgid_invalid_turn", "metadata", "turn",
      "Turn must be -1 or 1"
    )
  }

  if (!grepl("^(00|D|B|R|[1-6][1-6])$", fields[["dice_action"]])) {
    add_error(
      "xgid_invalid_dice", "metadata", "dice_action",
      "Dice/action must be `00`, `D`, `B`, `R`, or two dice from 1 through 6"
    )
  } else if (fields[["dice_action"]] %in% c("B", "R")) {
    add_error(
      "unsupported_action_marker", "unsupported_input", "dice_action",
      paste0(
        "Action marker `", fields[["dice_action"]],
        "` is preserved for diagnostics but is not supported in v1"
      )
    )
  }

  if (length(errors) == 0L) {
    parsed <- parse_validated_xgid_fields(fields)

    if (parsed$match_length == 0L) {
      if (!parsed$crawford_jacoby %in% 0:3) {
        add_error(
          "xgid_invalid_unlimited_rules", "metadata", "crawford_jacoby",
          "Unlimited play requires a Crawford/Jacoby field from 0 through 3"
        )
      }
    } else {
      if (!parsed$crawford_jacoby %in% 0:1) {
        add_error(
          "xgid_invalid_crawford", "metadata", "crawford_jacoby",
          "Match play requires a Crawford field of 0 or 1"
        )
      }
      if (parsed$score_white >= parsed$match_length ||
          parsed$score_black >= parsed$match_length) {
        add_error(
          "xgid_invalid_match_score", "factual_state", "score",
          "Both raw match scores must be below the match length"
        )
      }
    }

    if (!is.finite(2^parsed$max_cube_exponent)) {
      add_error(
        "xgid_invalid_max_cube_exponent", "metadata", "max_cube_exponent",
        "The encoded maximum-cube exponent is too large to represent"
      )
    }

    if (parsed$cube_exponent > parsed$max_cube_exponent) {
      add_error(
        "xgid_cube_above_maximum", "factual_state", "cube_exponent",
        "The factual cube value exceeds the encoded maximum cube"
      )
    }

    factual_cube <- 2^parsed$cube_exponent
    if (!is.finite(factual_cube) || factual_cube > supported_cube_max()) {
      add_error(
        "xgid_unsupported_cube_value", "unsupported_input", "cube_exponent",
        paste0(
          "The factual cube value exceeds the package-supported limit of ",
          supported_cube_max(),
          " [package_cube_limit]"
        )
      )
    }

    payload_state <- decode_xgid_payload(payload, parsed$turn_code)
    if (!payload_state$bar_valid) {
      add_error(
        "xgid_invalid_bar_owner", "factual_state", "position",
        "The XGID bar slots contain checkers for the wrong semantic player"
      )
    }
    if (payload_state$white_total > 15L || payload_state$black_total > 15L ||
        any(payload_state$off < 0L)) {
      add_error(
        "xgid_impossible_checker_total", "factual_state", "position",
        "Neither player may have more than 15 checkers on points plus bar"
      )
    }

    if (parsed$match_length > 0L && parsed$crawford_jacoby == 1L) {
      away <- parsed$match_length - c(
        white = parsed$score_white,
        black = parsed$score_black
      )
      if (sum(away == 1L) != 1L) {
        add_error(
          "xgid_invalid_crawford_state", "factual_state", "crawford_jacoby",
          "A Crawford game requires exactly one player to be 1-away"
        )
      }
    }

    if (parsed$match_length == 0L &&
        (parsed$score_white != 0L || parsed$score_black != 0L)) {
      add_warning(
        "xgid_unlimited_score_ignored", "metadata", "score",
        "Unlimited-play score fields are preserved factually but are not displayed as a match score"
      )
    }
  }

  if (length(errors) > 0L) {
    return(invalid_result())
  }

  canonical <- canonicalize_xgid_fields(fields)
  new_xgid_validation(
    valid = TRUE,
    canonical_xgid = canonical,
    errors = errors,
    warnings = warnings,
    action_marker = action_marker
  )
}

#' Normalize a complete eXtreme Gammon identifier
#'
#' `normalize_xgid()` asserts that `x` is a supported complete XGID and returns
#' one canonical string beginning with `XGID=`.
#'
#' @inheritParams validate_xgid
#'
#' @return A length-one character value.
#' @export
normalize_xgid <- function(x) {
  validation <- validate_xgid(x)
  if (!validation$valid) {
    first <- validation$errors[1L, , drop = FALSE]
    stop(
      paste0(first$message, " [", first$code, "]"),
      call. = FALSE
    )
  }
  validation$canonical_xgid
}

#' @export
print.backgammon_xgid_validation <- function(x, ...) {
  if (isTRUE(x$valid)) {
    cat("<backgammon_xgid_validation> valid\n", sep = "")
    cat("  ", x$canonical_xgid, "\n", sep = "")
    if (nrow(x$warnings) > 0L) {
      cat("  Warnings: ", nrow(x$warnings), "\n", sep = "")
    }
  } else {
    cat("<backgammon_xgid_validation> invalid\n", sep = "")
    if (!is.na(x$action_marker)) {
      cat("  Action marker: ", x$action_marker, "\n", sep = "")
    }
    if (nrow(x$errors) > 0L) {
      for (i in seq_len(nrow(x$errors))) {
        cat("  - ", x$errors$code[[i]], ": ", x$errors$message[[i]], "\n", sep = "")
      }
    }
  }
  invisible(x)
}

new_xgid_validation <- function(
    valid = FALSE,
    canonical_xgid = NA_character_,
    errors = list(),
    warnings = list(),
    action_marker = NA_character_) {
  structure(
    list(
      valid = isTRUE(valid),
      canonical_xgid = canonical_xgid,
      errors = bind_xgid_diagnostics(errors),
      warnings = bind_xgid_diagnostics(warnings),
      action_marker = action_marker
    ),
    class = "backgammon_xgid_validation"
  )
}

xgid_diagnostic <- function(code, category, field, message) {
  data.frame(
    code = as.character(code),
    category = as.character(category),
    field = as.character(field),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

bind_xgid_diagnostics <- function(x) {
  if (length(x) == 0L) {
    return(data.frame(
      code = character(),
      category = character(),
      field = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, x)
}

xgid_field_names <- function() {
  c(
    "position", "cube_exponent", "cube_owner", "turn", "dice_action",
    "score_white", "score_black", "crawford_jacoby", "match_length",
    "max_cube_exponent"
  )
}

is_unsigned_integer_text <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    grepl("^[0-9]+$", x) &&
    !is.na(suppressWarnings(as.integer(x)))
}

parse_validated_xgid_fields <- function(fields) {
  list(
    cube_exponent = as.integer(fields[["cube_exponent"]]),
    cube_owner_code = as.integer(fields[["cube_owner"]]),
    turn_code = as.integer(fields[["turn"]]),
    dice_action = fields[["dice_action"]],
    score_white = as.integer(fields[["score_white"]]),
    score_black = as.integer(fields[["score_black"]]),
    crawford_jacoby = as.integer(fields[["crawford_jacoby"]]),
    match_length = as.integer(fields[["match_length"]]),
    max_cube_exponent = as.integer(fields[["max_cube_exponent"]])
  )
}

canonicalize_xgid_fields <- function(fields) {
  numeric_fields <- c(
    "cube_exponent", "cube_owner", "turn", "score_white", "score_black",
    "crawford_jacoby", "match_length", "max_cube_exponent"
  )
  fields[numeric_fields] <- vapply(
    fields[numeric_fields],
    function(value) as.character(as.integer(value)),
    character(1)
  )
  paste0("XGID=", paste(fields, collapse = ":"))
}

decode_xgid_character <- function(entry, turn_code) {
  if (identical(entry, "-")) {
    return(0L)
  }

  if (entry %in% LETTERS[1:16]) {
    count <- match(entry, LETTERS)
    return(if (turn_code == 1L) as.integer(count) else -as.integer(count))
  }

  if (entry %in% letters[1:16]) {
    count <- match(entry, letters)
    return(if (turn_code == 1L) -as.integer(count) else as.integer(count))
  }

  stop("Unsupported XGID checker character.", call. = FALSE)
}

# Decode the turn-relative XGID payload into stable White-relative facts.
#
# The returned slots are always:
#   values[1]  = Black bar
#   values[2:25] = White-relative points 1:24
#   values[26] = White bar
# Positive counts are White and negative counts are Black.
decode_xgid_payload <- function(payload, turn_code = 1L) {
  if (!turn_code %in% c(-1L, 1L)) {
    stop("`turn_code` must be -1 or 1.", call. = FALSE)
  }

  entries <- strsplit(payload, "", fixed = TRUE)[[1L]]
  if (length(entries) != 26L) {
    stop("The XGID position field must contain 26 characters.", call. = FALSE)
  }

  canonical_entries <- if (turn_code == 1L) entries else rev(entries)
  values <- vapply(
    canonical_entries,
    decode_xgid_character,
    integer(1),
    turn_code = turn_code
  )

  points <- as.integer(values[2:25])
  bar <- c(
    white = max(values[[26L]], 0L),
    black = max(-values[[1L]], 0L)
  )

  white_total <- sum(pmax(points, 0L)) + bar[["white"]]
  black_total <- sum(pmax(-points, 0L)) + bar[["black"]]

  list(
    values = as.integer(values),
    points = points,
    bar = stats::setNames(as.integer(bar), names(bar)),
    off = c(
      white = 15L - white_total,
      black = 15L - black_total
    ),
    white_total = as.integer(white_total),
    black_total = as.integer(black_total),
    bar_valid = values[[1L]] <= 0L && values[[26L]] >= 0L
  )
}
