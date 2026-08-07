.is_scalar_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.is_gnu_position_id <- function(x) {
  .is_scalar_text(x) && grepl("^[A-Za-z0-9+/]{14}$", x)
}

.is_gnu_match_id <- function(x) {
  .is_scalar_text(x) && grepl("^[A-Za-z0-9+/]{12}$", x)
}

.is_complete_gnuid <- function(x) {
  .is_scalar_text(x) &&
    grepl("^[A-Za-z0-9+/]{14}:[A-Za-z0-9+/]{12}$", x)
}

.require_backgammoncalculator <- function() {
  if (!requireNamespace("backgammoncalculator", quietly = TRUE)) {
    stop(
      "GNUID input requires package `backgammoncalculator`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gnuid_to_xgid <- function(position_id, match_id = NULL) {
  .require_backgammoncalculator()

  normalize_xgid(
    backgammoncalculator::gnuid_to_xgid(
      position_id = position_id,
      match_id = match_id
    )
  )
}

.xgid_to_gnuid <- function(xgid) {
  .require_backgammoncalculator()

  backgammoncalculator::gnuid_from_position(
    backgammoncalculator::position_from_xgid(
      normalize_xgid(xgid)
    )
  )
}

.resolve_ggboard_input <- function(x, match_id = NULL) {
  if (!is.null(match_id)) {
    if (!.is_gnu_position_id(x)) {
      stop(
        "When `match_id` is supplied, `x` must be one 14-character GNU Position ID.",
        call. = FALSE
      )
    }

    if (!.is_gnu_match_id(match_id)) {
      stop(
        "`match_id` must be one 12-character GNU Match ID.",
        call. = FALSE
      )
    }

    return(.gnuid_to_xgid(x, match_id))
  }

  if (.is_complete_gnuid(x)) {
    return(.gnuid_to_xgid(x))
  }

  if (.is_gnu_position_id(x)) {
    stop(
      paste0(
        "A GNU Position ID requires its GNU Match ID. ",
        "Supply `match_id = ...` or a combined `<position_id>:<match_id>` GNUID."
      ),
      call. = FALSE
    )
  }

  # Existing XGID and backgammon_position inputs continue on the release path.
  x
}
