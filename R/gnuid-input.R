.is_scalar_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.is_gnu_position_id <- function(x) {
  .is_scalar_text(x) && grepl("^[A-Za-z0-9+/]{14}$", x)
}


.is_complete_gnuid <- function(x) {
  .is_scalar_text(x) &&
    grepl("^[A-Za-z0-9+/]{14}:[A-Za-z0-9+/]{12}$", x)
}

.require_backgammoncalculator <- function() {
  minimum_version <- "0.2.0"

  if (!requireNamespace("backgammoncalculator", quietly = TRUE)) {
    stop(
      paste0(
        "GNUID input requires package `backgammoncalculator` >= ",
        minimum_version, "."
      ),
      call. = FALSE
    )
  }

  installed_version <- as.character(
    utils::packageVersion("backgammoncalculator")
  )
  if (utils::compareVersion(installed_version, minimum_version) < 0L) {
    stop(
      paste0(
        "GNUID input requires package `backgammoncalculator` >= ",
        minimum_version, "; installed version is ", installed_version, "."
      ),
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

.resolve_ggboard_input <- function(x) {
  if (.is_complete_gnuid(x)) {
    return(.gnuid_to_xgid(x))
  }

  if (.is_gnu_position_id(x)) {
    stop(
      paste0(
        "A GNU Position ID alone is incomplete. ",
        "Pass a complete GNUID as `<position_id>:<match_id>`."
      ),
      call. = FALSE
    )
  }

  # Preserve the existing release path for XGID and
  # backgammon_position inputs.
  x
}
