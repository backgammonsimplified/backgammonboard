move_step_columns <- function() {
  c("step_id", "from_type", "from_point", "to_type", "to_point")
}


#' Construct ordered checker movements
#'
#' `board_moves()` constructs structured, ordered atomic movements. It does not
#' parse GNU or other source notation. Point numbers are relative to the mover:
#' `1` is that player's 1-point and `24` is that player's 24-point.
#'
#' @param from Source locations: points 1 through 24 or `"bar"`.
#' @param to Destination locations: points 1 through 24 or `"off"`.
#' @param die Optional die used by each atomic movement. Supply integers 1
#'   through 6 or `NA` for a row that should not receive limited die checking.
#' @param label Optional non-empty display labels, one per movement.
#'
#' @return A data frame inheriting from `backgammon_board_moves`.
#' @export
board_moves <- function(from, to, die = NULL, label = NULL) {
  if (missing(from) || missing(to)) {
    stop_move_error(
      "`from` and `to` are required",
      subclass = "backgammon_move_invalid_input"
    )
  }
  if (length(from) == 0L || length(to) == 0L || length(from) != length(to)) {
    stop_move_error(
      "`from` and `to` must have the same positive length",
      subclass = "backgammon_move_invalid_input"
    )
  }

  from_locations <- normalize_move_locations(from, "from")
  to_locations <- normalize_move_locations(to, "to")
  count <- length(from_locations$type)
  die <- normalize_move_die(die, count)
  label <- normalize_move_label(label, count)

  moves <- data.frame(
    step_id = seq_len(count),
    from_type = from_locations$type,
    from_point = from_locations$point,
    to_type = to_locations$type,
    to_point = to_locations$point,
    stringsAsFactors = FALSE
  )
  moves$step_id <- as.integer(moves$step_id)
  moves$from_point <- as.integer(moves$from_point)
  moves$to_point <- as.integer(moves$to_point)

  moves <- structure(
    moves,
    die = die,
    label = label,
    mover_relative = TRUE,
    class = c("backgammon_board_moves", "data.frame")
  )
  validate_board_moves(moves)
  moves
}


normalize_move_locations <- function(x, argument) {
  if (!(is.character(x) || is.numeric(x)) || anyNA(x)) {
    stop_move_error(
      paste0("`", argument, "` must contain points or location names"),
      subclass = "backgammon_move_invalid_location"
    )
  }
  values <- tolower(trimws(as.character(x)))
  numeric_location <- grepl("^(?:[1-9]|1[0-9]|2[0-4])$", values)
  special <- if (identical(argument, "from")) "bar" else "off"
  if (any(!numeric_location & values != special)) {
    stop_move_error(
      paste0(
        "`", argument, "` locations must be points 1 through 24 or `",
        special, "`"
      ),
      subclass = "backgammon_move_invalid_location"
    )
  }
  points <- rep(NA_integer_, length(values))
  points[numeric_location] <- as.integer(values[numeric_location])
  list(
    type = ifelse(numeric_location, "point", special),
    point = points
  )
}


normalize_move_die <- function(die, count) {
  if (is.null(die)) return(rep(NA_integer_, count))
  if (!is.numeric(die) || length(die) != count ||
      any(!is.na(die) & (die != as.integer(die) | !die %in% 1:6))) {
    stop_move_error(
      "`die` must be NULL or contain one value from 1 through 6 (or NA) per movement",
      subclass = "backgammon_move_invalid_die"
    )
  }
  as.integer(die)
}


normalize_move_label <- function(label, count) {
  if (is.null(label)) return(rep(NA_character_, count))
  if (!is.character(label) || length(label) != count || anyNA(label) ||
      any(!nzchar(trimws(label)))) {
    stop_move_error(
      "`label` must be NULL or contain one non-empty character value per movement",
      subclass = "backgammon_move_invalid_label"
    )
  }
  label
}


validate_board_moves <- function(x) {
  if (!inherits(x, "backgammon_board_moves") || !is.data.frame(x)) {
    stop_move_error(
      "Move data must be created by `board_moves()`",
      subclass = "backgammon_move_invalid_object"
    )
  }
  if (!identical(names(x), move_step_columns()) || nrow(x) == 0L) {
    stop_move_error(
      "Move data has an invalid structured-movement schema",
      subclass = "backgammon_move_invalid_object"
    )
  }
  if (!identical(x$step_id, seq_len(nrow(x)))) {
    stop_move_error(
      "`step_id` must preserve consecutive movement order",
      subclass = "backgammon_move_invalid_object"
    )
  }
  valid_from <- x$from_type %in% c("point", "bar") &
    ifelse(x$from_type == "point", x$from_point %in% 1:24, is.na(x$from_point))
  valid_to <- x$to_type %in% c("point", "off") &
    ifelse(x$to_type == "point", x$to_point %in% 1:24, is.na(x$to_point))
  if (anyNA(valid_from) || anyNA(valid_to) || !all(valid_from) || !all(valid_to)) {
    stop_move_error(
      "Structured movement locations are incomplete or invalid",
      subclass = "backgammon_move_invalid_object"
    )
  }
  die <- attr(x, "die")
  label <- attr(x, "label")
  if (!is.integer(die) || length(die) != nrow(x) ||
      !is.character(label) || length(label) != nrow(x)) {
    stop_move_error(
      "Structured movement attributes are invalid",
      subclass = "backgammon_move_invalid_object"
    )
  }
  invisible(x)
}


stop_move_error <- function(message, subclass = "backgammon_move_error", ...) {
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = unique(c(subclass, "backgammon_move_error", "error", "condition"))
  )
  stop(condition)
}


#' @export
print.backgammon_board_moves <- function(x, ...) {
  cat("<backgammon_board_moves>\n")
  cat("  Atomic movements: ", nrow(x), "\n", sep = "")
  displayed <- data.frame(
    order = x$step_id,
    from = ifelse(x$from_type == "point", x$from_point, x$from_type),
    to = ifelse(x$to_type == "point", x$to_point, x$to_type),
    die = attr(x, "die"),
    label = attr(x, "label"),
    stringsAsFactors = FALSE
  )
  print(displayed, row.names = FALSE, ...)
  invisible(x)
}
