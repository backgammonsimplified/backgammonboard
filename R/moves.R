move_step_columns <- function() {
  c(
    "step_id",
    "chain_id",
    "source_token",
    "from_type",
    "from_point",
    "to_type",
    "to_point",
    "hit_marked",
    "repeat_group"
  )
}


#' Parse backgammon checker-move notation
#'
#' `board_moves()` parses one selected checker play into ordered atomic
#' movements. It preserves compound chains, repetition groups, source tokens,
#' and explicit hit markers without applying the moves or claiming that the
#' play is legal in a particular position.
#'
#' Supported notation includes ordinary point moves, compound moves, bar
#' entry, bearing off, hit markers, and repetition suffixes. Move groups may be
#' separated by whitespace or commas.
#'
#' @param notation A length-one character value such as `"13/8 6/5"`,
#'   `"24/18/13"`, `"bar/24"`, or `"13/8(2)"`.
#'
#' @return A data frame inheriting from `backgammon_board_moves`, with one row
#'   per atomic movement and the columns `step_id`, `chain_id`,
#'   `source_token`, `from_type`, `from_point`, `to_type`, `to_point`,
#'   `hit_marked`, and `repeat_group`.
#'
#' @examples
#' board_moves("13/8 6/5")
#' board_moves("24/18/13")
#' board_moves("bar/24")
#' board_moves("6/off")
#' board_moves("13/8(2)")
#' board_moves("13/8*")
#'
#' @export
board_moves <- function(notation) {
  if (missing(notation)) {
    stop_move_error(
      "`notation` is required",
      subclass = "backgammon_move_invalid_input"
    )
  }

  validate_move_notation_input(notation)

  text <- tolower(trimws(notation))
  tokens <- tokenize_move_notation(text)

  rows <- list()
  next_step_id <- 1L
  next_chain_id <- 1L
  next_repeat_group <- 1L

  for (token_index in seq_along(tokens)) {
    token <- tokens[[token_index]]
    repeat_info <- parse_move_repeat(token, token_index)
    locations <- parse_move_path(
      repeat_info$core,
      source_token = token,
      token_index = token_index
    )

    repeat_group <- if (repeat_info$explicit) {
      group <- paste0("r", next_repeat_group)
      next_repeat_group <- next_repeat_group + 1L
      group
    } else {
      NA_character_
    }

    for (repeat_index in seq_len(repeat_info$count)) {
      chain_id <- next_chain_id
      next_chain_id <- next_chain_id + 1L

      for (location_index in seq_len(length(locations) - 1L)) {
        from <- locations[[location_index]]
        to <- locations[[location_index + 1L]]

        rows[[length(rows) + 1L]] <- data.frame(
          step_id = next_step_id,
          chain_id = chain_id,
          source_token = token,
          from_type = from$type,
          from_point = from$point,
          to_type = to$type,
          to_point = to$point,
          hit_marked = isTRUE(to$hit_marked),
          repeat_group = repeat_group,
          stringsAsFactors = FALSE
        )

        next_step_id <- next_step_id + 1L
      }
    }
  }

  moves <- do.call(rbind, rows)
  rownames(moves) <- NULL

  moves$step_id <- as.integer(moves$step_id)
  moves$chain_id <- as.integer(moves$chain_id)
  moves$from_point <- as.integer(moves$from_point)
  moves$to_point <- as.integer(moves$to_point)
  moves$hit_marked <- as.logical(moves$hit_marked)

  moves <- structure(
    moves,
    notation = paste(tokens, collapse = " "),
    class = c("backgammon_board_moves", "data.frame")
  )

  validate_board_moves(moves)
  moves
}


validate_move_notation_input <- function(notation) {
  if (missing(notation)) {
    stop_move_error(
      "`notation` is required",
      subclass = "backgammon_move_invalid_input"
    )
  }

  if (!is.character(notation)) {
    stop_move_error(
      "`notation` must be a character value",
      subclass = "backgammon_move_invalid_input"
    )
  }

  if (length(notation) != 1L) {
    stop_move_error(
      "`notation` must contain exactly one checker play",
      subclass = "backgammon_move_invalid_input"
    )
  }

  if (is.na(notation)) {
    stop_move_error(
      "`notation` must not be missing",
      subclass = "backgammon_move_invalid_input"
    )
  }

  text <- trimws(notation)
  if (!nzchar(text)) {
    stop_move_error(
      "`notation` must not be empty",
      subclass = "backgammon_move_invalid_input"
    )
  }

  if (grepl("[\r\n]", text)) {
    stop_move_error(
      "`notation` must contain one checker play on one line",
      subclass = "backgammon_move_invalid_input"
    )
  }

  invisible(notation)
}


tokenize_move_notation <- function(text) {
  if (grepl("(^|,)\\s*(,|$)", text, perl = TRUE)) {
    stop_move_error(
      "Move notation contains an empty comma-separated token",
      subclass = "backgammon_move_invalid_separator"
    )
  }

  separated <- gsub(",", " ", text, fixed = TRUE)
  tokens <- strsplit(separated, "[[:space:]]+", perl = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]

  if (length(tokens) == 0L) {
    stop_move_error(
      "Move notation does not contain a checker move",
      subclass = "backgammon_move_invalid_input"
    )
  }

  tokens
}


parse_move_repeat <- function(token, token_index) {
  match <- regexec("\\(([1-9][0-9]*)\\)$", token, perl = TRUE)
  captured <- regmatches(token, match)[[1L]]

  if (length(captured) == 0L) {
    if (grepl("[()]", token)) {
      stop_move_token_error(
        token = token,
        token_index = token_index,
        detail = "the repetition suffix must have the form `(n)` with n at least 2",
        subclass = "backgammon_move_invalid_repeat"
      )
    }

    return(list(core = token, count = 1L, explicit = FALSE))
  }

  count_text <- captured[[2L]]
  count_number <- suppressWarnings(as.numeric(count_text))

  if (!is.finite(count_number) ||
      count_number > .Machine$integer.max ||
      count_number < 2) {
    stop_move_token_error(
      token = token,
      token_index = token_index,
      detail = "the repetition count must be an integer of at least 2",
      subclass = "backgammon_move_invalid_repeat"
    )
  }

  suffix_length <- nchar(captured[[1L]], type = "chars")
  core_length <- nchar(token, type = "chars") - suffix_length
  core <- substr(token, 1L, core_length)

  if (!nzchar(core) || grepl("[()]", core)) {
    stop_move_token_error(
      token = token,
      token_index = token_index,
      detail = "the repetition suffix must appear once at the end of a move token",
      subclass = "backgammon_move_invalid_repeat"
    )
  }

  list(
    core = core,
    count = as.integer(count_number),
    explicit = TRUE
  )
}


parse_move_path <- function(core, source_token, token_index) {
  components <- strsplit(core, "/", fixed = TRUE)[[1L]]

  if (length(components) < 2L || any(!nzchar(components))) {
    stop_move_token_error(
      token = source_token,
      token_index = token_index,
      detail = "a move token must contain at least one non-empty `from/to` pair",
      subclass = "backgammon_move_invalid_token"
    )
  }

  locations <- vector("list", length(components))
  final_index <- length(components)

  for (component_index in seq_along(components)) {
    locations[[component_index]] <- parse_move_location(
      component = components[[component_index]],
      is_source = component_index == 1L,
      is_final = component_index == final_index,
      source_token = source_token,
      token_index = token_index,
      component_index = component_index
    )
  }

  locations
}


parse_move_location <- function(component,
                                is_source,
                                is_final,
                                source_token,
                                token_index,
                                component_index) {
  star_count <- nchar(component, type = "chars") -
    nchar(gsub("*", "", component, fixed = TRUE), type = "chars")

  hit_marked <- star_count == 1L && endsWith(component, "*")

  if (star_count > 0L && (!hit_marked || is_source)) {
    stop_move_component_error(
      token = source_token,
      token_index = token_index,
      component_index = component_index,
      detail = "a hit marker `*` may appear once, after a point destination",
      subclass = "backgammon_move_invalid_hit_marker"
    )
  }

  value <- if (hit_marked) {
    substr(component, 1L, nchar(component, type = "chars") - 1L)
  } else {
    component
  }

  if (identical(value, "bar")) {
    if (!is_source) {
      stop_move_component_error(
        token = source_token,
        token_index = token_index,
        component_index = component_index,
        detail = "`bar` may appear only as the first location",
        subclass = "backgammon_move_invalid_location"
      )
    }

    return(list(
      type = "bar",
      point = NA_integer_,
      hit_marked = FALSE
    ))
  }

  if (identical(value, "off")) {
    if (is_source || !is_final) {
      stop_move_component_error(
        token = source_token,
        token_index = token_index,
        component_index = component_index,
        detail = "`off` may appear only as the final destination",
        subclass = "backgammon_move_invalid_location"
      )
    }

    if (hit_marked) {
      stop_move_component_error(
        token = source_token,
        token_index = token_index,
        component_index = component_index,
        detail = "a hit marker cannot be attached to `off`",
        subclass = "backgammon_move_invalid_hit_marker"
      )
    }

    return(list(
      type = "off",
      point = NA_integer_,
      hit_marked = FALSE
    ))
  }

  if (!grepl("^(?:[1-9]|1[0-9]|2[0-4])$", value, perl = TRUE)) {
    stop_move_component_error(
      token = source_token,
      token_index = token_index,
      component_index = component_index,
      detail = "a location must be a point from 1 through 24, `bar`, or `off`",
      subclass = "backgammon_move_invalid_location"
    )
  }

  list(
    type = "point",
    point = as.integer(value),
    hit_marked = hit_marked
  )
}


validate_board_moves <- function(x) {
  if (!inherits(x, "backgammon_board_moves") || !is.data.frame(x)) {
    stop_move_error(
      "Move data must inherit from `backgammon_board_moves`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  expected <- move_step_columns()
  if (!identical(names(x), expected)) {
    stop_move_error(
      paste0(
        "Move data must contain exactly these columns: ",
        paste(expected, collapse = ", ")
      ),
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (nrow(x) == 0L) {
    stop_move_error(
      "Move data must contain at least one atomic movement",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!identical(x$step_id, seq_len(nrow(x)))) {
    stop_move_error(
      "`step_id` must preserve consecutive atomic-move order",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!is.integer(x$chain_id) || anyNA(x$chain_id) || any(x$chain_id < 1L)) {
    stop_move_error(
      "`chain_id` must contain positive integers",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!is.character(x$source_token) ||
      anyNA(x$source_token) ||
      any(!nzchar(x$source_token))) {
    stop_move_error(
      "`source_token` must contain non-empty move tokens",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!all(x$from_type %in% c("point", "bar"))) {
    stop_move_error(
      "`from_type` must contain only `point` or `bar`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!all(x$to_type %in% c("point", "off"))) {
    stop_move_error(
      "`to_type` must contain only `point` or `off`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  valid_from_points <- ifelse(
    x$from_type == "point",
    !is.na(x$from_point) & x$from_point >= 1L & x$from_point <= 24L,
    is.na(x$from_point)
  )
  if (!all(valid_from_points)) {
    stop_move_error(
      "`from_point` does not match `from_type`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  valid_to_points <- ifelse(
    x$to_type == "point",
    !is.na(x$to_point) & x$to_point >= 1L & x$to_point <= 24L,
    is.na(x$to_point)
  )
  if (!all(valid_to_points)) {
    stop_move_error(
      "`to_point` does not match `to_type`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (!is.logical(x$hit_marked) || anyNA(x$hit_marked)) {
    stop_move_error(
      "`hit_marked` must contain non-missing logical values",
      subclass = "backgammon_move_invalid_object"
    )
  }

  if (any(x$hit_marked & x$to_type != "point")) {
    stop_move_error(
      "A hit marker may apply only to a point destination",
      subclass = "backgammon_move_invalid_object"
    )
  }

  valid_repeat_groups <- is.na(x$repeat_group) |
    grepl("^r[1-9][0-9]*$", x$repeat_group)
  if (!is.character(x$repeat_group) || !all(valid_repeat_groups)) {
    stop_move_error(
      "`repeat_group` must be missing or use identifiers such as `r1`",
      subclass = "backgammon_move_invalid_object"
    )
  }

  invisible(x)
}


stop_move_token_error <- function(token,
                                  token_index,
                                  detail,
                                  subclass) {
  stop_move_error(
    paste0(
      "Invalid move token ", token_index, " (`", token, "`): ", detail
    ),
    subclass = subclass,
    token = token,
    token_index = token_index
  )
}


stop_move_component_error <- function(token,
                                      token_index,
                                      component_index,
                                      detail,
                                      subclass) {
  stop_move_error(
    paste0(
      "Invalid location ", component_index,
      " in move token ", token_index,
      " (`", token, "`): ", detail
    ),
    subclass = subclass,
    token = token,
    token_index = token_index,
    component_index = component_index
  )
}


stop_move_error <- function(message,
                            subclass = "backgammon_move_error",
                            token = NULL,
                            token_index = NULL,
                            component_index = NULL) {
  condition <- structure(
    list(
      message = message,
      call = NULL,
      token = token,
      token_index = token_index,
      component_index = component_index
    ),
    class = unique(c(
      subclass,
      "backgammon_move_error",
      "error",
      "condition"
    ))
  )

  stop(condition)
}


#' @export
print.backgammon_board_moves <- function(x, ...) {
  cat("<backgammon_board_moves>\n")
  cat("  Notation: ", attr(x, "notation"), "\n", sep = "")
  cat("  Atomic movements: ", nrow(x), "\n", sep = "")

  displayed <- as.data.frame(x, stringsAsFactors = FALSE)
  print(displayed, row.names = FALSE, ...)
  invisible(x)
}
