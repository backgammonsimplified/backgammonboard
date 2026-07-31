#' Adapt an Engine Kit RendererPosition envelope
#'
#' `renderer_position()` accepts the deterministic JSON envelope produced by
#' `backgammon-engine-kit` and maps its Universal Position facts into the
#' package's existing factual board model. Stable Engine Kit slots map as
#' `player_0` to White and `player_1` to Black internally; these names are
#' storage slots, not colour-based identities.
#'
#' For the initial learner view, the accepted `bottom_player` is always the
#' learner, `top_player` is always the opponent, and `point_labels_for` must
#' equal the learner. The home-board side remains an independent horizontal
#' mirror choice. Changing the canonical on-roll player moves the dice,
#' on-roll arrow, and explicit status text without rotating or mirroring the
#' board.
#'
#' The input may be a parsed named list, a JSON object string, or the path to a
#' UTF-8 JSON file. The adapter verifies supported schema versions and the
#' envelope structure, but deliberately does not calculate or replace Engine
#' Kit's semantic or view hashes.
#'
#' @param x A parsed RendererPosition JSON object, JSON object string, or path
#'   to a JSON file containing one accepted envelope.
#'
#' @return An object inheriting from `backgammon_position`, with
#'   `backgammon_renderer_position` as its first class.
#' @export
renderer_position <- function(x) {
  if (inherits(x, "backgammon_renderer_position")) {
    return(x)
  }

  envelope <- read_renderer_position_input(x)
  validated <- validate_renderer_position_envelope(envelope)
  map_renderer_position(validated)
}


read_renderer_position_input <- function(x) {
  if (is.list(x)) {
    return(x)
  }

  if (
    !is.character(x) ||
    length(x) != 1L ||
    is.na(x) ||
    !nzchar(trimws(x))
  ) {
    stop(
      "`x` must be a parsed RendererPosition object, JSON text, or a JSON file path.",
      call. = FALSE
    )
  }

  looks_like_json <- grepl("^\\s*[\\{\\[]", x, perl = TRUE)
  if (looks_like_json) {
    return(parse_renderer_position_json(x))
  }

  if (!isTRUE(file.exists(x)) || isTRUE(dir.exists(x))) {
    stop(
      "`x` is neither RendererPosition JSON text nor an existing JSON file.",
      call. = FALSE
    )
  }

  text <- paste(
    readLines(x, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  parse_renderer_position_json(text)
}


renderer_json_error <- function(message, offset) {
  stop(
    paste0("Malformed RendererPosition JSON at character ", offset, ": ", message),
    call. = FALSE
  )
}


parse_renderer_position_json <- function(text) {
  cursor <- new.env(parent = emptyenv())
  cursor$text <- enc2utf8(text)
  cursor$length <- nchar(cursor$text, type = "chars")
  cursor$index <- 1L

  current_character <- function(offset = 0L) {
    index <- cursor$index + offset
    if (index > cursor$length) "" else substr(cursor$text, index, index)
  }

  advance <- function(count = 1L) {
    cursor$index <- cursor$index + count
    invisible(NULL)
  }

  skip_whitespace <- function() {
    while (
      cursor$index <= cursor$length &&
      current_character() %in% c(" ", "\t", "\r", "\n")
    ) {
      advance()
    }
    invisible(NULL)
  }

  parse_string <- function() {
    if (!identical(current_character(), '"')) {
      renderer_json_error("expected a string", cursor$index)
    }
    advance()
    pieces <- character()

    while (cursor$index <= cursor$length) {
      character <- current_character()
      if (identical(character, '"')) {
        advance()
        return(paste0(pieces, collapse = ""))
      }
      if (identical(character, "\\")) {
        advance()
        escape <- current_character()
        replacements <- c(
          '"' = '"',
          "\\" = "\\",
          "/" = "/",
          "b" = "\b",
          "f" = "\f",
          "n" = "\n",
          "r" = "\r",
          "t" = "\t"
        )
        if (escape %in% names(replacements)) {
          pieces <- c(pieces, unname(replacements[[escape]]))
          advance()
          next
        }
        if (!identical(escape, "u")) {
          renderer_json_error("unsupported string escape", cursor$index)
        }

        hex <- substr(cursor$text, cursor$index + 1L, cursor$index + 4L)
        if (nchar(hex, type = "chars") != 4L ||
            !grepl("^[0-9A-Fa-f]{4}$", hex)) {
          renderer_json_error("invalid Unicode escape", cursor$index)
        }
        codepoint <- strtoi(hex, base = 16L)
        advance(5L)

        if (codepoint >= 0xD800L && codepoint <= 0xDBFFL) {
          if (!identical(current_character(), "\\") ||
              !identical(current_character(1L), "u")) {
            renderer_json_error(
              "high surrogate is not followed by a low surrogate",
              cursor$index
            )
          }
          low_hex <- substr(cursor$text, cursor$index + 2L, cursor$index + 5L)
          if (nchar(low_hex, type = "chars") != 4L ||
              !grepl("^[0-9A-Fa-f]{4}$", low_hex)) {
            renderer_json_error("invalid low-surrogate escape", cursor$index)
          }
          low <- strtoi(low_hex, base = 16L)
          if (low < 0xDC00L || low > 0xDFFFL) {
            renderer_json_error("invalid low surrogate", cursor$index)
          }
          codepoint <-
            0x10000L +
            (codepoint - 0xD800L) * 0x400L +
            (low - 0xDC00L)
          advance(6L)
        } else if (codepoint >= 0xDC00L && codepoint <= 0xDFFFL) {
          renderer_json_error("unexpected low surrogate", cursor$index)
        }

        pieces <- c(pieces, intToUtf8(codepoint))
        next
      }
      if (utf8ToInt(character)[[1L]] < 0x20L) {
        renderer_json_error("unescaped control character in string", cursor$index)
      }
      pieces <- c(pieces, character)
      advance()
    }

    renderer_json_error("unterminated string", cursor$index)
  }

  parse_number <- function() {
    remainder <- substring(cursor$text, cursor$index)
    match <- regexpr(
      "^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?",
      remainder,
      perl = TRUE
    )
    if (match[[1L]] != 1L) {
      renderer_json_error("invalid number", cursor$index)
    }
    spelling <- regmatches(remainder, match)
    advance(nchar(spelling, type = "chars"))
    value <- suppressWarnings(as.numeric(spelling))
    if (!is.finite(value)) {
      renderer_json_error("number is outside the supported range", cursor$index)
    }
    value
  }

  parse_value <- NULL

  parse_array <- function() {
    advance()
    skip_whitespace()
    values <- list()
    if (identical(current_character(), "]")) {
      advance()
      return(values)
    }

    repeat {
      values <- c(values, list(parse_value()))
      skip_whitespace()
      separator <- current_character()
      if (identical(separator, "]")) {
        advance()
        return(values)
      }
      if (!identical(separator, ",")) {
        renderer_json_error("expected `,` or `]`", cursor$index)
      }
      advance()
      skip_whitespace()
    }
  }

  parse_object <- function() {
    advance()
    skip_whitespace()
    values <- list()
    names(values) <- character()
    if (identical(current_character(), "}")) {
      advance()
      return(values)
    }

    repeat {
      if (!identical(current_character(), '"')) {
        renderer_json_error("object keys must be strings", cursor$index)
      }
      key <- parse_string()
      if (key %in% names(values)) {
        renderer_json_error(
          paste0("duplicate object key `", key, "`"),
          cursor$index
        )
      }
      skip_whitespace()
      if (!identical(current_character(), ":")) {
        renderer_json_error("expected `:` after object key", cursor$index)
      }
      advance()
      skip_whitespace()
      values[key] <- list(parse_value())
      skip_whitespace()
      separator <- current_character()
      if (identical(separator, "}")) {
        advance()
        return(values)
      }
      if (!identical(separator, ",")) {
        renderer_json_error("expected `,` or `}`", cursor$index)
      }
      advance()
      skip_whitespace()
    }
  }

  parse_literal <- function(spelling, value) {
    end <- cursor$index + nchar(spelling, type = "chars") - 1L
    if (!identical(substr(cursor$text, cursor$index, end), spelling)) {
      renderer_json_error(
        paste0("expected `", spelling, "`"),
        cursor$index
      )
    }
    advance(nchar(spelling, type = "chars"))
    value
  }

  parse_value <- function() {
    skip_whitespace()
    character <- current_character()
    if (identical(character, "{")) return(parse_object())
    if (identical(character, "[")) return(parse_array())
    if (identical(character, '"')) return(parse_string())
    if (identical(character, "t")) return(parse_literal("true", TRUE))
    if (identical(character, "f")) return(parse_literal("false", FALSE))
    if (identical(character, "n")) return(parse_literal("null", NULL))
    if (grepl("[-0-9]", character)) return(parse_number())
    renderer_json_error("expected a JSON value", cursor$index)
  }

  value <- parse_value()
  skip_whitespace()
  if (cursor$index <= cursor$length) {
    renderer_json_error("unexpected trailing content", cursor$index)
  }
  value
}


renderer_contract_error <- function(path, message) {
  stop(
    paste0("Invalid RendererPosition at `", path, "`: ", message),
    call. = FALSE
  )
}


renderer_object <- function(value, path, required, allowed = required) {
  if (!is.list(value) || is.null(names(value))) {
    renderer_contract_error(path, "must be a JSON object")
  }
  if (anyNA(names(value)) || any(!nzchar(names(value)))) {
    renderer_contract_error(path, "contains an empty object key")
  }
  if (anyDuplicated(names(value))) {
    renderer_contract_error(path, "contains duplicate object keys")
  }

  missing <- setdiff(required, names(value))
  if (length(missing) > 0L) {
    renderer_contract_error(
      path,
      paste0("is missing required field(s): ", paste(missing, collapse = ", "))
    )
  }

  unexpected <- setdiff(names(value), allowed)
  if (length(unexpected) > 0L) {
    renderer_contract_error(
      path,
      paste0("contains unsupported field(s): ", paste(unexpected, collapse = ", "))
    )
  }
  value
}


renderer_character <- function(value, path, allowed = NULL, nullable = FALSE) {
  if (is.null(value) && nullable) {
    return(NULL)
  }
  if (
    !is.character(value) ||
    length(value) != 1L ||
    is.na(value)
  ) {
    renderer_contract_error(path, "must be a string")
  }
  if (!is.null(allowed) && !value %in% allowed) {
    renderer_contract_error(
      path,
      paste0("must be one of: ", paste(allowed, collapse = ", "))
    )
  }
  value
}


renderer_integer <- function(
    value,
    path,
    minimum = NULL,
    maximum = NULL,
    nullable = FALSE
) {
  if (is.null(value) && nullable) {
    return(NULL)
  }
  if (
    !is.numeric(value) ||
    is.logical(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value) ||
    value != floor(value)
  ) {
    renderer_contract_error(path, "must be an integer")
  }
  if (!is.null(minimum) && value < minimum) {
    renderer_contract_error(path, paste0("must be at least ", minimum))
  }
  if (!is.null(maximum) && value > maximum) {
    renderer_contract_error(path, paste0("must be at most ", maximum))
  }
  as.integer(value)
}


renderer_boolean <- function(value, path, nullable = FALSE) {
  if (is.null(value) && nullable) {
    return(NULL)
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    renderer_contract_error(path, "must be true or false")
  }
  value
}


renderer_array <- function(value, path, length_required = NULL) {
  if (is.list(value)) {
    if (!is.null(names(value))) {
      renderer_contract_error(path, "must be a JSON array")
    }
    if (any(vapply(value, is.null, logical(1)))) {
      renderer_contract_error(path, "must not contain null elements")
    }
    value <- unlist(value, recursive = FALSE, use.names = FALSE)
  } else if (is.atomic(value) && !is.null(value)) {
    value <- unname(value)
  } else {
    renderer_contract_error(path, "must be a JSON array")
  }

  if (!is.null(length_required) && length(value) != length_required) {
    renderer_contract_error(
      path,
      paste0("must contain exactly ", length_required, " elements")
    )
  }
  value
}


renderer_integer_array <- function(
    value,
    path,
    length_required,
    minimum,
    maximum = NULL
) {
  value <- renderer_array(value, path, length_required)
  if (
    !is.numeric(value) ||
    is.logical(value) ||
    anyNA(value) ||
    any(!is.finite(value)) ||
    any(value != floor(value)) ||
    any(value < minimum) ||
    (!is.null(maximum) && any(value > maximum))
  ) {
    renderer_contract_error(path, "contains an invalid integer")
  }
  as.integer(value)
}


renderer_hash <- function(value, path) {
  value <- renderer_character(value, path)
  if (!grepl("^[0-9a-f]{64}$", value)) {
    renderer_contract_error(path, "must be a lowercase 64-character SHA-256 hash")
  }
  value
}


renderer_power_of_two <- function(value, path, nullable = FALSE) {
  value <- renderer_integer(value, path, minimum = 1L, nullable = nullable)
  if (is.null(value)) {
    return(NULL)
  }
  if (bitwAnd(value, value - 1L) != 0L) {
    renderer_contract_error(path, "must be a positive power of two")
  }
  value
}


validate_renderer_player_board <- function(value, path) {
  value <- renderer_object(
    value,
    path,
    c("points", "bar", "off")
  )
  list(
    points = renderer_integer_array(
      value$points,
      paste0(path, ".points"),
      24L,
      0L
    ),
    bar = renderer_integer(value$bar, paste0(path, ".bar"), minimum = 0L),
    off = renderer_integer(value$off, paste0(path, ".off"), minimum = 0L)
  )
}


validate_renderer_position_envelope <- function(value) {
  value <- renderer_object(
    value,
    "/",
    c("position", "semantic_state_hash", "view", "view_hash")
  )
  position <- renderer_object(
    value$position,
    "position",
    c("schema_version", "players", "board", "state", "cube", "score", "rules")
  )
  position_version <- renderer_character(
    position$schema_version,
    "position.schema_version"
  )
  if (!identical(position_version, "universal-position-v1")) {
    renderer_contract_error(
      "position.schema_version",
      paste0("unsupported contract version `", position_version, "`")
    )
  }

  players <- renderer_array(position$players, "position.players", 2L)
  if (!is.character(players) ||
      !identical(as.character(players), c("player_0", "player_1"))) {
    renderer_contract_error(
      "position.players",
      "must contain stable slots player_0 and player_1 in that order"
    )
  }

  board <- renderer_object(
    position$board,
    "position.board",
    c("coordinate_system", "checker_count", "player_0", "player_1")
  )
  coordinate_system <- renderer_character(
    board$coordinate_system,
    "position.board.coordinate_system"
  )
  if (!identical(coordinate_system, "self_relative_points")) {
    renderer_contract_error(
      "position.board.coordinate_system",
      paste0("unsupported coordinate system `", coordinate_system, "`")
    )
  }
  checker_count <- renderer_object(
    board$checker_count,
    "position.board.checker_count",
    c("player_0", "player_1")
  )
  checker_count <- c(
    player_0 = renderer_integer(
      checker_count$player_0,
      "position.board.checker_count.player_0",
      minimum = 1L
    ),
    player_1 = renderer_integer(
      checker_count$player_1,
      "position.board.checker_count.player_1",
      minimum = 1L
    )
  )
  player_0 <- validate_renderer_player_board(
    board$player_0,
    "position.board.player_0"
  )
  player_1 <- validate_renderer_player_board(
    board$player_1,
    "position.board.player_1"
  )

  totals <- c(
    player_0 = sum(player_0$points) + player_0$bar + player_0$off,
    player_1 = sum(player_1$points) + player_1$bar + player_1$off
  )
  if (!identical(as.integer(totals), as.integer(checker_count))) {
    renderer_contract_error(
      "position.board",
      "checker totals do not match checker_count"
    )
  }
  if (any(player_0$points > 0L & rev(player_1$points) > 0L)) {
    renderer_contract_error(
      "position.board",
      "both players occupy the same physical point"
    )
  }

  state <- renderer_object(
    position$state,
    "position.state",
    c(
      "game_state",
      "on_roll",
      "decision_player",
      "phase",
      "decision_type",
      "dice"
    )
  )
  game_state <- renderer_character(
    state$game_state,
    "position.state.game_state",
    c("setup", "playing", "game_over", "resigned", "unknown")
  )
  on_roll <- renderer_character(
    state$on_roll,
    "position.state.on_roll",
    c("player_0", "player_1"),
    nullable = TRUE
  )
  decision_player <- renderer_character(
    state$decision_player,
    "position.state.decision_player",
    c("player_0", "player_1"),
    nullable = TRUE
  )
  phase <- renderer_character(
    state$phase,
    "position.state.phase",
    c(
      "setup",
      "pre_roll",
      "checker_play",
      "cube_response",
      "beaver_response",
      "raccoon_response",
      "resignation_response",
      "game_over",
      "unknown"
    )
  )
  decision_type <- renderer_character(
    state$decision_type,
    "position.state.decision_type",
    c(
      "none",
      "roll",
      "roll_or_double",
      "checker_play",
      "take_or_drop",
      "take_drop_or_beaver",
      "take_drop_or_raccoon",
      "accept_or_reject_resignation",
      "unknown"
    )
  )
  dice <- if (is.null(state$dice)) {
    NULL
  } else {
    renderer_integer_array(
      state$dice,
      "position.state.dice",
      2L,
      1L,
      6L
    )
  }

  cube <- renderer_object(
    position$cube,
    "position.cube",
    c("enabled", "value", "owner", "pending_action")
  )
  cube_enabled <- renderer_boolean(
    cube$enabled,
    "position.cube.enabled",
    nullable = TRUE
  )
  cube_value <- renderer_power_of_two(
    cube$value,
    "position.cube.value",
    nullable = TRUE
  )
  cube_owner <- renderer_character(
    cube$owner,
    "position.cube.owner",
    c("center", "player_0", "player_1"),
    nullable = TRUE
  )
  pending <- renderer_object(
    cube$pending_action,
    "position.cube.pending_action",
    c(
      "type",
      "offerer",
      "responder",
      "offered_cube_value",
      "resignation_multiplier"
    )
  )
  pending <- list(
    type = renderer_character(
      pending$type,
      "position.cube.pending_action.type",
      c("none", "double", "beaver", "raccoon", "resignation", "unknown")
    ),
    offerer = renderer_character(
      pending$offerer,
      "position.cube.pending_action.offerer",
      c("player_0", "player_1"),
      nullable = TRUE
    ),
    responder = renderer_character(
      pending$responder,
      "position.cube.pending_action.responder",
      c("player_0", "player_1"),
      nullable = TRUE
    ),
    offered_cube_value = renderer_power_of_two(
      pending$offered_cube_value,
      "position.cube.pending_action.offered_cube_value",
      nullable = TRUE
    ),
    resignation_multiplier = renderer_integer(
      pending$resignation_multiplier,
      "position.cube.pending_action.resignation_multiplier",
      minimum = 1L,
      maximum = 3L,
      nullable = TRUE
    )
  )

  score <- renderer_object(
    position$score,
    "position.score",
    c("player_0", "player_1", "match_length")
  )
  score <- c(
    player_0 = renderer_integer(
      score$player_0,
      "position.score.player_0",
      minimum = 0L
    ),
    player_1 = renderer_integer(
      score$player_1,
      "position.score.player_1",
      minimum = 0L
    ),
    match_length = renderer_integer(
      score$match_length,
      "position.score.match_length",
      minimum = 0L
    )
  )

  rules <- renderer_object(
    position$rules,
    "position.rules",
    c(
      "variation",
      "crawford",
      "jacoby",
      "beavers",
      "raccoons",
      "automatic_doubles",
      "maximum_cube"
    )
  )
  rules <- list(
    variation = renderer_character(
      rules$variation,
      "position.rules.variation",
      c(
        "standard",
        "nackgammon",
        "hypergammon_1",
        "hypergammon_2",
        "hypergammon_3",
        "other"
      ),
      nullable = TRUE
    ),
    crawford = renderer_boolean(
      rules$crawford,
      "position.rules.crawford",
      nullable = TRUE
    ),
    jacoby = renderer_boolean(
      rules$jacoby,
      "position.rules.jacoby",
      nullable = TRUE
    ),
    beavers = renderer_boolean(
      rules$beavers,
      "position.rules.beavers",
      nullable = TRUE
    ),
    raccoons = renderer_boolean(
      rules$raccoons,
      "position.rules.raccoons",
      nullable = TRUE
    ),
    automatic_doubles = renderer_integer(
      rules$automatic_doubles,
      "position.rules.automatic_doubles",
      minimum = 0L,
      nullable = TRUE
    ),
    maximum_cube = renderer_power_of_two(
      rules$maximum_cube,
      "position.rules.maximum_cube",
      nullable = TRUE
    )
  )

  view <- renderer_object(
    value$view,
    "view",
    c(
      "schema_version",
      "top_player",
      "bottom_player",
      "point_labels_for",
      "bottom_home_board_side",
      "cube_display_side",
      "rotation",
      "view_origin"
    )
  )
  view_version <- renderer_character(
    view$schema_version,
    "view.schema_version"
  )
  if (!identical(view_version, "backgammon-view-v1")) {
    renderer_contract_error(
      "view.schema_version",
      paste0("unsupported contract version `", view_version, "`")
    )
  }
  view <- list(
    schema_version = view_version,
    top_player = renderer_character(
      view$top_player,
      "view.top_player",
      c("player_0", "player_1")
    ),
    bottom_player = renderer_character(
      view$bottom_player,
      "view.bottom_player",
      c("player_0", "player_1")
    ),
    point_labels_for = renderer_character(
      view$point_labels_for,
      "view.point_labels_for",
      c("player_0", "player_1")
    ),
    bottom_home_board_side = renderer_character(
      view$bottom_home_board_side,
      "view.bottom_home_board_side",
      c("left", "right")
    ),
    cube_display_side = renderer_character(
      view$cube_display_side,
      "view.cube_display_side",
      c("left", "right", "center", "off_board", "auto")
    ),
    rotation = renderer_character(
      view$rotation,
      "view.rotation",
      c("source", "rotated", "default", "custom")
    ),
    view_origin = renderer_character(
      view$view_origin,
      "view.view_origin",
      c("source", "external", "generated_default")
    )
  )
  if (identical(view$top_player, view$bottom_player)) {
    renderer_contract_error(
      "view",
      "top_player and bottom_player must differ"
    )
  }
  if (!identical(view$point_labels_for, view$bottom_player)) {
    renderer_contract_error(
      "view.point_labels_for",
      paste0(
        "initial learner-view policy requires point labels for bottom_player `",
        view$bottom_player,
        "`"
      )
    )
  }

  list(
    position = list(
      schema_version = position_version,
      players = c("player_0", "player_1"),
      board = list(
        coordinate_system = coordinate_system,
        checker_count = checker_count,
        player_0 = player_0,
        player_1 = player_1
      ),
      state = list(
        game_state = game_state,
        on_roll = on_roll,
        decision_player = decision_player,
        phase = phase,
        decision_type = decision_type,
        dice = dice
      ),
      cube = list(
        enabled = cube_enabled,
        value = cube_value,
        owner = cube_owner,
        pending_action = pending
      ),
      score = score,
      rules = rules
    ),
    semantic_state_hash = renderer_hash(
      value$semantic_state_hash,
      "semantic_state_hash"
    ),
    view = view,
    view_hash = renderer_hash(value$view_hash, "view_hash")
  )
}


renderer_slot_to_player <- function(value) {
  switch(
    value,
    player_0 = "white",
    player_1 = "black",
    renderer_contract_error("position", "unsupported stable player slot")
  )
}


renderer_null_fields <- function(position) {
  fields <- c(
    "cube.enabled" = is.null(position$cube$enabled),
    "cube.value" = is.null(position$cube$value),
    "cube.owner" = is.null(position$cube$owner),
    "rules.variation" = is.null(position$rules$variation),
    "rules.crawford" = is.null(position$rules$crawford),
    "rules.jacoby" = is.null(position$rules$jacoby),
    "rules.beavers" = is.null(position$rules$beavers),
    "rules.raccoons" = is.null(position$rules$raccoons),
    "rules.automatic_doubles" = is.null(position$rules$automatic_doubles),
    "rules.maximum_cube" = is.null(position$rules$maximum_cube)
  )
  names(fields)[fields]
}


map_renderer_position <- function(envelope) {
  position <- envelope$position
  board <- position$board
  state <- position$state
  cube <- position$cube
  score <- position$score
  rules <- position$rules

  if (is.null(state$on_roll)) {
    renderer_contract_error(
      "position.state.on_roll",
      "null is accepted by the source contract but is not representable by the current W7 factual model"
    )
  }
  if (
    !isFALSE(cube$enabled) &&
    (is.null(cube$value) || is.null(cube$owner))
  ) {
    renderer_contract_error(
      "position.cube",
      "a visible or availability-unknown cube requires known value and ownership in the current W7 renderer"
    )
  }
  if (!is.null(cube$value) && cube$value > supported_cube_max()) {
    renderer_contract_error(
      "position.cube.value",
      paste0(
        cube$value,
        " exceeds the current W7 rendering limit of ",
        supported_cube_max()
      )
    )
  }
  if (score[["match_length"]] > 0L && is.null(rules$crawford)) {
    renderer_contract_error(
      "position.rules.crawford",
      "null is not representable for a current W7 match display"
    )
  }
  if (!cube$pending_action$type %in% c("none", "double")) {
    renderer_contract_error(
      "position.cube.pending_action.type",
      paste0(
        "`",
        cube$pending_action$type,
        "` is accepted by Engine Kit but has no current W7 visual state"
      )
    )
  }

  points <- board$player_0$points - rev(board$player_1$points)
  play_context <- if (score[["match_length"]] == 0L) {
    "unlimited"
  } else {
    "match"
  }
  mapped_score <- c(
    white = score[["player_0"]],
    black = score[["player_1"]]
  )
  cube_owner <- if (is.null(cube$owner)) {
    NA_character_
  } else {
    switch(
      cube$owner,
      center = "center",
      player_0 = "white",
      player_1 = "black"
    )
  }
  action_marker <- if (identical(cube$pending_action$type, "double")) {
    "D"
  } else if (!is.null(state$dice)) {
    paste0(state$dice, collapse = "")
  } else {
    "00"
  }
  cube_value <- if (is.null(cube$value)) NA_integer_ else cube$value
  maximum_cube <- if (is.null(rules$maximum_cube)) {
    NA_real_
  } else {
    as.numeric(rules$maximum_cube)
  }

  structure(
    list(
      xgid = NA_character_,
      play_context = play_context,
      points = as.integer(points),
      bar = c(
        white = board$player_0$bar,
        black = board$player_1$bar
      ),
      off = c(
        white = board$player_0$off,
        black = board$player_1$off
      ),
      on_roll = renderer_slot_to_player(state$on_roll),
      dice = if (is.null(state$dice)) integer() else state$dice,
      cube_value = cube_value,
      cube_owner = cube_owner,
      score_white = unname(mapped_score[["white"]]),
      score_black = unname(mapped_score[["black"]]),
      score = mapped_score,
      match_length = if (identical(play_context, "match")) {
        score[["match_length"]]
      } else {
        NA_integer_
      },
      is_crawford = if (identical(play_context, "match")) {
        rules$crawford
      } else {
        FALSE
      },
      position_payload = NA_character_,
      action_marker = action_marker,
      dice_action = action_marker,
      cube_exponent = if (is.na(cube_value)) {
        NA_integer_
      } else {
        as.integer(log(cube_value, base = 2))
      },
      xgid_max_cube = maximum_cube,
      encoded_max_cube = maximum_cube,
      max_cube = maximum_cube,
      max_cube_exponent = if (is.na(maximum_cube)) {
        NA_integer_
      } else {
        as.integer(log(maximum_cube, base = 2))
      },
      max_supported_cube_value = supported_cube_max(),
      jacoby = if (is.null(rules$jacoby)) NA else rules$jacoby,
      beavers = if (is.null(rules$beavers)) NA else rules$beavers,
      raccoons = if (is.null(rules$raccoons)) NA else rules$raccoons,
      automatic_doubles = if (is.null(rules$automatic_doubles)) {
        NA_integer_
      } else {
        rules$automatic_doubles
      },
      variation = if (is.null(rules$variation)) NA_character_ else rules$variation,
      cube_enabled = if (is.null(cube$enabled)) NA else cube$enabled,
      game_state = state$game_state,
      phase = state$phase,
      decision_type = state$decision_type,
      decision_player = if (is.null(state$decision_player)) {
        NA_character_
      } else {
        renderer_slot_to_player(state$decision_player)
      },
      checker_count = c(
        white = board$checker_count[["player_0"]],
        black = board$checker_count[["player_1"]]
      ),
      player_labels = c(white = "player_0", black = "player_1"),
      learner_player = renderer_slot_to_player(envelope$view$bottom_player),
      opponent_player = renderer_slot_to_player(envelope$view$top_player),
      learner_slot = envelope$view$bottom_player,
      opponent_slot = envelope$view$top_player,
      renderer_display_policy = "learner-bottom-v1",
      renderer_position_schema = position$schema_version,
      renderer_semantic_state_hash = envelope$semantic_state_hash,
      renderer_view = envelope$view,
      renderer_view_hash = envelope$view_hash,
      renderer_unknown_fields = renderer_null_fields(position)
    ),
    class = c("backgammon_renderer_position", "backgammon_position")
  )
}
