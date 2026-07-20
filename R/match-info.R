position_pip_count <- function(position, player = c("white", "black")) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  player <- match.arg(player)
  points <- as.integer(position$points)

  if (length(points) != 24L) {
    stop("`position$points` must contain 24 point counts.", call. = FALSE)
  }

  if (identical(player, "white")) {
    point_pips <- sum(pmax(points, 0L) * seq_len(24L))
  } else {
    point_pips <- sum(abs(pmin(points, 0L)) * rev(seq_len(24L)))
  }

  bar_pips <- 25L * as.integer(position$bar[[player]])
  as.integer(point_pips + bar_pips)
}


sentence_case_player <- function(player) {
  if (identical(player, "white")) "White" else "Black"
}


validate_information_name <- function(value, argument) {
  if (
    !is.character(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(trimws(value))
  ) {
    stop(
      paste0("`", argument, "` must be one non-empty character value."),
      call. = FALSE
    )
  }

  trimws(value)
}


validate_optional_win_count <- function(value, argument) {
  if (is.null(value)) {
    return(NULL)
  }

  if (
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    value < 0 ||
    value != as.integer(value)
  ) {
    stop(
      paste0("`", argument, "` must be NULL or one non-negative integer."),
      call. = FALSE
    )
  }

  as.integer(value)
}


format_win_count <- function(value) {
  paste0(value, if (identical(value, 1L)) " win" else " wins")
}


match_context_label <- function(position) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  if (identical(position$play_context, "unlimited")) {
    return("Unlimited game,")
  }

  label <- paste0(
    position$match_length,
    "-pt Match, White ",
    position$score[["white"]],
    " - Black ",
    position$score[["black"]]
  )

  if (identical(position$crawford_status, "crawford")) {
    label <- paste0(label, ", Crawford")
  } else if (identical(position$crawford_status, "post_crawford")) {
    label <- paste0(label, ", Post-Crawford")
  }

  paste0(label, ",")
}


position_status_label <- function(position, status_text = NULL) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  if (!is.null(status_text)) {
    return(validate_information_name(status_text, "status_text"))
  }

  roller <- sentence_case_player(position$on_roll)

  if (length(position$dice) == 2L) {
    return(paste0(
      roller,
      " on roll, to play: ",
      position$dice[[1L]],
      "-",
      position$dice[[2L]]
    ))
  }

  if (identical(position$action, "double")) {
    return(paste0(
      roller,
      " on roll, cube offered: Take or Pass?"
    ))
  }

  roller_owns_cube <- identical(position$cube_owner, position$on_roll)

  if (roller_owns_cube) {
    return(paste0(
      roller,
      " on roll, cube action: Roll or Redouble?"
    ))
  }

  if (!identical(position$cube_owner, "center")) {
    return(paste0(roller, " on roll"))
  }

  double_prompt_suppressed <- FALSE

  if (identical(position$play_context, "match")) {
    away <- position$match_length - position$score
    double_prompt_suppressed <-
      identical(position$crawford_status, "crawford") ||
      any(away == 1L)
  }

  if (!double_prompt_suppressed) {
    return(paste0(
      roller,
      " on roll, cube action: Roll or Double?"
    ))
  }

  paste0(roller, " on roll")
}


plotmath_quote <- function(value) {
  encodeString(value, quote = '"', na.encode = FALSE)
}


information_sentence_label <- function(context, status) {
  paste0(
    "plain(",
    plotmath_quote(context),
    ")~bold(",
    plotmath_quote(status),
    ")"
  )
}


board_information_layout <- function(
    position,
    geometry,
    style,
    white_name = "White",
    black_name = "Black",
    white_wins = NULL,
    black_wins = NULL,
    status_text = NULL
) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  white_name <- validate_information_name(white_name, "white_name")
  black_name <- validate_information_name(black_name, "black_name")
  white_wins <- validate_optional_win_count(white_wins, "white_wins")
  black_wins <- validate_optional_win_count(black_wins, "black_wins")

  frame <- geometry$frame
  board_center_x <- mean(c(frame$xmin, frame$xmax))
  player_x <- frame$xmax + style$information_player_x_nudge

  white_pips <- position_pip_count(position, "white")
  black_pips <- position_pip_count(position, "black")

  if (identical(position$play_context, "match")) {
    away <- position$match_length - position$score
    white_secondary <- paste0(away[["white"]], "-away")
    black_secondary <- paste0(away[["black"]], "-away")
  } else {
    resolved_white_wins <- if (is.null(white_wins)) {
      as.integer(position$score[["white"]])
    } else {
      white_wins
    }

    resolved_black_wins <- if (is.null(black_wins)) {
      as.integer(position$score[["black"]])
    } else {
      black_wins
    }

    white_secondary <- format_win_count(resolved_white_wins)
    black_secondary <- format_win_count(resolved_black_wins)
  }

  top <- data.frame(
    player = "black",
    name = black_name,
    secondary = black_secondary,
    pip_label = paste0("Black pips: ", black_pips),
    player_x = player_x,
    player_name_y = frame$ymax + style$information_top_player_name_offset,
    secondary_y = frame$ymax + style$information_top_secondary_offset,
    pip_x = board_center_x,
    pip_y = frame$ymax + style$information_pip_offset,
    stringsAsFactors = FALSE
  )

  bottom <- data.frame(
    player = "white",
    name = white_name,
    secondary = white_secondary,
    pip_label = paste0("White pips: ", white_pips),
    player_x = player_x,
    player_name_y = frame$ymin - style$information_bottom_player_name_offset,
    secondary_y = frame$ymin - style$information_bottom_secondary_offset,
    pip_x = board_center_x,
    pip_y = frame$ymin - style$information_pip_offset,
    stringsAsFactors = FALSE
  )

  context <- match_context_label(position)
  status <- position_status_label(position, status_text)

  sentence <- data.frame(
    x = board_center_x + style$information_sentence_x_nudge,
    y = frame$ymin - style$information_sentence_offset,
    label = information_sentence_label(context, status),
    context = context,
    status = status,
    stringsAsFactors = FALSE
  )

  list(
    top = top,
    bottom = bottom,
    sentence = sentence
  )
}


add_board_information <- function(
    plot,
    position,
    geometry,
    colors,
    style,
    white_name = "White",
    black_name = "Black",
    white_wins = NULL,
    black_wins = NULL,
    status_text = NULL,
    family = "sans"
) {
  family <- validate_information_name(family, "information_family")

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style,
    white_name = white_name,
    black_name = black_name,
    white_wins = white_wins,
    black_wins = black_wins,
    status_text = status_text
  )

  plot +
    ggplot2::geom_text(
      data = information$top,
      ggplot2::aes(x = pip_x, y = pip_y, label = pip_label),
      inherit.aes = FALSE,
      color = colors$score_text,
      size = style$information_pip_text_size,
      family = family,
      fontface = "plain",
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$top,
      ggplot2::aes(x = player_x, y = player_name_y, label = name),
      inherit.aes = FALSE,
      color = colors$score_text,
      size = style$information_player_name_size,
      family = family,
      fontface = "bold",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$top,
      ggplot2::aes(x = player_x, y = secondary_y, label = secondary),
      inherit.aes = FALSE,
      color = colors$secondary_text,
      size = style$information_secondary_text_size,
      family = family,
      fontface = "plain",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$bottom,
      ggplot2::aes(x = pip_x, y = pip_y, label = pip_label),
      inherit.aes = FALSE,
      color = colors$score_text,
      size = style$information_pip_text_size,
      family = family,
      fontface = "plain",
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$bottom,
      ggplot2::aes(x = player_x, y = secondary_y, label = secondary),
      inherit.aes = FALSE,
      color = colors$secondary_text,
      size = style$information_secondary_text_size,
      family = family,
      fontface = "plain",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$bottom,
      ggplot2::aes(x = player_x, y = player_name_y, label = name),
      inherit.aes = FALSE,
      color = colors$score_text,
      size = style$information_player_name_size,
      family = family,
      fontface = "bold",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      data = information$sentence,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = colors$status_text,
      size = style$information_sentence_text_size,
      family = family,
      parse = TRUE,
      hjust = 0.5,
      vjust = 0.5
    )
}
