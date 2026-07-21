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
    position_raw_score(position, "white"),
    " - Black ",
    position_raw_score(position, "black")
  )

  if (isTRUE(position$is_crawford)) {
    label <- paste0(label, ", Crawford")
  }

  paste0(label, ",")
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
    context = NULL
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

  match_label <- match_context_label(position)
  status <- position_status_label(position, context = context)

  sentence <- data.frame(
    x = board_center_x + style$information_sentence_x_nudge,
    y = frame$ymin - style$information_sentence_offset,
    label = information_sentence_label(match_label, status),
    context = match_label,
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
    context = NULL,
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
    context = context
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
