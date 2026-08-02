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
    "-pt Match, ",
    position_player_label(position, "white"),
    " ",
    position_raw_score(position, "white"),
    " - ",
    position_player_label(position, "black"),
    " ",
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
    context = NULL,
    perspective = "white",
    score_format = c("away", "raw", "both")
) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  perspective <- normalize_board_perspective(perspective)
  score_format <- match.arg(score_format)
  white_name <- validate_information_name(white_name, "white_name")
  black_name <- validate_information_name(black_name, "black_name")
  white_wins <- validate_optional_win_count(white_wins, "white_wins")
  black_wins <- validate_optional_win_count(black_wins, "black_wins")

  frame <- geometry$frame
  board_center_x <- mean(c(frame$xmin, frame$xmax))
  player_x <- frame$xmax + style$information_player_x_nudge

  pips <- c(
    white = position_pip_count(position, "white"),
    black = position_pip_count(position, "black")
  )
  names_by_player <- c(white = white_name, black = black_name)

  if (identical(position$play_context, "match")) {
    away <- position$match_length - position$score
    secondary <- vapply(c("white", "black"), function(player) {
      raw_label <- paste0(position$score[[player]], " points")
      away_label <- paste0(away[[player]], "-away")
      switch(
        score_format,
        raw = raw_label,
        away = away_label,
        both = paste0(raw_label, " \u00b7 ", away_label)
      )
    }, character(1))
    names(secondary) <- c("white", "black")
  } else {
    resolved_wins <- c(
      white = if (is.null(white_wins)) as.integer(position$score[["white"]]) else white_wins,
      black = if (is.null(black_wins)) as.integer(position$score[["black"]]) else black_wins
    )
    secondary <- vapply(resolved_wins, format_win_count, character(1))
  }

  bottom_player <- perspective
  top_player <- other_semantic_player(bottom_player)

  make_row <- function(player, side) {
    is_top <- identical(side, "top")
    player_name_y <- if (is_top) {
      frame$ymax + style$information_top_player_name_offset
    } else {
      frame$ymin - style$information_bottom_player_name_offset
    }
    data.frame(
      player = player,
      name = unname(names_by_player[[player]]),
      on_roll = identical(player, position$on_roll),
      on_roll_arrow = if (identical(player, position$on_roll)) "\u2192" else "",
      on_roll_arrow_x =
        player_x - style$information_on_roll_arrow_x_offset,
      on_roll_arrow_y = player_name_y,
      secondary = unname(secondary[[player]]),
      pip_label = paste0(
        position_player_label(position, player),
        " pips: ",
        pips[[player]]
      ),
      player_x = player_x,
      player_name_y = player_name_y,
      secondary_y = if (is_top) {
        frame$ymax + style$information_top_secondary_offset
      } else {
        frame$ymin - style$information_bottom_secondary_offset
      },
      pip_x = board_center_x,
      pip_y = if (is_top) {
        frame$ymax + style$information_pip_offset
      } else {
        frame$ymin - style$information_pip_offset
      },
      stringsAsFactors = FALSE
    )
  }

  top <- make_row(top_player, "top")
  bottom <- make_row(bottom_player, "bottom")

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
    family = "sans",
    perspective = "white",
    score_format = c("away", "raw", "both")
) {
  family <- validate_information_name(family, "information_family")
  score_format <- match.arg(score_format)

  information <- board_information_layout(
    position = position,
    geometry = geometry,
    style = style,
    white_name = white_name,
    black_name = black_name,
    white_wins = white_wins,
    black_wins = black_wins,
    context = context,
    perspective = perspective,
    score_format = score_format
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
      ggplot2::aes(
        x = on_roll_arrow_x,
        y = on_roll_arrow_y,
        label = on_roll_arrow
      ),
      inherit.aes = FALSE,
      color = colors$on_roll_arrow,
      size = style$information_on_roll_arrow_size,
      family = family,
      fontface = "bold",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_label(
      data = information$top,
      ggplot2::aes(x = player_x, y = player_name_y, label = name),
      inherit.aes = FALSE,
      color = colors$score_text,
      fill = colors$outside_fill,
      linewidth = 0.25,
      label.r = grid::unit(0.18, "lines"),
      label.padding = grid::unit(0.20, "lines"),
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
      ggplot2::aes(
        x = on_roll_arrow_x,
        y = on_roll_arrow_y,
        label = on_roll_arrow
      ),
      inherit.aes = FALSE,
      color = colors$on_roll_arrow,
      size = style$information_on_roll_arrow_size,
      family = family,
      fontface = "bold",
      hjust = 1,
      vjust = 0.5
    ) +
    ggplot2::geom_label(
      data = information$bottom,
      ggplot2::aes(x = player_x, y = player_name_y, label = name),
      inherit.aes = FALSE,
      color = colors$score_text,
      fill = colors$outside_fill,
      linewidth = 0.25,
      label.r = grid::unit(0.18, "lines"),
      label.padding = grid::unit(0.20, "lines"),
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
