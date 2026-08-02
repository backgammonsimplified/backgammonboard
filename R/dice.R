die_pip_pattern <- function(value) {
  if (
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    value != as.integer(value) ||
    !value %in% 1:6
  ) {
    stop("`value` must be one integer from 1 through 6.", call. = FALSE)
  }

  patterns <- list(
    `1` = matrix(
      c(
        0, 0
      ),
      ncol = 2,
      byrow = TRUE
    ),
    `2` = matrix(
      c(
        -1,  1,
         1, -1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    `3` = matrix(
      c(
        -1,  1,
         0,  0,
         1, -1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    `4` = matrix(
      c(
        -1,  1,
         1,  1,
        -1, -1,
         1, -1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    `5` = matrix(
      c(
        -1,  1,
         1,  1,
         0,  0,
        -1, -1,
         1, -1
      ),
      ncol = 2,
      byrow = TRUE
    ),
    `6` = matrix(
      c(
        -1,  1,
         1,  1,
        -1,  0,
         1,  0,
        -1, -1,
         1, -1
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  pattern <- patterns[[as.character(as.integer(value))]]

  data.frame(
    horizontal = pattern[, 1],
    vertical = pattern[, 2],
    stringsAsFactors = FALSE
  )
}


die_face_polygon <- function(
    center_x,
    center_y,
    size,
    group,
    power = 5,
    resolution = 120L
) {
  theta <- seq(
    from = 0,
    to = 2 * pi,
    length.out = resolution + 1L
  )

  half_size <- size / 2
  exponent <- 2 / power

  horizontal <- sign(cos(theta)) * abs(cos(theta))^exponent
  vertical <- sign(sin(theta)) * abs(sin(theta))^exponent

  data.frame(
    x = center_x + half_size * horizontal,
    y = center_y + half_size * vertical,
    group = group,
    stringsAsFactors = FALSE
  )
}


empty_dice_layout <- function() {
  list(
    faces = data.frame(
      die = integer(),
      value = integer(),
      player = character(),
      x = numeric(),
      y = numeric(),
      group = integer(),
      stringsAsFactors = FALSE
    ),
    pips = data.frame(
      die = integer(),
      value = integer(),
      player = character(),
      x = numeric(),
      y = numeric(),
      radius = numeric(),
      stringsAsFactors = FALSE
    )
  )
}


dice_layout <- function(
    position,
    geometry,
    style,
    perspective = "white"
) {
  if (!inherits(position, "backgammon_position")) {
    stop("`position` must be a backgammon_position.", call. = FALSE)
  }

  if (length(position$dice) != 2L) {
    return(empty_dice_layout())
  }

  perspective <- normalize_board_perspective(perspective)

  field <- if (identical(position$on_roll, perspective)) {
    geometry$right_field
  } else {
    geometry$left_field
  }

  die_size <- 1.05 * style$die_scale
  die_gap <- style$die_gap * style$die_scale
  pip_distance <- 0.22 * die_size
  pip_radius <- 0.075 * die_size

  field_center_x <- mean(c(field$xmin, field$xmax))
  field_center_y <- mean(c(field$ymin, field$ymax))

  center_offset <- (die_size + die_gap) / 2

  die_centers <- data.frame(
    die = 1:2,
    value = as.integer(position$dice),
    x = c(
      field_center_x - center_offset,
      field_center_x + center_offset
    ),
    y = rep(field_center_y, 2L),
    stringsAsFactors = FALSE
  )

  face_rows <- lapply(seq_len(nrow(die_centers)), function(index) {
    die <- die_centers[index, , drop = FALSE]

    face <- die_face_polygon(
      center_x = die$x[[1L]],
      center_y = die$y[[1L]],
      size = die_size,
      group = die$die[[1L]]
    )

    face$die <- die$die[[1L]]
    face$value <- die$value[[1L]]

    face[, c("die", "value", "x", "y", "group")]
  })

  pip_rows <- lapply(seq_len(nrow(die_centers)), function(index) {
    die <- die_centers[index, , drop = FALSE]
    pattern <- die_pip_pattern(die$value[[1L]])

    data.frame(
      die = rep(die$die[[1L]], nrow(pattern)),
      value = rep(die$value[[1L]], nrow(pattern)),
      x = die$x[[1L]] + pattern$horizontal * pip_distance,
      y = die$y[[1L]] + pattern$vertical * pip_distance,
      radius = rep(pip_radius, nrow(pattern)),
      stringsAsFactors = FALSE
    )
  })

  faces <- do.call(rbind, face_rows)
  pips <- do.call(rbind, pip_rows)
  faces$player <- position$on_roll
  pips$player <- position$on_roll
  list(
    faces = faces[, c("die", "value", "player", "x", "y", "group")],
    pips = pips[, c("die", "value", "player", "x", "y", "radius")]
  )
}


add_dice_layers <- function(
    plot,
    position,
    geometry,
    colors,
    style,
    perspective = "white",
    dice = NULL
) {
  if (is.null(dice)) {
    dice <- dice_layout(
      position = position,
      geometry = geometry,
      style = style,
      perspective = perspective
    )
  }

  if (nrow(dice$faces) == 0L) {
    return(plot)
  }

  if (identical(position$on_roll, "white")) {
    face_fill <- colors$die_white_fill
    pip_fill <- colors$die_white_pips
    border_color <- colors$die_white_border
  } else {
    face_fill <- colors$die_black_fill
    pip_fill <- colors$die_black_pips
    border_color <- colors$die_black_border
  }

  plot +
    ggplot2::geom_polygon(
      data = dice$faces,
      ggplot2::aes(
        x = x,
        y = y,
        group = group
      ),
      inherit.aes = FALSE,
      fill = face_fill,
      color = border_color,
      linewidth = style$die_border_width,
      linejoin = "round"
    ) +
    ggforce::geom_circle(
      data = dice$pips,
      ggplot2::aes(
        x0 = x,
        y0 = y,
        r = radius
      ),
      inherit.aes = FALSE,
      fill = pip_fill,
      color = NA,
      n = 90
    )
}
