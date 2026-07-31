#' Render a factual backgammon board from a complete XGID
#'
#' `ggboard()` is the public rendering entry point. It validates and constructs
#' a factual position, resolves any factual pending cube offer and optional
#' instructional context, applies one semantic perspective, and returns an
#' ordinary `ggplot` object.
#'
#' @param x A complete XGID string or a `backgammon_position`.
#' @param colors A validated object created by [board_colors()].
#' @param style A validated object created by [board_style()].
#' @param decision One of `"auto"`, `"checker_play"`, `"roll_double"`,
#'   `"take_pass"`, or `"none"`. `"auto"` never infers a cube question.
#' @param perspective One of `"decision_maker"`, `"on_roll"`, `"white"`, or
#'   `"black"`. RendererPosition input is fixed to its learner perspective;
#'   an explicit value that would rotate the opponent to the bottom is rejected.
#' @param score_format Match score display format: `"away"`, `"raw"`, or
#'   `"both"`.
#' @param show_information Whether to draw score, pip, and status information.
#' @param brand_text Optional static brand text. The neutral default is `NULL`.
#'
#' @return An ordinary object inheriting from `ggplot`.
#' @export
ggboard <- function(
    x,
    colors = board_colors("default"),
    style = board_style("default"),
    decision = c("auto", "checker_play", "roll_double", "take_pass", "none"),
    perspective = c("decision_maker", "on_roll", "white", "black"),
    score_format = c("away", "raw", "both"),
    show_information = TRUE,
    brand_text = NULL
) {
  perspective_was_missing <- missing(perspective)
  decision <- match.arg(decision)
  perspective <- match.arg(perspective)
  score_format <- match.arg(score_format)

  position <- if (inherits(x, "backgammon_position")) {
    x
  } else {
    backgammon_position(x)
  }
  renderer_view <- if (inherits(position, "backgammon_renderer_position")) {
    position$renderer_view
  } else {
    NULL
  }

  resolved_decision <- if (identical(decision, "auto")) {
    if (length(position$dice) == 2L) "checker_play" else "none"
  } else {
    decision
  }

  pending_offer <- pending_offer_from_position(position)
  context <- board_context("none")
  cube_offer <- NULL

  if (identical(resolved_decision, "checker_play")) {
    if (!is.null(pending_offer)) {
      stop(
        "A checker-play context cannot be combined with a pending cube offer [offer_already_pending]",
        call. = FALSE
      )
    }
  } else if (identical(resolved_decision, "roll_double")) {
    context <- if (is.null(pending_offer)) {
      board_context("cube_offer")
    } else {
      board_context("cube_offer", offer = pending_offer)
    }
  } else if (identical(resolved_decision, "take_pass")) {
    response_offer <- if (is.null(pending_offer)) {
      proposed_cube_offer(position)
    } else {
      pending_offer
    }
    context <- board_context("cube_response", offer = response_offer)
  } else if (!is.null(pending_offer)) {
    cube_offer <- pending_offer
  }

  resolved_perspective <- if (
    !is.null(renderer_view) &&
    isTRUE(perspective_was_missing)
  ) {
    renderer_slot_to_player(renderer_view$bottom_player)
  } else {
    switch(
      perspective,
      white = "white",
      black = "black",
      on_roll = position$on_roll,
      decision_maker = if (identical(resolved_decision, "take_pass")) {
        context$offer$receiver
      } else {
        position$on_roll
      }
    )
  }
  resolved_perspective <- normalize_board_perspective(resolved_perspective)
  if (
    !is.null(renderer_view) &&
    !identical(resolved_perspective, position$learner_player)
  ) {
    stop(
      paste0(
        "Initial learner-view policy requires `",
        position$learner_slot,
        "` to remain at the bottom; `perspective` must not rotate the board."
      ),
      call. = FALSE
    )
  }

  bottom_home_board_side <- if (is.null(renderer_view)) {
    NULL
  } else {
    renderer_view$bottom_home_board_side
  }
  point_labels_for <- if (is.null(renderer_view)) {
    NULL
  } else {
    renderer_slot_to_player(renderer_view$point_labels_for)
  }
  cube_display_side <- if (is.null(renderer_view)) {
    "left"
  } else if (renderer_view$cube_display_side %in% c("left", "right")) {
    renderer_view$cube_display_side
  } else {
    stop(
      paste0(
        "Accepted view cube_display_side `",
        renderer_view$cube_display_side,
        "` has no current W7 placement; supported values are `left` and `right`."
      ),
      call. = FALSE
    )
  }
  player_labels <- if (
    !is.null(position$player_labels) &&
    length(position$player_labels) == 2L
  ) {
    position$player_labels
  } else {
    c(white = "White", black = "Black")
  }

  plot <- render_board_preview(
    x = position,
    colors = colors,
    style = style,
    perspective = resolved_perspective,
    bottom_home_board_side = bottom_home_board_side,
    point_labels_for = point_labels_for,
    brand_text = brand_text,
    show_cube = TRUE,
    cube_display_side = cube_display_side,
    cube_offer = cube_offer,
    show_information = show_information,
    white_name = unname(player_labels[["white"]]),
    black_name = unname(player_labels[["black"]]),
    context = context,
    score_format = score_format
  )
  status_text <- position_status_label(position, context = context)
  accessible_text <- if (grepl("on roll", status_text, fixed = TRUE)) {
    status_text
  } else {
    paste0(
      position_player_label(position, position$on_roll),
      " on roll. ",
      status_text
    )
  }
  plot <- plot + ggplot2::labs(alt = accessible_text)

  displayed_offer <- if (!is.null(cube_offer)) cube_offer else context$offer
  cube_display <- resolve_cube_display(position, offer = displayed_offer)

  attr(plot, "backgammon_position") <- position
  attr(plot, "backgammon_context") <- context
  attr(plot, "backgammon_decision") <- resolved_decision
  attr(plot, "backgammon_perspective") <- resolved_perspective
  attr(plot, "backgammon_cube_display") <- cube_display
  attr(plot, "backgammon_accessible_text") <- accessible_text
  if (!is.null(renderer_view)) {
    attr(plot, "backgammon_renderer_view") <- renderer_view
    attr(plot, "backgammon_semantic_state_hash") <-
      position$renderer_semantic_state_hash
    attr(plot, "backgammon_view_hash") <- position$renderer_view_hash
  }
  plot
}

pending_offer_from_position <- function(position) {
  assert_backgammon_position(position)

  marker <- if (!is.null(position$action_marker)) {
    position$action_marker
  } else {
    position$dice_action
  }

  if (!identical(marker, "D")) {
    return(NULL)
  }

  proposed_cube_offer(position)
}
