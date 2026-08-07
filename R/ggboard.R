#' Render a factual backgammon board from a complete XGID
#'
#' `ggboard()` resolves one conservative display context, applies independent
#' vertical and horizontal display transforms, and returns a static `ggplot`.
#'
#' @param x A complete XGID string or a factual `backgammon_position`.
#' @param colors A validated object created by [board_colors()].
#' @param style A validated object created by [board_style()].
#' @param movement_style Optional movement-overlay appearance created by
#'   [movement_overlay_style()]. `NULL` preserves the board preset's overlay.
#' @param moves Optional structured movements created by [board_moves()].
#' @param after_xgid Optional complete XGID used only to validate the applied
#'   checker layout. It does not replace the displayed before-position.
#' @param decision One of `"auto"`, `"checker_play"`, `"roll_double"`,
#'   `"take_pass"`, or `"none"`.
#' @param perspective One of `"decision_maker"`, `"on_roll"`, `"player_0"`,
#'   or `"player_1"`.
#' @param mirror_horizontal Whether to independently mirror the prepared layout
#'   left-to-right. This never changes player identity or the near player.
#' @param light_player Which factual player uses the light checker, die, and
#'   checker-associated badge palette. Use `"near_player"` to keep the bottom
#'   player light as perspective changes. This display choice never changes
#'   XGID decoding or factual ownership.
#' @param player_labels Display labels named `player_0` and `player_1`.
#' @param score_format Match score display format: `"away"`, `"raw"`, or
#'   `"both"`.
#' @param point_1_side Compatibility alias for the horizontal display control.
#'   Prefer `mirror_horizontal` in new code.
#' @param player_name_style Name-badge treatment: neutral package text or
#'   checker-associated BMS colors.
#'
#' @return An ordinary object inheriting from `ggplot`.
#' @export
#' @param match_id Optional 12-character GNU Match ID when `x` is a GNU Position ID.
ggboard <- function(
    x,
  match_id = NULL,
    colors = board_colors(),
    style = board_style(),
    moves = NULL,
    after_xgid = NULL,
    decision = "auto",
    perspective = "player_1",
    mirror_horizontal = FALSE,
    light_player = c("player_1", "player_0", "near_player"),
    player_labels = c(player_0 = "Foey", player_1 = "Homey"),
    score_format = "away",
    point_1_side = NULL,
    player_name_style = c("neutral", "checker"),
    movement_style = NULL) {
  x <- .resolve_ggboard_input(x, match_id = match_id)

  mirror_was_missing <- missing(mirror_horizontal)
  if (is.null(point_1_side)) {
    point_1_side <- if (isTRUE(mirror_horizontal)) "left" else "right"
  } else {
    point_1_side <- match.arg(point_1_side, c("right", "left"))
    compatible_mirror <- identical(point_1_side, "left")
    if (mirror_was_missing) {
      mirror_horizontal <- compatible_mirror
    } else if (!identical(isTRUE(mirror_horizontal), compatible_mirror)) {
      stop("`point_1_side` conflicts with `mirror_horizontal`.", call. = FALSE)
    }
  }
  light_player <- match.arg(light_player)
  player_name_style <- match.arg(player_name_style)
  if (inherits(x, "backgammon_render_position")) {
    stop("Internal render positions are not supported release inputs.", call. = FALSE)
  }
  position <- if (inherits(x, "backgammon_position")) x else backgammon_position(x)
  selected_moves <- normalize_move_overlay_input(moves, "moves")
  if (!is.null(after_xgid) && is.null(selected_moves)) {
    stop("`after_xgid` requires `moves` [after_xgid_without_moves]", call. = FALSE)
  }

  display <- resolve_display_context(
    position = position,
    moves = selected_moves,
    decision = decision,
    perspective = perspective,
    mirror_horizontal = mirror_horizontal,
    light_player = light_player,
    player_labels = player_labels,
    score_format = score_format
  )
  render_position <- as_render_position(position, display$player_labels)
  render_perspective <- to_render_player(display$near_player)
  cube_display_side <- if (identical(point_1_side, "left")) "right" else "left"

  if (!isTRUE(display$show_dice)) render_position$dice <- integer()
  render_context <- board_context("none")
  cube_offer <- NULL
  if (identical(display$decision, "roll_double")) {
    render_context <- board_context("cube_offer")
  } else if (identical(display$decision, "take_pass")) {
    cube_offer <- pending_offer_from_position(render_position)
    render_context <- board_context("cube_response", offer = cube_offer)
  }

  applied <- if (is.null(selected_moves)) {
    NULL
  } else {
    apply_board_moves(render_position, selected_moves)
  }
  if (!is.null(after_xgid)) validate_after_xgid(applied, after_xgid)

  plot <- render_board_preview(
    x = render_position,
    colors = colors,
    style = style,
    movement_style = movement_style,
    point_1_side = point_1_side,
    perspective = render_perspective,
    mirror_horizontal = display$mirror_horizontal,
    light_player = to_render_player(display$light_player),
    point_labels_for = render_perspective,
    brand_text = NULL,
    show_cube = isTRUE(display$show_normal_cube) || isTRUE(display$show_offered_cube),
    cube_offer = NULL,
    cube_display_side = cube_display_side,
    show_information = TRUE,
    white_name = unname(display$player_labels[[render_player_to_project_player("white")]]),
    black_name = unname(display$player_labels[[render_player_to_project_player("black")]]),
    context = render_context,
    score_format = display$score_format,
    player_name_style = player_name_style,
    moves = selected_moves,
    alternative_moves = NULL
  )

  accessible_text <- display_status_text(position, display)
  plot <- plot + ggplot2::labs(alt = accessible_text)
  public_display_position <- position_after_application(position, applied)
  rendered_cube <- resolve_cube_display(
    render_position,
    offer = if (identical(display$decision, "take_pass")) cube_offer else NULL
  )

  attr(plot, "backgammon_position") <- position
  attr(plot, "backgammon_display_position") <- public_display_position
  attr(plot, "backgammon_context") <- display
  attr(plot, "backgammon_decision") <- display$decision
  attr(plot, "backgammon_perspective") <- display$near_player
  attr(plot, "backgammon_near_player") <- display$near_player
  attr(plot, "backgammon_mirror_horizontal") <- display$mirror_horizontal
  attr(plot, "backgammon_light_player") <- display$light_player
  attr(plot, "backgammon_point_1_side") <- point_1_side
  attr(plot, "backgammon_player_name_style") <- player_name_style
  attr(plot, "backgammon_information_side") <- point_1_side
  attr(plot, "backgammon_cube_display_side") <- cube_display_side
  attr(plot, "backgammon_accessible_text") <- accessible_text
  attr(plot, "backgammon_cube_display") <- public_cube_display(rendered_cube)
  attr(plot, "backgammon_move_validation") <- if (is.null(applied)) NULL else list(
    die_validation_status = applied$die_validation_status,
    full_play_validation_status = applied$full_play_validation_status,
    after_xgid_checked = !is.null(after_xgid)
  )
  plot
}


public_cube_display <- function(display) {
  map_player <- function(value) {
    if (is.null(value)) return(NULL)
    if (value %in% c("white", "black")) render_player_to_project_player(value) else value
  }
  display$owner <- map_player(display$owner)
  display$offerer <- map_player(display$offerer)
  display$receiver <- map_player(display$receiver)
  display$placement <- switch(
    display$placement,
    white_side = paste0(render_player_to_project_player("white"), "_side"),
    black_side = paste0(render_player_to_project_player("black"), "_side"),
    offered_to_white = paste0("offered_to_", render_player_to_project_player("white")),
    offered_to_black = paste0("offered_to_", render_player_to_project_player("black")),
    display$placement
  )
  display
}


position_after_application <- function(position, applied) {
  if (is.null(applied)) return(position)
  result <- position
  result$points <- as.integer(applied$points)
  result$bar <- c(
    player_0 = unname(applied$bar[[project_player_to_render_player("player_0")]]),
    player_1 = unname(applied$bar[[project_player_to_render_player("player_1")]])
  )
  result$off <- c(
    player_0 = unname(applied$off[[project_player_to_render_player("player_0")]]),
    player_1 = unname(applied$off[[project_player_to_render_player("player_1")]])
  )
  result$point_occupancy <- signed_points_to_occupancy(result$points)
  result
}


display_status_text <- function(position, display) {
  label <- function(player) unname(display$player_labels[[player]])
  switch(
    display$decision,
    checker_play = if (length(position$dice) == 2L) {
      paste0(label(position$on_roll), " on roll, to play: ", paste(position$dice, collapse = "-"))
    } else {
      paste0(label(position$on_roll), " on roll")
    },
    roll_double = paste0(label(position$on_roll), " to decide: Roll or Double?"),
    take_pass = paste0(label(other_player(position$on_roll)), " to decide: Take or Pass?"),
    none = paste0(label(position$on_roll), " on roll")
  )
}


pending_offer_from_position <- function(position) {
  assert_backgammon_position(position)
  marker <- if (!is.null(position$action_marker)) position$action_marker else position$dice_action
  if (!identical(marker, "D")) return(NULL)
  proposed_cube_offer(position)
}
