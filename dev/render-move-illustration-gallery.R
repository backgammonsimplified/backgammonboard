arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) stop("Supply zero or one output directory path.", call. = FALSE)
output_dir <- if (length(arguments) == 1L) arguments[[1L]] else file.path("inst", "gallery", "move-illustration")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

devtools::load_all(".", quiet = TRUE)

gallery_position <- function(player = "white", white = integer(), black = integer(), white_bar = 0L, black_bar = 0L, cube_value = 1L, cube_owner = "center", offer = FALSE) {
  position <- backgammon_position("XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10")
  points <- integer(24)
  if (length(white)) points[as.integer(names(white))] <- as.integer(white)
  if (length(black)) points[as.integer(names(black))] <- -as.integer(black)
  position$points <- as.integer(points)
  position$bar <- c(white = as.integer(white_bar), black = as.integer(black_bar))
  position$off <- c(white = as.integer(15L - sum(points[points > 0L]) - white_bar), black = as.integer(15L - sum(abs(points[points < 0L])) - black_bar))
  position$on_roll <- player
  position$dice <- integer()
  position$action_marker <- if (offer) "D" else "00"
  position$dice_action <- position$action_marker
  position$cube_value <- as.integer(cube_value)
  position$cube_owner <- cube_owner
  position
}

case <- function(title, explanation, position, perspective = "white", move = NULL, brand_text = NULL) {
  list(title = title, explanation = explanation, position = position, perspective = perspective, move = move, brand_text = brand_text)
}

cases <- list(
  ordinary_board = case("Ordinary board", "A game-like opening layout with no instructional overlay.", backgammon_position("XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10")),
  branded_ordinary = case("Branded ordinary move", "The only branded case; the lower rail reads exactly Backgammon / Simplified.", gallery_position(white = c(`13` = 1L, `6` = 1L)), move = "13/8", brand_text = "Backgammon\nSimplified"),
  single_hit = case("Single hit", "A confirmed hit sends the lone opposing checker to the bar.", gallery_position(white = c(`13` = 1L), black = c(`8` = 1L)), move = "13/8*"),
  double_hit = case("Double hit", "Two distinct arrows and hit markers show two checkers being hit in one turn.", gallery_position(white = c(`13` = 1L, `12` = 1L), black = c(`8` = 1L, `7` = 1L)), move = "13/8* 12/7*"),
  hit_near_stack = case("Hit beside stack", "The hit is adjacent to a protected stack, keeping the tactical context visible.", gallery_position(white = c(`13` = 1L, `6` = 3L), black = c(`8` = 1L, `7` = 4L)), move = "13/8*"),
  move_onto_stack = case("Move onto stack", "The translucent destination ghost sits above the existing friendly stack.", gallery_position(white = c(`13` = 1L, `8` = 3L)), move = "13/8"),
  bar_entry = case("Bar entry", "A white checker enters from the centred lower bar lane onto point 24.", gallery_position(white_bar = 1L), move = "bar/24"),
  enter_and_hit = case("Enter and hit", "A bar entry lands on a blot and visibly records the hit.", gallery_position(white_bar = 1L, black = c(`24` = 1L)), move = "bar/24*"),
  multiple_on_bar = case("Multiple checkers on bar", "A three-checker bar stack stays centred inside its player's half of the bar lane.", gallery_position(white_bar = 3L, black = c(`6` = 2L))),
  both_bars = case("Both players on bar", "Each player's bar stack occupies its own correctly oriented half of the center lane.", gallery_position(white_bar = 2L, black_bar = 2L)),
  multi_checker_turn = case("Multiple checkers", "Two separate moves in one turn retain distinct arrows and destinations.", gallery_position(white = c(`13` = 1L, `6` = 1L)), move = "13/8 6/5"),
  four_checker_turn = case("Four-checker turn", "Repeated notation displays four parallel checker movements from one stack.", gallery_position(white = c(`13` = 4L)), move = "13/8(4)"),
  bearing_off = case("Bearing off", "An arrow exits to the off tray and the resulting off count remains visible.", gallery_position(white = c(`6` = 1L)), move = "6/off"),
  large_stacks = case("Large stacks", "Compressed stack counts show a high-density, game-like late position.", gallery_position(white = c(`6` = 8L, `13` = 5L), black = c(`19` = 7L, `24` = 4L))),
  centered_cube = case("Centred cube", "The cube is neutral at the center before either player owns it.", gallery_position(white = c(`13` = 2L), black = c(`12` = 2L))),
  white_owned_cube = case("White-owned cube", "An owned cube appears on White's side of the board.", gallery_position(white = c(`13` = 2L), black = c(`12` = 2L), cube_value = 2L, cube_owner = "white")),
  black_owned_cube = case("Black-owned cube", "An owned cube appears on Black's side of the board.", gallery_position(white = c(`13` = 2L), black = c(`12` = 2L), cube_value = 2L, cube_owner = "black")),
  offer_white_orientation = case("Pending offer: White view", "White offers; the cube is in White's right half and Black's left half.", gallery_position(player = "white", white = c(`13` = 2L), black = c(`12` = 2L), cube_value = 2L, cube_owner = "white", offer = TRUE), perspective = "white"),
  offer_black_orientation = case("Pending offer: Black view", "Black offers; the cube is in Black's right half and White's left half.", gallery_position(player = "black", white = c(`13` = 2L), black = c(`12` = 2L), cube_value = 2L, cube_owner = "black", offer = TRUE), perspective = "black"),
  reversed_move = case("Reversed orientation", "A Black-perspective ordinary move verifies orientation-aware arrow anchoring.", gallery_position(player = "black", black = c(`12` = 1L)), perspective = "black", move = "12/17")
)

html_escape <- function(x) gsub("\"", "&quot;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
items <- character()
for (case_name in names(cases)) {
  item <- cases[[case_name]]
  plot <- ggboard(item$position, colors = board_colors("bms"), style = board_style("bms"), decision = "none", perspective = item$perspective, show_information = FALSE, brand_text = item$brand_text, moves = item$move)
  output <- file.path(output_dir, paste0(case_name, ".svg"))
  grDevices::svg(output, width = 10.625, height = 7.5, bg = "white")
  print(plot)
  grDevices::dev.off()
  stopifnot(file.exists(output), file.info(output)$size > 0)
  items <- c(items, paste0("<figure id=\"", case_name, "\"><figcaption><strong>", html_escape(item$title), "</strong><br><span>", html_escape(item$explanation), "</span></figcaption><img src=\"", case_name, ".svg\" alt=\"", html_escape(item$title), ": ", html_escape(item$explanation), "\"></figure>"))
}
writeLines(c("<!doctype html><html><head><meta charset=\"utf-8\"><title>Backgammon board review gallery</title><style>body{font:16px system-ui;max-width:1100px;margin:2rem auto;padding:0 1rem}figure{margin:2.5rem 0;border-bottom:1px solid #ddd;padding-bottom:2rem}figcaption{margin-bottom:.75rem}span{color:#444}img{display:block;width:100%;height:auto;border:1px solid #ddd}</style></head><body><h1>Backgammon board review gallery</h1><p>All rendered positions appear below in review order.</p>", items, "</body></html>"), file.path(output_dir, "index.html"), useBytes = TRUE)
message("PASS: expanded move-illustration gallery rendered.")
