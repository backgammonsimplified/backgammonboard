# backgammonboard gallery

A copy-pasteable gallery of the main rendering modes in `backgammonboard`.
Each example returns an ordinary `ggplot`, so the result can be displayed,
composed with other `ggplot2` layers, or exported with `ggsave()`.

## Setup

```r
library(backgammonboard)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
```

## Default board

The default call renders the factual position with the package's neutral
presentation.

```r
ggboard(xgid)
```

## Backgammon Simplified preset

Use the explicit Backgammon Simplified color and style presets when the board
is being shown as part of the Backgammon Simplified project.

```r
ggboard(
  xgid,
  colors = board_colors("bs"),
  style = board_style("bs")
)
```

## Change perspective

Perspective changes which factual player is shown near the viewer. It does not
change checker ownership or the factual position.

```r
ggboard(
  xgid,
  perspective = "player_0"
)
```

## Mirror the board horizontally

Horizontal mirroring is independent of player perspective. This is useful when
matching a teaching diagram, stream layout, or analysis convention.

```r
ggboard(
  xgid,
  perspective = "player_0",
  mirror_horizontal = TRUE
)
```

## Keep the near player light

The visual checker palette can follow the near player while factual player
identity remains unchanged.

```r
ggboard(
  xgid,
  perspective = "player_0",
  light_player = "near_player"
)
```

## Use player names

Player labels are display-only metadata and can be changed without altering the
position.

```r
ggboard(
  xgid,
  player_labels = c(
    player_0 = "Opponent",
    player_1 = "You"
  )
)
```

## Structured checker movement

Movement overlays use ordered atomic movements. The renderer shows supplied
movement geometry but does not claim complete-play legality or strategic
correctness.

```r
moves <- board_moves(
  from = c(13, 6),
  to = c(8, 5),
  die = c(5, 1),
  label = c("first", "second")
)

ggboard(
  xgid,
  moves = moves
)
```

## GNUID input

Complete GNUIDs use the same `ggboard()` entry point when
`backgammoncalculator` 0.2.0 or later is installed.

```r
gnuid <- "4HPwATDgc/ABMA:8IhuACAACAAE"

ggboard(gnuid)
```

## Compose with ggplot2

Because `ggboard()` returns an ordinary `ggplot`, standard `ggplot2`
composition remains available.

```r
p <- ggboard(
  xgid,
  colors = board_colors("bs"),
  style = board_style("bs")
)

p + ggplot2::labs(
  title = "Backgammon position"
)
```

## Export a board

```r
p <- ggboard(xgid)

ggplot2::ggsave(
  filename = "backgammon-position.png",
  plot = p,
  width = 12,
  height = 7,
  dpi = 200
)
```

## What the renderer guarantees

The gallery demonstrates display controls, factual position rendering, and
structured movement visualization. `backgammonboard` deliberately keeps factual
position state separate from display context. It does not evaluate strategy,
select moves, or certify complete-play legality.

For the full public interface, see the repository README and the generated R
help for `ggboard()`, `backgammon_position()`, `board_colors()`,
`board_style()`, and `board_moves()`.
