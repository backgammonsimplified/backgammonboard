# backgammonboard

`backgammonboard` validates complete XGIDs, constructs factual positions, and
renders static `ggplot` boards. Contract version 1.2 has one release path:

```text
complete XGID
-> XGID source-role decode and fixed project-player mapping
-> factual backgammon_position
-> explicit or conservatively resolved display context
-> canonical Homey-near prepared layout
-> independent vertical and horizontal display transforms
-> optional structured movements
-> ggplot
```

```r
library(backgammonboard)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
plot <- ggboard(xgid)
inherits(plot, "ggplot")
```

The fixed factual identities are `player_0` (XGID top player, Foey) and
`player_1` (XGID bottom player, Homey). Homey is near by default. Perspective
changes only the near player; `mirror_horizontal` independently changes the
left-right arrangement. Neither display control changes factual identity.
The independent `light_player` control selects which factual player uses the
light visual palette; set it to `"near_player"` to keep the bottom player light.
Neutral centered and offered cubes remain on the board's vertical midline.
When the cube exponent is non-zero, an owned cube remains outside on its
factual owner's vertical side.

Complete XGID, with or without the `XGID=` prefix, is the only release source
identifier. GNU identifiers, Engine Kit/Node conversion, AnkiGammon, and
RendererPosition are outside the package v1.2 boundary.

## Display context

`auto` is conservative: structured movements imply `checker_play`, otherwise
factual dice imply `checker_play`, otherwise the decision is `none`. Cube
questions must be explicit.

```r
ggboard(xgid_without_dice, decision = "roll_double")
ggboard(xgid_with_D_marker, decision = "take_pass")
ggboard(xgid, perspective = "player_1")
ggboard(xgid, perspective = "player_0", mirror_horizontal = TRUE)
ggboard(xgid, perspective = "player_0", light_player = "near_player")
```

## Structured movements

The package does not parse source notation. Supply ordered atomic movements:

```r
moves <- board_moves(
  from = c(13, 6),
  to = c(8, 5),
  die = c(5, 1),
  label = c("first", "second")
)

ggboard(xgid, moves = moves, after_xgid = optional_after_xgid)
```

## Presets

Package defaults are neutral. Backgammon Made Simple styling is explicit:

```r
ggboard(xgid, colors = board_colors("bms"), style = board_style("bms"))
```

## Public API

```r
ggboard()
backgammon_position()
normalize_xgid()
validate_xgid()
board_colors()
board_style()
board_moves()
```
