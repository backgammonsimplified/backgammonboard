# backgammonboard

`backgammonboard` validates complete XGIDs, constructs factual positions, and
renders static `ggplot` boards. Contract version 1.1 has one release path:

```text
complete XGID
-> factual backgammon_position
-> explicit or conservatively resolved display context
-> prepared layout
-> optional structured movements
-> ggplot
```

```r
library(backgammonboard)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
plot <- ggboard(xgid)
inherits(plot, "ggplot")
```

The fixed factual identities are `player_0` (XGID bottom player) and
`player_1` (XGID top player). The default display labels are Homey and Foey;
perspective changes screen placement, not identity.

Complete XGID, with or without the `XGID=` prefix, is the only release source
identifier. GNU identifiers, Engine Kit/Node conversion, AnkiGammon, and
RendererPosition are outside the package v1.1 boundary.

## Display context

`auto` is conservative: structured movements imply `checker_play`, otherwise
factual dice imply `checker_play`, otherwise the decision is `none`. Cube
questions must be explicit.

```r
ggboard(xgid_without_dice, decision = "roll_double")
ggboard(xgid_with_D_marker, decision = "take_pass")
ggboard(xgid, perspective = "player_1")
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
