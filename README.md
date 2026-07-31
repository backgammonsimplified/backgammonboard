# backgammonboard

`backgammonboard` is a focused R package for validated factual backgammon
positions and static `ggplot` board rendering.

## Public path

```r
library(backgammonboard)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
plot <- ggboard(xgid)

inherits(plot, "ggplot")
```

Accepted Engine Kit renderer envelopes are additive input:

```r
position <- renderer_position("renderer-position.json")
plot <- ggboard(position)
```

`renderer_position()` also accepts the JSON text itself or an already parsed
named list. It requires no Python or Engine Kit runtime dependency.

RendererPosition rendering uses the initial learner-view policy: the accepted
learner remains at the bottom, the opponent remains at the top, and point
labels remain learner-relative. Home-board left/right is an independent
horizontal mirror. Changing the canonical on-roll player moves the orange
arrow and dice and updates visible/accessible on-roll text; it never changes
the board orientation. Cube ownership follows the canonical player slot.

Checker colours and themes are appearance only. They never define learner,
opponent, on-roll player, dice ownership, cube ownership, or analysis
perspective. Any website-provided engine percentage or cube analysis must
identify its canonical player perspective explicitly.

The package accepts a complete XGID with or without the `XGID=` prefix. It
constructs stable White/Black factual state before applying perspective.
Changing perspective does not change checker ownership, bars, borne-off counts,
cube ownership, scores, or point occupancies.

```r
ggboard(xgid, perspective = "white")
ggboard(xgid, perspective = "black")
```

## Presets

Package defaults are neutral. Backgammon Made Simple styling is explicit:

```r
ggboard(
  xgid,
  colors = board_colors("bms"),
  style = board_style("bms")
)
```

## Cube and decision context

Ordinary rendering remains factual. A valid XGID `D` marker displays the
pending offered cube without changing the factual current cube value or owner
and without inventing a quiz question.

Instructional questions are explicit:

```r
ggboard(xgid_without_dice, decision = "roll_double")
ggboard(xgid_with_D_marker, decision = "take_pass")
```

The package supports factual cube values through 64. XGID maximum-cube metadata
is preserved separately and does not expand that package limit.

## Current public API

```r
ggboard()
normalize_xgid()
validate_xgid()
backgammon_position()
renderer_position()
board_colors()
board_style()
```

Move parsing, move application, `after_xgid`, overlays, and Shiny migration are
deferred to later milestones.
