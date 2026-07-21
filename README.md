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
board_colors()
board_style()
```

Move parsing, move application, `after_xgid`, overlays, and Shiny migration are
deferred to later milestones.
