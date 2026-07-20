# backgammonboard

`backgammonboard` is a focused R package for factual backgammon positions and
static board rendering.

## Implementation checkpoint

Completed in this checkpoint:

1. `normalize_xgid()`
2. `validate_xgid()`
3. `backgammon_position()`
4. opening and asymmetric coordinate fixtures
5. `board_colors()` and `board_style()`
6. canonical geometry and checker rendering preview

The current preview is intentionally internal. It exists so geometry and
checker placement can be visually tested before the public `ggboard()`
orchestration layer is implemented.

## Current public API

```r
normalize_xgid(x)
validate_xgid(x)
backgammon_position(x)
board_colors(name = "default", overrides = NULL)
board_style(name = "default", overrides = NULL)
```

## Visual development preview

```r
devtools::load_all()

plot <- backgammonboard:::render_board_preview(
  "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10",
  colors = board_colors("bms"),
  style = board_style("bms"),
  point_1_side = "right"
)

print(plot)
```

The geometry now includes equal-width outer rails, cream side panels, a left
panel reserved for the cube, and a right panel for borne-off markers and later
score information. Use `point_1_side = "left"` to test the alternate numbering
convention without changing factual position state.

For a named list of XGIDs, interactive previews, and PNG output, source
`dev/visual-smoke-test.R`.

Perspective, scores, pips, status, dice, cubes, moves, and `after_xgid` remain
for later contract steps.
