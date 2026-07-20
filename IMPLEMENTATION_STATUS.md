# Implementation status

Contract version: 1.0
Package version: 0.0.0.9000

## Completed

1. `normalize_xgid()`
2. `validate_xgid()`
3. `backgammon_position()`
4. opening and asymmetric coordinate fixtures
5. `board_colors()` and `board_style()`
6. canonical geometry and checker rendering preview

## Current visual checkpoint

The internal development helper below renders canonical White-relative geometry
and checkers only:

```r
backgammonboard:::render_board_preview(
  xgid,
  point_1_side = "right"
)
```

The layout includes:

- equal-width outer rails using the center-bar color;
- cream side panels outside the playing fields;
- a left panel reserved for cube placement;
- a right panel for borne-off markers and later score information;
- numbered checker markers for borne-off counts;
- selectable point-1 placement on the right or left.

The preview intentionally does not yet include perspective, score, pip count,
status, dice, cube, move overlays, or `after_xgid`.

## Next

7. perspective
8. scores, pips, and status
9. dice
10. centered and owned cubes
