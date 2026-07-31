# Move Overlay Rendering Contract

**Contract version:** 1.1
**Status:** implemented for selected and alternative checker plays

## Scope

The renderer accepts one selected checker play and one optional alternative
checker play. Each play is parsed by `board_moves()` and validated against the
starting factual checker state by `apply_board_moves()` before drawing.

This checkpoint does not validate dice usage or full-play legality. It renders
only checker plays that pass the deterministic move-application layer.

## Board-scale resilience

Point outlines use a fixed rendered line width rather than a board-coordinate
polygon width. The BMS point outline is raised to a visible minimum of 0.38 mm
for direct small-width exports.

Reduced-size output must still be reviewed at:

```text
1200 px
768 px
480 px
320 px
```

## Arrow construction

Every move segment is drawn in this order:

```text
1. translucent ghost checker at the origin
2. dark outer halo
3. light inner halo
4. semantic coloured arrow
```

The default relative line widths are:

```text
dark halo     2.2w
light halo    1.6w
colour        1.0w
```

Arrowheads scale with the same layered hierarchy so the coloured arrowhead
does not cover both halo arrowheads completely.

The BMS semantic colours are:

```text
selected      #D9653B
alternative   #6E557A
hit           #923B45
order label   #111B35
marker fill   #FFFFFF
marker border #111B35
light halo    #FFFDF8
dark halo     #081126
```

## Structure beyond colour

```text
selected play       solid arrow with circular order markers
alternative play    dashed arrow with square order markers
multi-part play     numbered markers in atomic step order
confirmed hit       explicit bordered multiplication marker
```

A single-part play does not require a number because no order ambiguity exists.

## Geometry

The base checker layer shows the selected play's resulting position. Arrow
origins retain translucent ghost checkers so the starting locations remain
legible. Arrow endpoints are derived from the exposed checker before and after
each atomic step. Compound moves are simulated step by step, so chained
movement, bar entry, bearing off, hits, and repeated movements receive
deterministic perspective-aware coordinates.

Duplicate paths receive curvature separation. Overlay geometry never changes
the factual starting position.

## Layer order

The board, checkers, dice, cube, and decorative brand are drawn first. Move
overlays are drawn above those surfaces. Information bands remain above the
board area and are not part of the overlay coordinate system.

## Required review cases

```text
point-to-point
compound chain
repeated movement
bar entry
bearing off
confirmed hit
selected plus alternative
White perspective
Black perspective
1200, 768, 480, and 320 px outputs
```

## Deferred

```text
after_xgid comparison
dice-distance validation
higher-die and maximum-dice legality
multiple ranked alternatives
collision-avoidance optimization beyond duplicate-path curvature
```
