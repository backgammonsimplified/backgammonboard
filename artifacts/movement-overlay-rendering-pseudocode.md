# Movement Overlay Rendering Rules and Pseudocode

This is a language-neutral specification of the current backgammonboard movement-overlay renderer. It is intended to be complete enough to reproduce the behavior in JavaScript, Canvas, SVG, or another frontend renderer.

## 1. Inputs and coordinate conventions

Required inputs:

- `startingPosition`: the factual board before any move is applied.
- `moves`: ordered atomic moves. Each move has `from`, `to`, and optionally `die` and `role`.
- `boardGeometry`: point centers, point row (`top` or `bottom`), field bounds, bar bounds, off-tray anchors, board width, and board height.
- `boardStyle`: checker radius, checker ring width, checker margin, and checker stack step.
- `overlayStyle`: ghost and arrow appearance.
- `perspective` and `mirrorHorizontal`: display-only transforms.

Moves use mover-relative point numbers at the public boundary. Convert player 0 / Black moves to factual point IDs with `point = 25 - moverRelativePoint`. Player 1 / White point IDs are unchanged.

Build all overlay geometry first in the canonical orientation:

```text
near player       = player_1 / White
point 1 side      = right
horizontal mirror = false
```

Only after all source assignment, destination assignment, and arrow trimming are complete should the display transforms be applied.

## 2. Non-negotiable visual semantics

```text
drawn board position = startingPosition
simulated move state = used only to find successive source anchors and validate moves
```

- Never remove or relocate factual starting checkers in the displayed board.
- A destination ghost marks every atomic move's landing slot.
- A hit does not remove the displayed opposing blot. Draw the ghost above the blot.
- A move from the bar leaves the displayed factual checker on the bar.
- A chained move may start from a ghost created by an earlier atomic move.
- Do not draw move notation, separate hit crosses, or step labels inside the board.
- Ghost styling is neutral and identical for both players.
- Straight arrows are the default. Adaptive quadratic curvature is applied only
  when it is enabled and two or more visible paths conflict.

## 3. Validate and simulate the ordered moves

Before rendering, validate the complete ordered atomic sequence:

```pseudocode
function validateAndApplyMoves(startingPosition, moves):
    require exactly 24 point occupancies
    require valid bar/off counts and a valid on-roll player

    if moves are mover-relative and mover is player_0:
        transform every point P to 25 - P

    for each atomic move in order:
        if a mover checker is on the bar:
            require this move to start on the bar
        require the source to contain a mover checker
        require movement in the mover's legal direction
        require the destination not to contain two or more opposing checkers
        if a die is supplied:
            require the atomic distance to equal that die
        apply the atomic move to the simulated state
        if landing on one opposing checker:
            mark hitConfirmed and put that checker on the simulated bar

    return applied atomic steps and final simulated state
```

Rendering relies on the exact atomic order. A chain such as M17 must be supplied chronologically:

```text
13 -> 11
13 -> 11
11 -> 9
11 -> 9
```

This allows the final two source anchors to be the checkers that just landed on point 11.

## 4. Find source and provisional destination anchors

Maintain `state = clone(startingPosition)`. Process each validated atomic move in order.

### Exposed checker anchor

```pseudocode
function exposedCheckerAnchor(checkersAtLocation, mover):
    moverCheckers = checkersAtLocation owned by mover
    if moverCheckers is empty:
        return NONE

    if location is on bottom row:
        return checker with maximum y       // exposed toward board center
    else:
        return checker with minimum y       // exposed toward board center
```

The checker layout uses the current simulated state and the renderer's normal visible-stack cap. Therefore repeated removals advance through the visible source stack. If a tall stack is compressed, repeated anchors may overlap until its count falls within the visible range.

### Generic location anchor

```pseudocode
function locationAnchor(state, location, mover):
    layout = layoutCheckers(state)

    if location.type == POINT:
        anchor = exposedCheckerAnchor(layout.checkersOn(location.point), mover)
        if anchor exists:
            return anchor

        point = boardGeometry.point(location.point)
        depth = min(2.25, fieldHeight / 3)
        y = fieldYMin + depth if point.row == BOTTOM
            else fieldYMax - depth
        return (point.x, y)

    if location.type == BAR:
        anchor = exposedCheckerAnchor(layout.barCheckers, mover)
        if anchor exists:
            return anchor

        x = midpoint(barXMin, barXMax)
        y = fieldYMin + checkerRadius if mover is bottom player
            else fieldYMax - checkerRadius
        return (x, y)

    if location.type == OFF:
        if mover has a displayed off marker:
            return center of that marker

        x = midpoint(rightMarginXMin, rightMarginXMax)
        y = bottomOffMarkerY if mover is bottom player
            else topOffMarkerY
        return (x, y)
```

### Build one provisional segment per atomic move

```pseudocode
segments = []
state = clone(startingPosition)

for step in appliedMoves in atomic order:
    source = locationAnchor(state, step.from, mover)

    nextState = applyOnlyThisStep(state, step)
    provisionalDestination = locationAnchor(nextState, step.to, mover)

    if step hits an opposing blot:
        blot = locationAnchor(state, step.to, opponent)
        direction = +1 if destination point is on bottom row else -1
        provisionalDestination = (
            blot.x,
            blot.y + direction * checkerStackStep
        )

    append segment with:
        stepId             = atomic index
        sourceToken        = formatted factual "from/to"
        sourceCenter       = source
        lineStart          = source
        ghostCenter        = provisionalDestination
        lineEnd            = provisionalDestination
        destinationCenter  = provisionalDestination
        destinationType    = POINT or OFF
        destinationPoint   = factual point ID, when applicable
        lineType           = SOLID for selected move; DASHED for alternative
        hitConfirmed       = step hit status

    state = nextState
```

The provisional point and off destinations are normalized by the shared assignment rules below.

## 5. Assign point destination slots

Group segments by `(destinationType, destinationPoint)`. Point destinations use the factual starting occupancy, not the simulated occupancy after departures:

```pseudocode
function factualPointSlots(group, destinationPoint):
    point = boardGeometry.point(destinationPoint)
    initialCount = abs(startingPosition.points[destinationPoint])

    if point.row == BOTTOM:
        direction = +1
        baseY = fieldYMin + checkerMargin + checkerRadius
    else:
        direction = -1
        baseY = fieldYMax - checkerMargin - checkerRadius

    for arrivalIndex from 1 through group.length:
        level = min(initialCount + arrivalIndex, 6)
        slotX[arrivalIndex] = point.x
        slotY[arrivalIndex] =
            baseY + direction * (level - 1) * checkerStackStep

    return slots
```

Consequences:

- An empty destination receives levels 1, 2, 3, and so on.
- A destination with three factual checkers receives levels 4, 5, 6, 6, and so on.
- Every arrival beyond visible level 6 overlaps at level 6.
- An opposing blot counts as one factual checker, so a hit ghost occupies level 2 directly above it.
- Departures from the destination during the illustrated play do not lower these ghost slots because the displayed board still shows the factual starting checkers.

### 5.1 Repeated moves from one source location

Detect this case when every segment in the destination group has the same source location, using the portion before `/` in `sourceToken`.

```pseudocode
sourceRow = TOP if mean(group.sourceCenter.y) >= fieldMidY else BOTTOM
destinationRow = TOP if mean(slots.y) >= fieldMidY else BOTTOM

if sourceRow == destinationRow:
    // Applies at the top and bottom, for either player.
    sourceOrder = sort group by:
        1. edgeDepth(sourceCenter.y) ascending
        2. stepId ascending

    slotOrder = sort slots by:
        1. edgeDepth(slot.y) ascending
        2. original slot index ascending

    pair sourceOrder[i] with slotOrder[i]
    // Visually: top-to-top and bottom-to-bottom; arrows do not cross.
else:
    pair each segment with the next slot in atomic move order
    // Between rows, each newly exposed source maps to the next arrival slot.
```

Here:

```pseudocode
edgeDepth(y) = min(abs(y - fieldYMin), abs(fieldYMax - y))
```

This same-row rule fixes both M25's top-row `18 -> 16` pair and M27's bottom-row `7 -> 9` pair.

### 5.2 Multiple different source locations landing together

When multiple source points land on one destination, prefer sources already on the destination row, then use edge depth and horizontal proximity to reduce crossings:

```pseudocode
for each source segment:
    sourceRow = TOP if sourceCenter.y >= fieldMidY else BOTTOM
    crossesBoard = sourceRow != destinationRow
    sourceDepth = edgeDepth(sourceCenter.y)
    horizontalDistance = abs(sourceCenter.x - mean(slots.x))

sourceOrder = sort segments by:
    1. crossesBoard ascending        // same-row source first
    2. sourceDepth ascending
    3. horizontalDistance ascending
    4. stepId ascending

slotOrder = sort slots by:
    1. edgeDepth(slot.y) ascending
    2. original slot index ascending

pair sourceOrder[i] with slotOrder[i]
```

After pairing, overwrite each segment's `ghostCenter`, untrimmed `lineEnd`, and `destinationCenter` with its assigned slot.

## 6. Assign bearing-off destination slots

Bearing-off ghosts share the mover's off-tray x coordinate.

```pseudocode
function offSlots(group, mover):
    existing = startingPosition.off[mover]
    moverIsBottom = mover == canonicalPerspective
    direction = +1 if moverIsBottom else -1
    baseY = bottomOffMarkerY if moverIsBottom else topOffMarkerY

    if existing > 0:
        levels = [1, 2, ..., group.length]
        // First ghost is above the existing checker or numbered marker.
    else:
        levels = [0, 1, ..., group.length - 1]
        // First ghost occupies the empty off-marker location.

    slots[i] = (offTrayX, baseY + direction * levels[i] * checkerStackStep)
```

Order sources and off slots to reduce crossings:

```pseudocode
sourceOrder = sort segments by:
    1. edgeDepth(sourceCenter.y) ascending
    2. abs(sourceCenter.x - offTrayX) ascending
    3. stepId ascending

slotOrder = sort off slots by:
    1. edgeDepth(slot.y) ascending
    2. original slot index ascending

pair sourceOrder[i] with slotOrder[i]
```

This sends sources nearest the board edge and off tray to the nearest available off slot, preventing crossed bearing-off arrows where possible. Always draw off ghosts, including above an existing checker or numbered off marker.

## 7. Detect conflicts and assign adaptive curvature

First compute straight, checker-trimmed provisional segments for conflict
detection. Two paths conflict when any of these conditions holds:

```pseudocode
1. Their directions are within arrowCollinearAngleTolerance (ignoring reversal),
   their projected overlap / shorter path length is at least
   arrowOverlapThreshold, and the maximum symmetric endpoint-to-line lateral
   separation is at most 0.60 * checkerRadius.

2. They have the same stable source ID, their visible starts are within
   0.25 * checkerRadius, and their directed angle is within the tolerance.

3. They have the same stable destination ID, their visible ends are within
   0.25 * checkerRadius, and their directed angle is within the tolerance.
```

An isolated crossing is not a conflict. Build an undirected conflict graph and
use its connected components as curve groups. Within each component, sort
deterministically by move sequence, source ID, then destination ID.

```pseudocode
if group size == 2:
    curveLevels = [+arrowCurveTwoPathSplit, -arrowCurveTwoPathSplit]
else:
    curveLevels = [0, +1, -1, +2, -2, ...]

function magnitude(level):
    if level == 0: return 0
    if abs(level) < 1:
        value = arrowCurveOffset * abs(level)
    else:
        value = arrowCurveOffset + (abs(level) - 1) * arrowCurveStep
    return sign(level) * min(value, arrowCurveMax)

physicalOffset = magnitude(level) * checkerRadius
directUnit = normalize(destinationCenter - sourceCenter)
perpendicular = (-directUnit.y, directUnit.x)
controlPoint = midpoint(sourceCenter, destinationCenter) +
               perpendicular * physicalOffset
```

When curvature is disabled or an arrow has no conflict, its level and offset
are zero and its control point is the direct midpoint.

## 8. Trim the arrow endpoints

Keep the untrimmed source, ghost, and destination centers for metadata. Trim only the drawn shaft endpoints.

```pseudocode
sourceClearance =
    checkerOuterRadius + checkerOuterRingWidth + arrowCheckerGap
destinationClearance =
    checkerOuterRadius + checkerOuterRingWidth + arrowCheckerGap

require destinationClearance >= 0

for segment in segments:
    vector = untrimmedLineEnd - untrimmedLineStart
    distance = length(vector)
    if distance <= 0:
        continue

    sourceTangent = normalize(controlPoint - sourceCenter)
    destinationTangent = normalize(destinationCenter - controlPoint)
    sourceTrim = min(sourceClearance, 0.20 * distance)
    destinationTrim = min(destinationClearance, 0.20 * distance)

    drawnStart = sourceCenter + sourceTangent * sourceTrim
    drawnTip = destinationCenter - destinationTangent * destinationTrim

    // Preserve both endpoint tangent directions after trimming.
    drawnControl = intersection(
        ray(drawnStart, sourceTangent),
        ray(drawnTip, -destinationTangent)
    )
    if those rays are parallel:
        drawnControl = midpoint(drawnStart, drawnTip)
```

Interpretation of `arrowCheckerGap`:

- Positive: stop outside the ghost edge by that additional distance.
- Zero: stop at the configured checker edge clearance.
- Negative: move the arrow tip inside the ghost toward its center.
- The gallery uses `-0.20`, placing the tip roughly halfway between the outer edge and center.
- Trimming is capped at 20% from either end so very short arrows do not invert.

## 9. Construct the quadratic arrow

Construct the shaft as a quadratic Bezier plus a filled convex triangular head.
The trimmed destination endpoint is the triangle's point.

```pseudocode
function arrowParts(drawnStart, drawnControl, drawnTip, style):
    shaft(t) = (1-t)^2 * drawnStart +
               2*(1-t)*t * drawnControl +
               t^2 * drawnTip
    sample shaft for t in [0, 1] and draw it as one rounded path

    unit = normalize(drawnTip - drawnControl) // final Bezier tangent
    perpendicular = (-unit.y, unit.x)

    baseCenter = drawnTip - style.arrowheadLength * unit
    halfWidth = style.arrowheadWidth / 2

    headVertices = [
        drawnTip,
        baseCenter + halfWidth * perpendicular,
        baseCenter - halfWidth * perpendicular
    ]

    head = filledPolygon(headVertices)
    return { shaft, head }
```

Rendering details:

- The triangle is convex and pointed.
- Use rounded stroke caps and joins. The filled triangle retains a pointed tip.
- Draw the shaft all the way to the triangle point; the filled head covers the final section.
- Use the configured line cap on the shaft. The gallery uses a rounded tail.
- Use the same configured line width for the main shaft and arrowhead polygon stroke.
- Selected shafts are solid. Alternative shafts are dashed; their triangular heads remain filled.

### Optional contrast outline

```pseudocode
if arrowOutlineWidth > 0:
    draw shaft and filled head in arrowOutlineColour
    outlinePassWidth = arrowWidth + 2 * arrowOutlineWidth

draw shaft and filled head again in arrowColour
mainPassWidth = arrowWidth
```

Both passes use `arrowAlpha`, the configured shaft line cap, and a rounded polygon join. The current review gallery disables this outline with `arrowOutlineWidth = 0`.

## 10. Draw neutral ghost checkers

Every ghost has exactly the factual checker's outer-body radius:

```pseudocode
ghostRadius = checkerOuterRadius
```

Do not enlarge the radius to account for the outline. The outline is centered on the radius path.

```pseudocode
function drawGhost(center, style):
    if style.ghostFill is absent:
        fill = none
    else:
        fill = colorWithAlpha(style.ghostFill, style.ghostFillAlpha)

    draw circle(center, ghostRadius, fill, no stroke)

    patternRadius = ghostRadius - style.ghostGridInset
    require patternRadius > 0

    xOffsets = evenlySpaced(
        -patternRadius, +patternRadius, style.ghostGridCols
    )
    yOffsets = evenlySpaced(
        -patternRadius, +patternRadius, style.ghostGridRows
    )

    for every Cartesian pair (dx, dy):
        if sqrt(dx*dx + dy*dy) <= patternRadius:
            draw filled dot at center + (dx, dy), using:
                colour = ghostDotColour
                alpha = ghostDotAlpha
                size = ghostDotSize

    draw circle outline at ghostRadius using:
        colour = ghostOutline
        width = ghostOutlineWidth
        line style = solid
```

The same neutral ghost style is used for both players and over both point colours. Player ownership is communicated by the factual source checker and arrow, not by ghost colour.

## 11. Layer order

Use this exact paint order:

```text
1. board, points, bars, trays, and rails
2. alternative-move arrows
3. selected-move arrows
4. factual starting checkers
5. dice, cube, branding, and information text
6. alternative-move ghosts
7. selected-move ghosts
```

Important consequences:

- Arrows are below factual and ghost checkers.
- Ghosts remain above factual checkers so a hit destination is visible.
- Selected overlays are above alternatives.
- A chained arrow may geometrically begin at an earlier ghost; the ghost paints over
  its endpoint.

Within one arrow, draw the optional contrast pass first and the main-colour pass second.

## 12. Display transforms

Resolve all ordering and geometry in the canonical orientation first. Then transform the complete overlay with the rest of the board.

```pseudocode
if mirrorHorizontal:
    for every x-like value:
        x = boardXMin + boardXMax - x

if nearPlayer == player_0:
    for every y-like value:
        y = boardYMin + boardYMax - y
```

Transform all of these consistently:

```text
drawnStart, drawnTip,
sourceCenter, ghostCenter, destinationCenter,
ghost dots, hit metadata, and any overlay labels
```

Horizontal mirroring and vertical viewpoint changes are display-only. They must not change the factual move, source/destination assignment, player ownership, or move order.

## 13. Configurable style object

Equivalent language-neutral signature:

```pseudocode
movementOverlayStyle(
    ghostFill = NONE,
    ghostFillAlpha = 0.20,
    ghostOutline = "#000000",
    ghostOutlineWidth = 1.5,
    ghostDotColour = "#65707A",
    ghostDotAlpha = 1,
    ghostGridRows = 7,
    ghostGridCols = 7,
    ghostDotSize = 1,
    ghostGridInset = 0.15,
    arrowColour = "#D95F32",
    arrowAlpha = 1,
    arrowWidth = 2,
    arrowLineEnd = "round",
    arrowOutlineColour = "#000000",
    arrowOutlineWidth = 0,
    arrowheadLength = 0.18,
    arrowheadWidth = 0.12,
    arrowCheckerGap = 0.05,
    arrowCurveEnabled = false,
    arrowCurveOffset = 0.22,
    arrowCurveStep = 0.18,
    arrowCurveMax = 0.65,
    arrowCollinearAngleTolerance = 8,
    arrowOverlapThreshold = 0.40,
    arrowCurveTwoPathSplit = 0.5
)
```

Validation rules:

- Alpha values are in `[0, 1]`.
- Outline widths, dot size, arrow width, and arrowhead dimensions are positive.
- Grid inset and arrow outline width are non-negative.
- Grid rows and columns are integers of at least 2.
- Arrow checker gap may be any finite signed number, provided the resulting destination clearance is not negative.
- Arrow line cap is `butt`, `round`, or `square`.

### Resolved BMS renderer values when no style is supplied

The function signature above is also useful for constructing a custom style. The renderer resolves `movementStyle = null` to these BMS-specific values:

```pseudocode
ghostFill = "#D4D8DC"
ghostFillAlpha = 0.88
ghostOutline = "#27313B"
ghostOutlineWidth = 1.25
ghostDotColour = "#65707A"
ghostDotAlpha = 1
ghostGridRows = 7
ghostGridCols = 7
ghostDotSize = 0.62
ghostGridInset = 0.015

arrowColour = "#C94F2C"
arrowAlpha = 1
arrowWidth = 1.8
arrowLineEnd = "round"
arrowOutlineColour = "#081126"
arrowOutlineWidth = 0.405
arrowheadLength = 0.18
arrowheadWidth = 0.12
arrowCheckerGap = 0
arrowCurveEnabled = false
arrowCurveOffset = 0.22
arrowCurveStep = 0.18
arrowCurveMax = 0.65
arrowCollinearAngleTolerance = 8
arrowOverlapThreshold = 0.40
arrowCurveTwoPathSplit = 0.5
```

`arrowOutlineWidth` is derived as `arrowWidth * (1.45 - 1) / 2`.

### Exact review-gallery values

The current reviewed movement gallery overrides the generic signature with:

```pseudocode
ghostFill = NONE
ghostFillAlpha = 0.20              // irrelevant while fill is NONE
ghostOutline = "#000000"
ghostOutlineWidth = 1.5
ghostDotColour = "#65707A"
ghostDotAlpha = 1
ghostGridRows = 7
ghostGridCols = 7
ghostDotSize = 1
ghostGridInset = 0.04

arrowColour = "#D95F32"
arrowAlpha = 1
arrowWidth = 2
arrowLineEnd = "round"
arrowOutlineColour = "#000000"
arrowOutlineWidth = 0
arrowheadLength = 0.08
arrowheadWidth = 0.05
arrowCheckerGap = -0.20
arrowCurveEnabled = false
arrowCurveOffset = 0.22
arrowCurveStep = 0.18
arrowCurveMax = 0.65
arrowCollinearAngleTolerance = 8
arrowOverlapThreshold = 0.40
arrowCurveTwoPathSplit = 0.5
```

## 14. Porting invariants and minimum tests

A compatible implementation should test all of these:

```text
1. Displayed factual checkers remain at their starting locations.
2. A hit ghost is one stack level above the displayed opposing blot.
3. Bar-entry arrows start at the displayed bar checker.
4. Empty off tray: first ghost is at the off marker.
5. Occupied off tray: first ghost is one stack step above the marker.
6. Point arrivals use factual starting occupancy, not simulated occupancy.
7. Destination stacks cap at visible level 6; later ghosts overlap there.
8. Chained departures originate from ghosts made by earlier steps.
9. Repeated same-row moves preserve edge-depth rank on top and bottom rows.
10. The same rule works for both players.
11. Repeated between-row moves preserve atomic order.
12. Multiple different sources prioritize same-row, then edge depth, then horizontal proximity.
13. Bearing-off source/slot ordering minimizes crossings.
14. Arrow tip trimming honors positive, zero, and negative checker gaps.
15. Arrowhead is a short convex triangle with a pointed tip and rounded stroke joins.
16. Ghost diameter equals factual checker outer diameter.
17. Ghost style is player-neutral and readable over both point colours.
18. Arrows render below factual and ghost checkers.
19. Horizontal and vertical transforms preserve factual assignments exactly.
20. Light/dark checker palette changes do not alter overlay geometry.
21. Two conflicting paths receive symmetric shallow bends.
22. Larger groups receive 0, +1, -1, +2, -2 deterministic levels.
23. Curve magnitude is capped and isolated crossings remain straight.
24. Curved endpoints use checker-edge tangent trimming.
25. Arrowheads follow the final quadratic tangent.
```
