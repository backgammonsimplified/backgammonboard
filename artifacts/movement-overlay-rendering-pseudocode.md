# Movement Overlay Rendering Specification

This document specifies the movement-arrow and ghost-checker behavior in the
current `backgammonboard` renderer. It is written so another Codex instance can
reimplement the behavior in JavaScript, SVG, Canvas, or another frontend
environment without needing to infer visual rules from screenshots.

The normative implementation files are:

- `R/moves.R`: ordered atomic-move input.
- `R/move-application.R`: move validation and simulation.
- `R/move-overlay.R`: source/destination assignment, ghosts, arrows, curves,
  coincident-path labels, and drawing.
- `R/movement-overlay-style.R`: configurable visual parameters.
- `R/render-preview.R`: canonical rendering and final paint order.
- `R/layout-transforms.R`: viewpoint and horizontal transforms.

## 1. Core invariants

1. The board always displays the factual **starting position**.
2. Applying moves produces a simulated state used for validation and for
   finding successive source anchors. It does not replace the displayed board.
3. Every atomic landing gets a neutral ghost checker, including hits, bar
   entries, chained hops, and bearing off.
4. Factual checkers are never removed from the displayed starting position.
5. A checker hit in the simulated move remains visible in its factual starting
   location; the destination ghost is stacked above that blot.
6. A checker moved from the bar remains visible on the displayed bar.
7. Ghost styling is identical for both players. Ownership is communicated by
   the factual source checker and the arrow.
8. Straight arrows are the default. Curves are introduced only for qualifying
   chained same-line movement or genuinely overlapping independent paths.
9. Arrows are painted above factual checkers, ghosts, dice, cube, branding, and
   ordinary board text.
10. A coincident-path `N×` label is painted above everything.
11. Perspective and horizontal mirroring are display transforms only. They do
    not alter move facts, ownership, source/destination assignment, or ordering.

## 2. Terminology and coordinate conventions

The R renderer internally calls `player_1`/Homey `white` and
`player_0`/Foey `black`. These are factual identities, not necessarily visible
checker colours after palette changes.

Public moves are mover-relative:

```text
1  = the mover's 1-point
24 = the mover's 24-point
```

Convert public move points to canonical factual point IDs before rendering:

```pseudocode
function canonicalPoint(mover, moverRelativePoint):
    if mover == PLAYER_1:
        return moverRelativePoint
    return 25 - moverRelativePoint
```

Construct the overlay in one canonical display frame:

```text
near/bottom player = player_1
point 1 side       = right
horizontal mirror  = false
player_1 direction = decreasing canonical point numbers
player_0 direction = increasing canonical point numbers
```

Apply requested display transforms only after all source assignment,
destination assignment, curvature, trimming, and deduplication are complete.

All geometry values in this document are board-coordinate units unless a value
is explicitly expressed as a multiple of checker radius.

## 3. Required input model

```typescript
type Location =
  | { type: "point"; point: number } // 1..24
  | { type: "bar" }
  | { type: "off" };

type AtomicMove = {
  stepId: number;       // consecutive, beginning at 1
  from: Location;       // point or bar
  to: Location;         // point or off
  die?: number | null;  // 1..6 when supplied
  role: "selected" | "alternative";
};
```

The move list must already be decomposed into atomic die uses. For example,
one checker using two dice is two ordered rows:

```text
13 -> 9
 9 -> 5
```

Do not supply a synthetic `13 -> 5` segment for that play. Atomic order is
required for chained source anchors, separate arrowheads, hits, stack ordering,
and die-distance validation.

## 4. Validate and simulate the move sequence

Validate the position and the entire ordered move sequence before drawing.

```pseudocode
function validateAndApplyMoves(startingPosition, publicMoves):
    require 24 integer point occupancies
    require nonnegative named bar and off counts for both players
    require exactly 15 checkers per player across points + bar + off
    require onRoll is PLAYER_0 or PLAYER_1
    require stepIds are 1, 2, ..., N

    mover = startingPosition.onRoll

    for each public move:
        if die is supplied:
            distance =
                25 - toPoint  when from is BAR
                fromPoint     when to is OFF
                fromPoint - toPoint otherwise
            require distance == die

        convert mover-relative point IDs to canonical point IDs

    state = clone(startingPosition)

    for each canonical atomic move in order:
        if mover has any checker on bar:
            require move.from is BAR

        if move.from is BAR:
            require mover has a bar checker
            require entry is in opponent's home board
        else:
            require source point contains a mover checker

        if destination is a point:
            require movement is in mover's homeward direction
            require destination has fewer than 2 opposing checkers
        else if destination is OFF:
            require source is in mover's home board
            require mover has no checker outside the home board

        remove one mover checker from the simulated source

        if destination contains exactly one opponent checker:
            mark hitConfirmed = true
            move opponent checker to simulated opponent bar

        add mover checker to simulated destination
        save applied-step metadata and resulting state

    require resulting checker totals remain exactly 15 per player
    return applied steps and final simulated state
```

The current renderer validates the supplied die distance for each atomic row.
It does **not** perform a complete legal-play search or prove that all usable
dice were consumed. A browser implementation should keep that distinction
unless it deliberately adds a separate rules engine.

## 5. Overall geometry pipeline

```pseudocode
function buildMovementOverlay(startingPosition, moves, board, boardStyle, overlayStyle):
    applied = validateAndApplyMoves(startingPosition, moves)

    segments = buildProvisionalSegmentsStepByStep(
        startingPosition, applied, board, boardStyle
    )

    assignSharedDestinationSlots(
        segments, startingPosition, board, boardStyle
    )

    clearance =
        boardStyle.checkerOuterRadius +
        boardStyle.checkerOuterRingWidth +
        overlayStyle.arrowCheckerGap
    require clearance >= 0

    straightTrimmedCopy = trimAsStraightLines(segments, clearance)
    assignAdaptiveCurves(
        segments, straightTrimmedCopy, board, boardStyle, overlayStyle
    )
    trimFinalCubicPaths(segments, clearance)
    markCoincidentPaths(segments)

    ghosts = one ghost for every segment whose ghostVisible is true
    return { segments, ghosts, applied }
```

The straight-trimmed copy is used only for independent-overlap detection. The
final path is rebuilt and trimmed using its actual cubic endpoint tangents.

## 6. Source and provisional destination anchors

Process applied moves chronologically while maintaining a simulated state.

### 6.1 Exposed checker

The exposed checker is the checker closest to the middle of the board:

```pseudocode
function exposedChecker(checkerRows, mover):
    owned = checkerRows filtered to mover
    if owned is empty: return null

    if owned[0].side == "bottom":
        return owned row with maximum y
    else:
        return owned row with minimum y
```

This uses the renderer's normal checker layout and visible-stack compression.
Repeated removals therefore advance through the simulated stack. On compressed
tall stacks, multiple logical source checkers can share a visible center.

### 6.2 Location anchor

```pseudocode
function locationAnchor(state, location, mover, board, boardStyle, perspective):
    layout = layoutCheckers(state)

    if location.type == "point":
        exposed = exposedChecker(layout.point(location.point), mover)
        if exposed exists:
            return exposed.center

        point = board.point(location.point)
        depth = min(2.25, board.fieldHeight / 3)
        y = board.fieldYMin + depth if point.side == "bottom"
            else board.fieldYMax - depth
        return (point.x, y)

    if location.type == "bar":
        exposed = exposedChecker(layout.bar, mover)
        if exposed exists:
            return exposed.center

        x = midpoint(board.barXMin, board.barXMax)
        moverIsBottom = mover == perspective
        y = board.fieldYMin + boardStyle.checkerOuterRadius
            if moverIsBottom
            else board.fieldYMax - boardStyle.checkerOuterRadius
        return (x, y)

    if location.type == "off":
        marker = layout.off marker for mover
        if marker exists:
            return marker.center

        x = midpoint(board.rightMarginXMin, board.rightMarginXMax)
        y = boardStyle.offMarkerBottomY if mover == perspective
            else boardStyle.offMarkerTopY
        return (x, y)
```

### 6.3 Provisional segment construction

```pseudocode
state = clone(startingPosition)
segments = []

for step in appliedSteps in order:
    source = locationAnchor(state, step.from, mover)

    nextState = applyExactlyOneStep(state, step)
    destination = locationAnchor(nextState, step.to, mover)

    if step.hitConfirmed:
        blot = locationAnchor(state, step.to, opponent)
        pointSide = board.point(step.to.point).side
        stackDirection = +1 if pointSide == "bottom" else -1
        destination = (
            blot.x,
            blot.y + stackDirection * boardStyle.checkerStackStep
        )

    segments.push({
        stepId: step.stepId,
        sourceId: stableLocationId(step.from),
        destinationId: stableLocationId(step.to),
        sourceToken: stableLocationId(step.from) + "/" + stableLocationId(step.to),
        sourceType: step.from.type,
        sourcePoint: step.from.point ?? null,
        destinationType: step.to.type,
        destinationPoint: step.to.point ?? null,
        sourceCenter: source,
        destinationCenter: destination,
        ghostCenter: destination,
        lineStart: source,
        lineEnd: destination,
        role: step.role,
        lineType: step.role == "selected" ? "solid" : "dashed",
        hitConfirmed: step.hitConfirmed,
        ghostVisible: true
    })

    state = nextState
```

The hit-specific provisional destination is later normalized by the common
point-slot rule. It ensures the intended visual semantics: the opposing blot
remains factual and the ghost occupies the next stack level above it.

## 7. Point destination slots

Group segments by `(destinationType, destinationPoint)`.

Point arrival slots are based on the **factual starting occupancy**, not the
simulated occupancy after departures:

```pseudocode
function pointArrivalSlots(group, point, startingPosition, board, boardStyle):
    initialCount = abs(startingPosition.points[point])
    pointGeometry = board.point(point)
    direction = +1 if pointGeometry.side == "bottom" else -1

    baseY =
        board.fieldYMin + boardStyle.checkerMargin + boardStyle.checkerOuterRadius
        if pointGeometry.side == "bottom"
        else
        board.fieldYMax - boardStyle.checkerMargin - boardStyle.checkerOuterRadius

    for arrivalIndex in 1..group.length:
        level = min(initialCount + arrivalIndex, 6)
        slot[arrivalIndex] = (
            pointGeometry.x,
            baseY + direction * (level - 1) * boardStyle.checkerStackStep
        )

    return slots
```

Consequences:

- Empty point: arrivals occupy visible levels 1, 2, 3, and so on.
- Three factual checkers: arrivals occupy levels 4, 5, 6, 6, and so on.
- Level 6 is the visual overflow cap; all later arrivals overlap there.
- An opposing blot contributes factual occupancy 1, so the hit ghost is level 2.
- A factual checker that departs during the illustrated play still contributes
  to destination slot depth because the displayed board remains the start state.

### 7.1 Repeated moves from one source location

Treat a destination group as one-source when every `sourceToken` has the same
substring before `/`.

Determine source and destination visual rows using the vertical field midpoint.

If the repeated moves remain on the same visual row:

```pseudocode
sourceDepth[i] = edgeDepth(segment[i].sourceCenter.y)
slotDepth[i]   = edgeDepth(slot[i].y)

targetSegments = sort group by:
    sourceDepth ascending,
    stepId ascending

orderedSlots = sort slots by:
    slotDepth ascending,
    originalSlotIndex ascending

pair targetSegments[i] with orderedSlots[i]
```

where:

```pseudocode
edgeDepth(y) = min(abs(y - fieldYMin), abs(fieldYMax - y))
```

This preserves stack-depth rank on either row and for either player. In visual
terms, checker centers nearest the relevant outside edge map to destination
centers nearest that edge, preventing crisscrossed repeated arrows.

If the source and destination are on different rows, retain atomic move order:

```pseudocode
pair group[i] with slot[i]
```

### 7.2 Different sources landing on one destination

Order source segments to reduce crossings:

```pseudocode
for segment in group:
    sourceSide = "bottom" if sourceCenter.y < fieldMidY else "top"
    crossesBoard = sourceSide != destinationSide
    sourceDepth = edgeDepth(sourceCenter.y)
    horizontalDistance = abs(sourceCenter.x - mean(slot.x))

sourceOrder = sort by:
    crossesBoard ascending       // same-row source first
    sourceDepth ascending
    horizontalDistance ascending
    stepId ascending

slotOrder = sort by:
    edgeDepth(slot.y) ascending
    originalSlotIndex ascending

pair sourceOrder[i] with slotOrder[i]
```

After pairing, overwrite `ghostCenter`, untrimmed `lineEnd`, and
`destinationCenter` with the assigned slot.

## 8. Bearing-off destination slots

All off moves for a mover share one off-tray x coordinate.

```pseudocode
function bearingOffSlots(group, mover, startingPosition, boardStyle, perspective):
    existing = startingPosition.off[mover]
    moverIsBottom = mover == perspective
    direction = +1 if moverIsBottom else -1
    baseY = boardStyle.offMarkerBottomY if moverIsBottom
            else boardStyle.offMarkerTopY

    if existing > 0:
        levels = [1, 2, ..., group.length]
    else:
        levels = [0, 1, ..., group.length - 1]

    return levels mapped to:
        (offTrayX, baseY + direction * level * checkerStackStep)
```

Therefore:

- If the off tray is empty, the first ghost occupies the ordinary off-marker
  position and later ghosts stack above it.
- If the off tray already contains a checker or numbered marker, the first
  ghost is one stack step above it.
- A bearing-off ghost is always drawn.

Pair sources and slots as follows:

```pseudocode
sourceOrder = sort by:
    edgeDepth(sourceCenter.y) ascending
    abs(sourceCenter.x - offTrayX) ascending
    stepId ascending

slotOrder = sort by:
    edgeDepth(slot.y) ascending
    originalSlotIndex ascending

pair sourceOrder[i] with slotOrder[i]
```

This sends the source checker closest to the edge/off tray to the nearest off
slot first and avoids crossing bearing-off arrows where possible.

## 9. Straight-path clearance used for conflict detection

Before deciding curvature, make a temporary straight-line copy and trim both
ends:

```pseudocode
clearance = checkerOuterRadius + checkerOuterRingWidth + arrowCheckerGap
require clearance >= 0

function trimStraight(segment, clearance):
    vector = destinationCenter - sourceCenter
    distance = length(vector)
    if distance <= 0: return unchanged

    unit = vector / distance
    sourceTrim = min(clearance, distance * 0.20)
    destinationTrim = min(clearance, distance * 0.20)

    lineStart = sourceCenter + unit * sourceTrim
    lineEnd = destinationCenter - unit * destinationTrim
```

The 20% caps prevent very short shafts from inverting.

## 10. Chained-move curvature

Chained-move detection is separate from independent projected-overlap
detection. A chain pair qualifies for angle analysis when:

```pseudocode
second.stepId == first.stepId + 1
AND second.sourceId == first.destinationId
```

Shared endpoint continuity alone does not produce curvature.

### 10.1 Measure both semantic and visible turn angles

For a point endpoint, build a stable semantic anchor from the point's x center
and its field boundary:

```pseudocode
semanticPointAnchor(point):
    x = board.point(point).x
    y = fieldYMax if point.side == "top" else fieldYMin
```

For bar/off endpoints, use the actual source or destination center.

```pseudocode
semanticDirection = normalize(semanticDestination - semanticSource)
visibleDirection  = normalize(destinationCenter - sourceCenter)

semanticAngle = directedAngle(first.semanticDirection, second.semanticDirection)
visibleAngle  = directedAngle(first.visibleDirection, second.visibleDirection)
turnAngle = max(semanticAngle, visibleAngle)
```

Using the larger angle is intentional. A chain should curve only when both the
factual point route and its rendered stack-level route look directionally
continuous. This prevents:

- a long diagonal `13/8` followed by the sharply turning `8/7` from receiving
  a large hook; and
- collinear point numbers on crowded stacks from being curved when their actual
  visible segments turn sharply.

Angles are directed from 0 to 180 degrees. Reversed movement is not treated as
the same direction.

### 10.2 Configurable angle bands

```pseudocode
function chainMultiplier(turnAngle, style):
    if turnAngle <= style.arrowChainFullAngle:
        return style.arrowChainFullMultiplier
    if turnAngle <= style.arrowChainModerateAngle:
        return style.arrowChainModerateMultiplier
    if turnAngle <= style.arrowChainMaxAngle:
        return style.arrowChainShallowMultiplier
    return 0
```

Current defaults:

```text
0° through 6°       -> 1.00 × configured curvature
over 6° through 12° -> 0.60 × configured curvature
over 12° through 20°-> 0.25 × configured curvature
over 20°            -> no chain curvature
```

The projected-overlap threshold is not used for ordered chain pairs.

### 10.3 Multi-hop assignment

Walk adjacent move pairs in atomic order. Mark both rows as belonging to an
ordered chain whenever their stable endpoint IDs connect.

For each pair:

- Assign the pair's multiplier to the later hop.
- Assign it to the earlier hop if that hop does not already have a positive
  multiplier from its previous adjacency.
- Thus a middle hop already participating in a qualifying incoming same-line
  relationship retains that positive curvature even if the following hop turns.

Every row with a positive chain multiplier receives curve level `1`. This makes
each qualifying hop its own shallow cubic curve with its own arrowhead. It is
not rendered as one long continuous arrow.

## 11. Independent overlapping-path curvature

Run this rule only on segments whose chain multiplier is zero. A sharp ordered
turn can therefore remain straight as a chain but still curve if it independently
overlaps another movement path.

### 11.1 Pairwise conflict test

Given the straight-trimmed visible paths:

```pseudocode
directedAngle = angle(first.direction, second.direction)
projectedOverlapFraction =
    overlap length after projecting both segments onto first.direction
    divided by the shorter projected segment length

lateralDistance = maximum of:
    distance of second start to first line
    distance of second end to first line
    distance of first start to second line
    distance of first end to second line
```

Two ordinary independent paths conflict only when all are true:

```text
directedAngle <= arrowCollinearAngleTolerance
projectedOverlapFraction >= arrowOverlapThreshold
lateralDistance <= 0.60 * checkerRadius
source IDs are different
destination IDs are different
```

Repeated arrows sharing one destination are deliberately excluded. They remain
straight and are handled by stack assignment or the coincident `N×` treatment.

There is one narrow shared-source exception:

```text
same source ID
both paths are near vertical: abs(dy) >= 3 * abs(dx)
directedAngle <= arrowCollinearAngleTolerance
```

This catches nearly identical vertical departures without curving ordinary
shared-source arrows.

Do not curve arrows merely because they cross once. Opposite-direction paths
also do not qualify because the angle is directed rather than directionless.

### 11.2 Conflict groups and deterministic levels

Build an undirected graph whose edges are qualifying pairwise conflicts. Use
connected components as groups. Sort each group by:

```text
stepId, sourceId, destinationId
```

Keep the first path straight and curve only later ambiguous paths:

```text
curve levels = 0, 1, 2, 3, ...
```

The active algorithm does not split two arrows to opposite sides. The exposed
`arrowCurveTwoPathSplit` field is retained for compatibility/tuning but is not
currently consumed by curve assignment.

## 12. Curve magnitude and inward direction

### 12.1 Requested magnitude

```pseudocode
function curveMagnitude(level, style):
    a = abs(level)
    if a == 0: return 0

    if a < 1:
        magnitude = style.arrowCurveOffset * a
    else:
        magnitude =
            style.arrowCurveOffset + (a - 1) * style.arrowCurveStep

    return sign(level) * min(magnitude, style.arrowCurveMax)
```

For a chain hop, multiply the requested value by the chain angle-band
multiplier. Convert it to physical board units using checker radius.

### 12.2 Caps

For every curved path:

```pseudocode
physicalOffset = min(
    requestedMagnitude * checkerRadius,
    arrowCurveMax * checkerRadius,
    capLength * arrowCurveLengthCap
)
```

For independent-overlap paths, `capLength` is the actual direct source-to-
destination distance.

For chain paths, `capLength` is the semantic source-to-destination length. In
addition:

```pseudocode
if semanticLength <= arrowChainShortLengthRadii * checkerRadius:
    physicalOffset = min(
        physicalOffset,
        arrowChainShortCurveMax * checkerRadius
    )
```

Current short-hop defaults are `2.75` checker radii and a maximum bow of `0.15`
checker radius. In this board geometry, that threshold captures adjacent-point
short hops.

### 12.3 Bow toward the middle of the board

```pseudocode
function inwardNormal(source, destination, boardCenter):
    direction = normalize(destination - source)
    normal = (-direction.y, direction.x)
    midpoint = (source + destination) / 2

    nearVertical = abs(direction.y) >= 3 * abs(direction.x)
    if nearVertical:
        desired = (boardCenter.x - midpoint.x, 0)
    else:
        desired = (0, boardCenter.y - midpoint.y)

    if dot(normal, desired) < 0:
        normal = -normal
    return normal
```

Therefore top-half horizontal/diagonal moves bow downward, bottom-half moves bow
upward, and near-vertical moves bow horizontally toward the board center.

## 13. Cubic Bézier construction

Straight cubic controls begin at one-third and two-thirds of the direct line:

```pseudocode
C1 = source + (destination - source) / 3
C2 = source + 2 * (destination - source) / 3
```

For a curved path:

```pseudocode
normal = inwardNormal(source, destination, boardCenter)
controlDisplacement = physicalOffset * 4 / 3

C1 = source + (destination - source) / 3
     + normal * controlDisplacement
C2 = source + 2 * (destination - source) / 3
     + normal * controlDisplacement
```

The `4/3` factor makes the curve's midpoint displacement equal to
`physicalOffset`, because equal control displacements contribute `3/4` of that
displacement at `t = 0.5`.

The path is:

```pseudocode
B(t) =
    (1-t)^3 * P0 +
    3(1-t)^2 t * C1 +
    3(1-t)t^2 * C2 +
    t^3 * P3
```

For SVG, emit `M P0.x P0.y C C1.x C1.y C2.x C2.y P3.x P3.y`.
The R renderer samples 61 points for a curved shaft and 2 points for a straight
shaft because it draws with a polyline API; a browser SVG implementation should
use the cubic path directly.

## 14. Final tangent-based trimming

Retain untrimmed semantic centers for metadata. Trim only the rendered path.

```pseudocode
function trimFinalCubic(segment, clearance):
    P0 = sourceCenter
    P3 = destinationCenter
    C1 = control1
    C2 = control2
    directDistance = length(P3 - P0)
    if directDistance <= 0: return

    sourceTangent = normalize(C1 - P0)
    destinationTangent = normalize(P3 - C2)

    sourceTrim = min(clearance, directDistance * 0.20)
    destinationTrim = min(clearance, directDistance * 0.20)

    drawnStart = P0 + sourceTangent * sourceTrim
    drawnEnd = P3 - destinationTangent * destinationTrim

    drawnC1 = C1 + sourceTangent * sourceTrim
    drawnC2 = C2 - destinationTangent * destinationTrim

    finalTangent = destinationTangent
```

`arrowCheckerGap` is signed:

- Positive: leave additional space outside the checker/ghost.
- Zero: stop at `checkerOuterRadius + checkerOuterRingWidth`.
- Negative: move the arrow endpoint inside the checker toward its center.
- Reject values that make total clearance negative.

The reviewed gallery uses `-0.20`, so arrow endpoints sit inside the source and
destination marker rather than stopping at the outside edge.

## 15. Arrowhead and stroke rendering

Use the final cubic tangent at the destination:

```pseudocode
u = normalize(finalTangent)
p = (-u.y, u.x)
tip = drawnEnd
baseCenter = tip - arrowheadLength * u
halfWidth = arrowheadWidth / 2

head = polygon([
    tip,
    baseCenter + halfWidth * p,
    baseCenter - halfWidth * p
])
```

Rendering rules:

- The head is a short, filled, convex triangle with a point at `tip`.
- The shaft runs to `tip`; the filled head covers its final portion.
- Use `arrowWidth` for both the shaft and arrowhead polygon stroke.
- Use `arrowLineEnd` for the shaft tail; the gallery uses `round`.
- Use rounded joins.
- Selected shafts are solid.
- Alternative shafts are dashed; their triangular heads remain filled.
- Apply `arrowAlpha` to shaft and head.

Optional contrast outline:

```pseudocode
if arrowOutlineWidth > 0:
    draw shaft and head in arrowOutlineColour with width:
        arrowWidth + 2 * arrowOutlineWidth

draw shaft and head in arrowColour with width arrowWidth
```

The outline pass is underneath the main pass. The reviewed gallery disables it
with `arrowOutlineWidth = 0`.

## 16. Coincident paths and `N×` labels

After curvature and trimming, identify paths that are geometrically and
semantically identical.

Quantize these values with tolerance `1e-8`:

```text
drawn start x/y
drawn end x/y
control 1 x/y
control 2 x/y
final tangent x/y
```

The complete grouping key is:

```text
sourceId | destinationId | role | lineType | quantizedGeometry
```

For every coincident group:

```pseudocode
rows = sort by stepId
coincidentCount = rows.length
drawArrow = true only for rows[0]
```

Thus four identical atomic movements produce one arrow plus `4×`, not four
overpainted arrows. `N` is the count of identical atomic movement paths—not the
number of dice shown and not the checker count on either stack.

Place the label using the cubic midpoint and midpoint tangent:

```pseudocode
t = 0.5
midpoint = cubicPoint(segment, t)
tangent = cubicDerivative(segment, t)
normal = perpendicular(normalize(tangent))

if dot(normal, boardCenter - midpoint) < 0:
    normal = -normal

labelOffset = max(0.70, 1.75 * checkerOuterRadius)
labelPosition = midpoint + normal * labelOffset

labelPosition.x = clamp(labelPosition.x, frameXMin + 0.80, frameXMax - 0.80)
labelPosition.y = clamp(labelPosition.y, frameYMin + 0.55, frameYMax - 0.55)
```

Draw the label as bold text using the same family, size, and colour as the
renderer's `on roll` typography. Paint it last.

## 17. Neutral ghost-checker rendering

Every ghost uses the same appearance regardless of mover identity or checker
palette.

```pseudocode
ghostRadius = boardStyle.checkerOuterRadius
```

Do not add outline width to this radius. The outline stroke is centered on the
radius path.

```pseudocode
function drawGhost(center, boardStyle, style):
    radius = boardStyle.checkerOuterRadius

    if style.ghostFill is null/NA:
        fill = none
    else:
        fill = style.ghostFill at style.ghostFillAlpha

    draw circle(center, radius, fill, no stroke)

    patternRadius = radius - style.ghostGridInset
    require patternRadius > 0

    xs = evenlySpaced(-patternRadius, +patternRadius, style.ghostGridCols)
    ys = evenlySpaced(-patternRadius, +patternRadius, style.ghostGridRows)

    for each Cartesian pair (dx, dy) in xs × ys:
        if sqrt(dx^2 + dy^2) <= patternRadius:
            draw filled dot at center + (dx, dy) using:
                colour = style.ghostDotColour
                alpha = style.ghostDotAlpha
                size = style.ghostDotSize

    draw solid circle outline at radius using:
        colour = style.ghostOutline
        width = style.ghostOutlineWidth
```

The dot centers are filtered to the circular pattern radius. The active R code
does not install a separate clip path; `ghostGridInset` keeps the visible dots
inside or close to the outline.

Ghost rules:

- Same neutral ghost for light and dark movers.
- Same style over tan and blue points.
- Diameter is based on the factual checker's configured outer-body diameter.
- Fill, dots, and outline are independently configurable.
- `ghostFill = NA` means no interior fill.
- The outline is solid.
- Ghosts are placed above factual destination stacks, including opposing blots.
- Multiple arrivals may stack through level 6; overflow ghosts coincide at 6.
- Bearing-off destinations also receive ghosts.

## 18. Paint order

Use this exact back-to-front order:

```text
1. canvas, board frame, fields, bar, points, and point labels
2. factual point and bar checkers
3. factual off-tray checkers/markers
4. dice
5. cube or Crawford marker
6. branding
7. information rails and text
8. optional board guides
9. alternative-move ghosts
10. selected-move ghosts
11. alternative-move arrows
12. selected-move arrows
13. alternative coincident-path multiplier labels
14. selected coincident-path multiplier labels
```

Important consequences:

- Arrows are above factual checkers and destination ghosts.
- Arrows are also above dice, cube, branding, point numbers, and information
  text when geometry overlaps them.
- Selected arrows paint above alternative arrows.
- Ghosts paint above factual destination checkers but below arrows.
- `N×` labels paint above every arrow and every other board element.
- The movement overlay does not change watermark/branding placement or style.

## 19. Viewpoint and horizontal transforms

Build all geometry canonically, then transform every overlay coordinate with
the board.

```pseudocode
if mirrorHorizontal:
    x = xMin + xMax - x
    finalTangent.x *= -1

if nearPlayer == PLAYER_0:
    y = yMin + yMax - y
    finalTangent.y *= -1
```

Transform all x/y fields consistently, including:

```text
drawn start and end
source, ghost, and destination centers
both cubic control points
ghost locations
hit metadata
multiplier-label geometry
final tangent
```

When exactly one axis is reflected, the implementation negates signed curve
metadata (`curveLevel`, `curveOffset`, and compatibility `curvature`) so its
diagnostic sign remains consistent with the transformed geometry. Actual
control points are reflected directly.

Do not rerun source ordering, slot assignment, chain detection, or conflict
detection after a display transform.

## 20. Style object

JavaScript-oriented equivalent of the current R signature:

```typescript
type MovementOverlayStyle = {
  ghostFill: string | null;                 // R default NA
  ghostFillAlpha: number;                   // 0.20
  ghostOutline: string;                     // "#000000"
  ghostOutlineWidth: number;                // 1.5
  ghostDotColour: string;                   // "#65707A"
  ghostDotAlpha: number;                    // 1
  ghostGridRows: number;                    // 7
  ghostGridCols: number;                    // 7
  ghostDotSize: number;                     // 1
  ghostGridInset: number;                   // 0.15

  arrowColour: string;                      // "#D95F32"
  arrowAlpha: number;                       // 1
  arrowWidth: number;                       // 2
  arrowLineEnd: "butt" | "round" | "square"; // "round"
  arrowOutlineColour: string;               // "#000000"
  arrowOutlineWidth: number;                // 0
  arrowheadLength: number;                  // 0.18
  arrowheadWidth: number;                   // 0.12
  arrowCheckerGap: number;                  // 0.05

  arrowCurveEnabled: boolean;               // true
  arrowCurveOffset: number;                 // 0.22 checker radii
  arrowCurveStep: number;                   // 0.18 checker radii
  arrowCurveMax: number;                    // 0.65 checker radii
  arrowCurveLengthCap: number;              // 0.08 of path length

  arrowChainFullAngle: number;              // 6 degrees
  arrowChainModerateAngle: number;          // 12 degrees
  arrowChainMaxAngle: number;               // 20 degrees
  arrowChainFullMultiplier: number;         // 1.00
  arrowChainModerateMultiplier: number;     // 0.60
  arrowChainShallowMultiplier: number;      // 0.25
  arrowChainShortLengthRadii: number;        // 2.75
  arrowChainShortCurveMax: number;           // 0.15 checker radii

  arrowCollinearAngleTolerance: number;      // 8 degrees
  arrowOverlapThreshold: number;             // 0.40
  arrowCurveTwoPathSplit: number;            // 0.5; currently unused
};
```

Validation:

- Alpha values are within `[0, 1]`.
- Colours are valid single colours; only `ghostFill` may be absent.
- Outline width, dot size, arrow width, arrowhead dimensions, curve offset,
  maximum curve, length cap, chain thresholds, short threshold/cap, and
  two-path split are greater than zero.
- Grid inset, arrow outline width, and curve step are nonnegative.
- Grid rows and columns are integers of at least 2.
- `arrowCheckerGap` is any finite signed number whose resolved clearance is
  nonnegative.
- Collinear tolerance is greater than 0 and at most 90 degrees.
- Overlap threshold is in `[0, 1]`.
- Chain angle thresholds are strictly increasing and at most 180 degrees.
- Chain multipliers are in `[0, 1]`.

### 20.1 Resolved BMS style when no custom object is supplied

```text
ghostFill                    = #D4D8DC (movement_ghost_fill token)
ghostFillAlpha               = 0.88
ghostOutline                 = #27313B (movement_ghost_outline token)
ghostOutlineWidth            = 1.25
ghostDotColour               = #65707A (movement_ghost_pattern token)
ghostDotAlpha                = 1
ghostGridRows                = 7
ghostGridCols                = 7
ghostDotSize                 = 0.62
ghostGridInset               = 0.015

arrowColour                  = #C94F2C (arrow_primary token)
arrowAlpha                   = 1
arrowWidth                   = boardStyle.arrowLinewidth
arrowLineEnd                 = round
arrowOutlineColour           = #081126 (arrow_halo_dark token)
arrowOutlineWidth            = 0.405
                               // 1.8 * (1.45 - 1) / 2
arrowheadLength              = 0.18
arrowheadWidth               = 0.12
arrowCheckerGap              = 0

arrowCurveEnabled            = true
arrowCurveOffset             = 0.22
arrowCurveStep               = 0.18
arrowCurveMax                = 0.65
arrowCurveLengthCap          = 0.08
arrowChainFullAngle          = 6
arrowChainModerateAngle      = 12
arrowChainMaxAngle           = 20
arrowChainFullMultiplier     = 1.00
arrowChainModerateMultiplier = 0.60
arrowChainShallowMultiplier  = 0.25
arrowChainShortLengthRadii   = 2.75
arrowChainShortCurveMax      = 0.15
arrowCollinearAngleTolerance = 8
arrowOverlapThreshold        = 0.40
arrowCurveTwoPathSplit       = 0.5
```

### 20.2 Current comprehensive review-gallery overrides

```text
ghostFill                    = none
ghostFillAlpha               = 0.20  // inactive while fill is none
ghostOutline                 = #000000
ghostOutlineWidth            = 1.5
ghostDotColour               = #65707A
ghostDotAlpha                = 1
ghostGridRows                = 7
ghostGridCols                = 7
ghostDotSize                 = 1
ghostGridInset               = 0.04

arrowColour                  = #D95F32
arrowAlpha                   = 1
arrowWidth                   = 2
arrowLineEnd                 = round
arrowOutlineColour           = #000000
arrowOutlineWidth            = 0
arrowheadLength              = 0.08
arrowheadWidth               = 0.05
arrowCheckerGap              = -0.20

arrowCurveEnabled            = true
arrowCurveOffset             = 2.0
arrowCurveStep               = 0.75
arrowCurveMax                = 3.0
arrowCurveLengthCap          = 0.40
arrowChainFullAngle          = 6
arrowChainModerateAngle      = 12
arrowChainMaxAngle           = 20
arrowChainFullMultiplier     = 1.00
arrowChainModerateMultiplier = 0.60
arrowChainShallowMultiplier  = 0.25
arrowChainShortLengthRadii   = 2.75
arrowChainShortCurveMax      = 0.15
arrowCollinearAngleTolerance = 6
arrowOverlapThreshold        = 0.25
arrowCurveTwoPathSplit       = 0.5  // inherited; currently unused
```

These gallery values are tuning values, not a claim that all are final package
defaults.

## 21. Browser implementation outline

```pseudocode
function renderMovementOverlay(args):
    style = resolveAndValidateStyle(args.movementStyle)
    canonicalBoard = buildCanonicalBoardGeometry(args.boardStyle)

    overlay = buildMovementOverlay(
        args.startingPosition,
        args.atomicMoves,
        canonicalBoard,
        args.boardStyle,
        style
    )

    transformed = transformOverlay(
        overlay,
        args.nearPlayer,
        args.mirrorHorizontal,
        args.boardStyle.bounds
    )

    // Ordinary board has already been painted.
    drawGhosts(transformed.alternativeGhosts)
    drawGhosts(transformed.selectedGhosts)
    drawArrows(transformed.alternativeSegments.filter(drawArrow))
    drawArrows(transformed.selectedSegments.filter(drawArrow))
    drawMultiplierLabels(transformed.alternativeSegments)
    drawMultiplierLabels(transformed.selectedSegments)
```

Keep geometry generation separate from painting. Tests should inspect segment
records and control points directly rather than relying only on screenshots.

## 22. Required compatibility tests

A faithful port should include focused tests for at least these rules:

1. Factual starting checkers remain displayed after move simulation.
2. Public player-0 move points transform using `25 - point`.
3. Per-atomic-step die distances are validated.
4. Bar priority and blocked destinations are enforced.
5. A hit ghost sits one visible level above the factual opposing blot.
6. A bar-entry source remains anchored to the factual/simulated bar stack.
7. Point destination slots use factual starting occupancy.
8. Destination stacks cap at visible level 6.
9. Empty and occupied off trays use the correct first ghost slot.
10. Bearing-off source ordering avoids crossings where possible.
11. Same-row repeated sources preserve source/slot edge-depth rank.
12. Between-row repeated sources retain atomic order.
13. Different sources prioritize same-row, edge depth, proximity, then step ID.
14. Chained later moves use simulated landing-stack source anchors.
15. Non-collinear consecutive moves remain straight.
16. Collinear zero-overlap chains receive separate cubic bows and arrowheads.
17. Collinear chains retain full configured curvature.
18. Moderate and shallow turn bands apply 60% and 25% multipliers.
19. Turns over 20 degrees receive no chain curvature.
20. The long-diagonal `13/8` followed by short `8/7` remains straight or only
    minimally curved according to configured bands; with current thresholds it
    is straight.
21. Both semantic and visible angles must qualify.
22. Short chain hops obey the independent `0.15`-radius cap.
23. Top, bottom, and near-vertical bows point inward.
24. Independent collinear overlap curves only later paths.
25. Shared destinations and isolated crossings remain straight.
26. Conflict grouping and ordering are deterministic.
27. Curve maximum and length-relative caps are enforced.
28. Final endpoints are trimmed along cubic endpoint tangents.
29. Arrowhead orientation follows the final cubic tangent.
30. Arrowheads are convex, pointed triangles with configured dimensions.
31. Positive, zero, and negative checker gaps behave correctly.
32. Exact coincident paths collapse to one arrow plus the correct `N×` label.
33. `N` counts paths, not dice or checkers.
34. Ghost radius, neutral style, dot density, inset, and no-fill behavior match.
35. Ghosts remain readable over both point colours.
36. Arrows paint above ghosts, checkers, dice, branding, and text.
37. Multiplier labels paint above arrows and remain within board bounds.
38. Horizontal and vertical transforms preserve factual assignments and curve
    geometry.
39. Light/dark palette changes do not alter overlay geometry.
40. The complete accepted gallery remains byte-identical except for explicitly
    approved cases.

## 23. Deliberate non-features

- Do not display traditional notation such as `13/10` inside the board.
- Do not remove factual source checkers from the illustrated position.
- Do not recolour ghosts according to player ownership.
- Do not curve every arrow.
- Do not curve ordinary shared destinations merely to fan them apart.
- Do not curve isolated crossings.
- Do not combine chained dice into one long arrow.
- Do not infer a full legal play from dice alone.
- Do not change branding or watermark behavior as part of movement overlays.
