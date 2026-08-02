# `backgammonboard` Contract

**Version:** 1.2  
**Status:** Marty-directed corrective implementation contract  
**Package and repository:** `backgammonboard`  
**Primary output:** static `ggplot`  
**Supported source identifier for this release:** complete XGID  
**Factual player identities:** `player_0`, `player_1`  
**Default BMS labels:** `player_0 = Foey`, `player_1 = Homey`  
**Default BMS near player:** `player_1` / Homey  

This contract supersedes version 1.1 wherever player mapping, Homey/Foey labels, perspective, horizontal mirroring, vertical flipping, point numbering, cube placement, dice placement, or text orientation differ.

## 1. Purpose

`backgammonboard` renders factual backgammon positions and selected checker-movement overlays.

It may render:

```text
checker play
rolled position without a selected move
neutral no-dice position
Roll or Double
Take or Pass
```

It does not:

- run GNU, Sage, or another engine;
- parse GNU Position ID or GNU Match ID;
- evaluate checker or cube strategy;
- choose lesson meaning;
- generate every legal play;
- publish analysis or write D1.

## 2. Current package boundary

For this release:

```text
complete XGID
-> source-role decode
-> factual player mapping
-> canonical source layout
-> explicit display transforms
-> optional structured movement overlay
-> ggplot
```

Complete XGID is the only source identifier accepted by this package release.

This does not make XGID the project-wide analysis identity.

GNU-to-XGID conversion belongs outside this package.

## 3. Separate concepts

The implementation must keep these concepts separate:

### 3.1 XGID source roles

```text
top
bottom
```

These are roles defined by the XGID representation.

### 3.2 Project player identities

```text
player_0
player_1
```

These are stable factual identities used by the project.

### 3.3 BMS display labels

```text
player_0 -> Foey
player_1 -> Homey
```

Labels remain attached to their factual players.

### 3.4 Display sides

```text
near
far
left
right
```

Display sides are coordinates, not identities.

### 3.5 Display controls

```text
near_player
mirror_horizontal
```

`near_player` controls the vertical player arrangement.

`mirror_horizontal` controls the left-right arrangement.

They are independent.

## 4. Fixed XGID player mapping

Freeze this project convention:

```text
XGID top player    -> player_0 -> Foey
XGID bottom player -> player_1 -> Homey
```

The mapping never changes according to:

- turn;
- player on roll;
- cube action;
- decision maker;
- perspective;
- horizontal mirroring;
- vertical flipping;
- checker colour;
- lesson context.

## 5. Exact XGID field mapping

### 5.1 Checker component

For the 26-character XGID checker component:

```text
character 0      -> player_0 bar
characters 1-24 -> physical points from player_1's perspective
character 25     -> player_1 bar
lowercase        -> player_0 checkers
uppercase        -> player_1 checkers
```

The point sequence does not reverse because of turn.

### 5.2 Turn

```text
turn = -1 -> player_0 on roll
turn =  1 -> player_1 on roll
```

### 5.3 Cube owner

```text
cube owner = -1 -> player_0
cube owner =  0 -> centered
cube owner =  1 -> player_1
```

### 5.4 Scores

```text
first score field  -> player_1 score
second score field -> player_0 score
```

### 5.5 Pending double

For XGID action marker `D`:

```text
turn player -> offerer
other player -> receiver and decision maker
offered value -> 2 * current factual cube value
```

Missing dice do not imply an offer.

### 5.6 Default labels

```r
player_labels = c(
  player_0 = "Foey",
  player_1 = "Homey"
)
```

## 6. Factual position object

The factual object must be independent of display orientation.

Conceptually:

```r
list(
  xgid = ...,
  source_roles = ...,
  player_mapping = ...,
  points = ...,
  bar = ...,
  off = ...,
  on_roll = ...,
  dice = ...,
  cube_value = ...,
  cube_owner = ...,
  cube_action = ...,
  score = ...,
  match_length = ...,
  crawford_status = ...,
  jacoby = ...,
  beavers_allowed = ...,
  max_cube = ...
)
```

### 6.1 Point occupancy

The factual public meaning of every point must be explicit:

```text
point_id
owner = player_0 | player_1 | empty
count
```

The implementation may use a signed integer internally, but signed storage is not the semantic authority.

Any signed representation must document and test its sign convention.

### 6.2 Canonical point identity

A canonical point ID `p` is the XGID physical point number:

```text
p in 1..24
numbered from player_1's source perspective
```

For mover-relative movement notation:

```text
player_1 mover point p -> canonical point p
player_0 mover point p -> canonical point 25 - p
```

### 6.3 Factual immutability

Display transforms must not mutate:

- point ownership;
- checker counts;
- bars;
- off counts;
- on-roll player;
- dice;
- cube value;
- cube owner;
- offerer or receiver;
- scores;
- match state;
- structured movement ownership.

## 7. Canonical source layout

The first prepared layout uses XGID source geometry:

```text
player_1 / Homey -> near
player_0 / Foey  -> far
mirror_horizontal = false
```

This is the default BMS layout.

No vertical transform is needed for the default Homey-near view.

The canonical layout contains coordinates for:

- case and playing field;
- points;
- checker faces and rings;
- bars;
- off trays;
- dice;
- centered or owned cube;
- offered cube;
- player names;
- scores;
- pip counts;
- point-number anchors;
- movement arrows, ghosts, markers, and labels.

## 8. Display transform model

Transforms occur on prepared layout data before drawing.

Do not use a plotting coordinate reversal that mirrors or rotates text glyphs.

### 8.1 Horizontal mirror

When:

```text
mirror_horizontal = true
```

apply:

```text
x' = x_min + x_max - x
y' = y
```

to every geometry and anchor that belongs to the board layout.

Horizontal mirroring affects:

- points;
- checkers;
- bars;
- off trays;
- dice;
- centered cube;
- owned cube;
- offered cube;
- player-information anchors;
- point-number anchors;
- movement arrows;
- ghosts;
- destination markers;
- movement labels.

Horizontal mirroring does not:

- swap players;
- change near and far;
- change on-roll player;
- change cube owner;
- change scores;
- change point identity;
- change displayed point values;
- mirror text glyphs.

### 8.2 Vertical player transform

When:

```text
near_player = player_0
```

apply:

```text
x' = x
y' = y_min + y_max - y
```

to the complete prepared layout.

This moves Foey to the near side and Homey to the far side.

The vertical transform moves all player-attached components together:

- checkers;
- player bars;
- off trays;
- dice;
- owned cube;
- offered cube;
- player name;
- score;
- pip count;
- status labels;
- movement overlays.

The vertical transform does not change factual identity or ownership.

### 8.3 Transform composition

Horizontal and vertical transforms are independent and must commute:

```text
horizontal(vertical(layout))
=
vertical(horizontal(layout))
```

Each transform is its own inverse:

```text
horizontal(horizontal(layout)) = layout
vertical(vertical(layout))     = layout
```

### 8.4 Text rule

Text glyphs are never mirrored or drawn upside down.

For all text:

1. transform the anchor coordinate;
2. compute the correct semantic label;
3. redraw the text upright with normal glyph orientation.

This applies to:

- point numbers;
- Homey and Foey;
- scores;
- pip counts;
- cube values;
- status labels;
- movement labels.

## 9. Point-number display

Point identity and displayed point value are separate.

For canonical point `p`:

```text
near_player = player_1 -> displayed point value = p
near_player = player_0 -> displayed point value = 25 - p
```

Horizontal mirroring moves the point and label anchor horizontally but does not change the displayed value.

Point-number glyphs remain upright and readable from the page orientation.

The implementation must not rotate or mirror number glyphs.

## 10. Cube placement

Cube ownership remains factual.

### 10.1 Centered cube

The centered cube uses a neutral semantic anchor.

It follows horizontal and vertical layout transforms only when the selected style places that anchor inside the transformed board layout.

### 10.2 Owned cube

An owned cube is attached to its factual owner:

```text
cube_owner = player_0 -> Foey anchor
cube_owner = player_1 -> Homey anchor
```

Changing `near_player` moves the cube with its owner.

Horizontal mirroring mirrors its x anchor.

### 10.3 Offered cube

A pending offered cube is attached to the factual offerer.

Changing `near_player` moves the offered cube with the offerer.

Horizontal mirroring mirrors its x anchor.

Do not place the offered cube using an independent point-orientation rule.

### 10.4 Roll or Double

For `roll_double`:

- show the current factual cube;
- hide dice;
- do not draw an offered cube.

### 10.5 Take or Pass

For `take_pass` with a factual `D` action:

- hide dice;
- hide the normal cube;
- show the offered cube;
- show the doubled value;
- use the receiver as decision maker.

## 11. Dice placement

Dice remain attached to the factual on-roll player.

Changing `near_player` moves dice with that player.

Horizontal mirroring mirrors dice x coordinates.

The dice do not independently decide which side to use after the layout transform.

Render two distinct dice with upright pips.

## 12. Player labels and information rows

Homey and Foey remain attached to their factual players:

```text
player_1 -> Homey
player_0 -> Foey
```

When `near_player` changes:

- player name;
- score;
- pip count;
- owned cube;
- dice;
- related status text

move together to that player's new display side.

Horizontal mirroring may move information anchors horizontally, but it does not exchange players or labels.

## 13. Display context

Accepted decision values:

```text
auto
checker_play
roll_double
take_pass
none
```

Accepted perspective values may remain:

```text
decision_maker
on_roll
player_0
player_1
```

Every perspective value must resolve to one field:

```text
near_player
```

It must not imply horizontal mirroring.

Add or retain an independent public control:

```text
mirror_horizontal = false | true
```

### 13.1 Resolution

```text
perspective = player_0       -> near_player = player_0
perspective = player_1       -> near_player = player_1
perspective = on_roll        -> near_player = factual on-roll player
perspective = decision_maker -> near_player = resolved decision maker
```

For `none`, `decision_maker` may fall back to the on-roll player.

### 13.2 Default BMS view

```text
perspective = player_1
near_player = player_1
mirror_horizontal = false
```

Homey is near/bottom by default.

## 14. Rendering API

The public entry point should support:

```r
ggboard(
  x,
  colors = board_colors(),
  style = board_style(),
  moves = NULL,
  after_xgid = NULL,
  decision = "auto",
  perspective = "player_1",
  mirror_horizontal = FALSE,
  player_labels = c(
    player_0 = "Foey",
    player_1 = "Homey"
  ),
  score_format = "away"
)
```

Existing compatible calls may be preserved.

Deprecated behavior must not silently retain the v1.1 reverse mapping.

## 15. Conservative decision inference

Freeze:

```text
moves supplied
-> checker_play

otherwise factual dice present
-> checker_play

otherwise
-> none
```

`auto` never infers:

```text
roll_double
take_pass
pending cube offer
```

No dice does not mean a double was offered.

## 16. Structured movements

The renderer accepts ordered atomic movements.

It does not parse GNU movement text.

Movement points are mover-relative and are mapped to canonical point identity before layout transforms.

Movement geometry follows the same horizontal and vertical transforms as the board.

The base board remains the before-position.

## 17. Mandatory regression fixture: case 18

The following fixture is mandatory:

```text
XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8
```

Required factual interpretation:

```text
XGID top player    = player_0 = Foey
XGID bottom player = player_1 = Homey
turn = -1
on_roll = player_0 = Foey
dice = none
cube = centered at value 1
decision is not inferred as Take or Pass
```

Required display checks:

1. Default view:
   - Homey near;
   - Foey far;
   - Foey shown on roll;
   - no dice;
   - no offered cube.

2. Horizontal mirror:
   - same factual players and turn;
   - same near/far players;
   - board geometry mirrored left-right;
   - point numbers and text remain upright.

3. Foey-near view:
   - Foey near;
   - Homey far;
   - factual turn unchanged;
   - player names, information rows, owned cube or dice anchors move with players.

4. Foey-near plus horizontal mirror:
   - both transforms applied;
   - result independent of transform order.

The historical v1.1 interpretation and unchanged legacy SVG are not semantic authority.

## 18. Required transform fixtures

Use asymmetric fixtures that expose incorrect mapping.

At minimum:

- one checker owned by each player on distinct asymmetric points;
- one checker on each bar;
- borne-off checkers;
- player_0 on roll with dice;
- player_1 on roll with dice;
- centered cube;
- player_0-owned cube;
- player_1-owned cube;
- pending offer by player_0;
- pending offer by player_1;
- asymmetric scores;
- one checker movement;
- repeated movement;
- bar entry;
- bearing off.

Render each applicable fixture in:

```text
Homey near, not mirrored
Homey near, mirrored
Foey near, not mirrored
Foey near, mirrored
```

## 19. Automated tests

### 19.1 Decoder tests

Prove:

- lowercase checkers belong to player_0;
- uppercase checkers belong to player_1;
- character 0 is player_0 bar;
- character 25 is player_1 bar;
- turn -1 maps to player_0;
- turn 1 maps to player_1;
- cube owner -1 maps to player_0;
- cube owner 1 maps to player_1;
- first score maps to player_1;
- second score maps to player_0;
- point order never changes because of turn.

### 19.2 Factual immutability tests

For every display state, compare the factual object before and after layout preparation.

It must remain unchanged.

### 19.3 Transform property tests

Prove:

```text
horizontal twice = identity
vertical twice = identity
horizontal and vertical commute
```

### 19.4 Component alignment tests

Prove player-attached components stay together:

- name;
- score;
- pip count;
- dice;
- owned cube;
- offered cube;
- bar;
- off tray.

### 19.5 Text tests

Prove text is redrawn upright:

- point numbers;
- Homey and Foey;
- cube values;
- movement labels;
- status labels.

Do not accept snapshots with mirrored or upside-down glyphs.

### 19.6 Decision tests

Prove:

- `00 + none` is neutral;
- `00 + roll_double` shows no offered cube;
- `D + take_pass` shows the offered cube;
- no-dice state alone never creates an offered cube.

## 20. Visual review process

Preserve prior gallery assets as historical comparison evidence.

Do not treat an unchanged historical image as correct when it conflicts with this contract.

Generate one embedded-SVG HTML gallery.

For each fixture show:

```text
XGID
decoded player mapping
on-roll player
near player
mirror-horizontal value
cube owner or offerer
decision
inspection instruction
repository commit
review status
```

Marty reviews:

- factual player mapping;
- Homey bottom in default view;
- Foey and Homey movement under vertical change;
- horizontal mirroring;
- upright point numbers and labels;
- cube and dice attachment;
- bar and off placement;
- movement overlays;
- clipping and overlaps.

## 21. Public API and internal boundaries

Public:

```r
ggboard()
backgammon_position()
normalize_xgid()
validate_xgid()
board_colors()
board_style()
board_moves()
```

Internal:

```text
XGID field decoders
source-role to player mapping
canonical layout builder
horizontal transform
vertical transform
point-label resolver
display-context resolver
movement application
render helpers
```

One decoder owns source-role mapping.

One layout layer owns transforms.

Rendering helpers must not independently remap players or flip components.

## 22. Release acceptance

Release requires:

- fixed XGID top/bottom mapping;
- Homey = player_1 and default near/bottom;
- Foey = player_0;
- case 18 passing;
- independent horizontal and vertical transforms;
- upright text after every transform;
- factual-state immutability;
- cube, dice, names, scores, pips, bars, off trays, and arrows moving consistently;
- four-state asymmetric fixture coverage;
- all automated tests passing;
- full gallery review;
- Marty acceptance;
- exact package commit, contract version, preset version, and gallery checksum.

## 23. Deferred work

- GNU Position ID and Match ID input
- GNU-to-XGID conversion
- Engine Kit integration
- Node integration
- D1
- analysis interpretation
- post-Crawford inference not present in XGID
- complete beaver and raccoon support
- public Position Library
