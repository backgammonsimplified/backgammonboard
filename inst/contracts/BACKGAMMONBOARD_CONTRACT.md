# `backgammonboard` Contract

**Version:** 1.0  
**Status:** Accepted for implementation  
**Package and repository:** `backgammonboard`  
**Primary output:** static `ggplot`  
**Canonical identifier:** complete XGID

> **Implementation amendment:** `XGID_CUBE_RENDER_AMENDMENT_v1.md` supersedes
> conflicting older wording for current-game Crawford state, the package cube
> limit, receiver-aware offered-cube placement, and completing `ggboard(xgid)`
> before optional move support.

## 1. Scope

`backgammonboard` represents and renders factual backgammon positions for R scripts, Quarto, notebooks, tests, and Shiny.

It may frame a position as:

```text
checker play
Roll or Double
Take or Pass
neutral factual board
```

It does not evaluate strategy, rank moves, run engines, calculate match equity, generate all legal plays, or provide a board editor.

## 2. Public API

```r
ggboard()
backgammon_position()
normalize_xgid()
validate_xgid()
board_colors()
board_style()
board_moves()
```

Everything else remains internal until an external use is demonstrated.

## 3. Processing pipeline

```text
complete XGID
→ factual position
→ display context
→ prepared layout
→ optional move overlay
→ ggplot
```

One perspective transform controls points, checkers, labels, dice, cubes, off trays, information rows, and arrows.

Factual state never changes merely to change perspective.

## 4. XGID input

Accept:

```text
XGID=<complete-position-and-metadata>
<complete-position-and-metadata>
```

Do not accept only the 26-character checker payload in v1.

### `normalize_xgid()`

```text
valid supported scalar input → canonical full XGID
invalid or incomplete input  → error
```

### `validate_xgid()`

```text
valid scalar text   → backgammon_xgid_validation(valid = TRUE)
invalid scalar text → backgammon_xgid_validation(valid = FALSE)
programmer misuse   → error
```

Programmer misuse includes unsupported object types and vectors whose length is not one.

Validation result:

```r
list(
  valid = TRUE,
  canonical_xgid = "XGID=...",
  errors = data.frame(code, category, field, message),
  warnings = data.frame(code, category, field, message)
)
```

Stable diagnostic codes should cover missing fields, invalid payload length or characters, invalid ownership, turn, dice, match length, and impossible checker totals.

## 5. Factual position

Constructor and class:

```r
position <- backgammon_position(x)
# class: backgammon_position
```

Core fields:

```r
list(
  xgid,
  play_context,
  points,
  bar,
  off,
  on_roll,
  dice,
  cube_value,
  cube_owner,
  score,
  match_length,
  crawford_status,
  max_cube
)
```

### Play context

```text
unlimited
match
```

### Scores

Store raw factual scores:

```r
score <- c(white = raw_white_score, black = raw_black_score)
```

For match play, derive:

```r
away <- match_length - score
```

For unlimited play:

```text
match_length = not applicable
crawford_status = not_applicable
```

Display “Unlimited play” and no match score, regardless of `score_format`.

### Canonical points

Implementation target:

```r
points <- integer(24)

# index 1  = White's 1-point
# index 24 = White's 24-point
# positive = White
# negative = Black

bar <- c(white = 0L, black = 0L)
off <- c(white = 0L, black = 0L)
```

For a Black mover, mover-relative point `p` maps to `25L - p`.

Verify this against:

1. the opening position;
2. an asymmetric position;
3. known White and Black checker locations;
4. at least one bar checker;
5. preferably a borne-off checker.

## 6. Display context

Internal context values:

```text
decision:
    auto
    checker_play
    roll_double
    take_pass
    none

perspective:
    decision_maker
    on_roll
    white
    black

score_format:
    away
    raw
    both
```

### Conservative `auto`

```text
moves supplied          → checker_play
otherwise dice present  → checker_play
otherwise               → none
```

`auto` never infers a cube decision.

### Decision maker

```text
checker_play  → player on roll
roll_double   → player on roll
take_pass     → legal cube receiver
none          → undefined; perspective falls back to player on roll
```

## 7. Rendering API

```r
ggboard(
  x,
  colors = board_colors(),
  style = board_style(),
  moves = NULL,
  after_xgid = NULL,
  decision = "auto",
  perspective = "decision_maker",
  score_format = "away"
)
```

`x` may be a complete XGID or `backgammon_position`.

The result is a normal `ggplot` compatible with `print()`, `ggsave()`, Quarto, R Markdown, and Shiny.

Package defaults are neutral. Backgammon Simplified uses explicit presets:

```r
board_colors("bs")
board_style("bs")
```

Preset constructors:

```r
board_colors(name = "default", overrides = NULL)
board_style(name = "default", overrides = NULL)
```

Required v1 preset IDs:

```text
default
bs
```

Unknown presets and override keys error.

## 8. Decision and factual-state precedence

Explicit instructional context may supplement factual XGID state but may not contradict it.

```text
dice present + checker_play → valid
dice present + roll_double  → error
dice present + take_pass    → error
no dice + legal roll_double → valid
no dice + legal take_pass   → valid
```

A factual double marker may corroborate Take/Pass, but `auto` still does not infer it.

Authored Take/Pass without a marker may be allowed when otherwise legal. Record whether context was `xgid_corroborated` or `authored`.

## 9. Dice

```text
White on roll:
    light dice
    right of bar in canonical orientation

Black on roll:
    dark dice
    left of bar in canonical orientation
```

The complete layout is transformed once for perspective.

Render two distinct dice with correct pips. Hide dice in cube-decision contexts. Never show placeholder dice.

## 10. Cube behavior

| Decision | Dice | Normal cube | Offered cube |
|---|---:|---:|---:|
| checker play | factual | factual | no |
| Roll/Double | hidden | factual | no |
| Take/Pass | hidden | hidden | doubled value |
| none | factual if present | factual | no |

For Take/Pass:

```text
offered value = 2 × factual cube value
```

Cube states:

- centered cube: unused, in the middle outside the playing field;
- White-owned cube: outside the field on White’s semantic side;
- Black-owned cube: outside the field on Black’s semantic side;
- offered cube: on the playing surface and visually distinct;
- Crawford: cube hidden; Roll/Double and Take/Pass error;
- post-Crawford: factual cube restored;
- maximum cube: factual cube renders, but illegal further offers error.

Reject legally impossible contexts. Allow legally possible but strategically poor questions.

Automated cube fixtures must cover centered, both owners, offered cube, unlimited and match play, Crawford, post-Crawford, maximum cube, factual-dice conflicts, centered-cube offers, owned-cube redoubles, and missing legal offerer or receiver.

## 11. Checkers and layout

The package must support:

- one through five visible checkers on a point;
- larger stacks with a documented count label;
- large stacks near the bar;
- checkers on both bars;
- borne-off checker counts;
- point numbers inside the rails;
- separate checker-ring and checker-face shapes;
- stable pip, score, and status columns;
- no overlap at documented output sizes.

## 12. Moves

Constructor:

```r
board_moves(from, to, die = NULL, label = NULL)
```

Each row is one ordered atomic movement.

Locations:

```text
1 through 24
bar
off
```

The mover is always `position$on_roll`; the move object does not duplicate it.

### Structural validation

Check recognized locations, complete rows, movement order, optional die values, usable labels, no source `off`, and no destination `bar`.

### State application

Check or apply source occupancy, direction, blocks, hits, bar entry, bearing off, repeated movement, multiple arrivals, doubles, and resulting layout.

### Null-die behavior

When `die` is supplied:

```text
die_validation_status = checked
```

Validate atomic distance and applicable entry or bearoff distance.

When `die = NULL`:

```text
die_validation_status = not_checked
full_play_validation_status = not_performed
```

Still validate source occupancy, direction, blocking, hits, bar/off state, and resulting layout.

Do not claim to validate exact die distance, oversize bearoff rules, die assignment, maximum dice use, higher-die rules, alternate plays, or complete-play legality.

## 13. Move overlay and `after_xgid`

A selected play may show ordered arrows, source ghosts, destination markers, hits, bar entry, bearoff, repeated movement, and doubles.

Move order must not rely on colour alone.

`after_xgid` is optional authoritative checker-layout evidence.

```text
before position + ordered moves ≠ after position → error
```

It does not replace the displayed before-position. Supplying it without moves is an error. Moves are never altered to fit it.

## 14. Automated fixtures

At minimum:

```text
White on roll
Black on roll
centered cube
White-owned cube
Black-owned cube
offered cube
checker play
Roll or Double
Take or Pass
neutral board
unlimited play
ordinary match
Crawford
post-Crawford
maximum cube
small checker stacks
six-plus stacks
bar stacks
borne-off checkers
hit
bar entry
bearoff
repeated movement
four-part double
White perspective
Black perspective
after_xgid match
after_xgid mismatch
```

Use deterministic presets, stable dimensions, and documented font assumptions.

## 15. Marty manual review

After implementation and snapshot generation, Marty reviews:

- checker stacks and labels;
- bars and borne-off counts;
- centered, White-owned, Black-owned, offered, and maximum cubes;
- Crawford and post-Crawford;
- unlimited and match status;
- raw, away, and combined score displays;
- both perspectives;
- dice, doubles, and absent dice;
- checker play, Roll/Double, Take/Pass, and neutral boards;
- move arrows over dense positions;
- small and standard output sizes.

Record:

```yaml
manual_render_review:
  reviewer: Marty Gale
  date: YYYY-MM-DD
  package_commit: null
  contract_version: 1.0
  bs_preset: {id: bs, version: 1.0}
  status: pass
  failed_fixtures: []
  approved_exceptions: []
  notes: []
```

Allowed status:

```text
pass
pass_with_approved_exceptions
fail
```

No public release may contain an unresolved false or misleading render.

## 16. Implementation order

Do not begin by copying or rewriting the old `ggboard.R`.

```text
1. normalize_xgid()
2. validate_xgid()
3. backgammon_position()
4. coordinate fixtures
5. board_colors() and board_style()
6. geometry and checkers
7. perspective
8. scores, pips, and status
9. dice
10. centered and owned cubes
11. offered cube and legality
12. board_moves()
13. move application
14. overlay
15. after_xgid
16. ggboard()
17. snapshots
18. Marty review
19. Shiny migration
```

## 17. Release acceptance

Release requires:

- verified XGID parsing and coordinates;
- correct unlimited and match displays;
- all decision, dice, cube, checker-stack, bar, move, and perspective fixtures passing;
- structured XGID diagnostics;
- `after_xgid` validation;
- normal `ggplot` output;
- Quarto and Shiny examples;
- passing package checks;
- migrated-source provenance;
- Marty manual review;
- recorded package, contract, and BS preset versions.
