# Package Architecture

**Version:** 1.0  
**Status:** Accepted for implementation

## Core rule

```text
Board renders.
Calculator calculates.
Engine evaluates.
Website teaches.
```

## Components

### `backgammonboard`

Active R package for:

- complete XGID input;
- factual position state;
- perspective;
- static board rendering;
- dice, cube, score, and status display;
- one selected move overlay;
- normal `ggplot` output.

It does not evaluate strategy.

### `backgammoncalculator`

Deferred independent R package for transparent calculations such as match equity and take points.

It is backlog only and does not block the board, Shiny, or website work.

### `backgammon-engine-kit`

Owns:

- Sage, GNU, and other engine execution;
- engine-specific parsing;
- candidate ranking;
- rollout or search settings;
- engine provenance;
- structured engine reports.

No engine-kit change is required for the board package.

## Dependencies

- `backgammonboard` and `backgammoncalculator` do not depend on one another.
- Neither R package requires Python, an engine process, a worker, a browser, or a network connection.
- Quarto, Shiny, or the website may combine them.
- No shared package is created until substantial stable duplication proves a need.

## Direct R use

Both packages are ordinary R packages intended for:

```text
R scripts
Quarto
R Markdown
notebooks
tests
Shiny
```

Example:

```r
library(backgammonboard)

ggboard(
  xgid,
  decision = "take_pass",
  perspective = "decision_maker",
  colors = board_colors("bs"),
  style = board_style("bs")
)
```

## Shared vocabulary

```yaml
players:
  semantic: [white, black]
  relative: [player, opponent]

cube_ownership:
  semantic: [center, white, black]
  relative: [center, player, opponent]

play_context:
  - unlimited
  - match

board_decisions:
  - checker_play
  - roll_double
  - take_pass
  - none

crawford_status:
  - not_applicable
  - none
  - crawford
  - post_crawford

score_representation:
  board_factual: raw
  calculator_match: away
```

Rules:

- use `unlimited`, not `money`, in package APIs;
- White and Black are factual identities;
- player and opponent are relative roles;
- semantic and relative ownership must not be silently mixed.

A future calculator stores probabilities internally as fractions in `[0, 1]`, accepts explicitly declared fraction or percent input, and normally prints percentages.

## Identifier boundary

- XGID is canonical for `backgammonboard` v1.
- GNU Position ID and Match ID conversion are deferred.
- A consuming application may temporarily use an upstream converter.
- Full GNU analysis parsing stays outside both R package cores.

## Relationship to `bglab`

`bglab` is a prototype and research source, not the new architecture.

Migrate only verified behavior after recording source, licence, attribution, and tests. Do not copy the monolithic renderer wholesale.

## Website boundary

The website and Shiny app may combine outputs, manage controls, request engine analysis, cache results, and add teaching content.

The packages do not own navigation, authentication, rate limits, iframe behavior, deployment, worker scheduling, or editorial claims.

## Implementation sequence

```text
1. XGID normalization and diagnostics
2. factual position parsing
3. canonical coordinate fixtures
4. colours, styles, geometry, and checkers
5. perspective, score, status, dice, and cubes
6. selected move overlays
7. ggboard() orchestration
8. automated snapshots
9. Marty manual rendering review
10. package release acceptance
11. Shiny migration
12. website architecture update
```

## Private documentation

```text
backgammon-private/
└── architecture/
    ├── PACKAGE_ARCHITECTURE.md
    ├── BACKGAMMONBOARD_CONTRACT.md
    └── backlog/
        └── BACKGAMMONCALCULATOR.md
```

Detailed test cases, snapshots, and implementation notes belong in the package repository, not in more architecture contracts.
