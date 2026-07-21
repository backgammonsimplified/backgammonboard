# Implementation status

Contract version: 1.0
Package version: 0.1.0

## Completed in the XGID render integration checkpoint

1. strict complete-XGID normalization and structured validation;
2. turn-relative payload decoding into stable White-relative facts;
3. factual `backgammon_position()` with `is_crawford`, `action_marker`, and
   separate XGID maximum-cube metadata;
4. package-supported factual cube limit of 64;
5. centered, owned, hidden, and receiver-aware offered cube states;
6. explicit versus neutral status context;
7. semantic White and Black perspective across points, checkers, bars, off
   markers, dice, cubes, and information rows;
8. public `ggboard(xgid)` returning an ordinary `ggplot`;
9. static differential XGID fixtures and end-to-end tests;
10. deterministic 17-image factual visual-review script.

## Deferred

```text
board_moves()
move parsing
move application
after_xgid validation
move overlays
Shiny migration
website integration
```

## Review gate

Run:

```r
devtools::test()
source("dev/xgid-render-fixtures.R")
devtools::check()
```

Review the generated images under:

```text
dev/preview-output/xgid-render-fixtures/
```

Do not release while any factual render is false or misleading.
