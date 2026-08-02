# Implementation status

Contract version: 1.2
Package version: 0.1.0

## Current release path

- complete-XGID normalization and structured diagnostics;
- fixed XGID top -> `player_0`/Foey and bottom -> `player_1`/Homey mapping;
- conservative display-context resolution and Section 11 precedence;
- one resolved near player plus independent horizontal mirroring;
- independent light-player palette selection, including a near-player option;
- canonical prepared layout followed by shared vertical/horizontal transforms;
- a game-status sentence pinned to the bottom viewport band;
- neutral and offered cubes pinned to the vertical midline, with non-neutral owned cubes on the owner's side;
- factual dice, cube, score, Crawford, bar, and borne-off rendering;
- ordered structured movements with deterministic application;
- limited supplied-die checking and optional checker-layout `after_xgid` proof;
- static `ggplot` output and neutral/BMS presets.

## Deferred outside v1.2

GNU Position/Match ID, GNU move notation, Engine Kit, Node, AnkiGammon,
RendererPosition, analysis, complete legal-play generation, post-Crawford XGID
inference, and beaver/raccoon visual acceptance.

Historical contracts remain under `inst/contracts/` for provenance. They are
not current API authority unless the contract index marks them current.

## Verification

```r
devtools::test()
devtools::check(args = "--no-manual")
```

```text
Rscript dev/render-v12-gallery.R <staging-directory> <output-directory>
Rscript dev/render-v12-comparison.R <comparison-directory> <corrected-commit>
```
