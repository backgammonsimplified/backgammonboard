# Implementation status

Contract version: 1.1
Package version: 0.1.0

## Current release path

- complete-XGID normalization and structured diagnostics;
- fixed `player_0` and `player_1` factual state;
- conservative display-context resolution and Section 11 precedence;
- one resolved perspective shared by the visual layer;
- factual dice, cube, score, Crawford, bar, and borne-off rendering;
- ordered structured movements with deterministic application;
- limited supplied-die checking and optional checker-layout `after_xgid` proof;
- static `ggplot` output and neutral/BMS presets.

## Deferred outside v1.1

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
Rscript dev/render-v11-gallery.R <staging-directory> <output-directory>
```
