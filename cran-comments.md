## CRAN submission status

This is a new submission of `backgammonboard` 0.1.0.

## Test environments

Local release-candidate check completed on 2026-08-08:

- Ubuntu 24.04.4 LTS
- R 4.6.1 (2026-06-24)
- x86_64-pc-linux-gnu
- `R CMD build` source tarball
- `R CMD check --as-cran backgammonboard_0.1.0.tar.gz`

Result:

- 0 ERRORs
- 0 WARNINGs
- 1 NOTE

The complete package tests run from the source checkout with released
`backgammoncalculator` 0.2.0 installed also passed: 1096 PASS, 0 FAIL,
0 WARN, 0 SKIP.

Still to complete before submission:

- clean installation from the built source tarball with released
  `backgammoncalculator` 0.2.0 present
- clean installation and XGID smoke test with `backgammoncalculator` absent
- Winbuilder using current R-devel
- independent Codex terminal release-candidate audit
- final Codex pull-request review after any audit findings are resolved

## Check NOTE

The local `--as-cran` check reports one incoming-feasibility NOTE containing:

- new submission
- `backgammoncalculator` in `Suggests` is not in a mainstream repository

`backgammoncalculator` (>= 0.2.0) is optional and is used only for complete
GNUID input. Ordinary XGID parsing and rendering do not require it. Its source
and installation location are documented in `DESCRIPTION` and the README:

<https://github.com/backgammonsimplified/backgammoncalculator>

Tests that require `backgammoncalculator` use
`testthat::skip_if_not_installed("backgammoncalculator")`.
