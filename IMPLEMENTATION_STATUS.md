# Implementation status

Contract: `backgammonboard` 1.0

## Completed

- [x] `normalize_xgid()`
- [x] `validate_xgid()`
- [x] `backgammon_position()`
- [x] opening coordinate fixture
- [x] asymmetric coordinate fixture with bar and borne-off checkers

## Next

- [ ] `board_colors()` and `board_style()`
- [ ] geometry and checkers
- [ ] perspective
- [ ] scores, pips, and status
- [ ] dice
- [ ] centered and owned cubes
- [ ] offered cube and cube legality
- [ ] `board_moves()`
- [ ] move application
- [ ] overlays
- [ ] `after_xgid`
- [ ] `ggboard()`
- [ ] snapshots
- [ ] Marty review
- [ ] Shiny migration

## Runtime verification

The implementation was prepared in an environment without R or Rscript.
R source and fixtures received static checks, and the coordinate expectations
were independently evaluated, but `testthat`, `devtools::check()`, and
`R CMD check` still need to be run in an R environment.
