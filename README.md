# backgammonboard

[![R-CMD-check](https://github.com/backgammonsimplified/backgammonboard/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/backgammonsimplified/backgammonboard/actions/workflows/R-CMD-check.yaml)
[![GitHub release](https://img.shields.io/github/v/release/backgammonsimplified/backgammonboard)](https://github.com/backgammonsimplified/backgammonboard/releases/latest)
[![GitHub downloads](https://img.shields.io/github/downloads/backgammonsimplified/backgammonboard/total)](https://github.com/backgammonsimplified/backgammonboard/releases)
[![License](https://img.shields.io/github/license/backgammonsimplified/backgammonboard)](https://github.com/backgammonsimplified/backgammonboard)

`backgammonboard` validates complete XGIDs and optional complete GNUIDs, constructs factual backgammon positions, and renders static `ggplot` boards with display and structured movement controls.

## Installation

Install the released package from the public GitHub repository with:

```r
remotes::install_github(
  "backgammonsimplified/backgammonboard"
)
```

A local source checkout can instead be installed with `R CMD INSTALL` or an R
package installation tool of your choice.

XGID rendering works with `backgammonboard` alone. Complete GNUID input is an
optional feature and additionally requires `backgammoncalculator` 0.2.0 or
later, available from
<https://github.com/backgammonsimplified/backgammoncalculator>.

`backgammonboard` validates complete XGIDs, constructs factual positions, and
renders static `ggplot` boards. Complete GNUIDs are also accepted when
`backgammoncalculator` is installed; GNUID input is converted to XGID and then
uses the same rendering path.

```text
complete XGID
-> XGID source-role decode and fixed project-player mapping
-> factual backgammon_position
-> explicit or conservatively resolved display context
-> canonical Homey-near prepared layout
-> independent vertical and horizontal display transforms
-> optional structured movements
-> ggplot

complete GNUID
-> backgammoncalculator::gnuid_to_xgid()
-> existing complete-XGID path
```

```r
library(backgammonboard)

xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:0:0:10"
plot <- ggboard(xgid)
inherits(plot, "ggplot")
```

A complete GNUID can be passed through the same entry point:

```r
gnuid <- "4HPwATDgc/ABMA:8IhuACAACAAE"
plot <- ggboard(gnuid)
```

The fixed factual identities are `player_0` (XGID top player, Foey) and
`player_1` (XGID bottom player, Homey). Homey is near by default. Perspective
changes only the near player; `mirror_horizontal` independently changes the
left-right arrangement. Neither display control changes factual identity.
The independent `light_player` control selects which factual player uses the
light visual palette; set it to `"near_player"` to keep the bottom player light.
Neutral centered and offered cubes remain on the board's vertical midline.
When the cube exponent is non-zero, an owned cube remains outside on its
factual owner's vertical side.

## Display context

`auto` is conservative: structured movements imply `checker_play`, otherwise
factual dice imply `checker_play`, otherwise the decision is `none`. Cube
questions must be explicit.

```r
ggboard(xgid_without_dice, decision = "roll_double")
ggboard(xgid_with_D_marker, decision = "take_pass")
ggboard(xgid, perspective = "player_1")
ggboard(xgid, perspective = "player_0", mirror_horizontal = TRUE)
ggboard(xgid, perspective = "player_0", light_player = "near_player")
```

## Structured movements

The package does not parse source notation. Supply ordered atomic movements:

```r
moves <- board_moves(
  from = c(13, 6),
  to = c(8, 5),
  die = c(5, 1),
  label = c("first", "second")
)

ggboard(xgid, moves = moves, after_xgid = optional_after_xgid)
```

`backgammonboard` renders positions and supplied movement geometry. Rendering a
movement does not certify that the complete play is legal for the position and
dice. Structural input and application failures are rejected, while complete
dice consumption, maximum-dice and higher-die rules, doubles multiplicity,
complete bar-entry priority, combined-dice legality, complete legal-play
enumeration, and agreement with external engines are outside this release.

## Presets

Package defaults are neutral. Backgammon Simplified styling is explicit:

```r
ggboard(xgid, colors = board_colors("bs"), style = board_style("bs"))
```

## Public API

```r
ggboard()
backgammon_position()
normalize_xgid()
validate_xgid()
board_colors()
board_style()
board_moves()
```

## License and attribution

`backgammonboard` is Copyright (C) 2026 Marty Gale and is licensed under the
GNU Affero General Public License, version 3.

Selected XGID decoding behavior was informed by the MIT-licensed AnkiGammon
project. The package contains an independently authored native R implementation;
AnkiGammon source code is not bundled and AnkiGammon is not a runtime
dependency. Third-party notices and retained upstream license text are in
[`THIRD_PARTY_NOTICES.md`](inst/THIRD_PARTY_NOTICES.md),
[`XGID_SOURCES.md`](inst/provenance/XGID_SOURCES.md), and
[`AnkiGammon-MIT.txt`](inst/licenses/AnkiGammon-MIT.txt).

The software license does not grant rights to use the Backgammon Simplified
name, logo, or branding in a way that suggests an unofficial fork is the
official project.
