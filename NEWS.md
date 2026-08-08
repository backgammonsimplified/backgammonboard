# backgammonboard 0.1.0

* Adds factual backgammon position parsing and static `ggplot2` rendering from
  complete XGIDs.
* Accepts complete GNUIDs through the same `ggboard()` entry point when the
  optional `backgammoncalculator` package version 0.2.0 or later is installed.
* Separates factual player state from display perspective, horizontal mirroring,
  checker palette assignment, labels, dice, cube, score, and status rendering.
* Adds structured checker-movement rendering with arrows, destination ghosts,
  hits, bar entry, bearing off, repeated movements, and deterministic layout
  transforms.
* Adds neutral defaults plus the explicit Backgammon Simplified `bs` color and
  style presets.
* Returns ordinary `ggplot` objects for use in scripts, reports, notebooks, and
  applications.
