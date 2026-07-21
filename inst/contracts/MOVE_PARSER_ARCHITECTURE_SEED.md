# Native R Move-Parser Architecture Seed

## Branch scope

This branch defines architecture and deterministic fixtures only. It does not implement parsing, move application, checker-play legality, `after_xgid` comparison, or overlays.

## Pinned behavioral reference

```text
project: AnkiGammon
upstream file: ankigammon/utils/move_parser.py
upstream test file: tests/test_basic.py, TestMoveParser
upstream commit: eb39c8875691c0bfe1d096157254e6c4245eb5af
upstream tag: v1.7.0
source archive: ankigammon-1.7.0.tar.gz
source archive SHA256: 3c82ef42e0a603431d5839b7fa115bc416ca92d8c0df903dd3f99aa1be88e13c
license: MIT
```

The planned R implementation is a behavioral port, not a line-by-line translation. No Python runtime dependency will be added.

## Source and test inspection

The pinned parser and its direct tests were inspected before this architecture seed was frozen.

Observed parser behavior:

```text
lower-cases and trims notation
returns no checker steps for cube-action words
splits move groups on whitespace or commas
expands repetition suffixes such as (4)
expands chained notation into consecutive atomic movements
maps bar and off to sentinel values
strips hit markers before producing tuples
skips malformed components instead of returning structured diagnostics
also contains move-application and formatting helpers
```

The direct upstream tests in `tests/test_basic.py::TestMoveParser` cover:

```text
13/9 6/5
bar/22
6/off
```

The local fixture corpus deliberately goes beyond those direct tests. In particular, the R design preserves `hit_marked`, `chain_id`, `source_token`, and `repeat_group` rather than discarding that notation information.

No upstream Python source is included in this branch. The future R code must be independently expressed and tested against the local contract.

## Pipeline

```text
move notation
→ parse_board_moves()
→ normalized atomic steps
→ apply_board_moves()
→ resulting checker arrangement
→ compare_after_xgid()
→ draw_move_overlay()
```

Only the contract and fixture corpus are created in this branch.

## Normalized atomic-step schema

```text
step_id
chain_id
source_token
from_type
from_point
to_type
to_point
hit_marked
repeat_group
```

Token types:

```text
from_type: point | bar
to_type: point | off
```

`hit_marked` records the notation claim only. Actual hit detection belongs to later move application.

`chain_id` preserves repeated use of one checker through compound notation. `step_id` preserves complete display order. Move order must never be reconstructed from geometry or color.

## Seed corpus

The fixture at `inst/fixtures/move-notation-seed.csv` covers:

```text
13/8
13/8 6/5
24/18/13
13/8(2)
bar/24
6/off
13/8*
6/5*/3
four-part doubles
repeated use of one checker
```

## Separation of responsibilities

Parsing must not:

```text
apply moves
claim full move legality
detect unmarked hits
render arrows
alter move order
```

Move application will later consume normalized atomic steps and factual checker state. Overlay rendering will consume the ordered applied result.

## after_xgid comparison contract

The detailed comparison contract is stored at:

```text
inst/contracts/AFTER_XGID_COMPARISON_v1.md
```

Default comparison scope:

```text
24 point occupancies
White and Black bar counts
White and Black borne-off counts
```

Excluded unless strict validation is explicitly requested:

```text
dice
on-roll player
turn metadata
cube value
cube owner
match score
Crawford flag
other non-checker metadata
```

`after_xgid` never replaces the starting position, supplied notation, normalized steps, or move order. A mismatch is an error and the supplied moves are never rewritten to fit the evidence.
