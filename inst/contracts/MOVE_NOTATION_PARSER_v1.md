# Native R Move-Notation Parser Contract v1

## Status

Implemented parser contract for `board_moves()`.

This contract covers notation parsing only. Move application, checker-play
legality, hit detection, `after_xgid` comparison, and overlay rendering remain
deferred.

## Public entry point

```r
board_moves(notation)
```

`notation` is one length-one character value representing one selected checker
play.

## Accepted notation

```text
13/8
13/8 6/5
13/8, 6/5
24/18/13
13/8(2)
bar/24
6/off
13/8*
6/5*/3
13/10(2) 8/5(2)
```

Parsing is case-insensitive. Output tokens are normalized to lower case. Move
groups may be separated by whitespace or commas.

## Atomic-step schema

One output row represents one ordered atomic movement:

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

Types:

```text
from_type: point | bar
to_type: point | off
```

Point values are integers from 1 through 24. `from_point` is missing for a bar
source. `to_point` is missing for an off destination.

## Ordering and chains

`step_id` is the complete atomic display order.

A compound token such as:

```text
24/18/13
```

creates two steps with one `chain_id`:

```text
24 -> 18
18 -> 13
```

A repeated token such as:

```text
13/8(2)
```

creates two distinct chains. Both rows share one `repeat_group`.

## Hit markers

A marker such as:

```text
13/8*
```

sets `hit_marked = TRUE` for the step whose destination is 8.

For:

```text
6/5*/3
```

only the first atomic step is marked. The parser preserves the notation claim;
it does not inspect a position or prove that a hit occurred.

## Structural validation

The parser rejects:

```text
missing, empty, non-character, or vector input
empty comma-separated tokens
move tokens without a from/to pair
points outside 1 through 24
bar outside the first location
off outside the final destination
hit markers on a source or on off
malformed repetition suffixes
```

Errors include the failing token index and, when applicable, the location index.

## Explicit exclusions

`board_moves()` does not:

```text
read an XGID
apply checker movements
check source occupancy
check movement direction
check dice use
check blocked points
resolve hits
check bar priority
check bearing-off legality
limit a play to a particular dice sequence
compare after_xgid
create overlay coordinates
render arrows
```

Those responsibilities belong to later pipeline stages.

## Behavioral reference

The implementation is an independently authored native R behavioral port. It
uses the pinned AnkiGammon move parser as a behavioral reference and fixture
oracle, not as a runtime dependency or line-by-line source translation.

```text
project: AnkiGammon
release: v1.7.0
commit: eb39c8875691c0bfe1d096157254e6c4245eb5af
reference: ankigammon/utils/move_parser.py
reference tests: tests/test_basic.py, TestMoveParser
license: MIT
```
