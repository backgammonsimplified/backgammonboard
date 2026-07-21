# `after_xgid` Checker-Arrangement Comparison Contract

## Status

Architecture seed only. No move application or comparison code is implemented in this branch.

## Purpose

`after_xgid` is optional evidence for the checker arrangement produced by the supplied ordered moves.

It never replaces:

```text
the starting position
the supplied move notation
the normalized atomic steps
the supplied move order
```

## Default comparison scope

Compare exactly:

```text
24 signed point occupancies
White bar count
Black bar count
White borne-off count
Black borne-off count
```

The comparison is semantic by player identity and must not depend on board perspective.

## Excluded metadata

Do not require these fields to match during the default checker-arrangement comparison:

```text
dice
on-roll player
turn metadata
cube value
cube owner
match score
match length
Crawford flag
Jacoby or beaver metadata
encoded maximum-cube metadata
```

A later strict mode may compare explicitly named metadata fields, but strict comparison must be opt-in.

## Result shape

The later comparison helper should return a stable structured result such as:

```r
list(
  matches = FALSE,
  compared = c("points", "bar", "off"),
  mismatches = data.frame(
    component = "point",
    location = 8L,
    expected = 2L,
    actual = 1L
  )
)
```

## Failure behavior

When supplied moves and `after_xgid` disagree on checker arrangement:

```text
before position + ordered moves != after_xgid
    error
```

The implementation must not reorder, alter, infer, or discard supplied atomic steps to make them fit `after_xgid`.
