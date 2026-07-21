# Cube-State Model Contract v1

## Scope

`backgammonboard` separates factual position state from optional instructional context and resolved drawing state.

```text
identifier
→ factual position
→ optional display or decision context
→ resolved display state
→ layout
→ rendering
```

The factual position does not store instructional strings.

## Factual fields

`backgammon_position()` exposes:

```text
on_roll
dice
cube_value
cube_owner
match_length
score_white
score_black
is_crawford
```

Canonical semantic players and owners are lower-case internally:

```text
white
black
center
```

`score` is retained as a named compatibility vector, but `score_white` and `score_black` are the explicit factual fields.

## Crawford

Only the current game's Crawford fact is represented:

```text
is_crawford = TRUE
is_crawford = FALSE
```

There is no `post_crawford` state. A non-Crawford game follows ordinary cube-display and context rules.

A Crawford position retains the factual cube value and owner, but `resolve_cube_display()` returns a hidden display. Cube offers and cube-decision contexts are rejected with reason `crawford`.

## Package-supported cube limit

The package-supported maximum factual and displayed cube value is:

```text
64
```

This is a package interoperability and rendering contract. It is not a claim that every backgammon rule set universally prohibits larger cube values.

Required behavior:

```text
32 → 64      supported
64 → 128     rejected with package_cube_limit
factual > 64 rejected as unsupported XGID input
```

The XGID encoded maximum-cube metadata is preserved separately. It does not increase the package-supported factual cube limit.

## Pending offer context

A pending offer is represented separately from factual ownership:

```r
cube_offer_context(
  offerer = "white",
  receiver = "black",
  current_value = 8L,
  offered_value = 16L
)
```

The current factual owner and value are not mutated until a later response is resolved outside this contract.

## Cube display resolver

```r
resolve_cube_display(position, offer = NULL, max_cube_value = 64L)
```

Stable states:

```text
hidden
centered
owned
offered
```

The renderer consumes this result. It does not decide offer legality.

## Offer-context validation

```r
validate_cube_offer_context(position, offer = NULL, max_cube_value = 64L)
```

Structured reason values include:

```text
valid
crawford
dice_already_rolled
opponent_owns_cube
package_cube_limit
match_cube_limit
missing_offerer
missing_receiver
same_offer_parties
offerer_not_on_roll
invalid_offer_value
```

`offer_already_pending` is a board-decision-context error when a new `cube_offer` decision is requested while a pending offer is already supplied.

This validation is intentionally narrower than a complete backgammon rules engine.

## Match cube limit

For the player making the offer:

```text
offerer_away = match_length - offerer_raw_score
```

Another offer is unavailable when:

```text
current_cube_value >= offerer_away
```

Example at 5-away:

```text
owns 2 → may offer 4
owns 4 → may offer 8
owns 8 → may not offer 16
```

The factual cube still renders when another offer is unavailable.

## Status text

Default status is factual:

```text
White on roll
White on roll, to play: 5-1
```

Decision questions require explicit `board_context()`:

```text
cube_offer    → Roll or Double? or Roll or Redouble?
cube_response → Take or Pass?
```

The package never infers a quiz merely because a cube action is available.
