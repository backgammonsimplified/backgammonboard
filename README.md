# backgammonboard

`backgammonboard` is a focused R package for factual backgammon positions and
static board rendering.

## Implementation checkpoint

Completed in this checkpoint:

1. `normalize_xgid()`
2. `validate_xgid()`
3. `backgammon_position()`
4. opening and asymmetric coordinate fixtures

The renderer has deliberately not been started yet. The accepted contract
requires factual XGID parsing and canonical coordinates to be established
before geometry and drawing code.

## Current public API

```r
normalize_xgid(x)
validate_xgid(x)
backgammon_position(x)
```

The remaining accepted public functions will be added in contract order.
