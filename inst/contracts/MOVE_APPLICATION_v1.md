# Deterministic Move Application Contract

**Contract version:** 1.0  
**Status:** implementation checkpoint  
**Scope:** checker-state application only

## Purpose

Apply an ordered `backgammon_board_moves` object to a factual
`backgammon_position` without evaluating strategy or searching for alternate
plays.

```text
backgammon_position
+ ordered atomic movements
-> resulting checker arrangement
```

The starting position remains unchanged.

## Internal entry point

```r
apply_board_moves(position, moves)
```

This helper remains internal during this checkpoint. `board_moves()` remains
the public notation parser.

## Player and direction

The mover is the factual `position$on_roll` player.

```text
White: point numbers decrease toward off
Black: point numbers increase toward off
```

White enters from the bar on points 19 through 24. Black enters from the bar
on points 1 through 6.

## Applied result

Return a `backgammon_applied_moves` object containing:

```text
starting_xgid
player
moves
applied_steps
points
bar
off
checker_state
die_validation_status
full_play_validation_status
```

Each applied step preserves the parser fields and adds:

```text
player
hit_confirmed
hit_player
entered_from_bar
borne_off
```

`hit_marked` is a notation claim. `hit_confirmed` is derived from the factual
checker state at the moment the atomic step is applied.

## Checked now

```text
source occupancy
semantic player ownership
ordered application
compound-chain continuity
movement direction
bar priority
bar-entry range
blocked points
hits
bearing off only when all checkers are in the home board
resulting checker totals
```

An unmarked hit is detected and applied. A marked hit with no opposing blot is
an error.

## Explicitly not checked

```text
exact die distance
oversize bearing-off die rules
die assignment
maximum playable dice
higher-die requirement
alternate legal sequences
full-play legality
```

The result records:

```text
die_validation_status = not_checked
full_play_validation_status = not_performed
```

## Deferred

```text
after_xgid comparison
overlay coordinates
arrow rendering
source ghosts
destination markers
Shiny integration
```
