# Mapping and perspective migration: v1.1 to v1.2

Contract v1.2 corrects the factual convention used by v1.1.

The authoritative mapping is now:

- XGID top / lowercase / turn `-1` / cube owner `-1` -> `player_0` -> Foey;
- XGID bottom / uppercase / turn `1` / cube owner `1` -> `player_1` -> Homey;
- first XGID score -> `player_1`; second XGID score -> `player_0`;
- XGID character 0 -> `player_0` bar; character 25 -> `player_1` bar;
- point order is fixed and never reverses because of turn.

The v1.1 reverse mapping is not preserved. Code that attached Homey to
`player_0`, Foey to `player_1`, or treated turn as a reason to reverse the
checker payload must update its factual expectations.

Display orientation also changes from a coupled perspective/home-board model
to two independent controls:

```r
ggboard(x, perspective = "player_1", mirror_horizontal = FALSE)
```

`perspective` resolves only `near_player`. `mirror_horizontal` controls only
left-right placement. The old `point_1_side` argument remains as a compatibility
alias when supplied alone, but new code should use `mirror_horizontal`.

The default is Homey (`player_1`) near/bottom and not mirrored. Display
transforms operate on prepared coordinates and never mutate factual state.

Light/dark styling is also an independent display choice. The default keeps
`player_1` light; callers can choose a factual player or keep the near/bottom
player light as perspective changes:

```r
ggboard(x, perspective = "player_0", light_player = "near_player")
```

The match/game status sentence remains in the bottom information band. Player
names, scores, pip counts, and on-roll arrows continue to follow their factual
players through the vertical transform.
