# Contract Amendment: Core XGID and Cube Render Integration

**Applies to:** `BACKGAMMONBOARD_CONTRACT.md`, version 1.0
**Status:** Frozen for the XGID render integration branch

The following decisions supersede conflicting older wording:

```text
is_crawford is the only current-game Crawford field

non-Crawford includes ordinary games and games that may historically be
post-Crawford; no post_crawford state is introduced

package-supported factual cube limit is 64

XGID MC metadata is separate from the factual cube and package rendering limit

core ggboard(xgid) is completed before optional move support

later move arguments extend ggboard() without changing ggboard(xgid)
```

## Public core path

```text
complete XGID
→ validated factual position
→ resolved cube and optional decision context
→ perspective-aware layout
→ factual board rendering
→ ordinary ggplot
```

## Factual and display separation

```text
White and Black are stable semantic identities
bottom and top are display positions
perspective never mutates factual state
instructional questions require explicit context
```

## Pending offers

A factual `D` marker derives a separate pending offer. It does not mutate the
current factual cube owner or value. Both of these acceptance cases are required:

```text
offered to White
offered to Black
```

Placement is receiver-aware under the selected perspective. The two cases must
not share one hard-coded screen location.

## Deferred work

```text
board_moves()
move parsing
move application
after_xgid validation
move overlays
Shiny migration
website integration
```
