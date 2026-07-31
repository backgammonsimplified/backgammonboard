# RendererPosition to `backgammonboard` mapping

**Version:** 1.0  
**Accepted Engine Kit commit:** `33409334c6f4d6fca0d798ba4a324673e72e86ce`  
**Accepted position contract:** `universal-position-v1`  
**Accepted view contract:** `backgammon-view-v1`

## Boundary

```text
RendererPosition JSON
  position
  semantic_state_hash
  view
  view_hash
        |
        v
existing backgammon_position facts + retained view metadata
        |
        v
existing ggboard renderer
```

`renderer_position(x)` accepts:

- a parsed named R list representing the JSON object;
- a character scalar containing the JSON object;
- a character scalar naming an existing UTF-8 JSON file.

The adapter has no Python, Engine Kit, GNU, Sage, or JSON-package runtime
dependency. It validates the supported version names and the fields required
for mapping. It validates the presence and lowercase SHA-256 spelling of both
hashes, preserves them, and never calculates or replaces them.

## Stable player slots

W7's existing White/Black storage slots are reused rather than creating a
second board model:

| Engine Kit stable slot | W7 internal slot | Rendered label |
|---|---|---|
| `player_0` | `white` / positive point count | `player_0` |
| `player_1` | `black` / negative point count | `player_1` |

The White/Black names in this table are implementation slots, not identities
inferred from turn, checker colour, source spelling, or view orientation.
RendererPosition-derived information rows use the stable `player_0` and
`player_1` labels.

## Semantic position mapping

| Accepted field | Existing W7 field or behavior |
|---|---|
| `position.board.player_0.points[n-1]` | positive count at W7 point `n` |
| `position.board.player_1.points[n-1]` | negative count at W7 point `25-n` |
| `position.board.player_0.bar/off` | `bar["white"]` / `off["white"]` |
| `position.board.player_1.bar/off` | `bar["black"]` / `off["black"]` |
| `position.board.checker_count` | retained as `checker_count` |
| `position.state.on_roll` | `on_roll`, using the stable-slot table |
| `position.state.dice` | two integer dice; JSON `null` becomes absent dice |
| `position.state.game_state` | retained as `game_state` |
| `position.state.phase` | retained as `phase` |
| `position.state.decision_type` | retained as `decision_type` |
| `position.state.decision_player` | retained in the W7 stable slot, or `NA` |
| `position.cube.enabled` | retained as `cube_enabled`; known `false` hides the cube |
| `position.cube.value` | `cube_value` |
| `position.cube.owner` | `center`, `white` (`player_0`), or `black` (`player_1`) |
| pending `none` | normal factual cube rendering |
| pending `double` | existing factual `D` offer state |
| `position.score.match_length == 0` | `play_context = "unlimited"` |
| positive `position.score.match_length` | `play_context = "match"` and `match_length` |
| `position.score.player_0/player_1` | raw `score["white"/"black"]` |
| `position.rules.crawford` | `is_crawford` in match play |
| `position.rules.maximum_cube` | existing maximum-cube metadata |
| other accepted rule fields | retained without changing W7 move or cube legality |

For every physical point `n`, the adapter first checks that
`player_0.points[n-1]` and `player_1.points[24-n]` are not both occupied.
Changing the accepted view never changes the resulting signed point vector,
bar/off counts, scores, dice, cube facts, or semantic-state hash.

## Initial-release learner-view policy

For a RendererPosition envelope, the accepted `bottom_player` is the learner
and the accepted `top_player` is the opponent. The adapter records these as
`learner_slot` and `opponent_slot` and applies the following invariants:

- the learner is always displayed at the bottom and the opponent at the top;
- point labels are always in the learner's self-relative coordinates;
- `bottom_home_board_side` is an independent horizontal mirror choice;
- the on-roll player may be either the learner or the opponent;
- changing `state.on_roll` never rotates or mirrors the board;
- dice follow the canonical `state.on_roll` player;
- cube ownership follows the canonical `position.cube.owner` slot;
- an orange arrow is drawn immediately to the left of the displayed name of
  the on-roll player;
- visible status text and the plot's accessible alternative text explicitly
  identify the stable player slot that is on roll.

The adapter rejects an envelope whose `point_labels_for` is not the
`bottom_player`. `ggboard()` rejects any explicit `perspective` that would put
the opponent at the bottom. This enforcement is specific to
`backgammon_renderer_position`; existing XGID and plain
`backgammon_position` callers retain their legacy perspective choices.

The view is retained as `renderer_view` and used only by `ggboard()`:

| Accepted view field | W7 drawing behavior |
|---|---|
| `bottom_player` | identifies the learner and fixes that stable slot at the bottom |
| `top_player` | identifies the opponent and fixes that stable slot at the top |
| `bottom_home_board_side` | independently places the bottom home board left or right |
| `point_labels_for` | must equal `bottom_player`; labels physical points in learner-relative coordinates |
| `cube_display_side = left/right` | selects the normal cube's left/right display side |
| `rotation` | retained for diagnostics; physical fields above are authoritative |
| `view_origin` | retained for diagnostics |

Omitting `ggboard(..., perspective=)` uses the learner. Supplying the learner's
internal slot is allowed but redundant; supplying the opponent, `on_roll`, or
another choice that resolves to the opponent is rejected so on-roll changes
cannot alter the view.

## Identity, analysis, and appearance

Stable slots, not checker colours or theme names, determine learner/opponent,
on-roll, dice, cube owner, and decision identity. The internal White/Black
storage names and all `board_colors()`/`board_style()` choices are appearance
and implementation details only.

RendererPosition contains no engine percentages. Any website layer that adds
engine percentages, equities, or cube analysis must label the canonical player
perspective explicitly (for example, `player_1 perspective`). Cube-decision
status produced by this package names the canonical decision maker, such as
`player_1 to decide: Roll or Double?`; it must not be relabelled by colour or
display side.

The initial release uses the existing/default board appearance. Familiar
board-style galleries, user-created themes, hex colour selectors, and saved
custom themes are future scope and are not prerequisites for this integration.

## Bounded current limitations

The adapter fails clearly rather than inventing a drawing for:

- `state.on_roll = null`, because the current W7 factual model requires an
  on-roll player;
- match play with `rules.crawford = null`;
- pending beaver, raccoon, resignation, or unknown actions;
- a current cube above W7's existing value-64 rendering limit.

The envelope and view validate `cube_display_side` values `center`,
`off_board`, and `auto`, but the current W7 drawing path supports only `left`
and `right`; `ggboard()` reports the limitation if one of the other accepted
choices is requested.

When `cube.enabled` is `null` but nominal value and ownership are known, the
adapter preserves `NA` in `cube_enabled` and draws the supplied nominal cube.
W7 does not yet have a distinct visual marker for "cube availability unknown."
All accepted null rule fields are listed in `renderer_unknown_fields`.
