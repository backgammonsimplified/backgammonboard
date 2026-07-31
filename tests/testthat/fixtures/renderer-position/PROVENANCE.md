# RendererPosition fixture provenance

These JSON bytes were generated from the accepted read-only Engine Kit
checkout at:

```text
commit: 33409334c6f4d6fca0d798ba4a324673e72e86ce
accepted feature ancestor: 4ac82a1e3cc7c77c90dfbfad9cacdeb87de76dc0
```

The source identifier, semantic-state hash, view hash, and view origin for
every file are recorded in `provenance.json`.

Generation command from the W7 repository root:

```powershell
$env:PYTHONPATH = 'C:\Users\andre\Documents\backgammon-engine-kit\src'
python dev\generate-renderer-position-fixtures.py `
  --output task-work\B-BR-01\runtime\fixtures\regenerated
```

Only the final deterministic `*.json` fixture bytes and `provenance.json` are
copied into this directory. Generation environments, caches, logs, and
rendered review output remain under the ignored task runtime/output trees.

Every fixture uses the initial-release learner policy: `player_1` is the
learner at the bottom, `player_0` is the opponent at the top, and point labels
are for `player_1`. The opening pair differs only in the independent
left/right home-board choice. The player-on-roll pair has an identical view
and differs in canonical state, proving that on-roll identity does not drive
rotation or mirroring.

The fixture sources are existing accepted Engine Kit renderer-test vectors or
existing W7 factual XGID fixtures. The generator supplies the same bounded
context used by Engine Kit's accepted renderer tests:

```json
{
  "cube": {"enabled": true},
  "rules": {"automatic_doubles": 0, "variation": "standard"}
}
```
