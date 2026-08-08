# XGID implementation provenance

The XGID implementation was written specifically for `backgammonboard`; the
previous monolithic renderer was not copied wholesale.

## Pinned behavioral reference

```text
project: AnkiGammon
release: v1.7.0
commit: eb39c8875691c0bfe1d096157254e6c4245eb5af
source: ankigammon/utils/xgid.py
license: MIT
source archive SHA256: 3c82ef42e0a603431d5839b7fa115bc416ca92d8c0df903dd3f99aa1be88e13c
```

The native R code is independent. AnkiGammon was used historically to inspect
payload, cube, action, score, match, and maximum-cube fields. Its turn-relative
player interpretation is not current package authority. Contract v1.2 fixes
source roles directly: top/lowercase is `player_0`, bottom/uppercase is
`player_1`, and point order does not change because of turn.

The package deliberately differs where its accepted contract is stricter:

```text
complete ten-field XGIDs only
structured validation diagnostics
impossible checker totals rejected
B and R markers rejected as unsupported
factual cube values above 64 rejected
```

No Python runtime is used by the package or its tests. Any static fixtures used
during development are not an AnkiGammon runtime dependency.

The upstream copyright and MIT terms are retained in
`inst/licenses/AnkiGammon-MIT.txt` in the source repository and installed as
`licenses/AnkiGammon-MIT.txt`. The package-level third-party summary is
`inst/THIRD_PARTY_NOTICES.md`.

Historical audit and contract notes used during development are repository-only
evidence and are intentionally excluded from the installed package.
