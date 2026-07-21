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

The native R code is an independent behavioral port. AnkiGammon was used to
verify turn-relative payload decoding, bar and point ordering, checker-case
meaning, factual cube metadata, action markers, absolute raw scores, match
metadata, and maximum-cube metadata.

The package deliberately differs where its accepted contract is stricter:

```text
complete ten-field XGIDs only
structured validation diagnostics
impossible checker totals rejected
B and R markers rejected as unsupported
factual cube values above 64 rejected
```

No Python runtime is used by the package or its tests. The static fixture corpus
is generated during development and consumed as CSV by native R tests.

See also:

```text
inst/NOTICE/AnkiGammon-xgid-parser.txt
inst/contracts/XGID_AUDIT_ANKIGAMMON_v1.md
inst/contracts/XGID_CUBE_RENDER_AMENDMENT_v1.md
```
