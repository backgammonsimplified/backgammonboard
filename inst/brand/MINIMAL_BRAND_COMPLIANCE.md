# Minimal BMS Brand Compliance Note

This checkpoint deliberately avoids a full palette-registry migration.

The current `board_colors("bms")` values already match the supplied BMS board
palette for the board, checkers, dice, cube, text, and primary/secondary move
colours. The move-overlay tokens added here use the supplied guide values.

The supplied generated brand export records:

```text
brand kit version: 1.0.0
source commit: UNCOMMITTED-DRAFT
source registry SHA-256: 45a1938655558fc85cd22defd84292e65675a1549d97a5f1a171a3ed020b8956
generation date: 2026-07-21
```

The generated export is not vendored as a canonical package source while its
source commit remains `UNCOMMITTED-DRAFT`. A later registry-integration task
should regenerate it from a committed brand source, record the real source
commit, and replace the package-local flat mapping without changing the
accepted visual output.

This deferral is intentional. It does not block accessible move rendering.
