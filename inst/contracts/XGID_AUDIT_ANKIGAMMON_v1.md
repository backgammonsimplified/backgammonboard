# XGID Audit Against Pinned AnkiGammon

## Reference

```text
project: AnkiGammon
release: v1.7.0
commit: eb39c8875691c0bfe1d096157254e6c4245eb5af
source: ankigammon/utils/xgid.py
license: MIT
date inspected: 2026-07-20
port type: independent behavioral port
source archive SHA256: 3c82ef42e0a603431d5839b7fa115bc416ca92d8c0df903dd3f99aa1be88e13c
```

AnkiGammon is a decoding reference and fixture oracle. It is not a runtime
dependency.

## AnkiGammon player interpretation

During the XGID audit, preserve this distinction:

```text
AnkiGammon:
    O = its bottom player
    X = its top player

backgammonboard:
    White and Black are stable semantic player identities
    bottom and top are perspective-dependent display positions
```

Decode the turn-relative AnkiGammon payload into the package's stable,
White-relative points, bar, and off representation.

For `turn = -1`:

```text
reverse the point and bar mapping as required;
apply the corresponding checker-case interpretation;
preserve absolute cube ownership and raw scores.
```

After construction, changing perspective must never alter checker ownership,
cube ownership, scores, bar counts, off counts, or point occupancies.

## Field-by-field audit

| Field or behavior | Previous R result | Pinned reference result | Controlling package rule | Smallest correction |
|---|---|---|---|---|
| Prefix normalization | Complete prefixed and prefix-free input accepted | Reference primarily consumes complete XGID text | Accept exactly the two complete forms | Retained; added strict exact field-count diagnostics |
| Component count | Ten fields required, but missing and extra fields shared one diagnostic | Reference may tolerate omitted trailing metadata | v1 requires exactly ten non-empty fields | Split `xgid_missing_fields` and `xgid_extra_fields` |
| Payload length and characters | Exactly 26 characters; `-`, `A-P`, `a-p` | Same checker alphabet and 26 slots | Strict supported complete XGID | Retained |
| Turn-dependent point order | Payload always decoded in direct order | `turn = -1` reverses the payload mapping | Stable White-relative points | Reverse the 26 source slots for `turn = -1` before assigning canonical slots |
| Turn-dependent checker case | Uppercase always White and lowercase always Black | Case meaning changes with turn | Positive White, negative Black after construction | Make case decoding depend on turn |
| Bar mapping | First slot always Black bar; last always White bar | Bar slots reverse together with a `turn = -1` payload | Stable semantic bars | Apply the same turn-dependent reversal before validating bar ownership |
| Borne-off derivation | Derived from point and bar totals | Reference derives remaining checkers from 15 | Each player's point + bar + off total is 15 | Retained; reject negative off counts and totals above 15 |
| Cube exponent | Converted with `2^CV` | Converted with `2^CV` | Non-negative exponent; factual value through 64 | Retained; reject factual cube above 64 |
| Cube ownership | `-1`, `0`, `1` map to Black, center, White | Ownership is absolute O/center/X metadata, not turn-relative | Stable semantic ownership | Retained |
| Player on roll | `1` White, `-1` Black | Turn distinguishes O and X | Stable semantic on-roll player | Retained |
| Dice | `00` or two dice 1-6 | Supports ordinary dice and action markers | Do not silently ignore malformed values | Retained strict dice validation |
| `D` action marker | Stored only as legacy `dice_action`; no public XGID render integration | Represents a pending ordinary cube offer | Preserve factual marker and derive separate offer context | Add `action_marker`, pending-offer derivation, and default factual offered-cube rendering |
| `B` and `R` markers | Previously accepted as valid position input | Recognized markers | Unsupported in package v1 | Preserve marker in validation result and reject with `unsupported_action_marker` |
| Raw scores | Stored as absolute White and Black fields | O and X scores are absolute, not turn-relative | Raw semantic scores are factual | Retained |
| Match length | Zero represents unlimited play | Same | `unlimited` or `match` | Retained |
| Crawford/Jacoby field | Match Crawford boolean and unlimited flags decoded | Field interpretation depends on match length | Only current-game `is_crawford`; no `post_crawford` | Retained current-game boolean and separate unlimited flags |
| XGID maximum cube | Preserved as encoded metadata aliases | `MC` is a separate exponent-of-two field | Keep source metadata separate from package limit | Add explicit `xgid_max_cube`; retain compatibility aliases |
| Package cube limit | Factual cube over 64 rejected | Reference has no package-rendering limit | Package-supported limit is 64 | Retained structured `package_cube_limit` behavior |
| Perspective | Not part of factual decoding | Reference can render bottom/top orientation | Factual state never changes with perspective | Add one semantic perspective layer after construction |
| Offered-cube placement | Only offered-to-White drawing was frozen | Offer parties are semantic facts | Offered to White and offered to Black must differ | Resolve receiver semantically and place left/right according to receiver and perspective |

## Static fixture boundary

The fixture generator may use Python during development. The installed package,
package tests, and public API do not invoke Python. Static CSV fixtures record
canonical point occupancies, bars, borne-off counts, metadata, and invalid-input
diagnostics.
