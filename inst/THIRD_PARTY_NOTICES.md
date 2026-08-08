# Third-party notices

## AnkiGammon

Copyright (c) 2025 AnkiGammon

Selected XGID decoding behavior and development-time move-notation behavior were
informed by the MIT-licensed `AnkiGammon` project, pinned for release validation
at v1.7.0 / commit `eb39c8875691c0bfe1d096157254e6c4245eb5af`.
The principal behavioral references were `ankigammon/utils/xgid.py` and,
historically, `ankigammon/utils/move_parser.py` plus its direct parser tests.

The released `backgammonboard` implementation is independently authored native R
code with stricter package-specific validation and stable project player
mappings. It does not expose source move-notation parsing in the public API.
AnkiGammon source code is not bundled and AnkiGammon is not a runtime
dependency.

See `provenance/XGID_SOURCES.md` for detailed XGID behavioral provenance and
`licenses/AnkiGammon-MIT.txt` for the retained upstream MIT license text.
