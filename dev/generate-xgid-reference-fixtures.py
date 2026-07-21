"""Generate static XGID fixtures using the pinned AnkiGammon v1.7.0 rules.

Development-only fixture generation. The R package and its tests do not invoke
Python or import AnkiGammon at runtime.
"""
from __future__ import annotations

import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT_DIRS = [ROOT / "inst" / "fixtures", ROOT / "tests" / "testthat" / "fixtures"]


def encode_char(value: int, turn: int) -> str:
    if value == 0:
        return "-"
    count = abs(value)
    if not 1 <= count <= 16:
        raise ValueError(value)
    # Stable model: positive White/O, negative Black/X.
    if turn == 1:
        return chr(ord("A") + count - 1) if value > 0 else chr(ord("a") + count - 1)
    return chr(ord("a") + count - 1) if value > 0 else chr(ord("A") + count - 1)


def encode_payload(points: list[int], white_bar: int = 0, black_bar: int = 0, turn: int = 1) -> str:
    canonical = [-black_bar, *points, white_bar]
    source = canonical if turn == 1 else list(reversed(canonical))
    return "".join(encode_char(value, turn) for value in source)


def xgid(points, *, turn=1, dice="00", cube_exp=0, owner=0, sw=0, sb=0, cj=0, ml=0, mc=10, white_bar=0, black_bar=0):
    payload = encode_payload(points, white_bar=white_bar, black_bar=black_bar, turn=turn)
    return f"XGID={payload}:{cube_exp}:{owner}:{turn}:{dice}:{sw}:{sb}:{cj}:{ml}:{mc}"


def expected_row(fid, value, points, *, turn=1, dice="00", cube_exp=0, owner=0, sw=0, sb=0, cj=0, ml=0, mc=10, white_bar=0, black_bar=0):
    white_on = sum(max(p, 0) for p in points) + white_bar
    black_on = sum(max(-p, 0) for p in points) + black_bar
    return {
        "fixture_id": fid,
        "xgid": value,
        "points": ";".join(str(p) for p in points),
        "bar_white": white_bar,
        "bar_black": black_bar,
        "off_white": 15 - white_on,
        "off_black": 15 - black_on,
        "on_roll": "white" if turn == 1 else "black",
        "dice": "" if dice in {"00", "D"} else f"{dice[0]};{dice[1]}",
        "action_marker": dice,
        "cube_value": 2**cube_exp,
        "cube_owner": {-1: "black", 0: "center", 1: "white"}[owner],
        "score_white": sw,
        "score_black": sb,
        "match_length": "" if ml == 0 else ml,
        "is_crawford": str(bool(ml > 0 and cj == 1)).lower(),
        "xgid_max_cube": 2**mc,
    }


opening = [0] * 24
for point, count in {1: -2, 6: 5, 8: 3, 12: -5, 13: 5, 17: -3, 19: -5, 24: 2}.items():
    opening[point - 1] = count

asym = [0] * 24
for point, count in {1: 6, 2: 4, 3: -1, 4: 1, 19: -1, 21: 1, 22: -3, 23: -2, 24: -2}.items():
    asym[point - 1] = count

valid_specs = [
    ("opening_white_roll", opening, dict(turn=1, dice="52")),
    ("opening_black_roll", opening, dict(turn=-1, dice="52")),
    ("asymmetric_white_roll", asym, dict(turn=1, dice="42", cube_exp=1, owner=-1, sw=3, ml=7)),
    ("asymmetric_black_roll", asym, dict(turn=-1, dice="42", cube_exp=1, owner=-1, sw=3, ml=7)),
    ("white_bar", [*opening[:23], 1], dict(turn=1, white_bar=1)),
    ("black_bar", [-1, *opening[1:]], dict(turn=-1, black_bar=1)),
    ("borne_off", asym, dict(turn=1, sw=3, ml=7)),
    ("centered_cube", opening, dict(turn=1, cube_exp=0, owner=0)),
    ("white_owned_cube", opening, dict(turn=1, cube_exp=1, owner=1)),
    ("black_owned_cube", opening, dict(turn=1, cube_exp=2, owner=-1)),
    ("cube_8", opening, dict(turn=1, cube_exp=3, owner=1)),
    ("cube_16", opening, dict(turn=1, cube_exp=4, owner=1)),
    ("cube_32", opening, dict(turn=1, cube_exp=5, owner=1)),
    ("cube_64", opening, dict(turn=1, cube_exp=6, owner=1)),
    ("no_dice", opening, dict(turn=1, dice="00")),
    ("ordinary_dice", opening, dict(turn=-1, dice="63")),
    ("doubles", opening, dict(turn=1, dice="44")),
    ("offer_to_black", opening, dict(turn=1, dice="D", cube_exp=1, owner=1)),
    ("offer_to_white", opening, dict(turn=-1, dice="D", cube_exp=1, owner=-1)),
    ("unlimited_nonzero_score", opening, dict(turn=1, sw=3, sb=2, ml=0, cj=3)),
    ("ordinary_match", opening, dict(turn=1, sw=2, sb=2, ml=7)),
    ("crawford", opening, dict(turn=1, sw=6, sb=2, ml=7, cj=1)),
    ("non_crawford_match", opening, dict(turn=-1, sw=6, sb=3, ml=7, cj=0)),
    ("large_mc_metadata", opening, dict(turn=1, mc=20)),
]
valid = []
for fid, points, spec in valid_specs:
    value = xgid(points, **spec)
    valid.append(expected_row(fid, value, points, **spec))

base = xgid(opening)
body = base[5:]
payload, *meta = body.split(":")
invalid = [
    ("payload_25", f"XGID={payload[:-1]}:" + ":".join(meta), "xgid_invalid_position_length"),
    ("payload_27", f"XGID={payload}-:" + ":".join(meta), "xgid_invalid_position_length"),
    ("invalid_character", f"XGID=Q{payload[1:]}:" + ":".join(meta), "xgid_invalid_position_character"),
    ("invalid_cube_exponent", f"XGID={payload}:x:" + ":".join(meta[1:]), "xgid_invalid_cube_exponent"),
    ("cube_128", xgid(opening, cube_exp=7), "xgid_unsupported_cube_value"),
    ("invalid_owner", xgid(opening).replace(":0:0:1:", ":0:2:1:"), "xgid_invalid_cube_owner"),
    ("invalid_turn", xgid(opening).replace(":0:0:1:", ":0:0:0:"), "xgid_invalid_turn"),
    ("invalid_die", xgid(opening, dice="00").replace(":00:", ":70:"), "xgid_invalid_dice"),
    ("unsupported_beaver", xgid(opening, dice="B"), "unsupported_action_marker"),
    ("unsupported_raccoon", xgid(opening, dice="R"), "unsupported_action_marker"),
    ("negative_score", xgid(opening).replace(":0:0:0:0:10", ":-1:0:0:0:10"), "xgid_invalid_score_white"),
    ("invalid_match_length", xgid(opening).replace(":0:0:0:0:10", ":0:0:0:-1:10"), "xgid_invalid_match_length"),
    ("too_many_white", xgid([16, *([0] * 23)]), "xgid_impossible_checker_total"),
    ("too_many_black", xgid([-16, *([0] * 23)]), "xgid_impossible_checker_total"),
    ("negative_off_white", xgid([15, *([0] * 23)], white_bar=1), "xgid_impossible_checker_total"),
    ("missing_field", ":".join(base.split(":")[:-1]), "xgid_missing_fields"),
    ("extra_field", base + ":0", "xgid_extra_fields"),
    ("empty_field", base.replace(":0:0:1:", "::0:1:", 1), "xgid_missing_fields"),
    ("invalid_separator", base.replace(":0:0:1:", "|0|0|1|", 1), "xgid_missing_fields"),
    ("invalid_match_score", xgid(opening, sw=7, sb=0, ml=7), "xgid_invalid_match_score"),
]
invalid_rows = [{"fixture_id": a, "xgid": b, "expected_code": c} for a, b, c in invalid]

for directory in OUT_DIRS:
    directory.mkdir(parents=True, exist_ok=True)
    with (directory / "xgid-factual-fixtures.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(valid[0]), lineterminator="\n")
        writer.writeheader(); writer.writerows(valid)
    with (directory / "xgid-invalid-fixtures.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(invalid_rows[0]), lineterminator="\n")
        writer.writeheader(); writer.writerows(invalid_rows)
