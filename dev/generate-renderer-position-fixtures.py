"""Generate deterministic RendererPosition fixtures from accepted Engine Kit APIs.

This development helper is not a package runtime dependency. Set PYTHONPATH to
the accepted Engine Kit checkout's ``src`` directory before invoking it.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from backgammon_engine_kit import (
    BackgammonView,
    renderer_position_from_xgid,
    renderer_position_json,
)


ENGINE_KIT_COMMIT = "33409334c6f4d6fca0d798ba4a324673e72e86ce"
ENGINE_KIT_FEATURE_ANCESTOR = "4ac82a1e3cc7c77c90dfbfad9cacdeb87de76dc0"
OPENING = "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:0:0:0:0:10"
EXTERNAL_SETTINGS = {
    "cube": {"enabled": True},
    "rules": {"variation": "standard", "automatic_doubles": 0},
}


def learner_view(
    bottom_home_board_side="right",
    cube_display_side="left",
):
    """Return the initial-release view with player_1 as the learner."""

    return BackgammonView(
        top_player="player_0",
        bottom_player="player_1",
        point_labels_for="player_1",
        bottom_home_board_side=bottom_home_board_side,
        cube_display_side=cube_display_side,
        rotation="custom",
        view_origin="external",
    )


def fixture_specs():
    right_view = learner_view("right")
    left_view = learner_view("left")
    right_cube_view = learner_view("right", cube_display_side="right")
    return [
        ("opening-learner-right", OPENING, right_view),
        ("opening-learner-left", OPENING, left_view),
        ("opening-cube-right", OPENING, right_cube_view),
        (
            "player0-dice",
            "XGID=-b----E-C---eE---c-e----B-:0:0:-1:31:0:0:0:0:10",
            right_view,
        ),
        (
            "player1-dice",
            "XGID=-b----E-C---eE---c-e----B-:0:0:1:42:0:0:0:0:10",
            right_view,
        ),
        (
            "one-bar",
            "XGID=-b----E-C---eE---c-e----AA:0:0:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "both-bars-and-off",
            "XGID=" + "a" + ("-" * 24) + "B" + ":0:0:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "borne-off",
            "XGID=-FDaA--------------a-Acbb-:0:0:1:00:3:0:0:7:10",
            right_view,
        ),
        (
            "one-player-off",
            "XGID=-E---EE-----------------a-:0:0:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "both-fully-borne-off",
            "XGID=--------------------------:0:0:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "tall-stacks",
            "XGID=-O" + ("-" * 22) + "o-:0:0:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "cube-player0",
            "XGID=-b----E-C---eE---c-e----B-:1:-1:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "cube-player1",
            "XGID=-b----E-C---eE---c-e----B-:1:1:1:00:0:0:0:0:10",
            right_view,
        ),
        (
            "money-nonzero",
            "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:3:2:3:0:10",
            right_view,
        ),
        (
            "match-score",
            "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:2:4:0:7:10",
            right_view,
        ),
        (
            "crawford",
            "XGID=-b----E-C---eE---c-e----B-:0:0:1:00:2:6:1:7:10",
            right_view,
        ),
        (
            "late-bearoff",
            "XGID=---D---------------a--b-a-:0:0:-1:00:0:0:0:0:8",
            right_view,
        ),
    ]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True, type=Path)
    arguments = parser.parse_args()
    arguments.output.mkdir(parents=True, exist_ok=True)

    manifest = []
    for name, source_identifier, view in fixture_specs():
        result = renderer_position_from_xgid(
            source_identifier,
            view=view,
            external_settings=EXTERNAL_SETTINGS,
        )
        filename = f"{name}.json"
        (arguments.output / filename).write_text(
            renderer_position_json(result) + "\n",
            encoding="utf-8",
            newline="\n",
        )
        manifest.append(
            {
                "fixture": filename,
                "source_identifier": source_identifier,
                "semantic_state_hash": result.semantic_state_hash,
                "view_hash": result.view_hash,
                "view_origin": result.view.view_origin,
            }
        )

    provenance = {
        "engine_kit_commit": ENGINE_KIT_COMMIT,
        "engine_kit_feature_ancestor": ENGINE_KIT_FEATURE_ANCESTOR,
        "external_settings": EXTERNAL_SETTINGS,
        "generator": "dev/generate-renderer-position-fixtures.py",
        "fixtures": manifest,
    }
    (arguments.output / "provenance.json").write_text(
        json.dumps(provenance, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
        newline="\n",
    )
    print(
        json.dumps(
            {"count": len(manifest), "output": str(arguments.output)},
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    main()
