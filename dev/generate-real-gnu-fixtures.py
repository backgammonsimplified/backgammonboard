"""Prepare bounded real GNU RendererPosition gallery fixtures.

This is development-only bridge code.  GNU IDs never enter package runtime.
"""
from __future__ import annotations
import csv, json
from pathlib import Path
from backgammon_engine_kit import BackgammonView, renderer_position_from_gnuid, renderer_position_json

OUT = Path("tests/testthat/fixtures/real-gnu-gallery")
SETTINGS = {"cube": {"enabled": True}, "rules": {"variation": "standard", "automatic_doubles": 0}}

CASES = [
 ("opening-unbranded","gnu_vs_sage_match_2a_game_1.txt",1,"4HPwATDgc/ABMA","MADnAAAAAAAE","sage_checker4ply_cube3ply moves 13/7 8/7","checker_play","61","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Ordinary checker play, centered cube, no caller text, and no branding.","13/7 8/7"),
 ("opening-branded","gnu_vs_sage_match_2a_game_1.txt",1,"4HPwATDgc/ABMA","MADnAAAAAAAE","sage_checker4ply_cube3ply moves 13/7 8/7","checker_play","61","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Caller text renders exactly as two lines: Backgammon, then Simplified.","13/7 8/7"),
 ("single-hit","gnu_vs_sage_match_1a_game_2.txt",2,"4PPCATDgc/ABMA","MIHuAEAAAAAE","sage_checker4ply_cube3ply moves 24/16*","checker_play","53","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Arrow anchors, hit destination, ghost placement, and exactly one opposing checker sent to the bar.","24/16*"),
 ("double-hit","gnu_vs_sage_match_2a_game_2.txt",6,"yOeDASiMt8HBAA","MAHmAEAAAAAE","sage_checker4ply_cube3ply moves 6/2*/1*","checker_play","41","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Two ordered hits, distinct destinations, arrow sequence, and two opposing checkers on the bar.","6/2*/1*"),
 ("bar-entry","gnu_vs_sage_match_1a_game_2.txt",3,"4HPwESDg8+AAWA","cInyAEAAAAAE","gnubg_checker3ply_cube2ply moves bar/20 24/20","checker_play","54","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Bar checker placement, bar-source arrow anchor, shared destination stack, and perspective transform.","bar/20 24/20"),
 ("bar-entry-reversed","gnu_vs_sage_match_1a_game_2.txt",3,"4HPwESDg8+AAWA","cInyAEAAAAAE","gnubg_checker3ply_cube2ply moves bar/20 24/20","checker_play","54","cube_value=1; cube_owner=center; cube_offer_status=none","reversed","Bar checker placement, bar-source arrow anchor, shared destination stack, and perspective transform.","bar/20 24/20"),
 ("bar-entry-hit","gnu_vs_sage_match_2a_game_3.txt",3,"yOfgATDgc+QBUA","cAnmAEAAIAAE","gnubg_checker3ply_cube2ply moves bar/21* 24/23","checker_play","41","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Bar entry originates at the actual bar checker, lands on the correct point, and records the hit.","bar/21* 24/23"),
 ("two-on-bar-four-moves","gnu_vs_sage_match_2a_game_2.txt",7,"GbfBwQDI54MBYA","cIn2AEAAAAAE","gnubg_checker3ply_cube2ply moves bar/20(2) 8/3(2)","checker_play","55","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Two bar entries and two additional checker movements retain order and readable arrows.","bar/20(2) 8/3(2)"),
 ("three-enter-hit","4ply_gnu_analyzed_gnu_vs_sage_match_3a_003.txt",14,"jc7BAwLguckAcA","AQHyAEAAAAAE","sage_checker4ply_cube3ply moves bar/21(3) 9/5*","checker_play","44","cube_value=2; cube_owner=opponent; cube_offer_status=none","canonical","A three-checker bar stack, repeated entry, final hit, and non-overlapping arrow geometry.","bar/21(3) 9/5*"),
 ("four-checker-move","gnu_vs_sage_match_2a_game_2.txt",4,"4PORASiMZ/ABMA","MAH7AEAAAAAE","sage_checker4ply_cube3ply moves 24/18(2) 13/7(2)","checker_play","66","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Four ordered atomic movements remain legible and terminate at the correct destination slots.","24/18(2) 13/7(2)"),
 ("same-destination-stack","gnu_vs_sage_match_1a_game_2.txt",5,"4HPwMQDg8+CAIQ","cInuAEAAAAAE","gnubg_checker3ply_cube2ply moves 8/3 6/3","checker_play","53","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Successive destination ghosts use the evolving stack height and sit above real checkers.","8/3 6/3"),
 ("hit-stack-same-point","gnu_vs_sage_match_2a_game_3.txt",11,"4HPwAEUbnsgBEg","QQnpAEAAIAAE","gnubg_checker3ply_cube2ply moves 13/9 6/4*(2)","checker_play","22","cube_value=2; cube_owner=opponent; cube_offer_status=none","canonical","Repeated hit movement, destination-stack growth, solid real checkers, and ghost ordering.","13/9 6/4*(2)"),
 ("bearing-off","4ply_gnu_analyzed_gnu_vs_sage_match_3a_006.txt",54,"/7sACAB3mwEAAA","QQnzAGAAKAAE","gnubg_checker3ply_cube2ply moves 6/off 4/off","checker_play","64","cube_value=2; cube_owner=opponent; cube_offer_status=none","canonical","Off-tray destinations, two bearing-off arrows, owned cube, score 6-5, and post-Crawford state.","6/off 4/off"),
 ("crawford","4ply_gnu_analyzed_gnu_vs_sage_match_3a_005.txt",1,"4HPwATDgc/ABMA","sADuAGAAIAAE","sage_checker4ply_cube3ply moves 24/21 24/20","checker_play","43","Crawford=true; cube suppressed; cube_offer_status=none","canonical","Crawford label, score 6-4, and no active doubling cube.","24/21 24/20"),
 ("top-roll-double","gnu_vs_sage_match_2a_game_1.txt",7,"4NvgCUCxzeABIQ","MAHgAAAAAAAE","sage_checker4ply_cube3ply doubles","roll_double","none","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Explicit Roll or Double state with no dice and no offered cube.",""),
 ("top-roll-double-reversed","gnu_vs_sage_match_2a_game_1.txt",7,"4NvgCUCxzeABIQ","MAHgAAAAAAAE","sage_checker4ply_cube3ply doubles","roll_double","none","cube_value=1; cube_owner=center; cube_offer_status=none","reversed","Explicit Roll or Double state with no dice and no offered cube.",""),
 ("top-pending-offer","gnu_vs_sage_match_2a_game_1.txt",8,"4NvgCUCxzeABIQ","MBngAAAAAAAE","sage_checker4ply_cube3ply doubles to 2; gnubg_checker3ply_cube2ply accepts","take_pass","none","cube_value=1; cube_owner=center; cube_offer_status=pending; offered_value=2; offered_by=top; offered_to=bottom","canonical","Offered cube is centered in the offerer's half on the top offerer's right after the single perspective transform.",""),
 ("top-pending-offer-reversed","gnu_vs_sage_match_2a_game_1.txt",8,"4NvgCUCxzeABIQ","MBngAAAAAAAE","sage_checker4ply_cube3ply doubles to 2; gnubg_checker3ply_cube2ply accepts","take_pass","none","cube_value=1; cube_owner=center; cube_offer_status=pending; offered_value=2; offered_by=top; offered_to=bottom","reversed","Offered cube is centered in the offerer's half on the top offerer's right after the single perspective transform.",""),
 ("top-after-take","gnu_vs_sage_match_2a_game_1.txt",9,"4NvgCUCxzeABIQ","EYHuAAAAAAAE","gnubg_checker3ply_cube2ply accepted the preceding offer; sage_checker4ply_cube3ply moves 13/10*/5","checker_play","53","cube_value=2; cube_owner=bottom; cube_offer_status=none","canonical","After a take, render an owned cube near the receiver, never a pending offered cube.","13/10*/5"),
 ("bottom-roll-double","gnu_vs_sage_match_1a_game_2.txt",11,"hs/CGBDMzuCARA","cAngAEAAAAAE","gnubg_checker3ply_cube2ply doubles","roll_double","none","cube_value=1; cube_owner=center; cube_offer_status=none","canonical","Explicit Roll or Double state with no offered cube for the bottom player.",""),
 ("bottom-roll-double-reversed","gnu_vs_sage_match_1a_game_2.txt",11,"hs/CGBDMzuCARA","cAngAEAAAAAE","gnubg_checker3ply_cube2ply doubles","roll_double","none","cube_value=1; cube_owner=center; cube_offer_status=none","reversed","Explicit Roll or Double state with no offered cube for the bottom player.",""),
 ("bottom-pending-offer","gnu_vs_sage_match_1a_game_2.txt",12,"hs/CGBDMzuCARA","cBHgAEAAAAAE","gnubg_checker3ply_cube2ply doubles to 2; sage_checker4ply_cube3ply accepts","take_pass","none","cube_value=1; cube_owner=center; cube_offer_status=pending; offered_value=2; offered_by=bottom; offered_to=top","canonical","Offered cube is centered in the offerer's half on the bottom offerer's right after the single perspective transform.",""),
 ("bottom-pending-offer-reversed","gnu_vs_sage_match_1a_game_2.txt",12,"hs/CGBDMzuCARA","cBHgAEAAAAAE","gnubg_checker3ply_cube2ply doubles to 2; sage_checker4ply_cube3ply accepts","take_pass","none","cube_value=1; cube_owner=center; cube_offer_status=pending; offered_value=2; offered_by=bottom; offered_to=top","reversed","Offered cube is centered in the offerer's half on the bottom offerer's right after the single perspective transform.",""),
 ("bottom-after-take","gnu_vs_sage_match_1a_game_2.txt",13,"hs/CGBDMzuCARA","QQnlAEAAAAAE","sage_checker4ply_cube3ply accepted the preceding offer; gnubg_checker3ply_cube2ply moves bar/24 22/20","checker_play","21","cube_value=2; cube_owner=top; cube_offer_status=none","canonical","After a take, render factual cube ownership for the top receiver.","bar/24 22/20"),
]

def view(kind):
    if kind == "canonical":
        return BackgammonView("player_0", "player_1", "player_1", "right", "left", "custom", "external")
    return BackgammonView("player_1", "player_0", "player_0", "right", "left", "custom", "external")

def main():
    OUT.mkdir(parents=True, exist_ok=True)
    rows=[]
    for spec in CASES:
        ident = f"{spec[3]}:{spec[4]}"
        result = renderer_position_from_gnuid(ident, view=view(spec[9]), external_settings=SETTINGS)
        filename = f"{spec[0]}.json"
        (OUT/filename).write_text(renderer_position_json(result)+"\n", encoding="utf-8", newline="\n")
        score = result.position.score
        rows.append(dict(id=spec[0], title=spec[0].replace("-", " ").title(), source=spec[1], move_number=spec[2], position_id=spec[3], match_id=spec[4], original_event=spec[5], decision=spec[6], dice=spec[7], cube_facts=spec[8], perspective=spec[9], inspect=spec[10], notation=spec[11], fixture=filename, score_context=f"{score.player_0}-{score.player_1}; match {score.match_length}; Crawford={result.position.rules.crawford}"))
    with (OUT/"manifest.csv").open("w", newline="", encoding="utf-8") as f: csv.DictWriter(f, fieldnames=rows[0].keys()).writeheader(); csv.DictWriter(f, fieldnames=rows[0].keys()).writerows(rows)
    (OUT/"provenance.json").write_text(json.dumps({"generator":"dev/generate-real-gnu-fixtures.py","engine_kit":"renderer_position_from_gnuid","external_settings":SETTINGS,"count":len(rows)}, indent=2)+"\n", encoding="utf-8")
if __name__ == "__main__": main()
