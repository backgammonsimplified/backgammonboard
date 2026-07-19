# Board Canonical Coordinate Fixtures v1

These fixtures verify the v1 target representation:

```r
points <- integer(24)
# index 1  = White's 1-point
# index 24 = White's 24-point
# positive = White
# negative = Black

bar <- c(white = 0L, black = 0L)
off <- c(white = 0L, black = 0L)
```

The XGID position field is decoded as specified by eXtreme Gammon:

- character 1: Black/top player bar;
- characters 2 through 25: points 1 through 24 from White/bottom perspective;
- character 26: White/bottom player bar;
- uppercase: White/bottom player;
- lowercase: Black/top player.

## Opening position

```text
XGID=-b----E-C---eE---c-e----B-:0:0:1:52:0:0:3:0:10
```

Expected nonzero canonical points:

| Point | Count |
|---:|---:|
| 1 | -2 |
| 6 | 5 |
| 8 | 3 |
| 12 | -5 |
| 13 | 5 |
| 17 | -3 |
| 19 | -5 |
| 24 | 2 |

Expected bars and off counts:

```r
bar <- c(white = 0L, black = 0L)
off <- c(white = 0L, black = 0L)
```

## Asymmetric position

```text
XGID=aFDaA--------------a-Acbb-:1:-1:1:42:3:0:0:7:10
```

Expected nonzero canonical points:

| Point | Count |
|---:|---:|
| 1 | 6 |
| 2 | 4 |
| 3 | -1 |
| 4 | 1 |
| 19 | -1 |
| 21 | 1 |
| 22 | -3 |
| 23 | -2 |
| 24 | -2 |

Expected bars and off counts:

```r
bar <- c(white = 0L, black = 1L)
off <- c(white = 3L, black = 5L)
```

This fixture deliberately includes:

- asymmetric White and Black points;
- one Black checker on the bar;
- three White checkers borne off;
- five Black checkers borne off;
- a Black-owned cube;
- a 7-point match score.
