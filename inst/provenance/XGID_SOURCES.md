# XGID implementation provenance

The initial XGID implementation was written specifically for
`backgammonboard`; the previous monolithic renderer was not copied wholesale.

Field semantics were checked against the eXtreme Gammon 2 help description of
XGID:

- the position field contains 26 characters;
- the first character is the top/Black bar;
- the next 24 characters are points 1 through 24 from the bottom/White
  perspective;
- the last character is the bottom/White bar;
- uppercase characters represent the bottom/White player;
- lowercase characters represent the top/Black player;
- cube and maximum-cube fields are exponents of two;
- cube owner and turn use bottom/center/top relative codes;
- match length zero represents unlimited play.

Reference:

- *eXtreme Gammon 2 Help*, “Technical information — XGID,” pages 146–147.
  A public mirror was consulted at:
  `https://manualzz.com/doc/o/k3mtu/gamesite-2000-version-2-extreme-gammon-documentation-techincal-information`

The earlier `bglab` implementation and examples were used only as comparison
material. In particular, the new implementation corrects bar orientation by
following the XGID specification rather than carrying forward the prior
renderer’s internal bar indices.
