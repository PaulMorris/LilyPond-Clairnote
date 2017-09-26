\include "../test-includes.ly"
\language "english"

\markup "RhythmicStaff"
\markup \vspace #1

m = {
  c1
  c2 f
  c2. c4
  cs c4. c8 c8 c8
  c8 c16 c16 c16 c8 f8 f f16 f f f f
  f2. f4
}

\markup "Trad"
\new TradRhythmicStaff {
  \m
}

\markup "Clairnote"
\new RhythmicStaff {
  \m
}
