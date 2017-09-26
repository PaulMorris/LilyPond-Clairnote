\include "../test-includes.ly"
\language "english"

\markup "Whole notes"

m = \relative {
  \time 4/4
  c'1 f1 d1 g1
  c,4 f4 d4 g4
  \time 5/4
  f1 f4
  d1 d4
}

\new Staff {
  \m
}

\new TradStaff {
  \numericTimeSignature
  \m
}
