\include "../test-includes.ly"
\language "english"

\markup "Rests"

rests = {
  r\breve
  r1
  r2 r2
  r4 r4 r4 r4
  r8 r r r
  r16 r r r r
  r32 r r r r r
}

notes = \relative {
  c'4 cs d ds e f fs g gs a as b c1
}

\new Staff {
  \rests
}

\new TradStaff {
  \rests
}

\new Staff {
  \notes
}

\new TradStaff {
  \notes
}
