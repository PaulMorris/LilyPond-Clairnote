\include "../test-includes.ly"
\language "english"

m = \fixed c' {
  \time 3/4
  <c e gs>2.
  <c' e' g'>
  <c d g>
  <c' d' g'>
  c'
  r
  <c e g c'>
  c
  c2 r4
  c4. r8 r4
  e2.
  f
  fs
  g
}

\markup "Dot adjustment for half note stems."

\new Staff {
  \m
}

\markup "No dot adjustment. \override Dots.extra-offset = ##f"

\new Staff \with {
  \override Dots.extra-offset = ##f
} {
  \m
}

\markup "Trad dots."
\new TradStaff {
  \m
}
