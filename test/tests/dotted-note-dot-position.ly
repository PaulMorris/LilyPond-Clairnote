\include "../test-includes.ly"
\language "english"

gn = \once \override NoteHead.color = #green
bn = \once \override NoteHead.color = #blue

\markup "Dot position."

\new Staff \with {
  \remove Accidental_engraver
}
\relative f' {
  \time 18/4
  c,4. cs \bn d ds e f \bn fs g gs a \bn as b
  c4. cs \bn d ds e f fs g gs a as b
  \break
  \bn c4. cs d ds e f fs g gs a \bn as b
  c4. cs \bn d ds e f \bn fs g gs a \bn as b
  \break

  <c,,,, e g>4. <cs f gs> \bn <d fs a> \bn <ds g as>
  <e gs b> <f a c> \gn <fs as cs> \gn <g b d>
  <gs c ds> <a cs e> \bn <as d f> \gn <b ds fs>

  <c e g>4. <cs f gs> \bn <d fs a> \bn <ds g as>
  <e gs b> <f a c> \gn <fs as cs> \gn <g b d>
  <gs c ds> <a cs e> \bn <as d f> \gn <b ds fs>

  <c e g>4. <cs f gs> \bn <d fs a> \bn <ds g as>
  <e gs b> <f a c> \gn <fs as cs> \gn <g b d>
  <gs c ds> <a cs e> \bn <as d f> \gn <b ds fs>

  <c e g>4. <cs f gs> \bn <d fs a> \bn <ds g as>
  <e gs b> <f a c> \gn <fs as cs> \gn <g b d>
  <gs c ds> <a cs e> \bn <as d f> \gn <b ds fs>
  \break

  r1. r2. r4. r8. r16.
}
