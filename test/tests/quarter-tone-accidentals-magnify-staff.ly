\language "english"
\include "../test-includes.ly"

\markup "Quarter Tone Accidentals: Magnify Staff (with parenthesized accidentals, forced accidentals)"
\markup \vspace #0.5

example = \relative f' {
  \time 5/4
  | c4 cqs cs  ctqs css
  | d4 dqf df dtqf dff
  % parenthesized accidentals (?) and forced accidentals (!)
  | g? gtqf? gf? gqf? gqs?
  | gs? gtqs? g! g! gtqf!
  | gtqf! gf! gf! gqf! gqf!
  | gqs! gqs! gs! gs! gtqs!
  | gtqs! s s s s
}

\score {
  <<
    \new TradStaff {
      \example
    }
    \new Staff {
      \example
    }
  >>
}

{
  \magnifyStaff #0.75
  \example
}

{
  \magnifyStaff #1.75
  \example
}

{
  \magnifyStaff #2.5
  \example
}
