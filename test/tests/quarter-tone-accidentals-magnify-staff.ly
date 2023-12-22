\include "../test-includes.ly"

\markup "Quarter Tone Accidentals: Magnify Staff"
\markup \vspace #0.5

example = \relative f' {
  \time 5/4
  | c4 cih cis  cisih cisis
  | d4 deh des deseh deses
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
