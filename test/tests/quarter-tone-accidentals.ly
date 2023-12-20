\include "../test-includes.ly"

\markup "Quarter Tone Accidentals"
\markup \vspace #0.5

example = \relative f' {
  \time 5/4
  | c4 cih cis  cisih cisis
  | d4 deh des deseh deses
  | e4 eih eis eisih eisis
  | f4 feh fes feseh feses
  \break
  \time 4/4
  | c4 cisih d dih
  | d4 deseh c ceh
  | e4 eisih f fih
  | f4 feseh e eeh
  \break
  \time 5/4
  | c cih c cih c
  | d deh d deh d
  | cis cisih cis cisih cis
  | des deseh des deseh des
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
