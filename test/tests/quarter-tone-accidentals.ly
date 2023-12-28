\language "english"
\include "../test-includes.ly"

\markup "Quarter Tone Accidentals"
\markup \vspace #0.5

example = \relative f' {
  \time 5/4
  | c4 cqs cs  ctqs css
  | d4 dqf df dtqf dff
  | e4 eqs es etqs ess
  | f4 fqf ff ftqf fff
  \break
  \time 4/4
  | c4 ctqs d dqs
  | d4 dtqf c cqf
  | e4 etqs f fqs
  | f4 ftqf e eqf
  \break
  \time 5/4
  | c cqs c cqs c
  | d dqf d dqf d
  | cs ctqs cs ctqs cs
  | df dtqf df dtqf df
  \break

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
