\include "../test-includes.ly"
\language "english"

m = {
  \time 2/4
  c''4 f''
  \ottava #-1
  c'' f''
  \ottava #-2
  c'' f''
  \ottava #0
  c'' f''
  \ottava #1
  c'' f''
  \ottava #2
  c'' f''
  \ottava #0
  c'' f''
}

mm = {
  \m
  \clef treble \m
  \clef alto \m
  \clef bass \m
}

\markup "8va and Clefs"
\markup \vspace #0.5

<<
  \new Staff { \mm }
  \new TradStaff { \mm }
>>
