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

\markup "8va and Clefs, 3 Octaves"
\markup \vspace #0.5

<<
  \new Staff \with {
    \cnStaffOctaveSpan 3
  } { \mm }
  \new Staff \with {
    \cnStaffOctaveSpan 3
    \cnClefPositionShift -1
  } { \mm }
>>
