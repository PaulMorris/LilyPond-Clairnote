\include "../test-includes.ly"
\language "english"

m = {
  c'8 d' e' f' g' a' b' c''
}

\markup "Stems flip at center of big staves"

\new Staff \with {
  \cnStaffOctaveSpan 4
}{
  \key a \minor
  \transpose c c, { \m }
  \clef alto
  \transpose c c, { \m }
  \clef bass
  \transpose c c, { \m }
  \cnClefPositionShift -1
  \clef bass
  \transpose c c, { \m }
}

\new Staff \with {
  \cnClefPositionShift -1
  \cnStaffOctaveSpan 4
}{
  \key a \minor
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -2
  \clef bass
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 5
}{
  \key d \minor
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -1
  \clef bass
  \m
}
