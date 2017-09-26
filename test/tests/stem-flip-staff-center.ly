\include "../test-includes.ly"
\language "english"

\markup "Stems flip at center of staff"

m = {
  c'8 d' e' f' g' a' b' c''
}

\new TradStaff {
  \m
  \clef alto \m
  \clef bass \m
}

\new Staff \with {
  \cnStaffOctaveSpan 1
}{
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -1
  \clef bass
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 2
}{
  \key c \major
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -1
  \clef bass
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \time 4/4
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -1
  \clef bass
  \m
}

\new Staff \with {
  \cnClefPositionShift -1
  \cnStaffOctaveSpan 3
}{
  \key d \minor
  \m
  \clef alto
  \m
  \clef bass
  \m
  \cnClefPositionShift -2
  \clef bass
  \m
}
