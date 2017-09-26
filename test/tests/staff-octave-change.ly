\include "../test-includes.ly"
\language "english"

m = {
  c'8 d' e' f' g' a' b' c''
}

\markup "Clef, cue clef, and 8va stay the same as staff changes"

\new Staff \with {
  \cnStaffOctaveSpan 2
}{
  \m
  \clef alto
  \m
  \break

  \cnStaffOctaveSpan 3
  \m
  \cnClefPositionShift -1
  \m
  \break

  \m
  \cueClef treble
  \m
  \break

  \cnStaffOctaveSpan 2
  \m
  \cueClefUnset
  \m
  \cnClefPositionShift 0
  \m
  \ottava #1
  \m
  \break

  \cnStaffOctaveSpan 3
  \m
  \ottava #-1
  \m
  \break

  \cnStaffOctaveSpan 2
  \m
}
