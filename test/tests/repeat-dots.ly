\include "../test-includes.ly"
\language "english"

\markup "Repeat dots"

\paper {
  ragged-last-bottom = ##f
}

m = {
  \clef treble
  \repeat volta 2 {
    c'1
  }
}

\new TradStaff {
  \m
}

{ \m }


\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 4
} {
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 5
} {
  \m
}
