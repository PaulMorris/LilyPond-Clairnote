\include "../test-includes.ly"
\language "english"

\markup "\magnifyStaff and \cnStaffCompression"

\paper {
  ragged-last-bottom = ##f
}

m = \fixed f' {
  \key c \major
  a b c d e f g a
  \repeat volta 2 {
    a b c d e f g a
  }
  a b c d e f g a
  a b c d e f g a
  a b c d e f g a
  a b c d e f g a
}

{ \m }

\markup "\magnifyStaff"
\new Staff \with {
  \magnifyStaff #0.75
}
{ \m }

\new TradStaff {
  \m
}

\markup "\cnStaffCompression"
\new Staff \with {
  \cnStaffCompression 1.575
} { \m }
