\include "../test-includes.ly"
\language "english"

\markup "Staff compression, key signatures scale per staff"

m = \relative f' {
  c1 c
}

<<
  {
    \key c \minor
    \m
    \key f \major
    d1
    \m
    \key c \minor
    e1 e
  }

  \new Staff \with {
    \cnStaffCompression #1
  } {
    \key c \minor
    \m
    d1 d
    \key f \major
    \m
    \key c \minor
    e1 e
  }

>>

<<
  \new Staff \with {
    \cnClefPositionShift -1
    \cnStaffOctaveSpan 3
  } {
    \key c \minor
    \m
    \key f \major
    d1
    \m
    \key c \minor
    e1 e
  }
  \new Staff \with {
    \cnStaffCompression #1
    \cnClefPositionShift -1
    \cnStaffOctaveSpan 3
  } {
    \key c \minor
    \m
    d1 d
    \key f \major
    \m
    \key c \minor
    e1 e
  }
>>
