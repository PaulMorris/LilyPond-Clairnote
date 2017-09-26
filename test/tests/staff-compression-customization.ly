\include "../test-includes.ly"
\language "english"

m = \relative {
  \key e \major
  c'2 cs
  d ds
  e4 fs gs a
  b cs ds es
}

\markup "Different Staff Compression Settings"
\markup \vspace #0.5

\new Staff \with { \cnStaffCompression #7/12 } { \m }
\new Staff \with { \cnStaffCompression #8/12 } { \m }
\markup \teeny "Standard Clairnote size (9/12)"
\new Staff \with { \cnStaffCompression #9/12 } { \m }
\new Staff \with { \cnStaffCompression #10/12 } { \m }
\new Staff \with { \cnStaffCompression #11/12 } { \m }
\new Staff \with { \cnStaffCompression #12/12 } { \m }
\new Staff \with { \cnStaffCompression #13/12 } { \m }
