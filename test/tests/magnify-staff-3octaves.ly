\include "../test-includes.ly"
\language "english"

m =
\relative f' {
  \key d \major
  \repeat volta 2 {
    c8 cs d ds
    e f fs gs
    <df' c'>2 <cf bs,>
    fs,1 g1
  }
}

\markup "Magnify staff, 3 octaves"

\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \magnifyStaff #1.375
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \magnifyStaff #1.75
  \m
}

\new Staff \with {
  \cnStaffOctaveSpan 3
} {
  \magnifyStaff #2.5
  \m
}
