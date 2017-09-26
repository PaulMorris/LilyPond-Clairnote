\include "../test-includes.ly"
\language "english"

\layout {
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
  }
}

#(set-global-staff-size 18)

right = \relative c'' {
  \crossStaff {
    <c e>2 c2 <c e>4 c4 r2
  }
}

left = \relative c {
  \crossStaff {
    d2 d2 c4 c4 r2
  }
}

\markup "Cross-staff half note stems"
\markup \vspace #0.5

\score {
  \new PianoStaff
  <<
    \new Staff = "right" {
      \right
    }
    \new Staff = "left" {
      \clef bass \left
    }
  >>
  \layout { }
}

\score {
  \new PianoStaff
  <<
    \new Staff = "right" {
      \magnifyStaff 2
      \right
    }
    \new Staff = "left" {
      \magnifyStaff 2
      \clef bass \left
    }
  >>
  \layout { }
}
