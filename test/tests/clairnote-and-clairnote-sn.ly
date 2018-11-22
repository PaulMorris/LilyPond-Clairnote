\include "../test-includes.ly"
\language "english"

\markup "Clairnote SN and Clairnote DN in the same file"
\markup \vspace #0.5

% For making comparisons between the two variants.

% Make sure the test works regardless of earlier settings.
\initClairnoteDN
\initClairnoteSN

m = \relative {
  c' cs d ds
  e f fs g
  gs a as b
  c1
}

\new Staff {
  \m
}

% MIDI works with StaffClairnoteDN
\score {
  \new StaffClairnoteDN {
    \m
  }
  \layout {}
  \midi {}
}
