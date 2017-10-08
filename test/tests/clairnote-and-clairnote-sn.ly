\include "../test-includes.ly"
\language "english"

\markup "Clairnote (DN) and Clairnote SN in the same file"
\markup \vspace #0.5

% For making comparisons between the two variants.

% Make sure the test works regardless of earlier settings.
\initClairnoteSN
\initClairnoteDN

m = \relative {
  c' cs d ds
  e f fs g
  gs a as b
  c1
}

\new Staff {
  \m
}

% MIDI works with StaffClairnoteSN
\score {
  \new StaffClairnoteSN {
    \m
  }
  \layout {}
  \midi {}
}
