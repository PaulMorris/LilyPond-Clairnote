\version "2.25.13"

% clairnote-type = dn
\include "../clairnote.ly"

#(ly:set-option 'compile-scheme-code #t)
#(debug-enable 'backtrace)
#(debug-set! width 200)

\paper {
  indent = 0
}

\language "english"

m = \relative {
  gs''4 a as b
  c cs d ds

  \cnExtendStaffUp

  gs4 a as b
  c cs d ds
  e f fs g
  gs a as b
  c1
}