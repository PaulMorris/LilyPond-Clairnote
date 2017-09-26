\include "../test-includes.ly"
\language "english"

\markup "Expected warnings, errors"
\markup \vspace #1

\markup "Extending staves up and down"

% warning: \cnUnextendStaffDown failed, not enough staff to unextend
% warning: \cnUnextendStaffUp failed, not enough staff to unextend

m = { c' d' e' f' }

{
  \cnUnextendStaffUp
  \m
  \cnUnextendStaffDown
  \m
  \cnUnextendStaffUp
  \m
}


\markup "Unsupported clef"

% warning: clef unsupported by clairnote.ly, using another clef instead.

{
  \clef "vaticana-do1"
  c''
}
