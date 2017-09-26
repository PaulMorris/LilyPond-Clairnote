\include "../test-includes.ly"
\language "english"

% testing that \omit works for key signatures,
% accidental signs, note heads and stems
% (everywhere we override a stencil)

\markup "\omit (key signatures, accidental signs, note heads, stems, etc.)"
<<
  \new Staff {
    \key g \major
    g'4 g'
    gs' g'
    g'2
    g'4
    g'4
  }
  \new Staff {
    \once \omit Staff.KeySignature
    \key g \major
    g' g'
    \once \omit Staff.Accidental
    gs' g'
    \once \omit Staff.Stem
    g'2
    \once \omit Staff.Stem
    g'4
    \once \omit Staff.NoteHead
    g'4
  }
>>
