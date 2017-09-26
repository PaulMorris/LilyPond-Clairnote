\include "../test-includes.ly"
\language "english"

\markup "Accidental Style"
\markup \vspace #0.5

{
  \clef treble
  \key d \minor
  g' a' bf' c''
  b'

  % produces this alt-def in localAlterations context property
  % with 'clef as the alter value
  % ((0 . 6) clef 1 . #<Mom 1/4>)
  \clef alto
  b'

  \clef treble

  \key g \major
  f'

  % similarly:
  % ((0 . 3) clef 1 . #<Mom 3/4>)
  \clef bass
  f'

  % what if there are different alts at the same semi?
  \clef treble
  \key d \minor
  cf''8 b'

  \clef alto
  cf'' b'

  \clef treble
  \key d \minor
  cf''8 b'

  \clef alto
  b' cf'' ~
  cf''4 cf'' c'' b'

    \break
  \key d \major
  c'4 c' c' c'
  c' c' c' c'
  c' c' c' c'
  bs bs bs bs
  bs bs bs bs
  bs bs bs bs
}
