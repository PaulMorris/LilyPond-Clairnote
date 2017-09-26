\include "../test-includes.ly"
\language "english"

\markup "Weird vertical position of clefs when no notes follow them (e.g. at end of lines here)."
\markup \hspace #1

{
  \clef treble
  c1
  \clef bass
  c1
  \clef treble
  c1
  \clef bass
  c1
  \clef treble
  % c1
}

{
  \clef bass
  c1
  \clef treble
  c1
  \clef bass
  c1
  \clef treble
  c1
  \clef bass
  % c1
}

{
  \clef alto
  c1
  \clef treble
  c1
  \clef alto
  c1
  \clef treble
  c1
  \clef alto
  % c1
}
