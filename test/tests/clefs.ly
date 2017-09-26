\include "../test-includes.ly"
\language "english"

\markup "Clefs"

\paper {
  ragged-last-bottom = ##f
}

m = \relative f' {
  c2 c
  \clef "treble"
  c2 c
  \clef "alto"
  c1
  \clef "bass"
  c
  \clef "treble"
  c

  % treble synonyms
  \clef "G"
  c1
  \clef "G2"
  c
  \clef "violin"
  c

  % bass synonym
  \clef "F" c

  % treble in clairnote
  \clef "french"
  c
  \clef "soprano"
  c
  % alto in clairnote
  \clef "mezzosoprano"
  c
  \clef "tenor"
  c
  % bass in clairnote
  \clef "baritone"
  c
  \clef "varbaritone"
  c
  \clef "subbass"
  c
  % percussion
  \clef "percussion"
  c

  % clef transpositions
  \break
  <>^ \markup \teeny "clef transpositions"
  \clef treble
  c1
  \clef "treble_[8]"
  c1
  \clef "treble^(8)"
  c

  \clef "treble^8"
  c
  \clef "treble^15"
  c1
  \clef "treble_8"
  c
  \clef "treble_15"
  c

  <>^ \markup \teeny "clef transpositions, Clairnote values, 13, 25"
  \clef "treble^13"
  c
  \clef "treble^25"
  c
  \clef "treble_13"
  c
  \clef "treble_25"
  c

  % cue clefs
  \clef "bass"
  c1 c c
  <>^ \markup \teeny "cue clefs"
  \cueClef "treble"
  c c c
  \cueClef "alto_8"
  c c c
  \cueClefUnset
  c
  \clef treble
  c
}

<<
  { \m }
  \new TradStaff { \m }
>>

\markup " "
