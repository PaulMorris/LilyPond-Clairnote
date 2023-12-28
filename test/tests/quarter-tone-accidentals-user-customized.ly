\language "english"
\include "../test-includes.ly"

\markup "Quarter Tone Accidentals: user customized glyphs"
\markup \vspace #0.5

example = \relative f' {
  \time 5/4
  | c4 cqs cs  ctqs css
  | d4 dqf df dtqf dff
  % parenthesized accidentals (?) and forced accidentals (!)
  | g? gtqf? gf? gqf? gqs?
  | gs? gtqs? g! g! gtqf!
  | gtqf! gf! gf! gqf! gqf!
  | gqs! gqs! gs! gs! gtqs!
  | gtqs! s s s s
}

\score {
  <<
    \new TradStaff {
      \example
    }
    \new Staff {
      \example
    }
  >>
}

\new Staff \with {
  % Gould accidental signs
  \cnQuarterToneSharpGlyph "accidentals.natural.arrowup"
  \cnQuarterToneFlatGlyph "accidentals.flat.arrowup"
}{
  \example
}

\new Staff \with {
  alterationGlyphs =
  #'((0 . "accidentals.natural")
     (-1/2 . "accidentals.flat")
     (1/2 . "accidentals.sharp")
     (1 . "accidentals.doublesharp")
     (-1 . "accidentals.flatflat")
     (3/4 . "accidentals.sharp.slashslash.stemstemstem")
     (1/4 . "accidentals.sharp.arrowdown") ;; customized
     (-1/4 . "accidentals.flat.arrowup")   ;; customized
     (-3/4 . "accidentals.mirroredflat.flat"))
}{
  \example
}

\new Staff \with {
  \magnifyStaff #1.5
  % Gould accidental signs
  \cnQuarterToneSharpGlyph "accidentals.natural.arrowup"
  \cnQuarterToneFlatGlyph "accidentals.flat.arrowup"
}{
  \example
}

\new Staff \with {
  \magnifyStaff #1.5
  alterationGlyphs =
  #'((0 . "accidentals.natural")
     (-1/2 . "accidentals.flat")
     (1/2 . "accidentals.sharp")
     (1 . "accidentals.doublesharp")
     (-1 . "accidentals.flatflat")
     (3/4 . "accidentals.sharp.slashslash.stemstemstem")
     (1/4 . "accidentals.sharp.arrowdown") ;; customized
     (-1/4 . "accidentals.flat.arrowup")   ;; customized
     (-3/4 . "accidentals.mirroredflat.flat"))
}{
  \example
}
