\include "../test-includes.ly"
\language "english"

\markup "Chords and clusters"
\markup \vspace #0.5

\relative f {
  <a c e> <b d f> <c e g> <d f a>
  <c d> <a b> <e' fs>
  <c d e>
  <a b c>
  <e' fs g>
  <a e c> <f d b> <c' g e> <f a d>
  <a b e c>
  <a b g>
  <gs a b fs>
  r r r
  \break
  <c,, e g a b e>
  <d f a b g'>
  <c d e g a>
  <c d e g>
  <c d e f c'>
  <c d>
  <c d e>
  <c d e f>
  <c d e f g>
  r r r
  <c' e g a b e>
  <d f a b g'>
  <c d e g a>
  <c d e g>
  <c d e f c'>
  <c d>
  <c d e>
  <c d e f>
  <c d e f g>
  r r r
  \break
  <c, d> <d e> <e f> <f g>
  <g a> <a b> <b c> <c d>
  <c, d e> <d e f> <e f g> <f g a>
  <g a b> <a b c> <b c d> <c d e>
  <c, e> <d f> <e g> <f a>
  <g b> <a c> <b d> <c e>
  <c, e g> <d f a> <e g b> <f a c>
  <g b d> <a c e> <b d f> <c e g>
}
