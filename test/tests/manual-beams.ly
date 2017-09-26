\include "../test-includes.ly"
\language "english"

\markup "Manual beams"

{
  \time 3/8
  a'16 r r <e' c''>16[ <f' c''> <e' g' c''>]
  a'16 r r <e' c''> <f' c''> <e' g' c''>
}

{
  \time 4/4
  <c' e'>8[ <d' f'>] r4 r2
  <c' e'>8 <d' f'> r4 r2
}
