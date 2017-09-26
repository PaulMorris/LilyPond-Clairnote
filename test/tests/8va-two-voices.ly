\include "../test-includes.ly"
\language "english"

voiceUno = {
  c'1
  \ottava #-1 c'1 c'1 c'1 c'1 c'1
  \ottava #0 c'1
}

voiceDos = {
  g'1 g'1 g'1
  \ottava #-1 g'1 g'1
  \ottava #0 g'1 g'1
}

\markup "8va and Two Voices"
\markup \vspace #0.5

\new Staff \voiceUno

\new Staff \voiceDos

\new Staff
<<
  \new Voice = "uno" { \voiceOne \voiceUno }
  \new Voice = "dos" { \voiceTwo \voiceDos }
>>

\new Staff
{
  c'1 \ottava #1 c'1
  \clef bass c'1
  \clef alto c'1
  \ottava #0 c'1
}
