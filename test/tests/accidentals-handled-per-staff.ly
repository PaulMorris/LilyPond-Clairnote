\include "../test-includes.ly"
\language "english"

\markup "Accidentals are handled per-staff"
\markup \vspace #0.5

<<
  \relative f' {
    cs cs cs cs
    ds ds ds ds
    ds1
  }
  \relative f' {
    c4 c cs cs
    d d ds ds
    ds1
  }
>>

<<
  {
    \key e \major
    fs4 g gs a
    <c e g a>1
  }
  {
    \key e \minor
    fs4 g gs a
    <c e g a>1
  }
>>
