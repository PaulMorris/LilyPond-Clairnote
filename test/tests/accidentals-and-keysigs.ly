\include "../test-includes.ly"
\language "english"

n = \relative f' {
  \key c \major
  c1
  \key f \major
  c8 cs d ds e f fs g
  gs a as b c b bf a
  af g gf f e ef d df
  \key d \minor
  c1
  \key e \major
  c1
  \key f \major
  css2 cff
  cs2 cs
  f4 fs f fs
  ff fs ff fs
  fs gf fs gf
  fs gf fs! gf!
  bf b bf b
  cs1
  \once \omit Staff.Accidental cs1
  cs4 cs! cs! cs!
  \key c \major
  es8 f ff e es f ff e
  \key c \ionian
  c1
  \key c \mixolydian
  c1
  \once \omit Staff.KeySignature
  \key c \lydian
  c1
}

m = \relative f' {
  \key f \major
  f4 g a bf
  f4 fs f fs
  ff fs ff fs
  fs gf fs gf
  fs gf fs! gf!
  bf b bf b
  es8 f ff e es f ff e
}

\markup "Accidentals and Key Signatures"
\markup \vspace #0.5

{ \n }

<<
  { \m }
  \new TradStaff { \m }
>>
