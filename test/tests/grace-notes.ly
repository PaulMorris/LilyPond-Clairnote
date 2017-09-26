\include "../test-includes.ly"
\language "english"

\markup "Grace notes"

m = \relative c' {
  \key d \major
  \grace { cs8 d }
  cs d e4
  \grace { d2 }
  d

  \grace { c'8 d }
  f
  \grace { ds }
  es f4
  \grace { d2 }
  d

  \grace { ds1 }
  ds1 ds

  \break

  \key c \major
  c4 \grace b16 a4(
  \grace { b16 c16 } a2)

  \acciaccatura d8 c4
  \appoggiatura e8 d4
  \acciaccatura { g16 f } e2
  \slashedGrace a,8 g4
  \slashedGrace b16 a4(
  \slashedGrace b8 a2)

  \afterGrace d1 { c16[ d] }

  \acciaccatura {
    \stemDown
    f16->
    \stemNeutral
  }
  g4 e c2
}

<<
  \new Staff {
    \m
  }
  \new TradStaff {
    \m
  }
>>
