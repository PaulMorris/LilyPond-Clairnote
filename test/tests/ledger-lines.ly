\include "../test-includes.ly"
\language "english"

\markup "Ledger lines"

\paper {
  ragged-last-bottom = ##f
  }

m = \relative {
  e'4 ef d df
  c b bf a
  af g gf f
  e ef d df
  c1

  gs''4 a as b
  c cs d ds

  gs4 a as b
  c cs d ds
  e f fs g
  gs a as b
  c1
}

n = <<
  \new Voice {
    \relative {
      \stemUp
      gs'8 a as b c cs d ds
      a'8 as b c cs d ds e f fs g gs
      a as b c cs d
      ds e f fs g gs
      a as b c cs d ds e
    }
  }
  \new Voice {
    \relative {
      \stemDown
      r1
      fs,,8 g gs a as b c cs d
      ds e f fs g gs
      a as b c cs d ds e f fs g gs a
      as b c cs
    }
  }
>>

{
  \m
}

{
  \cnStaffOctaveSpan 4
  \cnClefPositionShift #-1
  \m
}

\markup "#cn-dn-ledgers-gradual"

\new Staff \with {
  \override StaffSymbol.cn-ledger-recipe = #cn-dn-ledgers-gradual
} { \n }
