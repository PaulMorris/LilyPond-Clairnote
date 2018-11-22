\include "../test-includes.ly"
\language "english"

\markup "Ledger recipes"

\paper {
  ragged-last-bottom = ##f
}

m = <<
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

\markup "ledgers-less-gradual"

\new Staff \with {
  \override StaffSymbol.cn-ledger-recipe =
  #(if (string= clairnoteTypeName "Clairnote SN")
       cn-sn-ledgers-less-gradual
       cn-dn-ledgers-less-gradual)
} { \m }

\markup "ledgers-keep-c-ledgers"

\new Staff \with {
  \override StaffSymbol.cn-ledger-recipe =
  #(if (string= clairnoteTypeName "Clairnote SN")
       cn-sn-ledgers-keep-c-ledgers
       cn-dn-ledgers-keep-c-ledgers)
} { \m }
