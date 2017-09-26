\include "../test-includes.ly"
\language "english"

\markup "Key signatures"

ch = {
  <c e f b>4
}

keyparade = \relative c'' {
  \time 1/4
  \key c \major
  \transpose c c' {
    \ch
  }
  \tag #'min {
    \key a \minor
    a
  }
  \key g \major
  \transpose c g' {
    \ch
  }
  \tag #'min {
    \key e \minor
    e'
  }
  \key d \major
  \transpose c d' {
    \ch
  }
  \tag #'min {
    \key b \minor
    b
  }
  \key a \major
  \transpose c a' {
    \ch
  }
  \tag #'min {
    \key fs \minor
    fs'
  }
  \key e \major
  \transpose c e' {
    \ch
  }
  \tag #'min {
    \key cs \minor
    cs
  }
  \key b \major
  \transpose c b' {
    \ch
  }
  \tag #'min {
    \key gs \minor
    gs
  }
  \key fs \major
  \transpose c fs' {
    \ch
  }
  \tag #'min {
    \key ds \minor
    ds
  }
  \key cs \major
  \transpose c cs' {
    \ch
  }
  \tag #'min {
    \key as \minor
    as
  }
  \break

  \key f \major
  \transpose c f' {
    \ch
  }
  \tag #'min {
    \key d \minor
    d
  }
  \key bf \major
  \transpose c bf' {
    \ch
  }
  \tag #'min {
    \key g \minor
    g'
  }
  \key ef \major
  \transpose c ef' {
    \ch
  }
  \tag #'min {
    \key c \minor
    c,
  }
  \key af \major
  \transpose c af' {
    \ch
  }

  \tag #'min {
    \key f \minor
    f
  }
  \key df \major
  \transpose c df' {
    \ch
  }
  \tag #'min {
    \key bf \minor
    bf,
  }
  \key gf \major
  \transpose c gf' {
    \ch
  }
  \tag #'min {
    \key ef \minor
    ef
  }
  \key cf \major
  \transpose c cf'' {
    \ch
  }
  \tag #'min {
    \key af \minor
    af
  }
}

modes = \relative {
  \key c \ionian
  c'1
  \key c \dorian
  c
  \key c \phrygian
  c
  \key c \lydian
  c
  \key c \mixolydian
  c
  \key c \aeolian
  c
  \key c \locrian
  c
}

\new Staff {
  \keyparade
}

\markup "Only major keys"
\new Staff {
  \removeWithTag #'min {
    \keyparade
  }

}
\markup "Modes"
{
  \modes
}
