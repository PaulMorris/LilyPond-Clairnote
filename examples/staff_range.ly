\version "2.25.13"

\include "./include.ly"

\new Staff {
  \m
}

\new Staff \with {
  \override StaffSymbol.cn-staff-range = #'(-8 . 20)
}{
  \m
}

\new Staff \with {
  \override StaffSymbol.cn-staff-range = #'(-4 . 16)
}{
  \m
}

\new Staff \with {
  \override StaffSymbol.cn-staff-range =
  #(grob-transformer
    'cn-staff-range
    (lambda (grob v)
      (cons (car v) (+ (cdr v) 12))))
}{
  \m
}
