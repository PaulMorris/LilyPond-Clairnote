\version "2.25.13"

\include "./include.ly"

\new Staff \with {
  \override StaffSymbol.cn-staff =
  #(lambda (ctx)
     (let ((background (cn:staff-background-default ctx)))
       (cn:region-draw-hline
        ctx
        (cn:select-remainder-y background 0 12) #:color "lightgreen")
       (cn:staff-dn ctx)))
}{
  \m
}

\new Staff \with {
  \override StaffSymbol.cn-staff =
  #(lambda (ctx)
     (let ((background (cn:staff-background-default ctx))
           (ledgers
            (cn:union
             (cn:mirror-to-height ctx 0 -2 2)
             (cn:mirror-to-height ctx 4 2 6)
             (cn:mirror-to-height ctx 8 6 10))))
       (cn:region-draw-box
        ctx
        (cn:select-remainder-y-range background 4 8 12)
        #:color "lightgreen")
       (cn:region-draw-hline ctx ledgers #:inherit 'ledger)))
}{
  \m
}

\new Staff \with {
  \override StaffSymbol.cn-staff =
  #(lambda (ctx)
     (cn:region-debug ctx (cn:rg-noteheads ctx))
     (cn:region-debug ctx (cn:shift-y (cn:rg-noteheads ctx) -16 -4))
     (cn:region-debug
      ctx
      (cn:select-remainder-y-range
       (cn:shift-y (cn:rg-noteheads ctx) -48 -16)
       0 4 12)))
}{
  \m
}
