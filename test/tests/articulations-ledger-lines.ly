\include "../test-includes.ly"
\language "english"

\markup "Articulations and Ledger Lines"
\markup \vspace #0.5

\relative c {
  \omit Accidental
  \key bf \major
  f-- fs-- g-- gs--
  a-- as-- b-- c-- 
  cs-- d-- ds-- e-- 
  f-- fs-- g-- gs--
  a-- as-- b-- c-- 
  cs-- d-- ds-- e-- 
  f-- fs-- g-- gs--
  a-- as-- b-- c-- 
  cs-- d-- ds-- e-- 
  <e, bf' g>--
  <f b a>--
  <g c b>-- 
  r
  \break
  f,,-^ fs-^ g-^ gs-^
  a-^ as-^ b-^ c-^ 
  cs-^ d-^ ds-^ e-^ 
  f-^ fs-^ g-^ gs-^
  a-^ as-^ b-^ c-^ 
  cs-^ d-^ ds-^ e-^ 
  f-^ fs-^ g-^ gs-^
  a-^ as-^ b-^ c-^ 
  cs-^ d-^ ds-^ e-^ 
  <e, bf' g>-^
  <f b a>-^
  <g c b>-^ 
  r 
  \break
  \magnifyStaff 2
  b->
  bf4->
  b-^
  bf-^
  b-!
  bf-!
  b-.
  bf-.
  b-_
  bf-_
  b--
  bf--
  b-\espressivo
  bf\espressivo
  
  b\fermata
  bf\fermata
  bf\shortfermata
  bf\longfermata
  bf\verylongfermata
  bf\segno
  bf\coda
  bf\varcoda
  bf\signumcongruentiae

  bf\upbow
  bf\downbow
  bf\snappizzicato
  bf\open
  bf-+
  bf\flageolet
  bf\thumb
  bf^\lheel
  bf\rheel
  bf^\ltoe
  bf\rtoe
  bf\halfopen
}
