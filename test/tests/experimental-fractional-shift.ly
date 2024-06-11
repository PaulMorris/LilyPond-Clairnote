\include "../test-includes.ly"
\language "english"

m = { c' d' e' f' }

\markup "Experimental: shifting everything"

{
  \m
  \cnExperimentalYshift 1
  \m
  \cnExperimentalYshift 2
  \m
  \cnExperimentalYshift 4
  \m
  \cnExperimentalYshift 6
  \m
  \cnExperimentalYshift 8
  \m
  \cnExtendStaffDown
  \m
  \cnExperimentalYshift 10
  \m
}