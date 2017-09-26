\include "../test-includes.ly"
\language "english"

m = { c' d' e' f' }

\markup "Extending staves up and down"

{
  \m
  \cnExtendStaffUp
  \m
  \cnExtendStaffUp
  \m
  \cnExtendStaffDown
  \m
  \cnUnextendStaffUp
  \m
  \cnUnextendStaffDown
  \m
  \cnUnextendStaffUp
  \m
  \cnUnextendStaffUp
  \m
}

{
  \m
  \cnStaffOctaveSpan 1
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \m
  \cnStaffOctaveSpan 2
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \m
  \cnStaffOctaveSpan 3
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \m
  \cnStaffOctaveSpan 4
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \m
}

{
  \cnStaffOctaveSpan 1
  \m
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \m

  \cnStaffOctaveSpan 2
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \cnUnextendStaffDown
  \m

  \cnStaffOctaveSpan 3
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \cnUnextendStaffDown
  \m

  \cnStaffOctaveSpan 4
  \m
  \cnExtendStaffUp
  \m
  \cnUnextendStaffUp
  \cnUnextendStaffDown
  \m
}

\markup "In staff groups, one staff shouldn't affect others"
<<
  {
    \m
    \m
    \m
    \m
    \m
  }
  {
    \m
    \cnExtendStaffUp
    \m
    \cnStaffOctaveSpan 3
    \m
    \cnExtendStaffUp
    \m
    \cnUnextendStaffDown
    \m
  }
>>
