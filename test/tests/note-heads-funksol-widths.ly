\include "../test-includes.ly"
\language "english"

\markup "funksol note head widths"

sz = #5

\new Staff \with {
  \cnNoteheadStyle "funksol"
} {
  \relative c' {
    \time 6/4
    e f e2 f

    \stopStaff \startStaff
    % 1.35 is standard width for funksol style
    \override Staff.NoteHead.stencil =
    #(cn-make-note-head-stencil-callback
      cn-funksol-note-head-stencil
      cn-white-note?
      1.1 1)
    e4_\markup \abs-fontsize #sz \sans "x1.1" f e2 f

    \stopStaff \startStaff
    \override Staff.NoteHead.stencil =
    #(cn-make-note-head-stencil-callback
      cn-funksol-note-head-stencil
      cn-white-note?
      1.2 1)
    e4_\markup \abs-fontsize #sz \sans "x1.2" f e2 f

    \stopStaff \startStaff
    \override Staff.NoteHead.stencil =
    #(cn-make-note-head-stencil-callback
      cn-funksol-note-head-stencil
      cn-white-note?
      1.3 1)
    e4_\markup \abs-fontsize #sz  \sans "x1.3" f e2 f

    \stopStaff \startStaff
    \override Staff.NoteHead.stencil =
    #(cn-make-note-head-stencil-callback
      cn-funksol-note-head-stencil
      cn-white-note?
      1.4 1)
    e4_\markup \abs-fontsize #sz  \sans "x1.4" f e2 f

    \stopStaff \startStaff
    \override Staff.NoteHead.stencil =
    #(cn-make-note-head-stencil-callback
      cn-funksol-note-head-stencil
      cn-white-note?
      1.5 1)
    e4_\markup \abs-fontsize #sz  \sans "x1.5" f e2 f

    \stopStaff \startStaff
    \cnNoteheadStyle "lilypond"
    e4_\markup \abs-fontsize #sz  \sans "lilypond" f e2 f

    \stopStaff \startStaff
    \cnNoteheadStyle "default"
    e4_\markup \abs-fontsize #sz  \sans "default" f e2 f
  }
}
