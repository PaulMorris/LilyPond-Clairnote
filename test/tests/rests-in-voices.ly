\version "2.22.2"
% clairnote-type = "dn"
\include "clairnote.ly"
\language "english"
#(set-default-paper-size "letter")

% for properly scaled svg images for clairnote.org
% #(set-global-staff-size 34)
% fsize = -6
% \pointAndClickOff

\layout {
  % \override Score.BarNumber.break-visibility = #all-invisible

  \context {
    \Staff
    % \override StaffSymbol.line-positions = #'(-8 -4 0 4 8)
  }
}

\header {
  title = "rests in voices"
  composer = ""
  % tagline = ""
}

\paper {
  % indent = 0
  % ragged-bottom = ##t
  % page-count = #1
  % page-breaking = #ly:one-line-auto-height-breaking

  % markup-system-spacing =
  % #'((basic-distance . 5) (padding . 0.5) (stretchability . 30))
  % #'((basic-distance . 5) (padding . 0.5) (stretchability . 30)) % default
  % from a (top-level) markup to a system

  % score-markup-spacing =
  % #'((basic-distance . 12) (padding . 0.5) (stretchability . 60))
  % #'((basic-distance . 12) (padding . 0.5) (stretchability . 60)) % default
  % from the last system of a score to a (top-level) markup

  % score-system-spacing =
  % #'((basic-distance . 14) (minimum-distance . 8) (padding . 1) (stretchability . 120))
  % #'((basic-distance . 14) (minimum-distance . 8) (padding . 1) (stretchability . 120)) % default
  % from the last system of a score to the first system of the next score
  % when no (top-level) markup exists between them

  % system-system-spacing =
  % #'((basic-distance . 12) (minimum-distance . 8) (padding . 1) (stretchability . 60))
  % #'((basic-distance . 12) (minimum-distance . 8) (padding . 1) (stretchability . 60)) % default
  % between two systems in the same score

  % markup-markup-spacing =
  % #'((basic-distance . 1) (padding . 0.5))
  % #'((basic-distance . 1) (padding . 0.5)) % default
  % between two (top-level) markups

  % last-bottom-spacing =
  % #'((basic-distance . 1) (minimum-distance . 0) (padding . 1) (stretchability . 30))
  % #'((basic-distance . 1) (minimum-distance . 0) (padding . 1) (stretchability . 30)) % default
  % from the last system or top-level markup on a page to the bottom of the printable area
  % (i.e., the top of the bottom margin)

  % top-system-spacing =
  % #'((basic-distance . 1) (minimum-distance . 0) (padding . 1))
  % #'((basic-distance . 1) (minimum-distance . 0) (padding . 1)) % default
  % from the top of the printable area (i.e., the bottom of the top margin)
  % to the first system on a page, when there is no (top-level) markup between the two

  % top-markup-spacing =
  % #'((basic-distance . 0) (minimum-distance . 0) (padding . 1))
  % #'((basic-distance . 0) (minimum-distance . 0) (padding . 1)) % default
  % from the top of the printable area (i.e., the bottom of the top margin)
  % to the first (top-level) markup on a page, when there is no system between them
}

restful = \relative c'' {
  c1
  r1
  r2 r2
  r4 r4 r4 r4
  r8 r r r
  r16 r r r r r r r
  % r32 r r r r r
}

spaceful = \relative c'' {
  s1 
  s1 
  s1
  s1
}

<<
  \new Staff {
    \restful
  }
  \new TradStaff {
    \restful
  }

\new Staff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice= "second"
    \relative { \voiceTwo \restful }
  >>
\new TradStaff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice= "second"
    \relative { \voiceTwo \restful }
  >>
  
\new Staff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice = "second"
    \relative { \voiceTwo \restful }
    \new Voice = "third"
    \relative { \voiceThree \restful }
  >>
\new TradStaff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice = "second"
    \relative { \voiceTwo \restful }
    \new Voice = "third"
    \relative { \voiceThree \restful }
  >>
  
  \new Staff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice = "second"
    \relative { \voiceTwo \restful }
    \new Voice = "third"
    \relative { \voiceThree \restful }
        \new Voice = "fourth"
    \relative { \voiceFour \restful }
  >>
  \new TradStaff <<
    \new Voice = "first"
    \relative { \voiceOne \restful }
    \new Voice = "second"
    \relative { \voiceTwo \restful }
    \new Voice = "third"
    \relative { \voiceThree \restful }
        \new Voice = "fourth"
    \relative { \voiceFour \restful }
  >>
>>


