\version "2.19.63"

#(define clairnote-type (or (getenv "CLAIRNOTE") "default"))

% supress the version error
#(define version-seen #t)
% #(ly:set-option 'warning-as-error #t)
#(ly:set-option 'compile-scheme-code #t)

#(debug-enable 'backtrace)
#(debug-set! width 200)

\include "../clairnote.ly"

#(set-default-paper-size "a4landscape")

\paper {
  indent = 0
}
