(define-module (clairnote utils))

(use-modules
  (srfi srfi-1)
  (srfi srfi-13)
  (lily))

(define-public (non-zero? n) (not (zero? n)))

(define-public (positive-integer? n) (and (positive? n) (integer? n)))

(define-public (map-pair proc pair)
  (cons
    (proc (car pair))
    (proc (cdr pair))))

(define-public (pitch-to-semitone pitch)
  ;; Takes a pitch object and returns a semitone integer that corresponds to
  ;; the pitch's position on the Clairnote staff. Used for
  ;; staffLineLayoutFunction. The return value is almost always the semitone
  ;; returned by (ly:pitch-semitones pitch) except for quarter tone
  ;; alteration exceptions. 1/4 and 3/4 alterations are quarter tone sharps
  ;; and their semitone needs to be adjusted down by one.
  (let
    ((alteration (ly:pitch-alteration pitch))
      (semitone (ly:pitch-semitones pitch)))
    (cond
      ((= 1/4 alteration) (- semitone 1))
      ((= 3/4 alteration) (- semitone 1))
      (else semitone))))

(define-public (notehead-pitch grob)
  ;; Takes a note head grob and returns its pitch.
  (define event (ly:grob-property grob 'cause))
  (if (ly:stream-event? event)
    (ly:event-property event 'pitch)
    (begin
      (ly:warning "clairnote.ly cannot access the pitch of a note head grob.  (Are you trying to use the Ambitus_engraver?  It is incompatible with clairnote.ly.)")
      (ly:make-pitch 0 0 0))))

(define-public (notehead-semitone grob)
  ;; Takes a note head grob and returns its semitone.
  (pitch-to-semitone (notehead-pitch grob)))

(define-public (staff-symbol-property grob prop default)
  ;; Takes a grob @var{grob}, a symbol @var{prop}, and
  ;; a @var{default} value. Returns that custom StaffSymbol
  ;; property or silently falls back to the default value.
  (define staff-sym (ly:grob-object grob 'staff-symbol))
  (if (ly:grob? staff-sym)
    (ly:grob-property staff-sym prop)
    default))

(define-public (get-base-staff-space grob)
  ;; Takes a grob and returns the custom StaffSymbol property
  ;; cn-base-staff-space.  Silently falls back to the default of 0.75.
  (staff-symbol-property grob 'cn-base-staff-space 0.75))

(define-public (magnification grob)
  ;; Return the current magnification (from magnifyStaff, etc.)
  ;; via a grob's font size.
  (magstep (ly:grob-property grob 'font-size 0)))

(define-public (get-staff-clef-adjust staff-octaves clef-octave-shift)
  ;; Calculate the amount to vertically adjust the position of the clef,
  ;; key signature, and time signature, in note-spaces / half-staff-spaces.
  (+
    (* 12 clef-octave-shift)
    (if (odd? staff-octaves)
      6
      (if (> staff-octaves 2) 12 0))))

(define-public (staff-clef-adjust-from-grob grob)
  (get-staff-clef-adjust
    (staff-symbol-property grob 'cn-staff-octaves 2)
    (staff-symbol-property grob 'cn-clef-shift 0)))

(define-public (note-heads-from-grob grob default)
  ;; Takes a grob like a Stem and returns a list of
  ;; NoteHead grobs or default.
  (let* ((heads-array (ly:grob-object grob 'note-heads))
         (heads-list (if (ly:grob-array? heads-array)
                      (ly:grob-array->list heads-array)
                      ;; should never/rarely? happen:
                      default)))
    heads-list))
