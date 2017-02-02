%
%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20140423 (2014 April 23)
%
%    Copyright © 2013, 2014 Paul Morris, except for three functions
%    that are in the public domain: shift-noteheads, setOtherScriptParent,
%    and adjustStem, copied (and edited slightly) from the LilyPond Snippet
%    Repository, snippet 861, "Re-positioning note heads on the opposite side
%    of the stem" http://lsr.di.unimi.it/LSR/Item?id=861 Thanks to David Nalesnik
%    and Thomas Morley for writing these functions.
%
%    Contact information: http://clairnote.org/about/
%
%    This file is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This file is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this file.  If not, see <http://www.gnu.org/licenses/>.

\version "2.18.2"

% Absolute value helper function
% when LilyPond upgrades to Guile 2.0, use "abs" and remove this function
#(define (abs x) (if (> x 0) x (- 0 x)))

%% NOTE HEADS AND STEM ATTACHMENT

#(define (clairnoteNoteHeads cfill xmod ymod)
   (lambda (grob)
     (let* ((fsz  (ly:grob-property grob 'font-size 0.0))
            (mult (magstep fsz))

            (ptch (ly:event-property (event-cause grob) 'pitch))
            (semi (ly:pitch-semitones ptch))
            (note-type (modulo (+ semi cfill) 2))
            (dur-log (ly:grob-property grob 'duration-log))
            (whole-note? (if (< dur-log 1) #t #f))

            (notecol (ly:grob-parent grob X))
            (stm (ly:grob-object notecol 'stem))
            (ypos (ly:grob-staff-position grob))
            (fnt (ly:grob-default-font grob)))

       ;; black notes can be rotated to -27, but -18 also works for white notes
       ;; currently -9 which is half of -18
       (if (not whole-note?)
           (set! (ly:grob-property grob 'rotation) '(-9 0 0)))

       ;; NOTE HEADS
       (set! (ly:grob-property grob 'stencil)
             (ly:stencil-scale
              (case note-type
                ;; white note
                ((0) (if whole-note?
                         ;; white whole note
                         ;; thicken top and bottom using an oval path so no white space shows
                         ;; through above and below staff lines for hollow whole notes
                         (ly:stencil-add
                          (ly:stencil-translate
                           (make-oval-stencil 0.7 0.58 0.11 #f)
                           '(0.98 . 0))
                          (ly:font-get-glyph fnt "noteheads.s0"))
                         ;; white non-whole note
                         ;; scale hollow note heads horizontally so they match solid ones
                         (ly:stencil-scale
                          (ly:font-get-glyph fnt "noteheads.s1")
                          0.945 1)))
                ;; black note
                ((1) (if whole-note?
                         ;; black whole note
                         ;; add a little black circle to make solid whole notes
                         (ly:stencil-add
                          (ly:font-get-glyph fnt "noteheads.s0")
                          (ly:stencil-translate (make-circle-stencil 0.47 0.1 #t) '(0.95 . 0)))
                         ;; black non-whole note
                         (ly:font-get-glyph fnt "noteheads.s2"))))
              (* xmod mult)
              (* ymod mult)))

       ;; for testing
       ;; (set! (ly:grob-property grob 'color) (case note-type ((0) blue) ((1) blue)))

       ;; STEM ATTACHMENT
       (set! (ly:grob-property grob 'stem-attachment)
             (if (= (remainder (abs semi) 2) 1)
                 ;; white notes: f g a b c# d#
                 (cons 1.06  0.3)
                 ;; black notes: c d e f# g# a#
                 (cons 1.04 0.3))))))


%% STEM LENGTH AND DOUBLE STEMS
% lengthen all stems and give half notes double stems

#(define ((stem-lengthen mult) grob)
   ;; multiply each of the values in the details propert of the stem grob by mult,
   ;; except for stem-shorten values which remain unchanged
   (set! (ly:grob-property grob 'details)
         (map (lambda (detail)
                (let ((head (car detail))
                      (args (cdr detail)))
                  (if (eq? head 'stem-shorten)
                      (cons head args)
                      (cons head
                        (map
                         (lambda (arg) (* arg mult))
                         args)))))
           (ly:grob-property grob 'details)))
   ;; double stems for half notes
   ;; use -0.42 or 0.15 to change which side the 2nd stem appears
   (if (= 1 (ly:grob-property grob 'duration-log))
       (set! (ly:grob-property grob 'stencil)
             (ly:stencil-combine-at-edge
              (ly:stem::print grob)
              X
              (- (ly:grob-property grob 'direction))
              (ly:stem::print grob)
              -0.42 ))))


%% ACCIDENTALS

#(define (make-sharp-flat-stencil isSharp)
   (let*((ln (ly:stencil-translate
              (make-connected-path-stencil
               '((0  1.0)) 0.2 1 1 #f #f)
              (cons 0 -0.5)))
         (crcl (make-circle-stencil 0.24 0.01 #t)))
     (if isSharp
         (ly:stencil-add ln (ly:stencil-translate crcl (cons 0  0.5))) ;; sharp
         (ly:stencil-add ln (ly:stencil-translate crcl (cons 0 -0.5)))))) % flat

#(define (make-double-sharp-flat-stencil isSharp)
   (ly:stencil-add
    (ly:stencil-translate (make-sharp-flat-stencil isSharp) (cons -0.25 0))
    (ly:stencil-translate (make-sharp-flat-stencil isSharp) (cons  0.25 0))))

#(define (redraw-acc-sign grob acc mult)
   "Changes and resizes an accidental stencil."
   ;; set the 'X-extent and 'Y-extent
   (ly:grob-set-property! grob 'Y-extent (cons -0.5 1.2))
   (ly:grob-set-property! grob 'X-extent
     (case acc
       ((-1/2) (cons 0 0.54)) ;; flat
       ((1/2) (cons -0.27 0.27)) ;; sharp
       ((0) (cons -0.0  (* 0.666666 0.65))) ;; natural
       ((-1) (cons -0.34 0.67)) ;; double flat
       ((1) (cons -0.54 0.47)))) ;; double sharp
   ;; set the stencil
   (ly:grob-set-property! grob 'stencil
     (ly:stencil-scale
      (case acc
        ((-1/2) (make-sharp-flat-stencil #f))
        ((1/2) (make-sharp-flat-stencil #t))
        ((0) (ly:stencil-scale (ly:grob-property grob 'stencil) 0.65 0.65 ))
        ((-1) (make-double-sharp-flat-stencil #f))
        ((1) (make-double-sharp-flat-stencil #t))
        (else (ly:grob-property grob 'stencil)))
      mult mult)))

% TODO: handle custom keysigs that have octave values:
% "keySignature (list) ... an alist containing (step . alter) or ((octave . step) . alter)"
% currently we don't handle the latter.

#(define (make-nats-list key-alt-list)
   "Takes a list of sharps or flats in a key sig (key-alt-list) and returns a list of
    natural notes in the key and whether they are sharp, flat, or natural (1/2, -1/2, 0).
    Converts diatonic pitch values (0-6) to semitone values (0-11)."
   (let ((nats-list '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)))
         (to-semi '((0 . 0) (1 . 2) (2 . 4) (3 . 5) (4 . 7) (5 . 9) (6 . 11))))
     ;; add sharps or flats from key sig to list of diatonic naturals (0-6)
     ;; D major example: ((0 . 1/2) (1 . 0) (2 . 0) (3 . 1/2) (4 . 0) (5 . 0) (6 . 0))
     (for-each
      (lambda (key-alt)
        (set! nats-list
              (assoc-set! nats-list (car key-alt) (cdr key-alt))))
      key-alt-list)
     ;; convert to semitone values
     ;; D major example: ((1 . 1/2) (2 . 0) (4 . 0) (6 . 1/2) (7 . 0) (9 . 0) (11 . 0))
     (map
      (lambda (nat)
        (let* ((acc (cdr nat))
               (semi (assoc-ref to-semi (car nat)))
               (semi-acc (+ semi (* 2 acc))))
          (cons (modulo semi-acc 12) acc)))
      nats-list)))

% for storing the bar number and the
% accidentals in the current bar/measure so far
clnt-barnum = 0
clnt-acc-list = #'()

#(define Clairnote_accidental_engraver
   (make-engraver
    (acknowledgers
     ((accidental-interface engraver grob source-engraver)
      (let* ((mult (magstep (ly:grob-property grob 'font-size 0.0)))
             (acc (accidental-interface::calc-alteration grob))
             ;; get the current bar number
             (context (ly:translator-context engraver))
             ;; (curr-barnum (ly:context-property context 'currentBarNumber))
             (internal-barnum (ly:context-property context 'internalBarNumber))
             ;; get the pitch of the note in semitones
             (note-head (ly:grob-parent grob Y))
             (pitch (ly:event-property (event-cause note-head) 'pitch))
             (semi (ly:pitch-semitones pitch))
             (semi-modulo (modulo semi 12))
             (key-sig (ly:context-property context 'keySignature))
             ;; nats-list is an association list of naturals in the current key by semitone (0-11)
             ;; and if they are sharp, flat, or natural. D major example:
             ;; ((1 . 1/2) (2 . 0) (4 . 0) (6 . 1/2) (7 . 0) (9 . 0) (11 . 0))
             (nats-list (make-nats-list key-sig))
             (in-the-key (equal? (cons semi-modulo acc) (assoc semi-modulo nats-list))))

        ;; if we're in a new measure, clear clnt-acc-list, and set clnt-barnum
        (cond ((not (= clnt-barnum internal-barnum))
               (set! clnt-acc-list '())
               (set! clnt-barnum internal-barnum)))

        (cond
         ;; 1. is an accidental and a new accidental in this measure
         ;; print sign and add accidental to clnt-acc-list
         ((and
           (not in-the-key)
           (not (equal? (cons semi acc) (assoc semi clnt-acc-list))))
          (redraw-acc-sign grob acc mult)
          (set! clnt-acc-list (assoc-set! clnt-acc-list semi acc)))
         ;; 2. is not an accidental but is canceling a previous accidental in this measure
         ;; (semi is in clnt-acc-list (assoc doesn't return #f) but the acc doesn't match)
         ;; print sign and remove accidental from the acc-list
         ((and
           in-the-key
           (assoc semi clnt-acc-list)
           (not (equal? acc (assoc-ref semi clnt-acc-list))))
          (redraw-acc-sign grob acc mult)
          (set! clnt-acc-list (assoc-remove! clnt-acc-list semi)))
         ;; 3. is an accidental but not a new accidental in this measure
         ;; 4. is not an accidental and is not canceling a previous accidental
         ;; print no sign
         ;; TODO: make sure this does not affect ledger line widths
         (else
          (ly:grob-set-property! grob 'stencil #f)
          (ly:grob-set-property! grob 'X-extent '(0 . 0))
          (ly:grob-set-property! grob 'Y-extent '(0 . 0)))))))))


%% KEY SIGNATURES ENGRAVER

% associative list for storing key signature stencils
key-stil-bank = #'()

%    acc-list: list of accidentals,
%    nats-list: list of naturals
%    key-acc-type: the accidental sign type, 1/2=sharp, -1/2=flat
%    acc-count: the number of accidentals in the key signature,
%      positive is sharps, negative is flats
%    tonic-num: number of the tonic note 0-6, C=0, B=6
%
#(define Clairnote_key_signature_engraver
   (make-engraver
    (acknowledgers
     ((key-signature-interface engraver grob source-engraver)
      (let* ((grob-name (assq-ref (ly:grob-property grob 'meta) 'name))
             (context (ly:translator-context engraver))
             (tonic-pitch (ly:context-property context 'tonic))
             (tonic-num (ly:pitch-notename tonic-pitch))
             (acc-list (ly:grob-property grob 'alteration-alist))
             ;; the key type: sharp (1/2), flat (-1/2), or natural (0)
             (key-acc-type (if (null? acc-list) 0 (cdr (list-ref acc-list 0))))
             (acc-count (* (length acc-list) (if (< key-acc-type 0) -1 1)))
             (sig #f)
             ;; create a unique key id (string) for key-stil-bank
             (key-id (string-append
                      (number->string tonic-num)
                      (if (> acc-count -1) "+" "")
                      (number->string acc-count)))
             (mult (magstep (ly:grob-property grob 'font-size 0.0))))

        ;; set the key signature stencil
        (cond
         ;; print nothing for key cancellations
         ((equal? 'KeyCancellation grob-name)
          (ly:grob-set-property! grob 'stencil #f)
          (set! sig #f))
         ;; new sig, create stil and add it to key-stil-bank
         ((equal? #f (assoc key-id key-stil-bank))
          (set! sig (draw-key-stencil grob acc-count tonic-num))
          (set! key-stil-bank (acons key-id sig key-stil-bank)))
         ;; else already in key-stil-bank, so use it
         (else
          (set! sig (cdr (assoc key-id key-stil-bank)))))

        ;; resize stil per current font size and set it
        (if (ly:stencil? sig)
            (ly:grob-set-property! grob 'stencil (ly:stencil-scale sig mult mult))))))))

% DRAW KEY SIGNATURE STENCIL
%
%    maj-num: number of the tonic note 0-6, if the key sig were major
%    sig-type: position of black or white notes: 0=starts with black, 1=starts with white
%    mode-num: number of the mode 0-6
%    tonic-psn: vertical position of the tonic/mode indicator within the key sig
%    tonic-lr: whether the tonic indicator is left or right of the sig, -1.2=left 1.2=right
%
#(define (draw-key-stencil grob acc-count tonic-num)
   (let* ((maj-num (case acc-count
                     ((0) 0)
                     ((1) 4)  ((2) 1)  ((3) 5)   ((4) 2)  ((5) 6)  ((6) 3)  ((7) 0)
                     ((-1) 3) ((-2) 6) ((-3) 2) ((-4) 5) ((-5) 1) ((-6) 4) ((-7) 0)))
          (mode-num (modulo (- tonic-num maj-num) 7))
          ;; calculate the distance between notes based on vertical staff scaling
          ;; (foo - (((foo - 1.2) * 0.7) + 0.85))
          (note-space (- clnt-vscale-staff (+ (* (- clnt-vscale-staff 1.2) 0.7) 0.85)))
          ;; calculate vertical position of the sig
          (vert-adj (case acc-count
                      ((0) (* note-space -12))
                      ((1) (* note-space -5))
                      ((2) (* note-space -10))
                      ((3) (* note-space -3))
                      ((4) (* note-space -8))
                      ((5) (* note-space -1))
                      ((6) (* note-space -6))
                      ((7) (* note-space -11))
                      ((-1) (* note-space -7))
                      ((-2) (* note-space -2))
                      ((-3) (* note-space -9))
                      ((-4) (* note-space -4))
                      ((-5) (* note-space -11))
                      ((-6) (* note-space -6))
                      ((-7) (* note-space -1))))
          (sig-type (modulo acc-count 2)) ;; 0 = C, D, E   1 = F, G, A, B
          ;; create sig-head (that floats at top of key sig)
          (sig-head-acc (cond
                         ((> acc-count 0) (make-sharp-flat-stencil #t))
                         ((< acc-count 0) (make-sharp-flat-stencil #f))
                         ((= acc-count 0) (ly:stencil-scale
                                           (grob-interpret-markup grob (markup #:natural))
                                           0.65 0.65))))
          (sig-head-num
           (ly:stencil-scale
            (grob-interpret-markup grob
              (markup (number->string (abs acc-count))))
            0.6 0.6))
          (sig-head (ly:stencil-combine-at-edge
                     (ly:stencil-aligned-to sig-head-acc Y CENTER)
                     0 1
                     (ly:stencil-aligned-to sig-head-num Y CENTER)
                     0.3))
          (sig empty-stencil)
          (xpos 0)
          (ypos 0)
          (solid-dot? (= sig-type 0)))

     ;; create the the sig (as a stack of circles and an oval)
     (for-each
      (lambda (n)
        ;; add the next circle (or oval) to the sig
        (set! sig
              (ly:stencil-add sig
                (ly:stencil-translate
                 (if (= n mode-num)
                     ;; make tonic oval
                     (if solid-dot?
                         (make-oval-stencil 0.55 0.38 0.001 #t)
                         (make-oval-stencil 0.55 0.33 0.15 #f))
                     ;; make regular circle
                     (if solid-dot?
                         (make-circle-stencil 0.3 0.001 #t)
                         (make-circle-stencil 0.25 0.15 #f)))
                 (cons xpos ypos))))
        ;; adjust values for next circle (or oval),
        ;; switching things up after the 3rd one
        (cond
         ((= n 2)
          (set! solid-dot? (not solid-dot?))
          (set! xpos (+ xpos 0.60))
          (set! ypos (+ ypos note-space))) ;; +0.335
         (else
          (set! ypos (+ ypos (* 2 note-space)))))) ;; +0.67
      (iota 7))

     ;; position the sig vertically
     (set! sig (ly:stencil-translate sig (cons 0 vert-adj)))

     ;; add the head to the sig
     (set! sig (ly:stencil-add
                (ly:stencil-aligned-to sig X CENTER)
                (ly:stencil-translate
                 (ly:stencil-aligned-to sig-head X CENTER)
                 (cons 0
                   (case acc-count
                     ((3) (* note-space 12.5))
                     ((5) (* note-space 14.5))
                     ((-2) (* note-space 13.5))
                     ((-7) (* note-space 14.5))
                     (else (* note-space 11.5)))))))

     ;; shift the whole sig to the right for proper spacing with clef
     (if (> mode-num 2)
         (set! sig (ly:stencil-translate sig (cons 0.35 0)))
         (set! sig (ly:stencil-translate sig (cons 0.9 0))))
     ;; return the sig stencil
     sig))


%% CHORDS - AUTO
% automatically place note heads in chords/harmonies on
% the correct side of stem, needed for 2 semitone intervals

% Step 1 of 3
#(define (chord-handler grob)
   (let* ((heads-array (ly:grob-object grob 'note-heads))
          (note-heads (if (ly:grob-array? heads-array)
                          (ly:grob-array->list heads-array)
                          ;; for case of no note-heads in NoteColumn grob (rests)
                          (list 0))))
     ;; rests and single notes don't need offsetting
     (if (> (length note-heads) 1)
         (chord-handler-two grob note-heads))))

% Step 2 of 3
#(define (chord-handler-two grob note-heads)
   (let* (;; create a list of the semitones of each note in the chord
          (semitones (map (lambda (head-grob)
                            (ly:pitch-semitones (ly:event-property (event-cause head-grob) 'pitch)))
                       note-heads))
          ;; create a list of lists to store input order of notes, like so: ((0 6) (1 10) (2 8) (3 5)...)
          (semi-lists (zip (iota (length note-heads)) semitones))
          ;; sort both by semitones, ascending
          (semitones-sorted (sort-list semitones <))
          (semi-lists-sorted
           (sort! semi-lists (lambda (a b) (< (list-ref a 1) (list-ref b 1)))))

          ;; calculate the intervals between the notes, uses a second copy
          ;; of the list with its first value dropped so the previous values are
          ;; in place to be subtracted from the next
          (int-list (map - (cdr semitones-sorted) semitones-sorted)))

     ;; no 2 semitone intervals? then there is no need to offset
     ;; any notes since standard layout is already correct
     (if (memq 2 int-list)
         (chord-handler-three grob note-heads int-list semi-lists-sorted))))

% Step 3 of 3
#(define (chord-handler-three grob note-heads int-list semi-lists-sorted)

   ;; recursive function for converting interval list into a list of clusters of notes
   (define (get-clusters int-list n)
     ;; (display int-list)(display "int-list") (newline)
     (if (pair? int-list)
         ;; notes 2 semitones apart or less are in the same cluster
         (if (> (car int-list) 2)
             ;; note is not in previous cluster
             ;; add previous count to list and reset count to 1
             (cons n (get-clusters (cdr int-list) 1))
             ;; note is in current cluster so just add one to the count
             (get-clusters (cdr int-list) (+ 1 n)))
         (cons n '())))

   ;; recursive function to calculate correct stemsides from cluster list
   (define (get-stemsides clust-list stmdir)
     ;; (display clust-list)(display "clust-list") (newline)
     (if (pair? clust-list)
         (let* ((clust-count (car clust-list))
                ;; if down-stem and odd-numbered cluster (or single note, since 1 = odd)
                ;; then first/lowest head is on right side of stem
                ;; else first/lowest head is on left side of stem
                (stemside (if (and (= stmdir -1) (odd? clust-count)) 1 -1)))
           (cons
            ;; for each note in this cluster add a 1 or -1 to the list and
            ;; putting every other note on the opposite side of the stem
            (map-in-order (lambda (x)
                            (if (even? x)
                                stemside
                                (* stemside -1)))
              (iota clust-count))
            (get-stemsides (cdr clust-list) stmdir)))
         '()))

   (let* ((cluster-list (get-clusters int-list 1))
          ;; stem direction, 1 is up, -1 is down
          (stmdir (ly:grob-property (ly:grob-object grob 'stem) 'direction))
          ;; new, desired note placements (-1 left, 1 right)
          (stemsides (concatenate (get-stemsides cluster-list stmdir)))
          ;; get the order the notes were entered in the input file and
          ;; assemble list of lists zipping the new-stemsides to their input positions
          ;; sort stemsides by order notes were entered in the input file
          (input-order-sorted (unzip1 semi-lists-sorted))
          (stemsides-B (zip input-order-sorted stemsides))
          (stemsides-sorted (sort! stemsides-B
                              (lambda (a b) (< (list-ref a 0) (list-ref b 0)))))
          ;; get old default stem-side positions
          (old-stemsides
           (map-in-order
            (lambda (head-grob)
              ;; use round to avoid floating point number errors
              (let ((pos (round (ly:grob-relative-coordinate head-grob grob 0))))
                (cond
                 ((and (= stmdir 1) (= pos 0)) -1) ;; left of up-stem
                 ((and (= stmdir 1) (positive? pos)) 1) ;; right of up-stem
                 ((and (= stmdir -1) (= pos 0)) 1) ;; right of down-stem
                 ((and (= stmdir -1) (negative? pos)) -1)))) ;; left of down-stem
            note-heads))
          ;; generate offsets
          ;; if old-stemside and stemside-sorted are the same,
          ;; 0 is the offset, else use -1 or 1 from stemsides-sorted
          (offset-list
           (map-in-order
            (lambda (a b) (if (= a b ) 0 b ))
            old-stemsides
            (concatenate (map cdr stemsides-sorted)))))
     ;; call the manual offset function if there's anything to offset
     ;; no need to send ( 0 0 0 0 )
     (if (or
          (memq 1 offset-list)
          (memq -1 offset-list))
         ((shift-noteheads offset-list) grob))))


%% CHORDS - MANUAL
% For chords and intervals, manually shift note heads to left or right of stem.
%
% Credit goes to David Nalesnik and Thomas Morley for these three functions:
% shift-noteheads, setOtherScriptParent, and adjustStem, copied (and edited slightly)
% from the LilyPond Snippet Repository, snippet 861, "Re-positioning note heads on
% the opposite side of the stem" http://lsr.di.unimi.it/LSR/Item?id=861
% The code for these three functions is in the public domain.

#(define ((shift-noteheads offsets) grob)
   "Defines how NoteHeads should be moved according to the given list of offsets."
   (let* (
           ;; NoteHeads
           ;; Get the NoteHeads of the NoteColumn
           (note-heads (ly:grob-array->list (ly:grob-object grob 'note-heads)))
           ;; Get their durations
           (nh-duration-log
            (map
             (lambda (note-head-grobs)
               (ly:grob-property note-head-grobs 'duration-log))
             note-heads))
           ;; Get the stencils of the NoteHeads
           (nh-stencils
            (map
             (lambda (note-head-grobs)
               (ly:grob-property note-head-grobs 'stencil))
             note-heads))
           ;; Get their length in X-axis-direction
           (stencils-x-lengths
            (map
             (lambda (x)
               (let* ((stencil (ly:grob-property x 'stencil))
                      (stencil-X-exts (ly:stencil-extent stencil X))
                      (stencil-lengths (interval-length stencil-X-exts)))
                 stencil-lengths))
             note-heads))
           ;; Stem
           (stem (ly:grob-object grob 'stem))
           (stem-thick (ly:grob-property stem 'thickness 1.3))
           (stem-x-width (/ stem-thick 10))
           (stem-dir (ly:grob-property stem 'direction))

           ;; stencil width method, doesn't work with non-default beams
           ;; so using thickness property above instead
           ;; (stem-stil (ly:grob-property stem 'stencil))
           ;; (stem-x-width (if (ly:stencil? stem-stil)
           ;;                 (interval-length (ly:stencil-extent stem-stil X))
           ;;                 ;; if no stem-stencil use 'thickness-property
           ;;                 (/ stem-thick 10)))

           ;; Calculate a value to compensate the stem-extension
           (stem-x-corr
            (map
             (lambda (q)
               ;; TODO better coding if (<= log 0)
               (cond ((and (= q 0) (= stem-dir 1))
                      (* -1 (+ 2  (* -4 stem-x-width))))
                 ((and (< q 0) (= stem-dir 1))
                  (* -1 (+ 2  (* -1 stem-x-width))))
                 ((< q 0)
                  (* 2 stem-x-width))
                 (else (/ stem-x-width 2))))
             nh-duration-log)))

     ;; Final Calculation for moving the NoteHeads
     (for-each
      (lambda (nh nh-x-length off x-corr)
        (if (= off 0)
            #f
            (ly:grob-translate-axis! nh (* off (- nh-x-length x-corr)) X)))
      note-heads stencils-x-lengths offsets stem-x-corr)))

% shift note heads
snhs =
#(define-music-function (parser location offsets) (list?)
   " Moves the NoteHeads, using (shift-noteheads offsets) "
   #{
     \once \override NoteColumn.before-line-breaking = #(shift-noteheads offsets)
   #})

setOtherScriptParent =
#(define-music-function (parser location which-note-head)(integer?)
   "If the parent-NoteHead of a Script is moved, another parent from the
    NoteColumn could be chosen.
    The NoteHeads are numbered 1 2 3...  not 0 1 2... "
   #{
     %% Let "staccato" be centered on NoteHead, if Stem 'direction is forced
     %% with \stemUp, \stemDown, \voiceOne, \voiceTwo etc
     \once \override Script.toward-stem-shift = #0

     \once \override Script.after-line-breaking =
     #(lambda (grob)
        (let* ((note-head (ly:grob-parent grob X))
               (note-column (ly:grob-parent note-head X))
               (note-heads-list
                (ly:grob-array->list
                 (ly:grob-object note-column 'note-heads)))
               (count-note-heads (length note-heads-list)))
          (if (> which-note-head count-note-heads)
              (ly:warning "Can't find specified note-head - ignoring")
              (set! (ly:grob-parent grob X)
                    (list-ref note-heads-list (- which-note-head 1))))))
   #})

adjustStem =
#(define-music-function (parser location val)(pair?)
   "Adjust 'stem-attachment via
   adding multiples of the stem-width to the x-default (car val)
   and multiplying the y-default with (cdr val). "
   #{
     \once \override NoteHead.before-line-breaking =
     #(lambda (grob)
        (let* ((stem-at (ly:grob-property grob 'stem-attachment))
               (stem (ly:grob-object grob 'stem))
               (stem-x-width (interval-length (ly:grob-property stem 'X-extent))))
          (ly:grob-set-property!
           grob
           'stem-attachment
           (cons (+ (car stem-at) (* stem-x-width (car val))) (* (cdr val) (cdr stem-at)))
           )))
   #})


%% CLEF SETTINGS

#(define (set-clefs treble-pos treble-c bass-pos bass-c alto-pos alto-c)
   "Helper function for modifying clef settings."
   ;; add-new-clef args:  clef-name  clef-glyph  clef-position  octavation  c0-position
   (add-new-clef "treble" "clefs.G" treble-pos 0 treble-c)
   (add-new-clef "G" "clefs.G" treble-pos 0 treble-c)
   (add-new-clef "violin" "clefs.G" treble-pos 0 treble-c)
   (add-new-clef "bass" "clefs.F" bass-pos 0 bass-c)
   (add-new-clef "F" "clefs.F" bass-pos 0 bass-c)
   (add-new-clef "alto" "clefs.C" alto-pos 0 alto-c)
   (add-new-clef "C" "clefs.C" alto-pos 0 alto-c))

% set (or reset) clef settings for clairnote
% subtracting the clef position (i.e. treble-pos) in the calculation of
% the middle c position (i.e. treble-c) lets us adjust the clef position
% without affecting the position of middle c (or other notes)
#(define (set-clairnote-clefs)
   (let* ((treble-pos -5)
          (treble-c (- -12 treble-pos))
          (bass-pos 5)
          (bass-c (- 12 bass-pos))
          (alto-pos 0)
          (alto-c (- 0 alto-pos)))
     (set-clefs
      treble-pos treble-c
      bass-pos bass-c
      alto-pos alto-c)))

#(set-clairnote-clefs)

% use this function to set clefs back to traditional settings to
% include both traditional notation and Clairnote in the same file
#(define (set-traditional-clefs)
   (let* ((treble-pos -2)
          (treble-c -4)
          (bass-pos 2)
          (bass-c 4)
          (alto-pos 0)
          (alto-c 0))
     (set-clefs
      treble-pos treble-c
      bass-pos bass-c
      alto-pos alto-c)))


% REPEAT SIGN DOTS / BAR LINES

% adjust the position of dots in repeat signs
% for Clairnote staves or a traditional staves

#(define ((make-custom-dot-bar-line dot-positions) grob extent)

   "Draw dots (repeat sign dots) at @var{dot-positions}. The
coordinates of @var{dot-positions} are equivalent to the
coordinates of @code{StaffSymbol.line-positions}, a dot-position
of X and a line-position of X indicate the same vertical position."

   (let* ((staff-space (ly:staff-symbol-staff-space grob))
          (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
          (stencil empty-stencil))
     (for-each
      (lambda (dp)
        (set! stencil (ly:stencil-add stencil
                        (ly:stencil-translate-axis dot (* dp (/ staff-space 2)) Y))))
      dot-positions)
     stencil))

#(define (select-dot-bar-line-procedure grob extent)
   "Based on a staff's line-positions, return a procedure for repeat sign dots."
     (if (equal?
          (ly:grob-property (ly:grob-object grob 'staff-symbol) 'line-positions '())
          '(-4 -2 0 2 4))
         ;; Traditional five line staff
         ((make-custom-dot-bar-line '(-1 1)) grob extent)
         ;; Clairnote staff
         ((make-custom-dot-bar-line '(-2 2)) grob extent)))

#(add-bar-glyph-print-procedure ":" select-dot-bar-line-procedure)


%% VERTICAL (CLAIRNOTE) STAFF COMPRESSION

% global variable holding the default vertical scaling value
% so it can be used with key signatures and staffSize function
% TODO: how to store this per-staff rather than globally?

clnt-vscale-staff = 1.2

% change the vertical distance between the staff lines (staff-space)
% and everything else to match.  vscale-staff = 1 gives a staff with an
% octave that is the same size as on a traditional staff (default is 1.2).
%   stems are extended back to their original/traditional size
%   time signature position is adjusted vertically
%   elsewhere key signatures are adjusted to fit the staff
vertScaleStaff =
#(define-music-function (parser location vscale-staff) (number?)
   (set! clnt-vscale-staff vscale-staff)
   #{
     \override StaffSymbol.staff-space = #(* vscale-staff 7/12)
     \override Stem.before-line-breaking = #(stem-lengthen (/ 1 (* vscale-staff 7/12)))
     \override TimeSignature.before-line-breaking =
     #(lambda (grob)
        (set! (ly:grob-property grob 'Y-offset)
              ;; ((((foo - 1.2) * -3.45) - 1.95) + foo)
              (+ (- (* (- vscale-staff 1.2) -3.45 ) 1.95) vscale-staff)))
   #})


%% USER STAFF SCALING FUNCTION

% helper macro lets user zoom a single staff size
% TODO: can this be improved?
% it does not work perfectly combined with vertScaleStaff
% especially key signatures and time signatures
staffSize =
#(define-music-function (parser location new-size) (number?)
   #{
     \set fontSize = #new-size
     \override StaffSymbol.staff-space = #(*  clnt-vscale-staff 7/12 (magstep new-size))
     \override StaffSymbol.thickness = #(magstep new-size)
   #})


%% CLAIRNOTE STAFF DEFINITIONS

\layout {

  % copy \Staff with its standard settings to a custom
  % staff context called \StaffTrad
  \context {
    \Staff
    \name StaffTrad
    \alias Staff
    % needed for staff identification for repeat sign dots
    \override StaffSymbol.line-positions = #'(-4 -2 0 2 4)
  }

  % allow parent contexts to accept \StaffTrad
  \context { \Score \accepts StaffTrad }
  \context { \ChoirStaff \accepts StaffTrad }
  \context { \GrandStaff \accepts StaffTrad }
  \context { \PianoStaff \accepts StaffTrad }
  \context { \StaffGroup \accepts StaffTrad }

  % customize \Staff to make it a Clairnote staff
  \context {
    \Staff
    staffLineLayoutFunction = #ly:pitch-semitones
    middleCPosition = -12
    clefPosition = -5

    \override StaffSymbol.line-positions = #'(-8 -4  4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)
    \override StaffSymbol.ledger-extra = 1
    \override Stem.no-stem-extend = ##t

    \vertScaleStaff 1.2
    % to vertically scale noteheads change the last argument here:
    \override NoteHead.before-line-breaking = #(clairnoteNoteHeads 1 1 1)

    \consists \Clairnote_key_signature_engraver
    printKeyCancellation = ##f

    \consists \Clairnote_accidental_engraver
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()

    \override NoteColumn.before-line-breaking = #chord-handler
    \numericTimeSignature
  }
}

% to prevent logging errors, also allow parent contexts
% to accept \StaffTrad in midi output.
\midi {
  \context {
    \Staff
    \name StaffTrad
    \alias Staff
  }
  \context { \Score \accepts StaffTrad }
  \context { \ChoirStaff \accepts StaffTrad }
  \context { \GrandStaff \accepts StaffTrad }
  \context { \PianoStaff \accepts StaffTrad }
  \context { \StaffGroup \accepts StaffTrad }
}
