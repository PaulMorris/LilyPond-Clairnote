%
%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20140428 (2014 April 28)
%
%    Copyright © 2013, 2014 Paul Morris, except for three functions
%    that are in the public domain: clnt-shift-noteheads, setOtherScriptParent,
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
% Guile 2.0 has an "abs" function built in, so when
% LilyPond upgrades to it, remove this
#(define (abs x) (if (> x 0) x (- 0 x)))


%% NOTE HEADS AND STEM ATTACHMENT

#(define (clnt-note-heads cfill xmod ymod)
   (lambda (grob)
     (let* ((fsz  (ly:grob-property grob 'font-size 0.0))
            (mult (magstep fsz))

            (ptch (ly:event-property (event-cause grob) 'pitch))
            (semi (ly:pitch-semitones ptch))
            (note-type (modulo (+ semi cfill) 2))
            (dur-log (ly:grob-property grob 'duration-log))
            (whole-note (if (< dur-log 1) #t #f))

            (notecol (ly:grob-parent grob X))
            (stm (ly:grob-object notecol 'stem))
            (ypos (ly:grob-staff-position grob))
            (fnt (ly:grob-default-font grob)))

       ;; black notes can be rotated to -27, but -18 also works for white notes
       ;; currently -9, half of -18
       (if (not whole-note)
           (ly:grob-set-property! grob 'rotation '(-9 0 0)))

       ;; Note Heads
       (ly:grob-set-property! grob 'stencil
         (ly:stencil-scale
          (case note-type
            ;; white note
            ((0) (if whole-note
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
            ((1) (if whole-note
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
       ;; (ly:grob-set-property! grob 'color (case note-type ((0) red) ((1) blue)))

       ;; Stem Attachment
       (ly:grob-set-property! grob 'stem-attachment
         (if (= (remainder (abs semi) 2) 1)
             ;; white notes: f g a b c# d#
             (cons 1.06  0.3)
             ;; black notes: c d e f# g# a#
             (cons 1.04 0.3))))))


%% STEM LENGTH AND DOUBLE STEMS
% lengthen all stems and give half notes double stems

#(define ((clnt-stems mult) grob)
   ;; multiply each of the values in the details property of the stem grob
   ;; by mult, except for stem-shorten values which remain unchanged
   (ly:grob-set-property! grob 'details
     (map
      (lambda (detail)
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
       (ly:grob-set-property! grob 'stencil
         (ly:stencil-combine-at-edge
          (ly:stem::print grob)
          X
          (- (ly:grob-property grob 'direction))
          (ly:stem::print grob)
          -0.42 ))))


%% ACCIDENTAL SIGNS

% use a closure
#(define Clnt_accidental_engraver #f)
% these two stencils are global, used by key sig engraver
#(define clnt-sharp-sign empty-stencil)
#(define clnt-flat-sign empty-stencil)

#(let ((clnt-double-sharp-sign empty-stencil)
       (clnt-double-flat-sign empty-stencil)
       ;; bar-num and acc-list, for storing the currently
       ;; active accidentals in the current bar/measure
       (bar-num 0)
       (acc-list '()))

   (define (draw-acc-sign is-sharp)
     "Return a sharp or flat sign stencil. @var{is-sharp} is boolean"
     (let ((line (ly:stencil-translate
                  (make-connected-path-stencil '((0  1.0)) 0.2 1 1 #f #f)
                  (cons 0 -0.5)))
           (circ (make-circle-stencil 0.24 0.01 #t)))
       (ly:stencil-add line
         (ly:stencil-translate circ (cons 0 (if is-sharp 0.5 -0.5))))))

   (define (draw-double-acc-sign acc-sign)
     "Return a double sharp or double flat sign stencil."
     (ly:stencil-add
      (ly:stencil-translate acc-sign (cons -0.25 0))
      (ly:stencil-translate acc-sign (cons  0.25 0))))

   (define (redo-acc grob mult stil x-ext)
     "Helper for redo-acc-signs."
     (ly:grob-set-property! grob 'X-extent x-ext)
     (ly:grob-set-property! grob 'stencil
       (ly:stencil-scale stil mult mult)))

   (define (redo-acc-signs grob acc mult)
     "Replaces the accidental sign stencil and resizes X/Y extents."
     ;; TODO: should natural signs have the same Y-extent as others?
     ;; TODO: shouldn't X/Y-extent scale with mult / font-size?
     (ly:grob-set-property! grob 'Y-extent (cons -0.5 1.2))
     (case acc
       ((-1/2) (redo-acc grob mult clnt-flat-sign (cons 0 0.54)))
       ((1/2) (redo-acc grob mult clnt-sharp-sign (cons -0.27 0.27)))
       ((-1) (redo-acc grob mult clnt-double-flat-sign (cons -0.34 0.67)))
       ((1) (redo-acc grob mult clnt-double-sharp-sign (cons -0.54 0.47)))
       ((0) (redo-acc grob mult
              (ly:stencil-scale (ly:grob-property grob 'stencil) 0.65 0.65)
              (cons -0.0 (* 2/3 0.65))))
       (else (ly:grob-property grob 'stencil))))

   (set! clnt-sharp-sign (draw-acc-sign #t))
   (set! clnt-flat-sign (draw-acc-sign #f))
   (set! clnt-double-sharp-sign (draw-double-acc-sign clnt-sharp-sign))
   (set! clnt-double-flat-sign (draw-double-acc-sign clnt-flat-sign))
   ;; TODO: add clnt-natural-sign, have to use the font glyph.
   ;; (grob-interpret-markup grob (markup #:natural))

   (set! Clnt_accidental_engraver
         (make-engraver
          (acknowledgers
           ((accidental-interface engraver grob source-engraver)
            (let* ((mult (magstep (ly:grob-property grob 'font-size 0.0)))
                   (acc (accidental-interface::calc-alteration grob))
                   ;; get the current bar number
                   (context (ly:translator-context engraver))
                   ;; (current-bar-num (ly:context-property context 'currentBarNumber))
                   (internal-bar-num (ly:context-property context 'internalBarNumber))
                   ;; get the pitch of the note in semitones
                   (note-head (ly:grob-parent grob Y))
                   (pitch (ly:event-property (event-cause note-head) 'pitch))
                   (semi (ly:pitch-semitones pitch))
                   (semi-modulo (modulo semi 12))

                   ;; key-sig is an association list of sharps or flats in the key.
                   ;; Example: D major = ((0 . 1/2) (3 . 1/2))
                   (key-sig (ly:context-property context 'keySignature))

                   ;; TODO: handle custom key sigs that have octave values:
                   ;;    "keySignature (list) ... an alist containing (step . alter)
                   ;;    or ((octave . step) . alter)"    <---- not currently handled

                   ;; nats-list is an association list, a pair for each note in the key with
                   ;; semitone (0-11) and alteration (sharp, flat, or natural) (1/2, -1/2, 0).
                   ;; Diatonic pitch values (0-6) are converted to semitones (0-11).
                   ;; D major example:
                   ;; hypothetical diatonic alist: ((0 . 1/2) (1 . 0) (2 . 0) (3 . 1/2) (4 . 0) (5 . 0) (6 . 0))
                   ;; returns semitone alist: ((1 . 1/2) (2 . 0) (4 . 0) (6 . 1/2) (7 . 0) (9 . 0) (11 . 0))
                   (nats-list
                    (map
                     (lambda (n)
                       (let* ((alt (assoc-ref key-sig n))
                              (alt (if alt alt 0))
                              (n-semi (+
                                       (list-ref '(0 2 4 5 7 9 11) n)
                                       (* alt 2))))
                         (cons (modulo n-semi 12) alt)))
                     (iota 7)))

                   (in-the-key (equal?
                                (cons semi-modulo acc)
                                (assoc semi-modulo nats-list))))

              ;; if we're in a new measure, clear acc-list, and set bar-num
              (cond ((not (= bar-num internal-bar-num))
                     (set! acc-list '())
                     (set! bar-num internal-bar-num)))

              (cond
               ;; 1. is an accidental and a new accidental in this measure
               ;; print sign and add accidental to acc-list
               ((and
                 (not in-the-key)
                 (not (equal? (cons semi acc) (assoc semi acc-list))))
                (redo-acc-signs grob acc mult)
                (set! acc-list (assoc-set! acc-list semi acc)))
               ;; 2. is not an accidental but is canceling a previous accidental in this measure
               ;; (semi is in acc-list (assoc-ref doesn't return #f) but the acc doesn't match)
               ;; print sign and remove accidental from the acc-list
               ((and
                 in-the-key
                 (assoc semi acc-list)
                 (not (equal? acc (assoc-ref semi acc-list))))
                (redo-acc-signs grob acc mult)
                (set! acc-list (assoc-remove! acc-list semi)))
               ;; 3. is an accidental but not a new accidental in this measure
               ;; 4. is not an accidental and is not canceling a previous accidental
               ;; print no acc sign
               ;; TODO: make sure this does not affect ledger line widths
               (else
                (ly:grob-set-property! grob 'stencil #f)
                (ly:grob-set-property! grob 'X-extent '(0 . 0))
                (ly:grob-set-property! grob 'Y-extent '(0 . 0))))))))))


%% KEY SIGNATURES

% use a closure
#(define Clnt_key_signature_engraver #f)

% key-stils, an associative list for storing key sig stencils
#(let ((key-stils '()))

   (define (draw-key-stencil grob alt-count tonic-num)
     "Draws Clairnote key signature stencils."
     (let* (;; maj-num: number of the tonic note 0-6, if the key sig were major
            ;; (alt-count maj-num)
            ;; (-7 0) (-5 1) (-3 2) (-1 3) (1 4) (3 5) (5 6) (7 0)
            ;; (-6 4) (-4 5) (-2 6) (0 0) (2 1) (4 2) (6 3)
            (maj-num
             (if (odd? alt-count)
                 (modulo (- (/ (+ alt-count 1) 2) 4) 7)
                 (modulo (/ alt-count 2) 7)))
            ;; mode-num: number of the mode (0-6)
            (mode-num (modulo (- tonic-num maj-num) 7))
            ;; note-space: calculate the distance between notes based on
            ;; vertical staff scaling. (foo - (((foo - 1.2) * 0.7) + 0.85))
            (note-space (- clnt-vscale-staff (+ (* (- clnt-vscale-staff 1.2) 0.7) 0.85)))
            ;; vert-adj: calculate vertical position of the sig
            ;; (alt-count  X)
            ;; (-7 -1) (-5 -11) (-3 -9) (-1 -7) (1 -5) (3 -3) (5 -1) (7 -11)
            ;; (-6 -6) (-4 -4) (-2 -2) (0 -12) (2 -10) (4 -8) (6 -6)
            ;; --> (X * note-space = vert-adj)
            (vert-adj
             (* note-space
               (+ -12
                 (modulo
                  (if (odd? alt-count) (+ alt-count 6) alt-count)
                  12))))
            ;; sig-type: position of black or white notes (0 = 3B 4W, 1 = 3W 4B)
            (sig-type (modulo alt-count 2))
            ;; create sig-head, the acc-sign and number at top of key sig
            (sig-head-acc
             (cond
              ((> alt-count 0) clnt-sharp-sign)
              ((< alt-count 0) clnt-flat-sign)
              ((= alt-count 0) (ly:stencil-scale
                                (grob-interpret-markup grob (markup #:natural))
                                0.65 0.65))))
            (sig-head-num
             (ly:stencil-scale
              (grob-interpret-markup grob
                (markup (number->string (abs alt-count))))
              0.6 0.6))
            (sig-head
             (ly:stencil-combine-at-edge
              (ly:stencil-aligned-to sig-head-acc Y CENTER) 0 1
              (ly:stencil-aligned-to sig-head-num Y CENTER) 0.3))
            (sig empty-stencil)
            (xpos 0)
            (ypos 0)
            (solid-dot? (= sig-type 0)))

       ;; create the sig (as a stack of circles and an oval)
       (for-each
        (lambda (n)
          ;; add a circle (or oval) to the sig
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
           (else (set! ypos (+ ypos (* 2 note-space)))))) ;; +0.67
        (iota 7))

       ;; position the sig vertically
       (set! sig (ly:stencil-translate-axis sig vert-adj Y))

       ;; add the head to the sig
       (set! sig
             (ly:stencil-add
              (ly:stencil-aligned-to sig X CENTER)
              (ly:stencil-translate-axis
               (ly:stencil-aligned-to sig-head X CENTER)
               (case alt-count
                 ((3) (* note-space 12.5))
                 ((5) (* note-space 14.5))
                 ((-2) (* note-space 13.5))
                 ((-7) (* note-space 14.5))
                 (else (* note-space 11.5)))
               Y)))

       ;; shift the whole sig to the right for proper spacing with clef
       ;; and return the sig stencil
       (if (> mode-num 2)
           (ly:stencil-translate-axis sig 0.35 X)
           (ly:stencil-translate-axis sig 0.9 X))))


   (set! Clnt_key_signature_engraver
         (make-engraver
          (acknowledgers
           ((key-signature-interface engraver grob source-engraver)
            (let* (;; tonic-num: number of the tonic note (0-6), (C=0, B=6)
                   ;; engraver --> context --> tonic (pitch) --> tonic-num
                   (tonic-num
                    (ly:pitch-notename
                     (ly:context-property
                      (ly:translator-context engraver)
                      'tonic)))
                   ;; grob-name not needed (was for KeyCancellations)
                   ;; (grob-name (assq-ref (ly:grob-property grob 'meta) 'name))
                   ;; alt-alist: list of accidentals
                   (alt-alist (ly:grob-property grob 'alteration-alist))
                   ;; alt-count: number of sharps or flats in key sig
                   ;; positive is sharps, negative is flats
                   (alt-count (if (null? alt-alist)
                                  0
                                  (* (length alt-alist) 2 (cdr (car alt-alist)))))
                   ;; create a unique key id (string) for key-stils
                   (key-id
                    (string-append
                     (number->string tonic-num)
                     (if (> alt-count -1) "+" "")
                     (number->string alt-count)))
                   (stil (assoc-ref key-id key-stils))
                   (mult (magstep (ly:grob-property grob 'font-size 0.0))))

              (cond
               ;; 1. print nothing for key cancellations
               ;; not needed, staff definition has: printKeyCancellation = ##f
               ;; ((equal? 'KeyCancellation grob-name)
               ;; (ly:grob-set-property! grob 'stencil #f))
               ;; 2. if key stencil is already in key-stils use that
               ((ly:stencil? stil)
                (ly:grob-set-property! grob 'stencil (ly:stencil-scale stil mult mult)))
               ;; 3. else new sig, draw stencil, set it, and add to key-stils
               (else
                (set! stil (draw-key-stencil grob alt-count tonic-num))
                (ly:grob-set-property! grob 'stencil (ly:stencil-scale stil mult mult))
                (set! key-stils (acons key-id stil key-stils)))) ))))))


%% CHORDS

#(define (clnt-chords-one grob)
   "Step 1 of 4. For notes in chords or harmonic intervals
    that are 2 semitones apart, automatically place them
    on opposite sides of the stem."
   (let* ((heads-array (ly:grob-object grob 'note-heads))
          (note-heads (if (ly:grob-array? heads-array)
                          (ly:grob-array->list heads-array)
                          ;; for case of no note-heads in NoteColumn grob (rests)
                          (list 0))))
     ;; rests and single notes don't need offsetting
     (if (> (length note-heads) 1)
         (clnt-chords-two grob note-heads))))


#(define (clnt-chords-two grob note-heads)
   "Step 2 of 4."
   (let* (;; create a list of the semitones of each note in the chord
          (semitones
           (map (lambda (head-grob)
                  (ly:pitch-semitones (ly:event-property (event-cause head-grob) 'pitch)))
             note-heads))
          ;; create a list of lists to store input order of notes, like so: ((0 6) (1 10) (2 8) ...)
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
         (clnt-chords-three grob note-heads int-list semi-lists-sorted))))


#(define (clnt-chords-three grob note-heads int-list semi-lists-sorted)
   "Step 3 of 4."
   (let* ((cluster-list
           ;; convert interval list into a list of clusters of notes
           ;; ex: (4 3 2 2 5) --> (1 1 3 1)
           (fold-right
            (lambda (int clust-list)
              ;; notes 2 semitones apart or less are in the same cluster
              ;; if not in previous cluster, start new cluster at 1
              ;; else in current cluster, add 1 to current cluster
              (if (> int 2)
                  (cons 1 clust-list )
                  (cons (+ 1 (car clust-list)) (cdr clust-list))))
            '(1)
            int-list))

          ;; stem direction, 1 is up, -1 is down
          (stmdir (ly:grob-property (ly:grob-object grob 'stem) 'direction))
          ;; convert cluster-list to stemsides list:
          ;; new, desired note placements (-1 left, 1 right)
          ;; ex: (1 1 3 1) --> upstem: (-1 -1 -1 1 -1 -1) or downstem: (1 1 1 -1 1 1)
          (stemsides
           (concatenate
            (fold-right
             (lambda (cluster stemside-list)
               ;; only with a down-stem and odd-numbered cluster
               ;; (or single note, 1 = odd) is the first/lowest head on the
               ;; right side of stem, else it is always on left side of stem
               (let ((stemside (if (and (= stmdir -1) (odd? cluster)) 1 -1)))
                 ;; for each note in a cluster alternate between -1 and 1
                 ;; putting every other note on the opposite side of the stem
                 (cons
                  (map-in-order
                   (lambda (n)
                     (if (even? n)
                         stemside
                         (* stemside -1)))
                   (iota cluster))
                  stemside-list)))
             '()
             cluster-list)))

          ;; get the order the notes were entered in the input file and
          ;; assemble list of lists zipping the new stemsides to their input positions
          ;; sort stemsides by order notes were entered in the input file
          (input-order-sorted (unzip1 semi-lists-sorted))
          (stemsides-zip (zip input-order-sorted stemsides))
          (stemsides-sorted
           (sort! stemsides-zip
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
          (offsets
           (map-in-order
            (lambda (a b) (if (= a b ) 0 b ))
            old-stemsides
            (concatenate (map cdr stemsides-sorted)))))

     ;; is there anything to offset?  no need to send ( 0 0 0 0 )
     (if (or
          (memq 1 offsets)
          (memq -1 offsets))
         ((clnt-shift-noteheads offsets) grob))))


% Credit goes to David Nalesnik and Thomas Morley for these three functions:
% clnt-shift-noteheads, setOtherScriptParent, and adjustStem, copied (and edited slightly)
% from the LilyPond Snippet Repository, snippet 861, "Re-positioning note heads on
% the opposite side of the stem" http://lsr.di.unimi.it/LSR/Item?id=861
% The code for these three functions is in the public domain.

#(define ((clnt-shift-noteheads offsets) grob)
   "Step 4 of 4. Moves NoteHeads according to a list of @var{offsets}."
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

snhs =
#(define-music-function (parser location offsets) (list?)
   "To manually shift note heads (snhs) to the other side of the stem."
   #{
     \once \override NoteColumn.before-line-breaking = #(clnt-shift-noteheads offsets)
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


%% CLEFS: CLEF SETTINGS

% see /scm/parser-clef.scm

#(define (clnt-set-clefs clefs)
   "Helper function for modifying clef settings. c0-position is middle c position
    @var{clefs} is a list of (clef-name clef-glyph clef-position c0-position)
    add-new-clef args: clef-name clef-glyph clef-position octavation c0-position"
   (for-each
    (lambda (clef)
      (let ((name (list-ref clef 0))
            (glyph (list-ref clef 1))
            (pos (list-ref clef 2))
            (c0 (list-ref clef 3)))
        (add-new-clef name glyph pos 0 c0)
        (cond
         ((equal? name "bass") (add-new-clef "F" glyph pos 0 c0))
         ((equal? name "alto") (add-new-clef "C" glyph pos 0 c0))
         ((equal? name "treble")
          (add-new-clef "G" glyph pos 0 c0)
          (add-new-clef "violin" glyph pos 0 c0)))))
    clefs))

setClairnoteClefs =
#(define-void-function (parser location) ()
   "Sets (or resets) clef settings for clairnote."
   ;; To calculate the middle c position subtract the clef position
   ;; from 12 for bass clef or -12 for treble clef (to adjust the clef position
   ;; without affecting the position of middle c or other notes)"
   (clnt-set-clefs
    '(("treble" "clefs.G" -5 -7) ;; -7 = -12 minus -5
       ("bass" "clefs.F" 5 7) ;; 7 = 12 minus 5
       ("alto" "clefs.C" 0 0) ;; no change
       ("tenor" "clefs.C" 0 0) ;; same as alto
       ("french" "clefs.G" -5 -7) ;; same as treble
       ("soprano" "clefs.G" -5 -7) ;; same as treble
       ("mezzosoprano" "clefs.C" 0 0) ;; same as alto
       ("baritone" "clefs.F" 5 7) ;; same as bass
       ("varbaritone" "clefs.F" 5 7) ;; same as bass
       ("subbass" "clefs.F" 5 7) ;; same as bass
       ("percussion" "clefs.percussion" 0 0)))) % no change

\setClairnoteClefs

setTraditionalClefs =
#(define-void-function (parser location) ()
   "Sets clef settings back to traditional settings."
   (clnt-set-clefs
    '(("treble" "clefs.G" -2 -4)
      ("bass" "clefs.F" 2 4)
      ("alto" "clefs.C" 0 0)
      ("tenor" "clefs.C" 2 2)
      ("french" "clefs.G" -4 -4)
      ("soprano" "clefs.C" -4 0)
      ("mezzosoprano" "clefs.C" -2 0)
      ("baritone" "clefs.C" 4 0)
      ("varbaritone" "clefs.F" 0 4)
      ("subbass" "clefs.F" 4 4)
      ("percussion" "clefs.percussion" 0 0))))


%% CLEFS: "TRANSPOSED" CLEFS

% see /scm/parser-clef.scm
% and /ly/music-functions-init.ly

#(use-modules (ice-9 regex))

clef =
#(define-music-function (parser location type) (string?)
   "Modify clef transposition number in clef input to fit Clairnote staff.
    Replaces standard clef command, must be named clef."
   ;; ex: "treble^8" becomes "treble^13"
   ;; ex: "bass_15" becomes "bass_25"
   (let ((new-type "")
         (match (string-match "^(.*[_^][^0-9a-zA-Z]*)([1-9][0-9]*)([^0-9a-zA-Z]*)$" type)))
     (if (and match (match:substring match 2))
         (set! new-type
               (string-append
                (match:substring match 1)
                (case (string->number (match:substring match 2))
                  ((8) "13")
                  ((-8) "-13")
                  ((15) "25")
                  ((-15) "-25")
                  (else (match:substring match 2)))
                (match:substring match 3)))
         (set! new-type type))
     (make-clef-set new-type)))


%% CLEFS: CLEFS FOR StaffTrad

% see /ly/music-functions-init.ly

clefsTrad =
#(define-music-function (parser location music) (ly:music?)
   "Takes music with Clairnote clef settings, and returns music
    with clef settings adjusted for use on a traditional staff."
   (let ((current-glyph ""))
     (music-map
      (lambda (m)
        (cond
         ((equal? 'clefGlyph (ly:music-property m 'symbol))
          (set! current-glyph (ly:music-property m 'value)))

         ((equal? 'clefPosition (ly:music-property m 'symbol))
          (set! (ly:music-property m 'value)
                (case (ly:music-property m 'value)
                  ((-5) -2) ;; treble
                  ((5) 2) ;; bass
                  ((0) 0) ;; alto
                  ;; ((0) 2) ;; tenor - conflicts with alto clef
                  (else (ly:music-property m 'value)))))

         ((equal? 'clefTransposition (ly:music-property m 'symbol))
          (set! (ly:music-property m 'value)
                (case (ly:music-property m 'value)
                  ((12) 7) ;; ^8
                  ((-12) -7) ;; _8
                  ((24) 14) ;; ^15
                  ((-24) 14) ;; _15
                  (else (ly:music-property m 'value)))))

         ((equal? 'middleCClefPosition (ly:music-property m 'symbol))
          (cond
           ((equal? current-glyph "clefs.G")
            (set! (ly:music-property m 'value)
                  (case (ly:music-property m 'value)
                    ((-12) -6) ;; treble ;; -4 + -2
                    ((-24) -13) ;; treble^8
                    ((-36) -20) ;; treble^15
                    ((0) 1) ;; treble_8
                    ((12) 8) ;; treble_15
                    (else (ly:music-property m 'value)))))

           ((equal? current-glyph "clefs.F")
            (set! (ly:music-property m 'value)
                  (case (ly:music-property m 'value)
                    ((12) 6) ;; bass ;; 4 + 2
                    ((24) 13) ;; bass_8
                    ((36) 20) ;; bass_15
                    ((0) -1) ;; bass^8
                    ((-12) -8) ;; bass^15
                    (else (ly:music-property m 'value)))))

           ((equal? current-glyph "clefs.C")
            (set! (ly:music-property m 'value)
                  (case (ly:music-property m 'value)
                    ((0) 0) ;; alto
                    ((-12) -7) ;; alto^8
                    ((12) 7) ;; alto_8
                    ((-24) -14) ;; alto^15
                    ((24) 14) ;; alto_15
                    (else (ly:music-property m 'value))))))))
        m)
      music)))


%% REPEAT SIGN DOTS (BAR LINES)

% adjust the position of dots in repeat signs
% for Clairnote staff or traditional staff

#(define ((clnt-make-repeat-dot-bar dot-positions) grob extent)
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

#(define (clnt-repeat-dot-bar-procedure grob extent)
   "Based on a staff's line-positions, return a procedure for repeat sign dots."
   (if (equal?
        (ly:grob-property (ly:grob-object grob 'staff-symbol) 'line-positions '())
        '(-4 -2 0 2 4))
       ;; Traditional five line staff or Clairnote staff
       ((clnt-make-repeat-dot-bar '(-1 1)) grob extent)
       ((clnt-make-repeat-dot-bar '(-2 2)) grob extent)))

#(add-bar-glyph-print-procedure ":" clnt-repeat-dot-bar-procedure)


%% VERTICAL (CLAIRNOTE) STAFF COMPRESSION

% clnt-vscale-staff is a global variable holding the default vertical scaling
% value so it can be used with key signatures and staffSize function
% TODO: a way to store this per-staff rather than globally?
clnt-vscale-staff = 1.2

vertScaleStaff =
#(define-music-function (parser location vscale-staff) (number?)
   "Change the vertical distance between the staff lines (staff-space)
    and everything else to match."
   ;; vscale-staff = 1 gives a staff with an octave that is the same size
   ;; as on a traditional staff (default is 1.2).
   ;;  - stems are extended back to their original/traditional size
   ;;  - time signature position is adjusted vertically
   ;;  - elsewhere key signatures are adjusted to fit the staff
   (set! clnt-vscale-staff vscale-staff)
   #{
     \override StaffSymbol.staff-space = #(* vscale-staff 7/12)
     \override Stem.before-line-breaking = #(clnt-stems (/ 1 (* vscale-staff 7/12)))
     \override TimeSignature.before-line-breaking =
     #(lambda (grob)
        (ly:grob-set-property! grob 'Y-offset
          ;; ((((foo - 1.2) * -3.45) - 1.95) + foo)
          (+ (- (* (- vscale-staff 1.2) -3.45 ) 1.95) vscale-staff)))
   #})


%% USER STAFF SCALING FUNCTION

staffSize =
#(define-music-function (parser location new-size) (number?)
   "Helper macro that lets the user zoom a single staff size."
   ;; TODO: this does not work perfectly combined with vertScaleStaff
   ;; especially key signatures and time signatures
   #{
     \set fontSize = #new-size
     \override StaffSymbol.staff-space = #(*  clnt-vscale-staff 7/12 (magstep new-size))
     \override StaffSymbol.thickness = #(magstep new-size)
   #})


%% STAFF DEFINITIONS

\layout {

  % copy \Staff context with its standard settings to
  % a custom staff context called \StaffTrad
  \context {
    \Staff
    \name StaffTrad
    \alias Staff
    % needed for staff identification for repeat sign dots
    \override StaffSymbol.line-positions = #'(-4 -2 0 2 4)
    % \consists \Clnt_clef_corrector
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
    \override NoteHead.before-line-breaking = #(clnt-note-heads 1 1 1)

    \consists \Clnt_key_signature_engraver
    printKeyCancellation = ##f

    \consists \Clnt_accidental_engraver
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()

    \override NoteColumn.before-line-breaking = #clnt-chords-one
    \numericTimeSignature
  }
}

% allow parent contexts to accept \StaffTrad in midi output too
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
