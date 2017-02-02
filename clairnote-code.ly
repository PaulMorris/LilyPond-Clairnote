%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20140702 (2014 July 02)
%
%    Copyright © 2013, 2014 Paul Morris, except for five functions:
%    A. two functions copied and modified from LilyPond source code:
%    define-grob-property and translator-property-description
%    B. three functions in the public domain: cn-shift-noteheads,
%    setOtherScriptParent, and adjustStem, copied (and edited slightly) from
%    the LilyPond Snippet Repository, snippet 861, "Re-positioning note heads
%    on the opposite side of the stem" http://lsr.di.unimi.it/LSR/Item?id=861
%    Thanks to David Nalesnik and Thomas Morley for these two functions.
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

% Absolute value function
% Guile 2.0 has an "abs" function built in, so when
% LilyPond upgrades to it, remove this
#(define (abs x) (if (> x 0) x (- 0 x)))


%% NOTE HEADS AND STEM ATTACHMENT

#(define (cn-draw-note-head-stencils grob)
   "Returns a list of note head stencils used for overriding note heads."
   (let*
    ((fnt (ly:grob-default-font grob))
     (wn (ly:font-get-glyph fnt "noteheads.s0")))
    (list
     ;; 0 = black note
     (ly:font-get-glyph fnt "noteheads.s2")
     ;; 1 = white note
     ;; scale hollow note heads horizontally to match solid ones
     (ly:stencil-scale (ly:font-get-glyph fnt "noteheads.s1") 0.945 1)
     ;; 2 = black whole note, add black circle to make solid
     (ly:stencil-add wn
       (ly:stencil-translate
        (make-circle-stencil 0.47 0.1 #t)
        '(0.95 . 0)))
     ;; 3 = white whole note, thicken top and bottom using an oval
     ;; path so no white space shows above and below staff lines
     (ly:stencil-add wn
       (ly:stencil-translate
        (make-oval-stencil 0.7 0.58 0.11 #f)
        '(0.98 . 0))) )))

#(define (cn-note-heads grob)
   "Override note head stencils and other properties."
   ;; make sure \omit is not in effect (i.e. stencil is not #f)
   (if (ly:grob-property-data grob 'stencil)
       (let
        ((mult (magstep (ly:grob-property grob 'font-size 0.0)))
         (whole-note (< (ly:grob-property grob 'duration-log) 1))
         ;; 0 = black note, 1 = white note
         (note-type (modulo
                     (ly:pitch-semitones
                      (ly:event-property (ly:grob-property grob 'cause) 'pitch))
                     2)))
        (if whole-note
            ;; adjust note-type: 2 = black whole-note, 3 = white whole-note
            (set! note-type (+ 2 note-type))
            ;; else set rotation and stem attachment properties
            (begin
             ;; black notes can be rotated to -27, but -18 also works for white notes
             ;; currently -9, half of -18
             (ly:grob-set-property! grob 'rotation '(-9 0 0))
             (ly:grob-set-property! grob 'stem-attachment
               (case note-type
                 ((0) (cons 1.04 0.3)) ;; black note
                 ((1) (cons 1.06  0.3)) )))) ;; white note
        ;; set note head stencil
        (ly:grob-set-property! grob 'stencil
          (ly:stencil-scale
           (list-ref
            (ly:grob-property
             (ly:grob-object grob 'staff-symbol)
             'cn-note-head-stencils)
            note-type)
           mult mult)))))


%% STEM LENGTH AND DOUBLE STEMS

#(define ((cn-stems mult) grob)
   "Lengthen all stems and give half notes double stems."
   ;; make sure \omit is not in effect (i.e. stencil is not #f)
   (if (ly:grob-property-data grob 'stencil)
       (begin
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
               -0.42 ))))))


%% BEAMS

#(define ((cn-beams staff-spc-inv) grob)
   "Adjust size and spacing of beams, needed due to
    smaller StaffSymbol.staff-space. Get font size from
    note head, not stem, especially for CueVoice."
   (let*
    ((stem (ly:grob-parent grob X))
     (note-col (ly:grob-parent stem X))
     (noteheads (ly:grob-object note-col 'note-heads))
     (notehead (ly:grob-array-ref noteheads 0))
     (mult (magstep (ly:grob-property notehead 'font-size 0)))
     ;; TODO: mult-space calculation handles regular and grace notes
     ;; but not CueVoice where spacing is slightly too wide.
     ;; Probably could be better overall.
     ;; (mult-space (+ -380/197 (* mult 577/197)))
     (mult-space (+ 1.1 (* 1/3 (- 1 mult))))
     ;; calculation from magnifyMusic by Mark Polesky
     (mult-thick (+ 119/925 (* mult 13/37))))
    (ly:grob-set-property! grob 'beam-thickness
      (* mult-thick staff-spc-inv))
    (ly:grob-set-property! grob 'length-fraction
      (* mult-space mult staff-spc-inv))))


%% ACCIDENTAL SIGNS

% sharp sign and flat sign stencils are also used by key sig engraver
#(define cn-sharp-sign empty-stencil)
#(define cn-flat-sign empty-stencil)
#(define cn-double-sharp-sign empty-stencil)
#(define cn-double-flat-sign empty-stencil)

% generate accidental sign stencils
#(let
  ((draw-acc-sign
    (lambda (is-sharp)
      "Return a sharp or flat sign stencil. @var{is-sharp} is boolean"
      (let ((line (ly:stencil-translate
                   (make-connected-path-stencil '((0  1.0)) 0.2 1 1 #f #f)
                   (cons 0 -0.5)))
            (circ (make-circle-stencil 0.24 0.01 #t)))
        (ly:stencil-add line
          (ly:stencil-translate circ (cons 0 (if is-sharp 0.5 -0.5)))))))

   (draw-double-acc-sign
    (lambda (acc-sign)
      "Return a double sharp or double flat sign stencil."
      (ly:stencil-add
       (ly:stencil-translate acc-sign (cons -0.25 0))
       (ly:stencil-translate acc-sign (cons  0.25 0))))))

  ;; TODO: (?) add cn-natural-sign, have to use the font glyph.
  ;; (grob-interpret-markup grob (markup #:natural))
  (set! cn-sharp-sign (draw-acc-sign #t))
  (set! cn-flat-sign (draw-acc-sign #f))
  (set! cn-double-sharp-sign (draw-double-acc-sign cn-sharp-sign))
  (set! cn-double-flat-sign (draw-double-acc-sign cn-flat-sign)))


#(define (cn-redo-acc grob stil x-ext)
   "Helper for cn-redo-acc-signs."
   (let ((mult (magstep (ly:grob-property grob 'font-size 0.0))))
     (ly:grob-set-property! grob 'X-extent x-ext)
     (ly:grob-set-property! grob 'stencil
       (ly:stencil-scale stil mult mult))))

#(define (cn-redo-acc-signs grob acc)
   "Replaces the accidental sign stencil and resizes X/Y extents."
   ;; TODO: should natural signs have the same Y-extent as others?
   ;; TODO: shouldn't X/Y-extent scale with mult / font-size?
   (ly:grob-set-property! grob 'Y-extent (cons -0.5 1.2))
   (case acc
     ((-1/2) (cn-redo-acc grob cn-flat-sign (cons 0 0.54)))
     ((1/2) (cn-redo-acc grob cn-sharp-sign (cons -0.27 0.27)))
     ((-1) (cn-redo-acc grob cn-double-flat-sign (cons -0.34 0.67)))
     ((1) (cn-redo-acc grob cn-double-sharp-sign (cons -0.54 0.47)))
     ((0) (cn-redo-acc grob
            (ly:stencil-scale (ly:grob-property grob 'stencil) 0.65 0.65)
            (cons -0.0 (* 2/3 0.65))))
     (else (ly:grob-property grob 'stencil))))

#(define (pitch-in-key? pitch key-sig)
   "key-sig is an association list of sharps or flats in the key sig.
    Example: D major (C#, F#) = ((0 . 1/2) (3 . 1/2))"
   ;; TODO: handle custom key sigs that have octave values:
   ;;    "keySignature (list) ... an alist containing (step . alter)
   ;;    or ((octave . step) . alter)"    <---- not currently handled
   (let
    ((note (ly:pitch-notename pitch))
     (alt (ly:pitch-alteration pitch)))
    ;; is the note-and-alt-pair in the key sig?
    ;;   yes --> #t sharp/flat in key sig
    ;;   no, is alt a sharp or flat (i.e. not a natural)?
    ;;     yes --> #f sharp/flat not in key sig
    ;;     no, is note (disregarding its alt) in the key sig?
    ;;       yes --> #f natural not in key sig
    ;;       no --> #t natural in key sig
    (if (equal? (cons note alt) (assoc note key-sig))
        #t
        (if (not (equal? 0 alt))
            #f
            (if (assoc-ref key-sig note)
                #f
                #t)))))

#(define (cn-engrave-acc context grob acc-list)
   "To show an acc sign or not. Updates cn-acc-list if needed.
    SHOW ACC SIGN:
    1. new acc: an acc not in the acc list
        add to cn-acc-list (remove previous acc if replacing it)
    2. cancel acc: in key, cancels a previous acc in acc list
        (semi is in cn-acc-list but the acc does not match)
        remove acc from cn-acc-list
    3. forced acc: acc wouldn't be shown, but it was forced with !
        no change to cn-acc-list
    DON'T SHOW ACC SIGN:
    4. is an acc but not a new one in this measure
    5. is not an acc and is not cancelling previous acc
    6. omit is in effect (stencil is #f)"
   (let*
    ((alt (accidental-interface::calc-alteration grob))
     (stl (ly:grob-property-data grob 'stencil))
     (note-head (ly:grob-parent grob Y))
     (pitch (ly:event-property (event-cause note-head) 'pitch))
     (semi (ly:pitch-semitones pitch))
     (key-sig (ly:context-property context 'keySignature))
     (in-the-key (pitch-in-key? pitch key-sig))
     (in-acc-list (equal? (cons semi alt) (assoc semi acc-list)))
     (semi-in-acc-list (equal? alt (assoc-ref acc-list semi))))
    (cond
     ;; 1. new acc
     ((and (not in-the-key) (not in-acc-list) stl)
      (cn-redo-acc-signs grob alt)
      (if semi-in-acc-list
          (ly:context-set-property! context 'cn-acc-list
            (assoc-remove! acc-list semi)))
      (ly:context-set-property! context 'cn-acc-list
        (assoc-set! acc-list semi alt)))
     ;; 2. cancel acc
     ((and in-the-key (not in-acc-list) semi-in-acc-list stl)
      (cn-redo-acc-signs grob alt)
      (ly:context-set-property! context 'cn-acc-list
        (assoc-remove! acc-list semi)))
     ;; 3. forced acc
     ((and (equal? #t (ly:grob-property grob 'forced)) stl)
      (cn-redo-acc-signs grob alt))
     ;; 4, 5, 6
     ;; TODO: does this affect ledger line widths?
     (else (ly:grob-suicide! grob)))))

#(define Cn_accidental_engraver
   (make-engraver
    (acknowledgers
     ((accidental-interface engraver grob source-engraver)
      (let*
       ((context (ly:translator-context engraver))
        ;; (current-bar-num (ly:context-property context 'currentBarNumber))
        (bar-num (ly:context-property context 'internalBarNumber)))
       ;; if we're in a new measure, clear cn-acc-list, and set cn-bar-num
       (if (equal? bar-num (ly:context-property context 'cn-bar-num))
           (cn-engrave-acc context grob
             (ly:context-property context 'cn-acc-list))
           (begin
            (ly:context-set-property! context 'cn-acc-list '())
            (ly:context-set-property! context 'cn-bar-num bar-num)
            (cn-engrave-acc context grob '()))))))))


%% KEY SIGNATURES

#(define (cn-get-major-tonic alt-count)
   "Return number of the tonic note 0-6, as if the key sig were major."
   ;; (alt-count maj-num)
   ;; (-7 0) (-5 1) (-3 2) (-1 3) (1 4) (3 5) (5 6) (7 0)
   ;; (-6 4) (-4 5) (-2 6) (0 0) (2 1) (4 2) (6 3)
   (if (odd? alt-count)
       (modulo (- (/ (+ alt-count 1) 2) 4) 7)
       (modulo (/ alt-count 2) 7)))

#(define (cn-get-note-space grob)
   "Return the distance between two adjacent notes given vertical staff
    scaling factor from custom grob property in StaffSymbol grob."
   (let ((vscale-staff
          (ly:grob-property
           (ly:grob-object grob 'staff-symbol)
           'cn-vscale-staff '())))
     ;; (foo - (((foo - 1.2) * 0.7) + 0.85))
     (- vscale-staff (+ (* (- vscale-staff 1.2) 0.7) 0.85))))

#(define (cn-make-keysig-stack mode alt-count note-space)
   "Create the stack of circles (and tonic oval) for the key sig."
   (let*
    ((xposns '(0 0 0 0.6 0.6 0.6 0.6))
     (yposns (map (lambda (p) (* p note-space)) '(0 2 4 5 7 9 11))) ;; +0.335 +0.67
     ;; sig-type: position of black or white notes (#t = 3B 4W, #f = 3W 4B)
     (sig-type (= 0 (modulo alt-count 2)))
     (bw-list (if sig-type '(#t #t #t #f #f #f #f) '(#f #f #f #t #t #t #t)))
     (stack-list
      (map
       (lambda (n x y bw)
         (ly:stencil-translate
          (if (= n mode)
              (if bw
                  (make-oval-stencil 0.55 0.38 0.001 #t)
                  (make-oval-stencil 0.55 0.33 0.15 #f))
              (if bw
                  (make-circle-stencil 0.3 0.001 #t)
                  (make-circle-stencil 0.25 0.15 #f)))
          (cons x y)))
       (iota 7) xposns yposns bw-list)))
    (fold ly:stencil-add empty-stencil stack-list)))

#(define (cn-get-keysig-vert-pos alt-count)
   "Calculate vertical position of the key sig."
   ;; (alt-count  return-value)
   ;; (-7 -1) (-5 -11) (-3 -9) (-1 -7) (1 -5) (3 -3) (5 -1) (7 -11)
   ;; (-6 -6) (-4 -4) (-2 -2) (0 -12) (2 -10) (4 -8) (6 -6)
   (+ (modulo
       (if (odd? alt-count) (+ alt-count 6) alt-count)
       12) -12))

#(define (cn-make-keysig-head grob alt-count)
   "Make sig-head, the acc-sign and number at top of key sig."
   (let
    ((acc (cond
           ((> alt-count 0) cn-sharp-sign)
           ((< alt-count 0) cn-flat-sign)
           ((= alt-count 0) (ly:stencil-scale
                             (grob-interpret-markup grob (markup #:natural))
                             0.65 0.65))))
     (num (ly:stencil-scale
           (grob-interpret-markup grob
             (markup (number->string (abs alt-count))))
           0.6 0.6)))
    (ly:stencil-combine-at-edge
     (ly:stencil-aligned-to acc Y CENTER) 0 1
     (ly:stencil-aligned-to num Y CENTER) 0.3)))

#(define (cn-make-keysig-head-stack head stack alt-count note-space)
   (ly:stencil-add
    (ly:stencil-aligned-to stack X CENTER)
    (ly:stencil-translate-axis
     (ly:stencil-aligned-to head X CENTER)
     (case alt-count
       ((3) (* note-space 12.5))
       ((5) (* note-space 14.5))
       ((-2) (* note-space 13.5))
       ((-7) (* note-space 14.5))
       (else (* note-space 11.5)))
     Y)))

#(define (cn-draw-keysig grob alt-count tonic-num)
   "Draws Clairnote key signature stencils."
   (let*
    ((major-tonic-num (cn-get-major-tonic alt-count))
     ;; number of the mode (0-6)
     (mode (modulo (- tonic-num major-tonic-num) 7))
     (note-space (cn-get-note-space grob))
     (stack (cn-make-keysig-stack mode alt-count note-space))
     ;; position the sig vertically
     (vert-adj (* note-space (cn-get-keysig-vert-pos alt-count)))
     (stack (ly:stencil-translate-axis stack vert-adj Y))
     (head (cn-make-keysig-head grob alt-count))
     ;; add the head to the stack
     (head-stack (cn-make-keysig-head-stack head stack alt-count note-space)))
    ;; shift the sig to the right for better spacing
    (if (> mode 2)
        (ly:stencil-translate-axis head-stack 0.35 X)
        (ly:stencil-translate-axis head-stack 0.9 X))))

#(define (cn-get-keysig-alt-count grob)
   "Return number of sharps or flats in key sig
    positive values for sharps, negative for flats."
   (let ((alt-alist (ly:grob-property grob 'alteration-alist)))
     (if (null? alt-alist)
         0
         (* (length alt-alist) 2 (cdr (car alt-alist))))))

#(define (cn-make-keysig-id tonic-num alt-count)
   "Create a unique key id (string) for storing and
    retrieving key sig stencils."
   (string-append
    (number->string tonic-num)
    (if (> alt-count -1) "+" "")
    (number->string alt-count)))

#(define (cn-engrave-keysig context grob)
   (let*
    ((alt-count (cn-get-keysig-alt-count grob))
     ;; number of the tonic note (0-6) (C-B) 'tonic (pitch) --> tonic-num
     (tonic-num (ly:pitch-notename (ly:context-property context 'tonic)))
     (mult (magstep (ly:grob-property grob 'font-size 0.0)))
     (key-id (cn-make-keysig-id tonic-num alt-count))
     (key-stils (ly:context-property context 'cn-key-stils))
     (stored-stil (assoc-ref key-stils key-id))
     (stil-is-stored (ly:stencil? stored-stil))
     (stil (if stil-is-stored
               stored-stil
               (cn-draw-keysig grob alt-count tonic-num))))
    ;; store stencil
    (if (not stil-is-stored)
        (ly:context-set-property! context 'cn-key-stils
          (acons key-id stil key-stils)))
    ;; set the stencil
    (ly:grob-set-property! grob 'stencil
      (ly:stencil-scale stil mult mult))))

#(define Cn_key_signature_engraver
   ;; the staff definition has printKeyCancellation = ##f, which
   ;; prevents all key cancellations, except for changing to C major
   ;; or A minor, so this engraver prevents all key cancellations"
   (make-engraver
    (acknowledgers
     ((key-signature-interface engraver grob source-engraver)
      (let*
       ((context (ly:translator-context engraver))
        ;; (pkc (ly:context-property context 'printKeyCancellation))
        (grob-name (assq-ref (ly:grob-property grob 'meta) 'name))
        (key-cancellation (equal? 'KeyCancellation grob-name))
        (omitted (equal? #f (ly:grob-property-data grob 'stencil))))
       (cond
        (key-cancellation (ly:grob-set-property! grob 'stencil #f))
        (omitted #f)
        (else (cn-engrave-keysig context grob))))))))


%% CHORDS

#(define (cn-chords-one grob)
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
         (cn-chords-two grob note-heads))))

#(define (cn-chords-two grob note-heads)
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
         (cn-chords-three grob note-heads int-list semi-lists-sorted))))

#(define (cn-make-cluster-list int-list)
   "convert interval list into a list of clusters of notes
    ex: (4 3 2 2 5) --> (1 1 3 1)"
   ;; notes 2 semitones apart or less are in the same cluster
   ;; if not in previous cluster, start new cluster at 1
   ;; else in current cluster, add 1 to current cluster
   (fold-right
    (lambda (int clust-list)
      (if (> int 2)
          (cons 1 clust-list)
          (cons (+ 1 (car clust-list)) (cdr clust-list))))
    '(1)
    int-list))

#(define (cn-make-stemsides-list cluster-list stmdir)
   "convert cluster-list to stemsides list:
    new, desired note placements (-1 left, 1 right)
    ex: (1 1 3 1) --> upstem: (-1 -1 -1 1 -1 -1) or downstem: (1 1 1 -1 1 1)"
   ;; For each note in a cluster alternate between -1 and 1
   ;; putting every other note on the opposite side of the stem.
   ;; The first/lowest head in a cluster is usually on the left side of
   ;; the stem. It is only on the right with a down-stem and
   ;; odd-numbered cluster (single note = odd-numbered cluster).
   (concatenate
    (fold-right
     (lambda (cluster stemside-list)
       (let ((stemside (if (and (= stmdir -1) (odd? cluster)) 1 -1)))
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

#(define (cn-get-old-stemsides grob stmdir note-heads)
   "Get old/default stem-side positions."
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

#(define (cn-chords-three grob note-heads int-list semi-lists-sorted)
   "Step 3 of 4."
   (let* ((cluster-list (cn-make-cluster-list int-list))
          ;; stem direction, 1 is up, -1 is down
          (stmdir (ly:grob-property (ly:grob-object grob 'stem) 'direction))
          (stemsides (cn-make-stemsides-list cluster-list stmdir))
          ;; get the order the notes were entered in the input file and
          ;; assemble list of lists zipping the new stemsides to their input positions
          ;; sort stemsides by order notes were entered in the input file
          (input-order-sorted (unzip1 semi-lists-sorted))
          (stemsides-zip (zip input-order-sorted stemsides))
          (stemsides-sorted
           (sort! stemsides-zip
             (lambda (a b) (< (list-ref a 0) (list-ref b 0)))))
          (old-stemsides (cn-get-old-stemsides grob stmdir note-heads))
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
         ((cn-shift-noteheads offsets) grob))))


% Credit goes to David Nalesnik and Thomas Morley for these three functions:
% cn-shift-noteheads, setOtherScriptParent, and adjustStem, copied (and edited slightly)
% from the LilyPond Snippet Repository, snippet 861, "Re-positioning note heads on
% the opposite side of the stem" http://lsr.di.unimi.it/LSR/Item?id=861
% The code for these three functions is in the public domain.

#(define ((cn-shift-noteheads offsets) grob)
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
               (cond
                ;; original, doesn't work (whole notes)
                ;; ((and (= q 0) (= stem-dir 1))
                ;;      (* -1 (+ 2  (* -4 stem-x-width))))
                ;; new, quick fix, could be better?
                ((= q 0) 0.223)

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
     \once \override NoteColumn.before-line-breaking = #(cn-shift-noteheads offsets)
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

#(define* (set-clairnote-clefs #:optional trad-settings)
   "Set clef settings for clairnote staff when trad-settings = #f (or omitted) or
    for traditional staff when trad-settings = #t. (c0-position is middle c position)
    add-new-clef args: clef-name clef-glyph clef-position octavation c0-position"
   ;; To calculate the middle c position subtract the clef position
   ;; from 12 for bass clef or -12 for treble clef (to adjust the clef position
   ;; without affecting the position of middle c or other notes)
   (for-each
    (lambda (c)
      (add-new-clef
       (list-ref c 0) (list-ref c 1) (list-ref c 2) (list-ref c 3) (list-ref c 4)))
    (if (not trad-settings)
        ;; clairnote clef settings
        '(("treble" "clefs.G" -5 0 -7) ;; -7 = -12 minus -5
           ("G" "clefs.G" -5 0 -7) ;; treble synonym
           ("G2" "clefs.G" -5 0 -7) ;; treble synonym
           ("violin" "clefs.G" -5 0 -7) ;; treble synonym
           ("bass" "clefs.F" 5 0 7) ;; 7 = 12 minus 5
           ("F" "clefs.F" 5 0 7) ;; bass synonym
           ("tenor" "clefs.C" 0 0 0) ;; => alto
           ("french" "clefs.G" -5 0 -7) ;; => treble
           ("soprano" "clefs.G" -5 0 -7) ;; => treble
           ("mezzosoprano" "clefs.C" 0 0 0) ;; => alto
           ("baritone" "clefs.F" 5 0 7) ;; => bass
           ("varbaritone" "clefs.F" 5 0 7) ;; => bass
           ("subbass" "clefs.F" 5 0 7)) ;; => bass
        ;; traditional clef settings
        '(("treble" "clefs.G" -2 0 -4)
          ("G" "clefs.G" -2 0 -4) ;; treble synonym
          ("G2" "clefs.G" -2 0 -4) ;; treble synonym
          ("violin" "clefs.G" -2 0 -4) ;; treble synonym
          ("bass" "clefs.F" 2 0 4)
          ("F" "clefs.F" 2 0 4) ;; bass synonym
          ("tenor" "clefs.C" 2 0 2)
          ("french" "clefs.G" -4 0 -4)
          ("soprano" "clefs.C" -4 0 0)
          ("mezzosoprano" "clefs.C" -2 0 0)
          ("baritone" "clefs.C" 4 0 0)
          ("varbaritone" "clefs.F" 0 0 4)
          ("subbass" "clefs.F" 4 0 4)))))

% No changes are needed for these clefs:
% ("alto" "clefs.C" 0 0 0)
% ("C" "clefs.C" 0 0 0)
% ("percussion" "clefs.percussion" 0 0 0)

#(set-clairnote-clefs)

#(define (set-traditional-clefs) (set-clairnote-clefs #t))


%% CLEFS: TRANSPOSED CLEFS

% see /scm/parser-clef.scm
% and /ly/music-functions-init.ly

#(use-modules (ice-9 regex))

#(define (cn-clef-transposition type)
   "Modify clef transposition number for Clairnote staff."
   ;; ex: "treble^8" becomes "treble^13"
   ;; ex: "bass_15" becomes "bass_25"
   (let ((match (string-match "^(.*[_^][^0-9a-zA-Z]*)([1-9][0-9]*)([^0-9a-zA-Z]*)$" type)))
     (if (and match (match:substring match 2))
         (string-append
          (match:substring match 1)
          (let ((num (string->number (match:substring match 2))))
            (number->string
             ;; if input is 13 or 25, use that.
             (if (or (= num 13) (= num 25))
                 num
                 ;; else convert from 7 notes per octave to 12
                 ;; 8-> 13, 15-> 25
                 ;; ((((X - 1) / 7) [round] * 12) + 1)
                 (+ 1 (* 12 (round (/ (- num 1) 7)))))))
          (match:substring match 3))
         type)))

clef =
#(define-music-function (parser location type) (string?)
   "Set the current clef to @var{type}. Replaces standard clef."
   (make-clef-set (cn-clef-transposition type)))

cueClef =
#(define-music-function (parser location type) (string?)
   "Set the current cue clef to @var{type}. Replaces standard cueClef."
   (make-cue-clef-set (cn-clef-transposition type)))


%% CLEFS: CLEFS FOR StaffTrad

% see /ly/music-functions-init.ly

clefsTrad =
#(define-music-function (parser location mus) (ly:music?)
   "Takes music with Clairnote clef settings, and returns music
    with clef settings adjusted for use on a traditional staff."
   (let ((current-glyph ""))
     (music-map
      (lambda (m)
        (let ((sym (ly:music-property m 'symbol)))
          (cond

           ((or (eq? 'clefGlyph sym)
                (eq? 'cueClefGlyph sym))
            (set! current-glyph (ly:music-property m 'value)))

           ((or (eq? 'clefPosition sym)
                (eq? 'cueClefPosition sym))
            (ly:music-set-property! m 'value
              (* 2/5 (ly:music-property m 'value))))

           ((or (eq? 'middleCClefPosition sym)
                (eq? 'middleCCuePosition sym))
            (ly:music-set-property! m 'value
              (+ (round (* 7/12 (ly:music-property m 'value)))
                (cond
                 ((equal? current-glyph "clefs.G") 1)
                 ((equal? current-glyph "clefs.F") -1)
                 (else 0))))) ;; includes "clefs.C"

           ((or (eq? 'clefTransposition sym)
                (eq? 'cueClefTransposition sym))
            (ly:music-set-property! m 'value
              (round (* 7/12 (ly:music-property m 'value)))))))
        m)
      mus)))

%{
   actual expected values and conversions for clefsTrad function
   clefPosition
   ((-5) -2) ;; treble
   ((5) 2) ;; bass
   ;; ((0) 0) ;; alto - no change
   ;; ((0) 2) ;; tenor - conflicts with alto clef
   middleCClefPosition / clefs.G
   ((-12) -6) ;; treble ;; -4 + -2
   ((-24) -13) ;; treble^8
   ((-36) -20) ;; treble^15
   ((0) 1) ;; treble_8
   ((12) 8) ;; treble_15
   middleCClefPosition / clefs.F
   ((12) 6) ;; bass ;; 4 + 2
   ((24) 13) ;; bass_8
   ((36) 20) ;; bass_15
   ((0) -1) ;; bass^8
   ((-12) -8) ;; bass^15
   middleCClefPosition / clefs.C
   ;; ((0) 0) ;; alto - no change
   ((-12) -7) ;; alto^8
   ((12) 7) ;; alto_8
   ((-24) -14) ;; alto^15
   ((24) 14) ;; alto_15
   clefTransposition
   ((12) 7) ;; ^8
   ((-12) -7) ;; _8
   ((24) 14) ;; ^15
   ((-24) -14) ;; _15
%}


%% REPEAT SIGN DOTS (BAR LINES)

% adjust the position of dots in repeat signs
% for Clairnote staff or traditional staff

#(define ((cn-make-repeat-dot-bar dot-positions) grob extent)
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

#(define (cn-repeat-dot-bar-procedure grob extent)
   "Return a procedure for repeat sign dots based on a custom grob
    property: StaffSymbol.cn-is-clairnote-staff."
   (if (ly:grob-property (ly:grob-object grob 'staff-symbol) 'cn-is-clairnote-staff '())
       ;; Clairnote staff or Traditional five line staff
       ((cn-make-repeat-dot-bar '(-2 2)) grob extent)
       ((cn-make-repeat-dot-bar '(-1 1)) grob extent)))

#(add-bar-glyph-print-procedure ":" cn-repeat-dot-bar-procedure)


%% USER STAFF SCALING FUNCTION

staffSize =
#(define-music-function (parser location new-size) (number?)
   "Helper macro that lets the user zoom a single staff size."
   ;; TODO: this does not work perfectly combined with vertScaleStaff
   ;; see especially key signatures and time signatures
   #{
     \set fontSize = #new-size
     \override StaffSymbol.thickness = #(magstep new-size)
     \override StaffSymbol.staff-space =
     #(lambda (grob)
        (* 7/12 (magstep new-size)
          (ly:grob-property grob 'cn-vscale-staff)))
   #})


%% CUSTOM GROB PROPERTIES

% function from "scm/define-grob-properties.scm" (modified)
#(define (define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% StaffSymbol.cn-is-clairnote-staff is set to tell whether
% a staff is a Clairnote staff or not. Used for repeat sign dots.
#(define-grob-property 'cn-is-clairnote-staff boolean?)

% StaffSymbol.cn-vscale-staff stores the vertical scaling factor of the
% current staff.  Used for key signatures and staffSize user function.
#(define-grob-property 'cn-vscale-staff number?)

% StaffSymbol.cn-note-head-stencils stores custom note head
% stencils, used to override default note head stencils
#(define-grob-property 'cn-note-head-stencils procedure?)


%% CUSTOM CONTEXT PROPERTIES

% function from "scm/define-context-properties.scm" (modified)
#(define (translator-property-description symbol type?)
   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc "custom context property")
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)

% Store key signature stencils for each staff context.
#(translator-property-description 'cn-key-stils list?)

% Used by accidental engraver to tell when a new measure occurs
% and to track accidentals in the current measure.
#(translator-property-description 'cn-bar-num number?)
#(translator-property-description 'cn-acc-list list?)


%% VERTICAL (CLAIRNOTE) STAFF COMPRESSION

vertScaleStaff =
#(define-music-function (parser location vscale-staff) (number?)
   "Change the vertical distance between the staff lines (staff-space)
    and everything else to match."
   ;; vscale-staff = 1 gives a staff with an octave that is the same size
   ;; as on a traditional staff (default is 1.2).
   ;;  - stems are extended back to their original/traditional size
   ;;  - beam size and spacing is restored to original size
   ;;  - time signature position is adjusted vertically
   ;;  - elsewhere key signatures are adjusted to fit the staff
   (let* ((staff-spc (* vscale-staff 7/12))
          (staff-spc-inv (/ 1 staff-spc)))
     #{
       \override StaffSymbol.staff-space = #staff-spc
       \override Stem.before-line-breaking = #(cn-stems staff-spc-inv)
       \override Beam.before-line-breaking = #(cn-beams staff-spc-inv)
       \override TimeSignature.before-line-breaking =
       #(lambda (grob)
          (ly:grob-set-property! grob 'Y-offset
            ;; ((((foo - 1.2) * -3.45) - 1.95) + foo)
            (+ (- (* (- vscale-staff 1.2) -3.45 ) 1.95) vscale-staff)))

       % StaffSymbol.cn-vscale-staff is a custom grob property that
       % stores the vertical staff scaling value so it can be used with
       % key signatures and staffSize function
       \override StaffSymbol.cn-vscale-staff = #vscale-staff
     #}))


%% STAFF DEFINITIONS

\layout {
  % copy \Staff context with its standard settings to
  % a custom staff context called \StaffTrad
  \context {
    \Staff
    \name StaffTrad
    \alias Staff
    % custom grob property
    \override StaffSymbol.cn-is-clairnote-staff = ##f
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
    % custom grob properties
    \override StaffSymbol.cn-is-clairnote-staff = ##t
    \override StaffSymbol.cn-note-head-stencils = #cn-draw-note-head-stencils

    \override Stem.no-stem-extend = ##t

    \vertScaleStaff 1.2
    \override NoteHead.before-line-breaking = #cn-note-heads

    printKeyCancellation = ##f
    \consists \Cn_key_signature_engraver
    % custom context property to store key sig stencils (associative list)
    cn-key-stils = #'()

    \consists \Cn_accidental_engraver
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()
    % custom context properties to track accidental signs in the
    % current measure, used by accidental engraver
    cn-bar-num = #0
    cn-acc-list = #'()

    \override NoteColumn.before-line-breaking = #cn-chords-one

    \override LedgerLineSpanner.length-fraction = 0.45
    \override LedgerLineSpanner.minimum-length-fraction = 0.35
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
