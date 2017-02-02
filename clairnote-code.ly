%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20150324
%
%    Copyright © 2013, 2014, 2015 Paul Morris, except for:
%    A. functions copied and modified from LilyPond source code:
%    define-grob-property
%    B. functions in the public domain:
%    cn-shift-notehead, copied and edited from the LilyPond Snippet Repository,
%    snippet 861, "Re-positioning note heads on the opposite side of the stem"
%    http://lsr.di.unimi.it/LSR/Item?id=861
%    Thanks to David Nalesnik and Thomas Morley for this snippet.
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


% UTILITY FUNCTIONS

#(define (cn-notehead-pitch grob)
   "Takes a note head grob and returns its pitch."
   (ly:event-property (ly:grob-property grob 'cause) 'pitch))

#(define (cn-notehead-semitone grob)
   "Takes a note head grob and returns its semitone."
   (ly:pitch-semitones (cn-notehead-pitch grob)))

#(define (cn-staff-symbol-prop grob prop)
   "Gets custom StaffSymbol props. Takes a grob and
    a property name and returns that StaffSymbol property."
   (ly:grob-property (ly:grob-object grob 'staff-symbol) prop))


%% NOTE HEADS AND STEM ATTACHMENT

#(define (cn-draw-note-head-stencils note-type font)
   "Returns a custom note head stencil."
   (case note-type
     ;; black note
     ((0) (ly:font-get-glyph font "noteheads.s2"))
     ;; white note, scale horizontally to match black ones
     ((1) (ly:stencil-scale (ly:font-get-glyph font "noteheads.s1") 0.945 1))
     ;; black whole note, add black circle to make solid
     ((2) (ly:stencil-add (ly:font-get-glyph font "noteheads.s0")
            (ly:stencil-translate
             (make-circle-stencil 0.47 0.1 #t)
             '(0.95 . 0))))
     ;; white whole note, thicken top and bottom using an oval
     ;; path so no white space shows above and below staff lines
     ((3) (ly:stencil-add (ly:font-get-glyph font "noteheads.s0")
            (ly:stencil-translate
             (make-oval-stencil 0.7 0.58 0.11 #f)
             '(0.98 . 0))))))

#(define Cn_note_heads_engraver
   ;; Sets custom notehead stencils, stem-attachment, rotation.
   (make-engraver
    (acknowledgers
     ((note-head-interface engraver grob source-engraver)
      ;; make sure \omit is not in effect (i.e. stencil is not #f)
      ;; and do nothing for certain notehead styles
      ;; TODO: better handling of various notehead styles
      ;; http://lilypond.org/doc/v2.18/Documentation/notation/note-head-styles
      ;; output-lib.scm
      (if
       (and
        (ly:grob-property-data grob 'stencil)
        (not (memq (ly:grob-property-data grob 'style)
               (list 'harmonic 'harmonic-black 'harmonic-mixed
                 'diamond 'cross 'xcircle 'triangle 'slash))))
       (let ((whole-note (< (ly:grob-property grob 'duration-log) 1))
             ;; 0 = black note, 1 = white note
             (note-type (modulo (cn-notehead-semitone grob) 2)))
         (if whole-note
             ;; adjust note-type: 2 = black whole-note, 3 = white whole-note
             (set! note-type (+ 2 note-type))
             ;; else set rotation and stem attachment properties
             (begin
              ;; black notes can be rotated to -27, but -18 also works for white notes
              ;; currently -9, half of -18
              (ly:grob-set-property! grob 'rotation '(-9 0 0))
              (ly:grob-set-property! grob 'stem-attachment
                (if (equal? 0 note-type)
                    (cons 1.04 0.3) ;; black note (0)
                    (cons 1.06  0.3))))) ;; white note (1)
         ;; replace note head stencil
         (ly:grob-set-property! grob 'stencil
           (cn-draw-note-head-stencils note-type (ly:grob-default-font grob)))))))))


% DOTS ON DOTTED NOTES

#(define (cn-note-dots grob)
   "Adjust vertical position of dots for certain notes."
   (let* ((parent (ly:grob-parent grob Y))
          ;; parent is a Rest grob or a NoteHead grob
          (semi (if (grob::has-interface parent 'rest-interface)
                    #f
                    (modulo (cn-notehead-semitone parent) 12))))
     (cond
      ((equal? semi 0)
       (ly:grob-set-property! grob 'staff-position
         (if (equal? -1 (ly:grob-property grob 'direction))
             -1 ;; down
             1))) ;; up or neutral
      ((member semi (list 2 6 10))
       (ly:grob-set-property! grob 'Y-offset -0.36)))))


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

#(define (cn-redo-acc-signs grob alt)
   "Replaces the accidental sign stencil and resizes X/Y extents."
   ;; TODO: should natural signs have the same Y-extent as others?
   ;; TODO: shouldn't X/Y-extent scale with mult / font-size?
   (ly:grob-set-property! grob 'Y-extent (cons -0.5 1.2))
   (case alt
     ((-1/2) (cn-redo-acc grob cn-flat-sign (cons 0 0.54)))
     ((1/2) (cn-redo-acc grob cn-sharp-sign (cons -0.27 0.27)))
     ((-1) (cn-redo-acc grob cn-double-flat-sign (cons -0.34 0.67)))
     ((1) (cn-redo-acc grob cn-double-sharp-sign (cons -0.54 0.47)))
     ((0) (cn-redo-acc grob
            (ly:stencil-scale (ly:grob-property grob 'stencil) 0.65 0.65)
            (cons -0.0 (* 2/3 0.65))))
     (else (ly:grob-property grob 'stencil))))

#(define (pitch-in-key pitch key-sig)
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

#(define Cn_accidental_engraver
   ;; using a closure for persistent barnum and acclist
   (lambda (context)
     (let ((barnum 0)
           (acclist '()))
       (make-engraver
        (acknowledgers
         ((accidental-interface engraver grob source-engraver)
          (let ((current-barnum (ly:context-property context 'currentBarNumber)))
            ;; another option: (ly:context-property context 'internalBarNumber)
            ;; if we're in a new bar, clear acclist and set barnum
            (if (not (equal? barnum current-barnum))
                (begin
                 (set! acclist '())
                 (set! barnum current-barnum)))
            (let*
             ((alt (accidental-interface::calc-alteration grob))
              (stl (ly:grob-property-data grob 'stencil))
              (note-head (ly:grob-parent grob Y))
              (pitch (cn-notehead-pitch note-head))
              (semi (ly:pitch-semitones pitch))
              (key-sig (ly:context-property context 'keySignature))
              (in-the-key (pitch-in-key pitch key-sig))
              (in-acclist (equal? (cons semi alt) (assoc semi acclist)))
              (semi-in-acclist (equal? alt (assoc-ref acclist semi))))
             ;;Show an acc sign? Yes: 1,2,3, No: 4,5,6
             (cond
              ;; 1. new acc: an acc not in the acclist
              ;; add to acclist (any previous alt for that semi is replaced)
              ((and (not in-the-key) (not in-acclist) stl)
               (cn-redo-acc-signs grob alt)
               (set! acclist (assoc-set! acclist semi alt)))
              ;; 2. cancel acc: in key, cancels a previous acc in acc list
              ;; (i.e. semi is in acclist but the alt does not match)
              ;; remove acc from acclist
              ((and in-the-key (not in-acclist) semi-in-acclist stl)
               (cn-redo-acc-signs grob alt)
               (set! acclist (assoc-remove! acclist semi)))
              ;; 3. forced acc: acc wouldn't be shown, but it was forced with !
              ;; no change to acclist
              ((and (equal? #t (ly:grob-property grob 'forced)) stl)
               (cn-redo-acc-signs grob alt))
              ;; 4. is an acc but not a new one in this measure
              ;; 5. is not an acc and is not cancelling previous acc
              ;; 6. omit is in effect (stencil is #f)"
              ;; TODO: does this affect ledger line widths?
              (else (ly:grob-suicide! grob)))))))))))


%% KEY SIGNATURES

#(define (cn-get-keysig-alt-count alt-alist)
   "Return number of sharps/flats in key sig, (+) for sharps, (-) for flats."
   (if (null? alt-alist)
       0
       (* (length alt-alist) 2 (cdr (car alt-alist)))))

#(define (cn-get-major-tonic alt-count)
   "Return number of the tonic note 0-6, as if the key sig were major."
   ;; (alt-count maj-num)
   ;; (-7 0) (-5 1) (-3 2) (-1 3) (1 4) (3 5) (5 6) (7 0)
   ;; (-6 4) (-4 5) (-2 6) (0 0) (2 1) (4 2) (6 3)
   (if (odd? alt-count)
       (modulo (- (/ (+ alt-count 1) 2) 4) 7)
       (modulo (/ alt-count 2) 7)))

#(define (cn-get-note-space vscale-staff)
   "Return the distance between two adjacent notes given vertical staff
    scaling factor from StaffSymbol.cn-vscale-staff."
   ;; (foo - (((foo - 1.2) * 0.7) + 0.85))
   (- vscale-staff (+ (* (- vscale-staff 1.2) 0.7) 0.85)))

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

#(define (cn-draw-keysig grob tonic-num)
   "Draws Clairnote key signature stencils."
   (let*
    ((alt-count (cn-get-keysig-alt-count (ly:grob-property grob 'alteration-alist)))
     (major-tonic-num (cn-get-major-tonic alt-count))
     ;; number of the mode (0-6)
     (mode (modulo (- tonic-num major-tonic-num) 7))
     (note-space (cn-get-note-space
                  (cn-staff-symbol-prop grob 'cn-vscale-staff)))
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

#(define Cn_key_signature_engraver
   ;; Clairnote's staff definition has printKeyCancellation = ##f, which
   ;; prevents key cancellations, except for changing to C major
   ;; or A minor, so this engraver prevents all key cancellations.
   ;; Spare parts: (ly:context-property context 'printKeyCancellation)
   (make-engraver
    (acknowledgers
     ((key-signature-interface engraver grob source-engraver)
      (cond
       ;; key cancellation?
       ((grob::has-interface grob 'key-cancellation-interface)
        (ly:grob-set-property! grob 'stencil #f))
       ;; omitted?
       ((equal? #f (ly:grob-property-data grob 'stencil)) #f)
       ;; else set grob stencil
       (else
        (let*
         ((context (ly:translator-context engraver))
          ;; number of the tonic note (0-6) (C-B) 'tonic (pitch) --> tonic-num
          (tonic-num (ly:pitch-notename (ly:context-property context 'tonic)))
          (mult (magstep (ly:grob-property grob 'font-size 0.0))))
         (ly:grob-set-property! grob 'stencil
           (ly:stencil-scale
            (cn-draw-keysig grob tonic-num)
            mult mult)))))))))


%% CHORDS

% Credit goes to David Nalesnik and Thomas Morley for much of
% cn-shift-notehead, copied and edited from the LilyPond Snippet
% Repository, snippet 861, "Re-positioning note heads on the
% opposite side of the stem," http://lsr.di.unimi.it/LSR/Item?id=861
% Use that snippet for any manual adjustments of note head positions.

#(define (cn-shift-notehead note-head nh-dir stem-dir)
   "Shift a note head to the left or right."
   (let*
    ((dur-log (ly:grob-property note-head 'duration-log))
     (stil (ly:grob-property note-head 'stencil))
     (stil-x-length (interval-length (ly:stencil-extent stil X)))
     (stem (ly:grob-object note-head 'stem))
     (stem-thick (ly:grob-property stem 'thickness 1.3))
     (stem-x-width (/ stem-thick 10))
     ;; (stem-dir (ly:grob-property stem 'direction))

     ;; stencil width method, doesn't work with non-default beams
     ;; so using thickness property above instead
     ;; (stem-stil (ly:grob-property stem 'stencil))
     ;; (stem-x-width (if (ly:stencil? stem-stil)
     ;;                 (interval-length (ly:stencil-extent stem-stil X))
     ;;                 ;; if no stem-stencil use 'thickness-property
     ;;                 (/ stem-thick 10)))

     ;; Calculate a value to compensate the stem-extension
     (stem-x-corr
      ;; TODO: better coding if (<= log 0)
      (cond
       ;; original, doesn't work (whole notes)
       ;; ((and (= q 0) (= stem-dir 1))
       ;;      (* -1 (+ 2  (* -4 stem-x-width))))
       ;; new, quick fix, could be better?
       ((= dur-log 0) 0.223)

       ((and (< dur-log 0) (= stem-dir 1))
        (* -1 (+ 2  (* -1 stem-x-width))))
       ((< dur-log 0)
        (* 2 stem-x-width))
       (else (/ stem-x-width 2)))))
    ;; final calculation for moving the note head
    (ly:grob-translate-axis! note-head (* nh-dir (- stil-x-length stem-x-corr)) X)))

#(define (cn-chords-recurse note-heads last-semi parity stem-dir note-col)
   "Use recursion to iterate through the note heads, shifting them as needed."
   (if (> (length note-heads) 0)
       (let* ((note-head (car note-heads))
              (semi (cn-notehead-semitone note-head))
              (interval (abs (- semi last-semi))))

         (if (> interval 2)
             ;; on to the next note head
             (cn-chords-recurse (cdr note-heads) semi #t stem-dir note-col)
             ;; else maybe shift this note head
             (let*
              ((nh-dir (if parity stem-dir (* -1 stem-dir)))
               ;; check the old/default position to see if the note head needs moving
               ;; use round to avoid floating point number errors
               (pos (round (ly:grob-relative-coordinate note-head note-col X)))
               (old-nh-dir
                (if (= stem-dir 1)
                    ;; stem up (stem-dir is 1)
                    (if (= pos 0) -1 1) ;; -1 is left (pos is 0), 1 is right (pos is positive)
                    ;; stem down (stem-dir is -1)
                    (if (= pos 0) 1 -1)))) ;; 1 is right (pos is 0), -1 is left (pos is negative)

              (if (not (= nh-dir old-nh-dir))
                  (cn-shift-notehead note-head nh-dir stem-dir))

              (cn-chords-recurse (cdr note-heads) semi (not parity) stem-dir note-col))))))

#(define (cn-chords note-col)
   "For notes in chords or harmonic intervals that are 2 semitones
    apart or less, automatically position them on opposite sides of the stem.
    (See Stem::calc_positioning_done in LilyPond source code lily/stem.cc)"
   (let* ((heads-array (ly:grob-object note-col 'note-heads))
          (note-heads-raw
           (if (ly:grob-array? heads-array)
               (ly:grob-array->list heads-array)
               ;; rests, no note heads in NoteColumn grob
               (list 0))))
     (display note-heads-raw)(newline)
     ;; rests and single notes don't need shifting
     (if (> (length note-heads-raw) 1)
         (let*
          ((note-heads-sorted
            (sort-list note-heads-raw
              (lambda (a b)
                (< (cn-notehead-semitone a)
                   (cn-notehead-semitone b)))))
           ;; stem direction, 1 is up, -1 is down
           (stem-dir (ly:grob-property (ly:grob-object note-col 'stem) 'direction))
           ;; if stem is down then reverse the order
           (note-heads
            (if (< stem-dir 0)
                (reverse note-heads-sorted)
                note-heads-sorted))
           (first-semi (cn-notehead-semitone (car note-heads))))

          ;; start with cdr of note-heads because the first one never needs shifting
          (cn-chords-recurse (cdr note-heads) first-semi #t stem-dir note-col)))))


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
   (if (cn-staff-symbol-prop grob 'cn-is-clairnote-staff)
       ;; Clairnote staff or Traditional five line staff
       ((cn-make-repeat-dot-bar '(-2 2)) grob extent)
       ((cn-make-repeat-dot-bar '(-1 1)) grob extent)))

#(add-bar-glyph-print-procedure ":" cn-repeat-dot-bar-procedure)


%% STAFF SPACE

#(define (cn-staff-space grob)
   "Scale staff space based on StaffSymbol.cn-vscale-staff."
   (* 7/12 (ly:grob-property grob 'cn-vscale-staff)))


%% TIME SIGNATURES

#(define (cn-timesigs grob)
   "Adjust vertical position of time sig based on vertical staff scaling."
   (define vss (cn-staff-symbol-prop grob 'cn-vscale-staff))
   (ly:grob-set-property! grob 'Y-offset
     ;; ((((vss - 1.2) * -3.45) - 1.95) + vss)
     (+ (- (* (- vss 1.2) -3.45 ) 1.95) vss)))


%% STEM LENGTH AND DOUBLE STEMS

#(define (cn-stems grob)
   "Lengthen all stems and give half notes double stems."
   ;; make sure \omit is not in effect (i.e. stencil is not #f)
   (if (ly:grob-property-data grob 'stencil)
       (let ((staff-space-inv (/ 1 (ly:staff-symbol-staff-space grob))))
         ;; multiply each of the values in the details property of the stem grob
         ;; by the inverse of staff-space, except for stem-shorten values
         (ly:grob-set-property! grob 'details
           (map
            (lambda (detail)
              (let ((head (car detail))
                    (args (cdr detail)))
                (if (eq? head 'stem-shorten)
                    (cons head args)
                    (cons head
                      (map
                       (lambda (arg) (* arg staff-space-inv))
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

#(define (cn-beams grob)
   "Adjust size and spacing of beams, needed due to smaller
    StaffSymbol.staff-space"
   (let*
    ((staff-space-inv (/ 1 (ly:staff-symbol-staff-space grob)))
     (thick (ly:grob-property-data grob 'beam-thickness))
     (len-frac (ly:grob-property-data grob 'length-fraction))
     (space (if (number? len-frac) len-frac 1)))
    (ly:grob-set-property! grob 'beam-thickness
      (* thick staff-space-inv))
    ;; 1.1 adjustment below was visually estimated
    (ly:grob-set-property! grob 'length-fraction
      (* space 1.1 staff-space-inv))))


%% USER VERTICAL STAFF COMPRESSION FUNCTION

vertScaleStaff =
#(define-music-function (parser location n) (number?)
   #{
     \override StaffSymbol.cn-vscale-staff = #n
   #})


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
        (* 7/12 (magstep new-size) (ly:grob-property grob 'cn-vscale-staff)))
   #})


%% USER STAFF EXTENSION FUNCTIONS

cn-extend-staff =
#(lambda (upwards)
   ;; upwards is a boolean
   (lambda (grob)
     (let*
      ((positions (sort (ly:grob-property grob 'line-positions) <))
       (furthest (if upwards
                     (last positions)
                     (first positions)))
       (new-positions
        (if upwards
            ;; extendStaffUp
            (append positions (list (+ 8 furthest) (+ 12 furthest)))
            ;; extendStaffDown
            (append (list (+ -12 furthest) (+ -8 furthest)) positions))))
      (ly:grob-set-property! grob 'line-positions new-positions))))

extendStaffUp = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #t)
}

extendStaffDown = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #f)
}

unextendStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #'()
}


%% USER SHORTCUTS FOR DIFFERENT STAFF CONFIGURATIONS

oneOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.line-positions = #'(-8 -4)
}

twoOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.line-positions = #'(-8 -4 4 8)
}

threeOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.line-positions = #'(-20 -16 -8 -4 4 8)
}

fourOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.line-positions = #'(-20 -16 -8 -4 4 8 16 20)
}


%% CUSTOM GROB PROPERTIES

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% StaffSymbol.cn-is-clairnote-staff is used for repeat sign dots.
#(cn-define-grob-property 'cn-is-clairnote-staff boolean?)

% StaffSymbol.cn-vscale-staff stores the vertical scaling factor for the staff.
#(cn-define-grob-property 'cn-vscale-staff number?)


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
    \override StaffSymbol.line-positions = #'(-8 -4 4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)
    \override StaffSymbol.ledger-extra = 1

    % custom grob properties
    \override StaffSymbol.cn-is-clairnote-staff = ##t
    \override StaffSymbol.cn-vscale-staff = #1.2
    % StaffSymbol.cn-vscale-staff stores the vertical staff scaling value.
    % 1.2 is default, 1 gives a staff with same size octave as traditional.
    % These depend on it:
    % - staff-space, the vertical distance between the staff lines
    % - time signature
    % - key signatures
    % - staffSize function
    % must be kept in this separate property from staff-space because
    % we need to zoom the staff-space using staffSize without losing the
    % vertical scaling number
    \override StaffSymbol.staff-space = #cn-staff-space

    \override TimeSignature.before-line-breaking = #cn-timesigs
    % stems, beams restored to original/traditional size, via staff-space
    \override Stem.before-line-breaking = #cn-stems
    \override Beam.before-line-breaking = #cn-beams

    \consists \Cn_note_heads_engraver
    \override Dots.before-line-breaking = #cn-note-dots
    \override Stem.no-stem-extend = ##t

    \consists \Cn_key_signature_engraver
    printKeyCancellation = ##f

    \consists \Cn_accidental_engraver
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()

    \override NoteColumn.before-line-breaking = #cn-chords

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
