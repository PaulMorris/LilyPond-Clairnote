%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20150406
%
%    Copyright © 2013, 2014, 2015 Paul Morris, except for functions copied
%    and modified from LilyPond source code or from the LilyPond Snippet
%    Repository, as noted in comments below.
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

#(define-public (grob::name grob)
   "Return the name of the grob @var{grob} as a symbol.
    TODO: Delete this after LilyPond 2.20 arrives."
   (assq-ref (ly:grob-property grob 'meta) 'name))


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
   (let*
    ((acc (cond
           ((> alt-count 0) cn-sharp-sign)
           ((< alt-count 0) cn-flat-sign)
           ((= alt-count 0) (ly:stencil-scale
                             (grob-interpret-markup grob (markup #:natural))
                             0.65 0.65))))
     ;; expected staff-space given the current value of cn-vscale-staff
     (expected-staff-space (* 7/12 (cn-staff-symbol-prop grob 'cn-vscale-staff)))
     ;; actual staff-space currently in effect, as modified by \magnifyStaff, etc.
     (actual-staff-space (cn-staff-symbol-prop grob 'staff-space))
     (scale (/ 0.6 (/ actual-staff-space expected-staff-space)))
     (num (ly:stencil-scale
           (grob-interpret-markup grob
             (markup (number->string (abs alt-count))))
           scale scale)))
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

% cn-shift-notehead, copied and modified from the LilyPond Snippet
% Repository, snippet 861, "Re-positioning note heads on the
% opposite side of the stem," http://lsr.di.unimi.it/LSR/Item?id=861
% Thanks to David Nalesnik and Thomas Morley for work on that snippet.
% Use that snippet for any manual adjustments of note head positions.

#(define (cn-shift-notehead nh nh-dir stem-dir)
   "Shift a note head to the left or right."
   (let*
    ((dur-log (ly:grob-property nh 'duration-log))
     (stil (ly:grob-property nh 'stencil))
     (stil-x-length (interval-length (ly:stencil-extent stil X)))
     (stem (ly:grob-object nh 'stem))
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
    (ly:grob-translate-axis! nh (* nh-dir (- stil-x-length stem-x-corr)) X)))

#(define (cn-chords-loop note-heads first-semi stem-dir note-col)
   "Use recursion to iterate through the note heads, shifting them as needed.
    Use internal procedure (loop) since we need stem-dir and note-col in scope,
    and don't want to pass them as arguments."
   (define (loop nhs last-semi parity)
     (if (> (length nhs) 0)
         (let* ((nh (car nhs))
                (semi (cn-notehead-semitone nh))
                (interval (abs (- semi last-semi))))

           (if (> interval 2)
               ;; on to the next note head
               (loop (cdr nhs) semi #t)

               ;; else maybe shift this note head
               (let*
                ((nh-dir (if parity stem-dir (* -1 stem-dir)))
                 ;; check the old/default position to see if the note head needs moving
                 ;; use round to avoid floating point number errors
                 (pos (round (ly:grob-relative-coordinate nh note-col X)))
                 (old-nh-dir
                  (if (> stem-dir 0)
                      ;; stem up (stem-dir is 1)
                      (if (= pos 0) -1 1) ;; -1 is left (pos is 0), 1 is right (pos is positive)
                      ;; stem down (stem-dir is -1)
                      (if (= pos 0) 1 -1)))) ;; 1 is right (pos is 0), -1 is left (pos is negative)

                (if (not (= nh-dir old-nh-dir))
                    (cn-shift-notehead nh nh-dir stem-dir))
                (loop (cdr nhs) semi (not parity)))))))

   ;; start the loop
   (loop note-heads first-semi #t))

#(define (cn-chords note-col)
   "For notes in chords or harmonic intervals that are 2 semitones
    apart or less, automatically position them on opposite sides of the stem.
    (See Stem::calc_positioning_done in LilyPond source code lily/stem.cc)"
   (let* ((heads-array (ly:grob-object note-col 'note-heads))
          (nhs-raw
           (if (ly:grob-array? heads-array)
               (ly:grob-array->list heads-array)
               ;; rests, no note heads in NoteColumn grob
               (list 0))))
     ;; rests and single notes don't need shifting
     (if (> (length nhs-raw) 1)
         (let*
          ((nhs-sorted
            (sort-list nhs-raw
              (lambda (a b)
                (< (cn-notehead-semitone a)
                   (cn-notehead-semitone b)))))
           ;; stem direction, 1 is up, -1 is down
           (stem-dir (ly:grob-property (ly:grob-object note-col 'stem) 'direction))
           ;; if stem is down then reverse the order
           (nhs-cooked
            (if (< stem-dir 0)
                (reverse nhs-sorted)
                nhs-sorted))
           (first-semi (cn-notehead-semitone (car nhs-cooked))))

          ;; Start with (cdr nhs-cooked). The first nh never needs shifting.
          (cn-chords-loop (cdr nhs-cooked) first-semi stem-dir note-col)))))


%% CLEFS: CLEF SETTINGS

% see /scm/parser-clef.scm
% To calculate the clairnote middle c position subtract the clef position
% from 12 for bass clef or -12 for treble clef (to adjust the clef position
% without affecting the position of middle c or other notes)

#(begin
  (add-new-clef "treble" "clefs.G" -5 0 -7) ;; -7 = -12 minus -5
  (add-new-clef "G" "clefs.G" -5 0 -7) ;; treble synonym
  (add-new-clef "G2" "clefs.G" -5 0 -7) ;; treble synonym
  (add-new-clef "violin" "clefs.G" -5 0 -7) ;; treble synonym
  (add-new-clef "bass" "clefs.F" 5 0 7) ;; 7 = 12 minus 5
  (add-new-clef "F" "clefs.F" 5 0 7) ;; bass synonym
  (add-new-clef "tenor" "clefs.C" 0 0 0) ;; => alto
  (add-new-clef "french" "clefs.G" -5 0 -7) ;; => treble
  (add-new-clef "soprano" "clefs.G" -5 0 -7) ;; => treble
  (add-new-clef "mezzosoprano" "clefs.C" 0 0 0) ;; => alto
  (add-new-clef "baritone" "clefs.F" 5 0 7) ;; => bass
  (add-new-clef "varbaritone" "clefs.F" 5 0 7) ;; => bass
  (add-new-clef "subbass" "clefs.F" 5 0 7)) % => bass

% No changes needed for these clefs, no need to add them:
% (add-new-clef "alto" "clefs.C" 0 0 0)
% (add-new-clef "C" "clefs.C" 0 0 0)
% (add-new-clef "percussion" "clefs.percussion" 0 0 0)

% TODO: add these clefs that come with LilyPond 2.20
% need to decide 5th argument and adjust others
% (add-new-clef "GG" "clefs.GG" -2 0)
% (add-new-clef "tenorG" "clefs.tenorG" -2 0)
% (add-new-clef "varC" "clefs.varC" 0 0)
% (add-new-clef "altovarC" "clefs.varC" 0 0)
% (add-new-clef "tenorvarC" "clefs.varC" 2 0)
% (add-new-clef "baritonevarC" "clefs.varC" 4 0)
% (add-new-clef "baritonevarF" "clefs.F" 0 0)
% (add-new-clef "varpercussion" "clefs.varpercussion" 0 0)
% (add-new-clef "tab" "clefs.tab" 0 0)

% TODO: add mensural clefs?


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


%% TIME SIGNATURES

#(define (cn-timesigs grob)
   "Adjust vertical position of time sig based on vertical staff scaling."
   (let*
    ((vscale-staff (cn-staff-symbol-prop grob 'cn-vscale-staff))
     ;; ((((vss - 1.2) * -3.45) - 1.95) + vss)
     (basic-y-offset (+ (- (* (- vscale-staff 1.2) -3.45 ) 1.95) vscale-staff))
     ;; expected staff-space given the current value of cn-vscale-staff
     (expected-staff-space (* 7/12 vscale-staff))
     ;; actual staff-space currently in effect, as modified by \magnifyStaff, etc.
     (actual-staff-space (cn-staff-symbol-prop grob 'staff-space))
     ;; adjustment for \magnifyStaff
     (adjustment (/ actual-staff-space expected-staff-space)))
    (ly:grob-set-property! grob 'Y-offset
      (* basic-y-offset adjustment))))


%% STEM LENGTH AND DOUBLE STEMS

#(define (cn-stems grob)
   "Lengthen all stems and give half notes double stems."
   ;; make sure \omit is not in effect (i.e. stencil is not #f)
   (if (ly:grob-property-data grob 'stencil)
       (let ((staff-space-inverse
              (/ 1 (* 7/12 (cn-staff-symbol-prop grob 'cn-vscale-staff)))))
         ;; default staff-space-inverse is 1/0.7 = 1.42857714286...
         ;; 0.7 is default staff-space (* 7/12 1.2)
         ;; 1.2 is default cn-vscale-staff
         ;; multiply each of the values in the details property of the stem grob
         ;; by the staff-space-inverse, except for stem-shorten values
         (ly:grob-set-property! grob 'details
           (map
            (lambda (detail)
              (let ((head (car detail))
                    (args (cdr detail)))
                (if (eq? head 'stem-shorten)
                    (cons head args)
                    (cons head
                      (map
                       (lambda (arg) (* arg staff-space-inverse))
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
   "Adjust size and spacing of beams, needed due to vertically scaled staff."
   (let*
    ;; expected staff space based on cn-vscale-staff
    ((expected-staff-space
      (* 7/12 (cn-staff-symbol-prop grob 'cn-vscale-staff)))
     (staff-space-inverse (/ 1 expected-staff-space))
     (thick (ly:grob-property-data grob 'beam-thickness))
     (len-frac (ly:grob-property-data grob 'length-fraction))
     (space (if (number? len-frac) len-frac 1)))

    (ly:grob-set-property! grob 'beam-thickness
      (* thick staff-space-inverse))
    ;; TODO: the 1.1 adjustment below was just visually estimated
    (ly:grob-set-property! grob 'length-fraction
      (* space 1.1 staff-space-inverse))))


%% USER VERTICAL STAFF COMPRESSION FUNCTION

% must be used before \magnifyStaff in a staff's \with block
vertScaleStaff =
#(define-music-function (parser location n) (number?)
   #{
     \override StaffSymbol.cn-vscale-staff = #n
     \override StaffSymbol.staff-space = #(* 7/12 n)
   #})


%% USER STAFF EXTENSION FUNCTIONS

cn-extend-staff =
#(lambda (upwards extend)
   ;; upwards and extend are booleans
   (lambda (grob)
     (let*
      ((vertical-axis-group (ly:grob-parent grob Y))
       (positions (sort (ly:grob-property vertical-axis-group 'cn-staff-lines) <))
       (furthest (if upwards
                     (last positions)
                     (first positions)))
       (new-positions
        (if extend
            ;; extend
            (if upwards
                ;; extendStaffUp
                (append positions (list (+ 8 furthest) (+ 12 furthest)))
                ;; extendStaffDown
                (append (list (+ -12 furthest) (+ -8 furthest)) positions))
            ;; unextend
            (if (> (length positions) 2)
                (if upwards
                    ;; unextendStaffUp
                    (drop-right positions 2)
                    ;; unextendStaffDown
                    (drop positions 2))
                positions))))
      (if (not (eq? (grob::name vertical-axis-group) 'VerticalAxisGroup))
          (ly:warning "clairnote-code.ly cannot find VerticalAxisGroup"))
      ;; store current positions in custom property VerticalAxisGroup.cn-staff-lines
      ;; so that they are accessible after \stopStaff \startStaff
      (ly:grob-set-property! vertical-axis-group 'cn-staff-lines new-positions)
      (ly:grob-set-property! grob 'line-positions new-positions))))

extendStaffUp = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #t #t)
}

extendStaffDown = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #f #t)
}

unextendStaffUp = {
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #t #f)
  \stopStaff \startStaff
}

unextendStaffDown = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-extend-staff #f #f)
}


%% USER SHORTCUTS FOR DIFFERENT STAFF CONFIGURATIONS

cn-set-staff-lines =
#(lambda (positions)
   ;; positions is a list of staff line positions
   (lambda (grob)
     (let ((vertical-axis-group (ly:grob-parent grob Y)))
       (if (not (eq? (grob::name vertical-axis-group) 'VerticalAxisGroup))
           (ly:warning "clairnote-code.ly cannot find VerticalAxisGroup"))
       ;; store current positions in custom property VerticalAxisGroup.cn-staff-lines
       ;; so that they are accessible after \stopStaff \startStaff
       (ly:grob-set-property! vertical-axis-group 'cn-staff-lines positions)
       (ly:grob-set-property! grob 'line-positions positions))))

oneOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-set-staff-lines '(-8 -4))
}

twoOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-set-staff-lines '(-8 -4 4 8))
}

threeOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-set-staff-lines '(-20 -16 -8 -4 4 8))
}

fourOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking = #(cn-set-staff-lines '(-20 -16 -8 -4 4 8 16 20))
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

% VerticalAxisGroup.cn-staff-lines stores the staff line positions, stored
% in VerticalAxisGroup so they are accessible after \stopStaff \startStaff.
% Used with user functions for extending the staff, etc.
#(cn-define-grob-property 'cn-staff-lines list?)


%% STAFF CONTEXT DEFINITION

% customize \Staff to make it a Clairnote staff
\layout {
  \context {
    \Staff
    staffLineLayoutFunction = #ly:pitch-semitones
    middleCPosition = -12
    clefPosition = -5
    \override StaffSymbol.line-positions = #'(-8 -4 4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)
    \override StaffSymbol.ledger-extra = 1
    % staff-space is coordinated with cn-vscale-staff, 0.7 = (* 7/12 1.2)
    \override StaffSymbol.staff-space = #0.7

    % custom grob properties
    \override VerticalAxisGroup.cn-staff-lines = #'(-8 -4 4 8)
    \override StaffSymbol.cn-is-clairnote-staff = ##t
    \override StaffSymbol.cn-vscale-staff = #1.2
    % cn-vscale-staff stores the vertical staff scaling value
    % 1.2 is the default, 1 gives a staff with same size octave as traditional
    % stem and beam size, time sig and key sig position depend on it
    % stored in a separate property from staff-space because we need
    % to be able to \magnifyStaff the staff-space without losing
    % the vertical scaling value

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
