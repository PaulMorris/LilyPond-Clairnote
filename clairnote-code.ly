%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20150408
%
%    Copyright © 2013, 2014, 2015 Paul Morris, except for functions copied
%    and modified from LilyPond source code, the LilyPond Snippet
%    Repository, and openLilyLib, as noted in comments below.
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


%% UTILITY FUNCTIONS

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

#(define (cn-magnification grob)
   "Return the current magnification (from magnifyStaff, etc.)
    as the ratio of actual staff-space over cn-base-staff-space."
   (/ (ly:staff-symbol-staff-space grob)
     (cn-staff-symbol-prop grob 'cn-base-staff-space)))

#(if (not (defined? 'grob::name))
     ;; TODO: Delete this after we stop supporting LilyPond 2.18
     (define (grob::name grob)
       "Return the name of the grob @var{grob} as a symbol."
       (assq-ref (ly:grob-property grob 'meta) 'name)))


%% LILYPOND VERSION CHECKING

% copied and modified from openLilyLib, lilypond-version-predicates.ily

#(define (cn-calculate-version version-list)
   "Return an integer representation of the LilyPond version,
    can be compared with =, <, >, etc."
   (+ (* 1000000 (first version-list))
     (* 1000 (second version-list))
     (third version-list)))

#(define (cn-check-ly-version proc ref-version-list)
   "Compare the LilyPond version with the reference version
    in @code{ref-version-list} (e.g. (list 2 18 2) or '(2 19 7) )
    using @code{proc} (e.g. >  <  >=  <=) and return #t or #f."
   (proc (cn-calculate-version (ly:version))
     (cn-calculate-version ref-version-list)))


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
   ;; Customizes stencil, stem-attachment, rotation.
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


%% DOTS ON DOTTED NOTES

#(if (cn-check-ly-version < '(2 19 18))
     ;; TODO: remove when we stop supporting LilyPond 2.18
     (define (cn-note-dots grob)
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
          ((member semi '(2 6 10))
           (ly:grob-set-property! grob 'Y-offset -0.36))))))


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

  (set! cn-sharp-sign (draw-acc-sign #t))
  (set! cn-flat-sign (draw-acc-sign #f))
  (set! cn-double-sharp-sign (draw-double-acc-sign cn-sharp-sign))
  (set! cn-double-flat-sign (draw-double-acc-sign cn-flat-sign)))


#(define (cn-redo-acc-sign grob alt)
   "Replaces the accidental sign stencil."
   (let*
    ((mag (cn-magnification grob))
     (stil
      (case alt
        ((-1/2) cn-flat-sign)
        ((1/2) cn-sharp-sign)
        ((-1) cn-double-flat-sign)
        ((1) cn-double-sharp-sign)
        ;; else covers natural sign (0)
        (else (ly:stencil-scale
               (ly:grob-property grob 'stencil)
               0.63 0.63)))))
    (ly:grob-set-property! grob 'stencil
      (ly:stencil-scale stil mag mag))))

#(define (cn-pitch-in-key pitch key-sig)
   "key-sig is an association list of sharps or flats in the key sig.
    Example: D major (C#, F#) = ((0 . 1/2) (3 . 1/2))"
   ;; TODO: handle custom key sigs that have octave values:
   ;;    "keySignature (list) ... an alist containing (step . alter)
   ;;    or ((octave . step) . alter)"    <---- not currently handled
   (let
    ((note (ly:pitch-notename pitch))
     (alt (ly:pitch-alteration pitch)))
    ;; is the note-and-alt-pair in the key sig?
    (if (equal? (cons note alt) (assoc note key-sig))
        ;; yes --> #t sharp/flat in key sig
        #t
        ;; no, is alt a sharp or flat (i.e. not a natural)?
        (if (not (equal? 0 alt))
            ;; yes --> #f sharp/flat not in key sig
            #f
            ;; no, is note (disregarding its alt) in the key sig?
            (if (assoc-ref key-sig note)
                ;; yes --> #f natural not in key sig
                #f
                ;; no --> #t natural in key sig
                #t)))))

#(define Cn_accidental_engraver
   ;; using a closure for persistent barnum and alt-list (alteration list)
   (lambda (context)
     (let ((barnum 0)
           (alt-list '()))
       (make-engraver
        (acknowledgers
         ((accidental-interface engraver grob source-engraver)
          (let ((current-barnum (ly:context-property context 'currentBarNumber)))
            ;; another option: (ly:context-property context 'internalBarNumber)
            ;; if we're in a new bar, clear alt-list and set barnum
            (if (not (equal? barnum current-barnum))
                (begin
                 (set! alt-list '())
                 (set! barnum current-barnum)))
            (let*
             ((alt (accidental-interface::calc-alteration grob))
              (stl (ly:grob-property-data grob 'stencil))
              (note-head (ly:grob-parent grob Y))
              (pitch (cn-notehead-pitch note-head))
              (semi (ly:pitch-semitones pitch))
              (key-alts
               (if (cn-check-ly-version >= '(2 19 7))
                   ;; TODO: redo this when we drop support for LilyPond 2.18
                   (ly:context-property context 'keyAlterations '())
                   (ly:context-property context 'keySignature '())))
              (in-the-key (cn-pitch-in-key pitch key-alts))
              (in-alt-list (equal? (cons semi alt) (assoc semi alt-list)))
              (semi-in-alt-list (equal? alt (assoc-ref alt-list semi))))
             ;;Show an acc sign? Yes: 1,2,3, No: 0,4,5
             (cond
              ;; 0. omit is in effect (stencil is #f)
              ((not stl)
               (ly:grob-suicide! grob))
              ;; 1. new acc: an acc not in the alt-list
              ;; add to alt-list (any previous alt for that semi is replaced)
              ((and (not in-the-key) (not in-alt-list))
               (cn-redo-acc-sign grob alt)
               (set! alt-list (assoc-set! alt-list semi alt)))
              ;; 2. cancel acc: in the key, cancels a previous alt in alt-list
              ;; (i.e. semi is in alt-list but the alt does not match)
              ;; remove alt from alt-list
              ((and in-the-key (not in-alt-list) semi-in-alt-list)
               (cn-redo-acc-sign grob alt)
               (set! alt-list (assoc-remove! alt-list semi)))
              ;; 3. forced acc: acc wouldn't be shown, but it was forced with !
              ;; no change to alt-list
              ((and (equal? #t (ly:grob-property grob 'forced)))
               (cn-redo-acc-sign grob alt))
              ;; 4. is an acc but not a new one in this measure
              ;; 5. is not an acc and is not cancelling previous acc
              ;; TODO: does grob-suicide affect ledger line widths?
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
    ((mag (cn-magnification grob))
     (num-scale (/ 0.6 mag))
     (acc-scale (/ 0.65 mag))
     (acc (cond
           ((> alt-count 0) cn-sharp-sign)
           ((< alt-count 0) cn-flat-sign)
           ((= alt-count 0) (ly:stencil-scale
                             (grob-interpret-markup grob (markup #:natural))
                             acc-scale acc-scale))))
     (num (ly:stencil-scale
           (grob-interpret-markup grob
             (markup (number->string (abs alt-count))))
           num-scale num-scale)))
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
     ;; the distance between two adjacent notes given vertical staff compression
     (note-space (* 0.5 (cn-staff-symbol-prop grob 'cn-base-staff-space)))
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

#(define (cn-make-repeat-dot-bar dot-positions)
   "Return a procedure that draws dots (repeat sign dots) at
    @var{dot-positions}. The coordinates of @var{dot-positions} are
    the same coordinates as @code{StaffSymbol.line-positions}, a
    dot-position of X and a line-position of X are equivalent."
   (lambda (grob extent)
     (let* ((staff-space (ly:staff-symbol-staff-space grob))
            (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
            (stencil empty-stencil))
       (for-each
        (lambda (dp)
          (set! stencil (ly:stencil-add stencil
                          (ly:stencil-translate-axis dot (* dp (/ staff-space 2)) Y))))
        dot-positions)
       stencil)))

#(add-bar-glyph-print-procedure ":" (cn-make-repeat-dot-bar '(-2 2)))


%% TIME SIGNATURES

#(define (cn-timesigs grob)
   "Adjust vertical position of time sig based on vertical staff scaling."
   (let*
    ;; default base-staff-space is 0.7
    ;; default vscale-staff is 1.2
    ;; default basic-y-offset is -0.75
    ((base-staff-space (cn-staff-symbol-prop grob 'cn-base-staff-space))
     (vscale-staff (* 12/7 base-staff-space))
     (basic-y-offset (* (- vscale-staff 0.9) -2.5))
     ;; adjustment for \magnifyStaff
     (mag (cn-magnification grob)))
    (ly:grob-set-property! grob 'Y-offset
      (* basic-y-offset mag))))


%% STEM LENGTH AND DOUBLE STEMS

#(define (cn-stems grob)
   "Lengthen all stems and give half notes double stems."
   ;; make sure \omit is not in effect (i.e. stencil is not #f)
   (if (ly:grob-property-data grob 'stencil)
       (let*
        ;; default base-staff-space-inverse is 1/0.7 = 1.42857714286...
        ((bss-inverse (/ 1 (cn-staff-symbol-prop grob 'cn-base-staff-space)))
         (mag (cn-magnification grob)))
        ;; multiply each of the values in the details property of the stem grob
        ;; by bss-inverse, except for stem-shorten values
        (ly:grob-set-property! grob 'details
          (map
           (lambda (detail)
             (let ((head (car detail))
                   (args (cdr detail)))
               (if (eq? head 'stem-shorten)
                   (cons head args)
                   (cons head
                     (map
                      (lambda (arg) (* arg bss-inverse))
                      args)))))
           (ly:grob-property grob 'details)))
        ;; double stems for half notes
        ;; use -0.42 or 0.15 to change which side the 2nd stem appears
        (if (= 1 (ly:grob-property grob 'duration-log))
            (begin
             (ly:grob-set-property! grob 'stencil
               (ly:stencil-combine-at-edge
                (ly:stem::print grob)
                X
                (- (ly:grob-property grob 'direction))
                (ly:stem::print grob)
                (* mag -0.42)))
             ;; X-extent needs to be set here because its usual callback
             ;; ly:stem::width doesn't take the actual stencil width into account
             (ly:grob-set-property! grob 'X-extent
               (ly:stencil-extent (ly:grob-property grob 'stencil) 0))
             )))))


%% BEAMS

#(define (cn-beams grob)
   "Adjust size and spacing of beams, needed due to vertically compressed staff."
   (let*
    ((base-staff-space-inverse
      (/ 1 (cn-staff-symbol-prop grob 'cn-base-staff-space)))
     (thick (ly:grob-property-data grob 'beam-thickness))
     (len-frac (ly:grob-property-data grob 'length-fraction))
     (space (if (number? len-frac) len-frac 1)))
    (ly:grob-set-property! grob 'beam-thickness
      (* thick base-staff-space-inverse))
    ;; TODO: the 1.1 adjustment below was just visually estimated
    (ly:grob-set-property! grob 'length-fraction
      (* space 1.1 base-staff-space-inverse))))


%% USER STAFF EXTENSION FUNCTIONS

#(define (cn-extend-staff upwards extend)
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

#(define (cn-set-staff-lines positions)
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
  \override Staff.StaffSymbol.before-line-breaking =
  #(cn-set-staff-lines '(-8 -4))
}

twoOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking =
  #(cn-set-staff-lines '(-8 -4 4 8))
}

threeOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking =
  #(cn-set-staff-lines '(-20 -16 -8 -4 4 8))
}

fourOctaveStaff = {
  \stopStaff \startStaff
  \override Staff.StaffSymbol.before-line-breaking =
  #(cn-set-staff-lines '(-20 -16 -8 -4 4 8 16 20))
}


%% USER VERTICAL STAFF COMPRESSION FUNCTION

% must be used before \magnifyStaff for both to work
vertScaleStaff =
#(define-music-function (parser location vscale-staff) (number?)
   "1.2 is the default vscale-staff which gives default ss (staff-space) of 0.7
    vscale-staff of 1 gives a staff with same size octave as traditional"
   (define ss (* 7/12 vscale-staff))
   #{
     \override Staff.StaffSymbol.cn-base-staff-space = #ss
     \override Staff.StaffSymbol.staff-space = #ss
   #})


%% CUSTOM GROB PROPERTIES

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% StaffSymbol.cn-base-staff-space stores the base staff space
% given the vertical compression of the Clairnote staff, which
% may differ from the actual staff-space, with \magnifyStaff, etc.
#(cn-define-grob-property 'cn-base-staff-space number?)

% VerticalAxisGroup.cn-staff-lines stores the staff line positions. Stored
% in VerticalAxisGroup so they are accessible after \stopStaff \startStaff.
% Used with user functions for extending the staff.
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
    % staff-space reflects vertical compression of Clairnote staff.
    % Default of 0.7 makes the Clairnote octave 1.2 times
    % the size of the traditional octave. 0.7 * 12/7 = 1.2
    \override StaffSymbol.staff-space = #0.7

    % Custom grob property that stores the base staff-space, encoding
    % the vertical compression of the Clairnote staff. It may differ from
    % the actual staff-space which is scaled by \magnifyStaff etc.
    % Stem and beam size, time sig and key sig position depend on it.
    \override StaffSymbol.cn-base-staff-space = #0.7

    % Custom grob property for user staff extension functions
    \override VerticalAxisGroup.cn-staff-lines = #'(-8 -4 4 8)

    \override TimeSignature.before-line-breaking = #cn-timesigs
    % stems and beams restored to their pre-staff-compression size
    \override Stem.before-line-breaking = #cn-stems
    \override Beam.before-line-breaking = #cn-beams

    \consists \Cn_note_heads_engraver
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

    #(if (cn-check-ly-version < '(2 19 18))
         #{ \override Dots.before-line-breaking = #cn-note-dots #})
  }
}
