%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20151019
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

#(define (non-zero? n) (or (positive? n) (negative? n)))

#(define (cn-notehead-pitch grob)
   "Takes a note head grob and returns its pitch."
   (define event (ly:grob-property grob 'cause))
   (if (ly:stream-event? event)
       (ly:event-property event 'pitch)
       (begin
        (ly:warning "clairnote-code.ly cannot access the pitch of a note head grob.  (Are you trying to use the Ambitus_engraver?  It is incompatible with clairnote-code.ly.)")
        (ly:make-pitch 0 0 0))))

#(define (cn-notehead-semitone grob)
   "Takes a note head grob and returns its semitone."
   (ly:pitch-semitones (cn-notehead-pitch grob)))

#(define (cn-staff-symbol-property prop default grob)
   "Takes a grob @var{grob}, a symbol @var{prop}, and
    a @var{default} value. Returns that custom StaffSymbol
    property or silently falls back to the default value."
   (define staff-sym (ly:grob-object grob 'staff-symbol))
   (if (ly:grob? staff-sym)
       (ly:grob-property staff-sym prop)
       default))

#(define (cn-get-base-staff-space grob)
   "Takes a grob and returns the custom StaffSymbol property
    cn-base-staff-space.  Silently falls back to the default of 0.75."
   (cn-staff-symbol-property 'cn-base-staff-space 0.75 grob))

#(define (cn-magnification grob)
   "Return the current magnification (from magnifyStaff, etc.)
    as the ratio of actual staff-space over cn-base-staff-space."
   (/ (ly:staff-symbol-staff-space grob)
     (cn-get-base-staff-space grob)))

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
   (+ (* 1000000 (list-ref version-list 0))
     (* 1000 (list-ref version-list 1))
     (list-ref version-list 2)))

#(define (cn-check-ly-version proc ref-version-list)
   "Compare the LilyPond version with the reference version
    in @code{ref-version-list} (e.g. (list 2 18 2) or '(2 19 7) )
    using @code{proc} (e.g. >  <  >=  <=) and return #t or #f."
   (proc (cn-calculate-version (ly:version))
     (cn-calculate-version ref-version-list)))


%% NOTE HEADS AND STEM ATTACHMENT

% when we drop 2.18 support, use make-path-stencil see v.20150410

#(define cn-whole-note-black
   '((moveto 0 0)
     (curveto 0 0.16054432 0.12694192 0.28001904 0.272552 0.35842432)
     (curveto 0.47416576 0.4666984 0.70564816 0.50776784 0.93339712 0.50776784)
     (curveto 1.16114576 0.50776784 1.39636192 0.4666984 1.59797568 0.35842432)
     (curveto 1.74358576 0.28001904 1.87052768 0.16054432 1.87052768 0)
     (curveto 1.87052768 -0.16054432 1.74358576 -0.2800192 1.59797568 -0.35842448)
     (curveto 1.39636192 -0.46669856 1.16114592 -0.507768 0.93339712 -0.507768)
     (curveto 0.70564816 -0.507768 0.47416576 -0.46669856 0.272552 -0.35842448)
     (curveto 0.12694192 -0.2800192 0 -0.16054432 0 0)
     (closepath)))

#(define cn-whole-note-white
   (append
    cn-whole-note-black
    '((moveto 1.06033904 -0.36566768)
      (curveto 1.24701856 -0.36566768 1.32542384 -0.2688184 1.32542384 -0.09707328)
      (curveto 1.32542384 0.19788 1.10140848 0.36566752 0.80645504 0.36566752)
      (curveto 0.61977552 0.36566752 0.545104 0.26881824 0.545104 0.09707312)
      (curveto 0.545104 -0.19788016 0.7653856 -0.36566768 1.06033904 -0.36566768)
      (closepath))))

#(define Cn_note_heads_engraver
   (lambda (context)
     ;; Customizes stencil, stem-attachment, rotation.
     (make-engraver
      (acknowledgers
       ((note-head-interface engraver grob source-engraver)
        ;; make sure \omit is not in effect (i.e. stencil is not #f)
        ;; and do nothing for certain notehead styles
        (if
         (and
          (ly:grob-property-data grob 'stencil)
          (not (memq (ly:grob-property-data grob 'style)
                 (list 'harmonic 'harmonic-black 'harmonic-mixed
                   'diamond 'cross 'xcircle 'triangle 'slash))))
         ;; TODO: better handling of various notehead styles
         ;; http://lilypond.org/doc/v2.18/Documentation/notation/note-head-styles
         ;; output-lib.scm
         (let ((black-note (= 0 (modulo (cn-notehead-semitone grob) 2))))
           (if (< (ly:grob-property grob 'duration-log) 1)

               ;; whole note
               (let ((mag (cn-magnification grob)))
                 (ly:grob-set-property! grob 'stencil
                   (if black-note
                       (ly:stencil-scale
                        (grob-interpret-markup grob
                          (markup (#:override '(filled . #t)
                                    (#:path 0.0001 cn-whole-note-black))))
                        mag mag)
                       (ly:stencil-scale
                        (grob-interpret-markup grob
                          (markup (#:override '(filled . #t)
                                    (#:path 0.0001 cn-whole-note-white))))
                        mag mag))))

               ;; not whole note
               (let ((font (ly:grob-default-font grob))
                     (width-scale (ly:context-property context 'cnNoteheadWidthScale 1))
                     (style (ly:context-property context 'cnNoteheadStyle "lilypond")))

                 (if (equal? style "funksol")

                     ;; funk sol note heads
                     (ly:grob-set-property! grob 'stencil
                       (if black-note
                           (ly:font-get-glyph font "noteheads.s2solFunk")
                           (ly:font-get-glyph font "noteheads.s1solFunk")))

                     ;; standard style "lilypond" (emmentaler font) note heads
                     (begin
                      (ly:grob-set-property! grob 'stencil
                        (if black-note
                            (ly:font-get-glyph font "noteheads.s2")
                            ;; white notes are scaled horizontally to match black ones
                            (ly:stencil-scale (ly:font-get-glyph font "noteheads.s1") 0.945 1)))
                      ;; black notes can be rotated as far as -27,
                      ;; but -18 also works for white notes, currently -9
                      (ly:grob-set-property! grob 'rotation '(-9 0 0))
                      (ly:grob-set-property! grob 'stem-attachment
                        (if black-note
                            (cons 1.04 0.3)
                            (cons 1.06  0.3)))))

                 (if (not (= 1 width-scale))
                     (ly:grob-set-property! grob 'stencil
                       (ly:stencil-scale (ly:grob-property grob 'stencil) width-scale 1)))
                 )))))))))


%% DOTS ON DOTTED NOTES

#(if (cn-check-ly-version < '(2 19 18))
     ;; TODO: remove when we stop supporting LilyPond 2.18
     (define Cn_dots_engraver
       ;; "Adjust vertical position of dots for certain notes."
       (make-engraver
        (acknowledgers
         ((dots-interface engraver grob source-engraver)
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
              (ly:grob-set-property! grob 'Y-offset -0.36))
             )))))))


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
      (ly:stencil-scale stil mag mag))

    (if (cn-check-ly-version <= '(2 19 0))
        (begin
         ;; TODO: should natural signs have the same Y-extent as others?
         ;; TODO: shouldn't X/Y-extent scale with mult / font-size?
         (ly:grob-set-property! grob 'Y-extent '(-0.5 . 1.2))
         (ly:grob-set-property! grob 'X-extent
           (case alt
             ((-1/2) '(0 . 0.54))
             ((1/2) '(-0.27 . 0.27))
             ((-1) '(-0.34 . 0.67))
             ((1) '(-0.54 . 0.47))
             ;; else covers natural sign (0)
             (else '(-0.0 . 0.44)))
           )))))

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
               ;; redo this when we drop support for LilyPond 2.18
               (ly:context-property context
                 (if (cn-check-ly-version >= '(2 19 7)) 'keyAlterations 'keySignature)
                 '()))
              (in-the-key (cn-pitch-in-key pitch key-alts))
              (in-alt-list (equal? (cons semi alt) (assoc semi alt-list)))
              (semi-in-alt-list (equal? alt (assoc-ref alt-list semi))))
             ;;Show an acc sign? Yes: 1,2,3, No: 0,4,5
             (cond
              ;; 0. omit is in effect (stencil is #f)
              ;; was (ly:grob-suicide! grob) but this led to crashes in 2.18
              ((not stl)
               (ly:grob-suicide! grob))
              ; '())

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
              ;; was (ly:grob-suicide! grob) but this led to crashes in 2.18
              ;; TODO: does grob-suicide affect ledger line widths?
              (else
               (ly:grob-suicide! grob)
               ; (ly:grob-set-property! grob 'stencil #f)
               ))
             ))))))))


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
     (note-space (* 0.5 (cn-get-base-staff-space grob)))
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


#(define Cn_note_column_engraver
   ; "For notes in chords or harmonic intervals that are 2 semitones
   ; apart or less, automatically position them on opposite sides of the stem.
   ; (See Stem::calc_positioning_done in LilyPond source code lily/stem.cc)"
   (make-engraver
    (acknowledgers
     ((note-column-interface engraver grob source-engraver)
      (let* ((heads-array (ly:grob-object grob 'note-heads))
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
              (stem-dir (ly:grob-property (ly:grob-object grob 'stem) 'direction))
              ;; if stem is down then reverse the order
              (nhs-cooked
               (if (< stem-dir 0)
                   (reverse nhs-sorted)
                   nhs-sorted))
              (first-semi (cn-notehead-semitone (car nhs-cooked))))

             ;; Start with (cdr nhs-cooked). The first nh never needs shifting.
             (cn-chords-loop (cdr nhs-cooked) first-semi stem-dir grob))))
      ))))


%% CLEFS AND OTTAVA (8VA 8VB 15MA 15MB)

% see /scm/parser-clef.scm and /ly/music-functions-init.ly

#(define (trad-to-cn-clef glyph pos)
   "Takes trad clef glyph and position and returns a list with Clairnote
    clefGlyph, clefPosition, and middleCClefPosition."
   ;; To calculate the clairnote middle c position subtract the clef position
   ;; from 12 for bass clef or -12 for treble clef (to adjust the clef position
   ;; without affecting the position of middle c or other notes)
   (let*
    ((cn-treble '("clefs.G" -5 -12))
     (cn-bass '("clefs.F" 5 12))
     (cn-alto '("clefs.C" 0 0))
     (cn-percussion '("clefs.percussion" 0 0))
     (new-clef
      (cond
       ((string= "clefs.G" glyph)
        (case pos
          ((-4) cn-treble) ;; french => treble
          ((-2) cn-treble) ;; treble => treble
          (else #f)))

       ((string= "clefs.C" glyph)
        (case pos
          ((-4) cn-treble) ;; soprano => treble
          ((-2) cn-alto) ;; mezzosoprano => alto
          ((0) cn-alto) ;; alto => alto (unchanged, but needed)
          ((2) cn-alto) ;; tenor => alto
          ((4) cn-bass) ;; baritone => bass
          (else #f)))

       ((string= "clefs.F" glyph)
        (case pos
          ((0) cn-bass) ;; varbaritone => bass
          ((2) cn-bass) ;; bass => bass
          ((4) cn-bass) ;; subbass => bass
          (else #f)))

       ((string= "clefs.percussion" glyph) cn-percussion)
       (else #f))))

    (if new-clef
        new-clef
        (begin
         (ly:warning "clef unsupported by clairnote-code.ly, using another clef instead.")
         (cond
          ((string= "clefs.F" glyph) cn-bass)
          ((string= "clefs.C" glyph) cn-alto)
          (else cn-treble))))))

#(define (trad-to-cn-clef-transposition trans)
   ;; If trans is already a Clairnote value (...-12, 12, 24...) just return trans,
   ;; else convert from 7 notes per octave to 12.  7-->12, 14-->24. Rounding
   ;; means only multiples of 12 are ever returned (-24, -12, 0, 12, 24, etc.).
   (if (= 0 (modulo trans 12))
       trans
       (* 12 (round (/ trans 7)))))

#(define Cn_clef_ottava_engraver
   ;; Override clef and ottava settings. A closure stores the previous
   ;; properties in order to detect new settings (sigh). Uses listeners
   ;; to modify context properties before grobs are created.
   (lambda (context)
     (let*
      ((prev-mid-c-off '())
       ;; clefGlyph clefPosition middleCClefPosition clefTransposition
       (prev-clef '("clefs.G" -5 -12 0))
       (prev-cue '(() () () ())))
      (make-engraver
       (listeners
        ;; TODO: confirm that rhythmic-event is best event to listen to.
        ((rhythmic-event engraver event)
         (let*
          ((clef-glyph (ly:context-property context 'clefGlyph))
           (clef-pos (ly:context-property context 'clefPosition))
           (mid-c-clef-pos (ly:context-property context 'middleCClefPosition))
           (clef-transpo (ly:context-property context 'clefTransposition))
           (current-clef (list clef-glyph clef-pos mid-c-clef-pos clef-transpo))

           (cue-glyph (ly:context-property context 'cueClefGlyph))
           (cue-pos (ly:context-property context 'cueClefPosition))
           (cue-transpo (ly:context-property context 'cueClefTransposition))
           (mid-c-cue-pos (ly:context-property context 'middleCCuePosition))
           (current-cue (list cue-glyph cue-pos mid-c-cue-pos cue-transpo))

           (mid-c-off (ly:context-property context 'middleCOffset)))

          (cond
           ;; new clef?
           ((not (equal? current-clef prev-clef))
            (let*
             ((cn-clef (trad-to-cn-clef clef-glyph clef-pos))
              (cn-transpo (trad-to-cn-clef-transposition clef-transpo))
              (new-mid-c (- (list-ref cn-clef 2) cn-transpo)))
             (ly:context-set-property! context 'clefGlyph (list-ref cn-clef 0))
             (ly:context-set-property! context 'clefPosition (list-ref cn-clef 1))
             (ly:context-set-property! context 'middleCClefPosition new-mid-c)
             (ly:context-set-property! context 'clefTransposition cn-transpo)
             (ly:set-middle-C! context)
             (set! prev-clef (list (list-ref cn-clef 0) (list-ref cn-clef 1) new-mid-c cn-transpo))))

           ;; or new cue clef?
           ((not (equal? current-cue prev-cue))
            (if (equal? current-cue '(() () () ()))
                ;; \cueClefUnset
                (set! prev-cue current-cue)
                ;; else \cueClef
                (let*
                 ((cn-cue (trad-to-cn-clef cue-glyph cue-pos))
                  (cn-cue-transpo  (trad-to-cn-clef-transposition cue-transpo))
                  (new-mid-c (- (list-ref cn-cue 2) cn-cue-transpo)))
                 (ly:context-set-property! context 'cueClefGlyph (list-ref cn-cue 0))
                 (ly:context-set-property! context 'cueClefPosition (list-ref cn-cue 1))
                 (ly:context-set-property! context 'middleCCuePosition new-mid-c)
                 (ly:context-set-property! context 'cueClefTransposition cn-cue-transpo)
                 (ly:set-middle-C! context)
                 (set! prev-cue
                       (list (list-ref cn-cue 0) (list-ref cn-cue 1) new-mid-c cn-cue-transpo))))))

          ;; new ottava? (8va 8vb etc.)
          (if (not (equal? mid-c-off prev-mid-c-off))
              (let ((new-mid-c-off (* mid-c-off 12/7)))
                (ly:context-set-property! context 'middleCOffset new-mid-c-off)
                (ly:set-middle-C! context)
                (set! prev-mid-c-off new-mid-c-off)
                )))))))))


%% REPEAT SIGN DOTS (BAR LINES)

% adjust the position of dots in repeat signs
% for Clairnote staff or traditional staff

#(define (cn-make-colon-bar-line grob extent)
   "A procedure that draws dots (repeat sign dots) at
    @var{dot-positions}. The coordinates of @var{dot-positions} are
    the same coordinates as @code{StaffSymbol.line-positions}, a
    dot-position of X and a line-position of X are equivalent."
   (let*
    ((dot-positions
      (if (cn-staff-symbol-property 'cn-is-clairnote-staff #t grob) '(-2 2) '(-1 1)))
     (staff-space (ly:staff-symbol-staff-space grob))
     (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
     (stencil empty-stencil))
    (for-each
     (lambda (dp)
       (set! stencil (ly:stencil-add stencil
                       (ly:stencil-translate-axis dot (* dp (/ staff-space 2)) Y))))
     dot-positions)
    stencil))

#(add-bar-glyph-print-procedure ":" cn-make-colon-bar-line)


%% TIME SIGNATURES

#(define Cn_time_signature_engraver
   ; "Adjust vertical position of time sig based on vertical staff scaling."
   (make-engraver
    (acknowledgers
     ((time-signature-interface engraver grob source-engraver)
      (let*
       ((base-staff-space (cn-get-base-staff-space grob))
        (vscale-staff (* 12/7 base-staff-space))
        (basic-y-offset (* (- vscale-staff 0.9) -2.5))
        ;; adjustment for \magnifyStaff
        (mag (cn-magnification grob)))
       (ly:grob-set-property! grob 'Y-offset
         (* basic-y-offset mag)))
      ))))


%% STEM LENGTH AND DOUBLE STEMS

#(define Cn_stem_engraver
   (lambda (context)
     ; "Lengthen all stems and give half notes double stems."
     (make-engraver
      (acknowledgers
       ((stem-interface engraver grob source-engraver)
        ;; make sure \omit is not in effect (i.e. stencil is not #f) and the stem has a
        ;; notehead (is not for a rest, rest grobs have stem grobs that have no stencil)
        (if (and (ly:grob-property-data grob 'stencil)
                 (not (equal? '() (ly:grob-object grob 'note-heads))))
            (let
             ((bss-inverse (/ 1 (cn-get-base-staff-space grob))))
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
             (if (= 1 (ly:grob-property grob 'duration-log))
                 (let*
                  ((stem (ly:stem::print grob))
                   ;; second stem is 1.5 times as thick as standard stem by default
                   (thick-scale (ly:context-property context 'cnDoubleStemWidthScale 1.5))
                   (thick-stem (ly:stencil-scale stem thick-scale 1))
                   (dir (- (ly:grob-property grob 'direction)))
                   (stem-extent (ly:stencil-extent stem X))
                   (stem-width (- (car stem-extent) (cdr stem-extent)))
                   ;; old: use -0.42 or 0.15 to change which side the 2nd stem appears
                   ;; 4.5 * stem-width = -0.585
                   (spacing-scale (ly:context-property context 'cnDoubleStemSpacing 4.5))
                   (spacing (* spacing-scale stem-width)))

                  (ly:grob-set-property! grob 'stencil
                    (ly:stencil-combine-at-edge stem X dir thick-stem spacing))
                  ;; X-extent needs to be set here because its usual callback
                  ;; ly:stem::width doesn't take the actual stencil width into account
                  (ly:grob-set-property! grob 'X-extent
                    (ly:stencil-extent (ly:grob-property grob 'stencil) 0))
                  )))))))))


%% BEAMS

#(define Cn_beam_engraver
   ; "Adjust size and spacing of beams, needed due to vertically compressed staff."
   (make-engraver
    (acknowledgers
     ((beam-interface engraver grob source-engraver)
      (let*
       ((base-staff-space-inverse
         (/ 1 (cn-get-base-staff-space grob)))
        (thick (ly:grob-property-data grob 'beam-thickness))
        (len-frac (ly:grob-property-data grob 'length-fraction))
        (space (if (number? len-frac) len-frac 1)))
       (ly:grob-set-property! grob 'beam-thickness
         (* thick base-staff-space-inverse))
       ;; TODO: the 1.1 adjustment below was just visually estimated
       (ly:grob-set-property! grob 'length-fraction
         (* space 1.1 base-staff-space-inverse)))
      ))))


%% USER SHORTCUTS FOR EXTENDING STAVES
%% AND FOR DIFFERENT SIZE STAVES

#(define (cn-get-new-staff-positions posns base-positions going-up going-down)

   (define recurser (lambda (proc posns extension n)
                      (if (<= n 0)
                          posns
                          (recurser proc (proc posns extension) extension (- n 1)))))

   (define (extend-up posns extension)
     (let ((furthest (reduce max '() posns)))
       (append posns (map (lambda (ext) (+ furthest ext)) extension))))

   (define (extend-down posns extension)
     (let ((furthest (reduce min '() posns)))
       (append (map (lambda (ext) (- furthest ext)) extension) posns)))

   (let*
    ((max-bp (reduce max '() base-positions))
     (min-bp (reduce min '() base-positions))
     ;; the number of empty positions between staves from one octave to the next
     ;; 8 for Clairnote
     (gap (+ min-bp (- 12 max-bp)))

     ;; relative locations of a new octave of staff lines,
     ;; as if 0 is the the furthest current staff line.  '(8 12) for Clairnote
     (extension (map (lambda (bp) (+ bp gap (- min-bp))) base-positions))
     (extension-length (length extension))

     (posns-up
      (cond
       ((positive? going-up) (recurser extend-up posns extension going-up))
       ((negative? going-up)
        (if (> (length posns) extension-length)
            (drop-right (sort posns <) extension-length)
            (begin
             (ly:warning "\\cnUnextendStaffUp failed, not enough staff to unextend")
             posns)))
       (else posns)))

     (posns-down
      (cond
       ((positive? going-down) (recurser extend-down posns-up extension going-down))
       ((negative? going-down)
        (if (> (length posns) extension-length)
            (drop (sort posns <) extension-length)
            (begin
             (ly:warning "\\cnUnextendStaffDown failed, not enough staff to unextend")
             posns)))
       (else posns-up))))

    posns-down))

cnExtendStaff =
#(define-music-function
  (parser location reset going-up going-down)
  (boolean? integer? integer?)
  #{
    \context Staff \applyContext
    #(lambda (context)
       (let*
        ((grob-def (ly:context-grob-definition context 'StaffSymbol))
         (current-lines (ly:assoc-get 'line-positions grob-def '(-8 -4 4 8)))
         ;; base is '(-8 -4) for Clairnote
         (base-lines (ly:context-property context 'cnBaseStaffLines))
         (posns (if reset base-lines current-lines))
         (new-posns (cn-get-new-staff-positions posns base-lines going-up going-down)))
        (ly:context-pushpop-property context 'StaffSymbol 'line-positions new-posns)))
    \stopStaff
    \startStaff
  #})

cnExtendStaffUp = \cnExtendStaff ##f 1 0
cnExtendStaffDown = \cnExtendStaff ##f 0 1
cnUnextendStaffUp = \cnExtendStaff ##f -1 0
cnUnextendStaffDown = \cnExtendStaff ##f 0 -1
cnOneOctaveStaff = \cnExtendStaff ##t 0 0
cnTwoOctaveStaff = \cnExtendStaff ##t 1 0
cnThreeOctaveStaff = \cnExtendStaff ##t 1 1
cnFourOctaveStaff = \cnExtendStaff ##t 2 1


%% USER FUNCTION TO SET STAFF COMPRESSION

% must be used before \magnifyStaff for both to work
cnStaffCompression =
#(define-music-function (parser location ss) (number?)
   "0.75 is the default Clairnote staff-space (ss). An ss arg of
    1 gives an uncompressed staff. 7/12 gives a staff with
    same size octave as traditional"
   (let*
    ((trad-octave (/ (round (* 10000 (exact->inexact (* 12/7 ss)))) 10000))
     (notehead-overlap (+ 0.5 (- 0.5 (/ ss 2)))))
    (ly:message "Clairnote: custom staff compression of ~a will produce octaves ~a times the size of octaves in traditional notation; adjacent note heads (a semitone apart) will overlap by about ~a of their height."
      ss trad-octave notehead-overlap)
    #{
      \override Staff.StaffSymbol.cn-base-staff-space = #ss
      \override Staff.StaffSymbol.staff-space = #ss
    #}))


%% USER FUNCTIONS TO CUSTOMIZE NOTEADS

cnNoteheadStyle =
#(define-music-function (parser location style) (string?)
   (if (and
        (not (equal? style "lilypond"))
        (not (equal? style "funksol")))
       (ly:warning "\\cnNoteheadStyle used with an unrecognized style, using default instead."))
   #{ \set Staff.cnNoteheadStyle = #style #})

cnNoteheadWidth =
#(define-music-function (parser location width) (number?)
   "1.4 results in about the same funksol width as standard LilyPond noteheads."
   #{ \set Staff.cnNoteheadWidthScale = #width #})


%% CUSTOM CONTEXT PROPERTIES

% function from "scm/define-context-properties.scm" (modified)
#(define (cn-translator-property-description symbol type?)
   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc "custom context property")
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)

% Stores the base staff line positions used for extending the staff
% up or down. See cnExtendStaff function.
#(cn-translator-property-description 'cnBaseStaffLines list?)

% Staff context properties for double stems for half notes.
#(cn-translator-property-description 'cnDoubleStemSpacing number?)
#(cn-translator-property-description 'cnDoubleStemWidthScale non-zero?)

% Staff context properties for note head style and scaling note head width
#(cn-translator-property-description 'cnNoteheadStyle string?)
#(cn-translator-property-description 'cnNoteheadWidthScale non-zero?)


%% CUSTOM GROB PROPERTIES

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% StaffSymbol.cn-is-clairnote-staff is used for repeat sign dots.
#(cn-define-grob-property 'cn-is-clairnote-staff boolean?)

% StaffSymbol.cn-base-staff-space stores the base staff space
% given the vertical compression of the Clairnote staff. The actual
% staff-space may differ from cn-base-staff-space, with \magnifyStaff, etc.
#(cn-define-grob-property 'cn-base-staff-space positive?)


%% STAFF CONTEXT DEFINITION

\layout {
  % copy \Staff context with its standard settings to
  % a custom staff context called \TradStaff
  \context {
    \Staff
    \name TradStaff
    \alias Staff
    % custom grob property
    \override StaffSymbol.cn-is-clairnote-staff = ##f
  }
  % allow parent contexts to accept \TradStaff
  \context { \ChoirStaff \accepts TradStaff }
  \context { \GrandStaff \accepts TradStaff }
  \context { \PianoStaff \accepts TradStaff }
  \context { \StaffGroup \accepts TradStaff }
  \context {
    \Score
    \accepts TradStaff
    % prevents barlines at start of single system from being shown
    \override SystemStartBar.collapse-height = #9
  }

  % customize \Staff to make it a Clairnote staff
  \context {
    \Staff

    % context property settings
    clefGlyph = "clefs.G"
    clefPosition = -5
    middleCClefPosition = -12
    clefTransposition = 0
    middleCPosition = -12
    staffLineLayoutFunction = #ly:pitch-semitones
    printKeyCancellation = ##f
    \numericTimeSignature

    % custom context properties
    cnBaseStaffLines = #'(-8 -4)
    cnDoubleStemSpacing = #4.5
    cnDoubleStemWidthScale = #1.5
    cnNoteheadStyle = "lilypond"
    cnNoteheadWidthScale = #1

    % custom grob property overrides
    \override StaffSymbol.line-positions = #'(-8 -4 4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)
    \override StaffSymbol.ledger-extra = 1
    \override Stem.no-stem-extend = ##t
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()
    % TODO: whole note ledger lines are a bit too wide
    \override LedgerLineSpanner.length-fraction = 0.45
    \override LedgerLineSpanner.minimum-length-fraction = 0.35

    % staff-space reflects vertical compression of Clairnote staff.
    % Default of 0.75 makes the Clairnote octave 1.28571428571429
    % times the size of the traditional octave (3/4 * 12/7 = 9/7).
    % Adjacent note heads overlap by 0.625 (5/8).
    \override StaffSymbol.staff-space = #0.75

    % Custom grob property that stores the base staff-space, encoding
    % the vertical compression of the Clairnote staff. It may differ from
    % the actual staff-space which is scaled by \magnifyStaff etc.
    % Stem and beam size, time sig and key sig position depend on it.
    \override StaffSymbol.cn-base-staff-space = #0.75

    % custom engravers
    \consists \Cn_clef_ottava_engraver
    \consists \Cn_key_signature_engraver
    \consists \Cn_time_signature_engraver
    \consists \Cn_note_heads_engraver
    \consists \Cn_note_column_engraver
    \consists \Cn_stem_engraver
    \consists \Cn_beam_engraver
    #(if (cn-check-ly-version < '(2 19 18))
         #{ \with { \consists \Cn_dots_engraver } #})
    % Put all engravers before Cn_accidental_engraver or we may get a segfault crash.
    % Probably because accidentals can trigger ly:grob-suicide! on accidental grobs,
    % which seems to be the source of the problem.
    \consists \Cn_accidental_engraver
  }
}

% allow parent contexts to accept \TradStaff in midi output too
\midi {
  \context {
    \Staff
    \name TradStaff
    \alias Staff
  }
  \context { \Score \accepts TradStaff }
  \context { \ChoirStaff \accepts TradStaff }
  \context { \GrandStaff \accepts TradStaff }
  \context { \PianoStaff \accepts TradStaff }
  \context { \StaffGroup \accepts TradStaff }
}
