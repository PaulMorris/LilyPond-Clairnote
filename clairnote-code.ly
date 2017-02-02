%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20150912
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
   (define event (ly:grob-property grob 'cause))
   (if (ly:stream-event? event)
       (ly:event-property event 'pitch)
       (begin
        (ly:warning "clairnote-code.ly cannot access the pitch of a note head grob.  (Are you trying to use the Ambitus_engraver?  It is incompatible with clairnote-code.ly.)")
        (ly:make-pitch 0 0 0))))

#(define (cn-notehead-semitone grob)
   "Takes a note head grob and returns its semitone."
   (ly:pitch-semitones (cn-notehead-pitch grob)))

#(define (cn-get-is-clairnote-staff grob)
   "Takes a grob and returns the custom StaffSymbol property
    cn-is-clairnote-staff.  Silently falls back to the default of #t."
   (define staff-sym (ly:grob-object grob 'staff-symbol))
   (if (ly:grob? staff-sym)
       (ly:grob-property staff-sym 'cn-is-clairnote-staff)
       #t))

#(define (cn-get-base-staff-space grob)
   "Takes a grob and returns the custom StaffSymbol property
    cn-base-staff-space.  Silently falls back to the default of 0.7."
   (define staff-sym (ly:grob-object grob 'staff-symbol))
   (if (ly:grob? staff-sym)
       (ly:grob-property staff-sym 'cn-base-staff-space)
       0.7))

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
             (let ((font (ly:grob-default-font grob)))
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
                     (cons 1.06  0.3)))
               ))))))))


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
           (ly:grob-set-property! grob 'Y-offset -0.36))
          ))))


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

#(define (cn-repeat-dot-bar-procedure grob extent)
   "Return a procedure for repeat sign dots based on a custom grob
    property: StaffSymbol.cn-is-clairnote-staff."
   (if (cn-get-is-clairnote-staff grob)
       ;; Clairnote staff or Traditional five line staff
       ((cn-make-repeat-dot-bar '(-2 2)) grob extent)
       ((cn-make-repeat-dot-bar '(-1 1)) grob extent)))

#(add-bar-glyph-print-procedure ":" cn-repeat-dot-bar-procedure)


%% TIME SIGNATURES

#(define (cn-timesigs grob)
   "Adjust vertical position of time sig based on vertical staff scaling."
   (let*
    ;; default base-staff-space is 0.7
    ;; default vscale-staff is 1.2
    ;; default basic-y-offset is -0.75
    ((base-staff-space (cn-get-base-staff-space grob))
     (vscale-staff (* 12/7 base-staff-space))
     (basic-y-offset (* (- vscale-staff 0.9) -2.5))
     ;; adjustment for \magnifyStaff
     (mag (cn-magnification grob)))
    (ly:grob-set-property! grob 'Y-offset
      (* basic-y-offset mag))))


%% STEM LENGTH AND DOUBLE STEMS

#(define (cn-stems grob)
   "Lengthen all stems and give half notes double stems."
   ;; make sure \omit is not in effect (i.e. stencil is not #f) and the stem has a
   ;; notehead (is not for a rest, rest grobs have stem grobs that have no stencil)
   (if (and (ly:grob-property-data grob 'stencil)
            (not (equal? '() (ly:grob-object grob 'note-heads))))
       (let
        ;; default base-staff-space-inverse is 1/0.7 = 1.42857714286...
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
              ;; second stem is 1.5 times as thick as standard stem
              (thick-stem (ly:stencil-scale stem 1.5 1))
              (dir (- (ly:grob-property grob 'direction)))
              (stem-extent (ly:stencil-extent stem X))
              (stem-width (- (car stem-extent) (cdr stem-extent)))
              ;; old: use -0.42 or 0.15 to change which side the 2nd stem appears
              ;; 4.5 * stem-width = -0.585
              (spacing (* 4.5 stem-width)))
             (ly:grob-set-property! grob 'stencil
               (ly:stencil-combine-at-edge stem X dir thick-stem spacing))
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
      (/ 1 (cn-get-base-staff-space grob)))
     (thick (ly:grob-property-data grob 'beam-thickness))
     (len-frac (ly:grob-property-data grob 'length-fraction))
     (space (if (number? len-frac) len-frac 1)))
    (ly:grob-set-property! grob 'beam-thickness
      (* thick base-staff-space-inverse))
    ;; TODO: the 1.1 adjustment below was just visually estimated
    (ly:grob-set-property! grob 'length-fraction
      (* space 1.1 base-staff-space-inverse))))


%% USER SHORTCUTS FOR EXTENDING STAVES
%% AND FOR DIFFERENT SIZE STAVES

#(define (cn-extend-staff reset going-up going-down)
   ;; reset is boolean, going-up and going-down are integers
   ;; of the number of octaves to extend up or down, negative
   ;; values unextend the staff one octave only
   (lambda (grob)

     (define recurser (lambda (proc posns extension  n)
                        (if (<= n 0)
                            posns
                            (recurser proc (proc posns extension) extension (- n 1)))))

     (define (extend-up posns extension)
       (let ((furthest (reduce max '() posns)))
         (append posns (map (lambda (ext) (+ furthest ext)) extension))))

     (define (extend-down posns extension)
       (let ((furthest (reduce min '() posns)))
         (append (map (lambda (ext) (- furthest ext)) extension) posns)))

     (let ((vertical-axis-group (ly:grob-parent grob Y)))
       (if (not (eq? (grob::name vertical-axis-group) 'VerticalAxisGroup))
           (ly:warning "cannot find VerticalAxisGroup")
           (let*
            ((base-positions (ly:grob-property vertical-axis-group 'cn-base-staff-lines))
             (max-bp (reduce max '() base-positions))
             (min-bp (reduce min '() base-positions))
             ;; the number of empty positions between staves from one octave to the next
             ;; 8 for Clairnote
             (gap (+ min-bp (- 12 max-bp)))

             ;; relative locations of a new octave of staff lines,
             ;; as if 0 is the the furthest current staff line.  '(8 12) for Clairnote
             (extension (map (lambda (bp) (+ bp gap (- min-bp))) base-positions))
             (extension-length (length extension))

             (current-positions (ly:grob-property vertical-axis-group 'cn-current-staff-lines))
             (posns (if reset base-positions current-positions))

             (posns-up
              (cond
               ((positive? going-up) (recurser extend-up posns extension going-up))
               ((negative? going-up)
                (if (> (length current-positions) extension-length)
                    (drop-right (sort current-positions <) extension-length)
                    (begin (ly:warning "\\unextendStaffUp failed, not enough staff to unextend") posns)))
               (else posns)))

             (posns-down
              (cond
               ((positive? going-down) (recurser extend-down posns-up extension going-down))
               ((negative? going-down)
                (if (> (length current-positions) extension-length)
                    (drop (sort current-positions <) extension-length)
                    (begin (ly:warning "\\unextendStaffDown failed, not enough staff to unextend") posns)))
               (else posns-up))))

            ;; store current positions in custom property VerticalAxisGroup.cn-current-staff-lines
            ;; so that they are accessible after \stopStaff \startStaff
            (ly:grob-set-property! vertical-axis-group 'cn-current-staff-lines posns-down)
            (ly:grob-set-property! grob 'line-positions posns-down)
            )))))

cnExtendStaff =
#(define-music-function
  (parser location reset going-up going-down)
  (boolean? integer? integer?)
  #{
    \stopStaff \startStaff
    \override Staff.StaffSymbol.before-line-breaking =
    #(cn-extend-staff reset going-up going-down)
  #})

extendStaffUp = \cnExtendStaff ##f 1 0
extendStaffDown = \cnExtendStaff ##f 0 1
unextendStaffUp = \cnExtendStaff ##f -1 0
unextendStaffDown = \cnExtendStaff ##f 0 -1
oneOctaveStaff = \cnExtendStaff ##t 0 0
twoOctaveStaff = \cnExtendStaff ##t 1 0
threeOctaveStaff = \cnExtendStaff ##t 1 1
fourOctaveStaff = \cnExtendStaff ##t 2 1


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

% StaffSymbol.cn-is-clairnote-staff is used for repeat sign dots.
#(cn-define-grob-property 'cn-is-clairnote-staff boolean?)

% StaffSymbol.cn-base-staff-space stores the base staff space
% given the vertical compression of the Clairnote staff, which
% may differ from the actual staff-space, with \magnifyStaff, etc.
#(cn-define-grob-property 'cn-base-staff-space number?)

% VerticalAxisGroup.cn-current-staff-lines stores the current staff line positions.
% Stored in VerticalAxisGroup so they are accessible after \stopStaff \startStaff.
% Used with user functions for extending the staff.
#(cn-define-grob-property 'cn-current-staff-lines list?)

% VerticalAxisGroup.cn-base-staff-lines stores the base staff line positions.
% Stored in VerticalAxisGroup so they are accessible after \stopStaff \startStaff.
% Used with user functions for extending the staff.
#(cn-define-grob-property 'cn-base-staff-lines list?)


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
  \context { \Score \accepts TradStaff }
  \context { \ChoirStaff \accepts TradStaff }
  \context { \GrandStaff \accepts TradStaff }
  \context { \PianoStaff \accepts TradStaff }
  \context { \StaffGroup \accepts TradStaff }

  % customize \Staff to make it a Clairnote staff
  \context {
    \Staff

    staffLineLayoutFunction = #ly:pitch-semitones

    % clef settings
    clefGlyph = "clefs.G"
    clefPosition = -5
    middleCClefPosition = -12
    clefTransposition = 0
    middleCPosition = -12

    \consists \Cn_clef_ottava_engraver

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

    % Custom grob properties for user staff extension functions
    \override VerticalAxisGroup.cn-current-staff-lines = #'(-8 -4 4 8)
    \override VerticalAxisGroup.cn-base-staff-lines = #'(-8 -4)

    \override TimeSignature.before-line-breaking = #cn-timesigs
    % stems and beams restored to their pre-staff-compression size
    \override Stem.before-line-breaking = #cn-stems
    \override Beam.before-line-breaking = #cn-beams

    \consists \Cn_note_heads_engraver
    \override Stem.no-stem-extend = ##t

    % Cn_key_signature_engraver has to come before
    % Cn_accidental_engraver or we get a segfault crash
    \consists \Cn_key_signature_engraver
    printKeyCancellation = ##f

    \consists \Cn_accidental_engraver
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()

    \override NoteColumn.before-line-breaking = #cn-chords

    % TODO: whole note ledger lines are a bit too wide
    \override LedgerLineSpanner.length-fraction = 0.45
    \override LedgerLineSpanner.minimum-length-fraction = 0.35
    \numericTimeSignature

    #(if (cn-check-ly-version < '(2 19 18))
         #{ \override Dots.before-line-breaking = #cn-note-dots #})
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
