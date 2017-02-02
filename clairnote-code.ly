%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20160201
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

#(define (positive-integer? n) (and (positive? n) (integer? n)))

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

#(define (cn-magnification grob context)
   "Return the current magnification (from magnifyStaff, etc.)
    as the ratio of actual staff-space over cnBaseStaffSpace."
   (/ (ly:staff-symbol-staff-space grob)
     (ly:context-property context 'cnBaseStaffSpace)))

#(define (cn-get-staff-clef-adjust staff-octaves clef-octave-shift)
   "Calculate the amount to vertically adjust the position of the clef,
    key signature, and time signature, in note-spaces / half-staff-spaces."
   (+
    (* 12 clef-octave-shift)
    (if (odd? staff-octaves)
        6
        (if (> staff-octaves 2) 12 0))))

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
       (let ((context (ly:translator-context engraver))
             (black-note (= 0 (modulo (cn-notehead-semitone grob) 2))))

         (if (< (ly:grob-property grob 'duration-log) 1)

             ;; whole note
             (let ((mag (cn-magnification grob context))
                   (wn-path (if black-note
                                cn-whole-note-black
                                cn-whole-note-white)))
               (ly:grob-set-property! grob 'stencil
                 (ly:stencil-scale
                  (grob-interpret-markup grob
                    (markup (#:override '(filled . #t)
                              (#:path 0.0001 wn-path))))
                  mag mag)))

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
               ))))))))


%% ACCIDENTAL SIGNS

#(define cn-acc-sign-stils
   ;; associative list of accidental sign stencils
   ;; sharp and flat sign stencils are also used by key sig engraver
   (let*
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
         (ly:stencil-translate acc-sign (cons  0.25 0)))))

     (sharp (draw-acc-sign #t))
     (flat (draw-acc-sign #f))
     (double-sharp (draw-double-acc-sign sharp))
     (double-flat (draw-double-acc-sign flat)))
    (list
     (cons 1/2 sharp)
     (cons -1/2 flat)
     (cons 1 double-sharp)
     (cons -1 double-flat))))

#(define (cn-set-acc-extents grob alt)
   ;; Only used for LilyPond 2.18
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
       (else '(-0.0 . 0.44)))))

#(define (cn-redo-acc-sign grob alt mag)
   "Replaces the accidental sign stencil."
   (let*
    ((acc-lookup (assoc-ref cn-acc-sign-stils alt))
     (stil (if acc-lookup
               acc-lookup
               ;; natural sign (alt is 0)
               (ly:stencil-scale
                (ly:grob-property grob 'stencil)
                0.63 0.63))))
    (ly:grob-set-property! grob 'stencil
      (ly:stencil-scale stil mag mag))
    (if (cn-check-ly-version <= '(2 19 0))
        (cn-set-acc-extents grob alt))))

#(define (cn-pitch-in-key note alt key-sig)
   "key-sig is an association list of sharps or flats in the key sig.
    Example: D major (C#, F#) = ((0 . 1/2) (3 . 1/2))"
   ;; TODO: handle custom key sigs that have octave values:
   ;;    "keySignature (list) ... an alist containing (step . alter)
   ;;    or ((octave . step) . alter)"    <---- not currently handled
   (cond
    ;; 0. sharp or flat, in the key sig. (note-alt pair in key-sig)
    ((equal? (cons note alt) (assoc note key-sig)) #t)
    ;; 1. sharp or flat, not in key sig. (alt is sharp or flat)
    ((not (equal? 0 alt)) #f)
    ;; 2. not sharp or flat, not in the key sig. (note is in key-sig, alt is not)
    ((assoc-ref key-sig note) #f)
    ;; 3. not sharp or flat, in the key sig.
    (else #t)))

#(define Cn_accidental_engraver
   ;; context has to be accessed like this (and not with
   ;; ly:translator-context) for accidentals to be tracked per staff
   ;; using a closure for persistent barnum and alt-list (alteration list)
   (lambda (context)
     (let ((barnum 0)
           (alt-list '()))
       (make-engraver
        (acknowledgers
         ((accidental-interface engraver grob source-engraver)

          ;; refresh barnum and acc-list if we're in a new bar
          (let ((current-barnum (ly:context-property context 'currentBarNumber)))
            ;; another option: (ly:context-property context 'internalBarNumber)
            (if (not (equal? barnum current-barnum))
                (begin
                 (set! alt-list '())
                 (set! barnum current-barnum)))

            ;; 0. omit in effect, stencil = #f
            (if (not (ly:grob-property-data grob 'stencil))
                (ly:grob-suicide! grob)

                (let*
                 ((pitch (cn-notehead-pitch (ly:grob-parent grob Y)))
                  (semi (ly:pitch-semitones pitch))
                  (note (ly:pitch-notename pitch))
                  (alt (accidental-interface::calc-alteration grob))
                  (key-alts
                   (ly:context-property context
                     (if (cn-check-ly-version >= '(2 19 7))
                         'keyAlterations 'keySignature) '()))

                  (in-the-key (cn-pitch-in-key note alt key-alts))
                  (in-alt-list (equal? (cons semi alt) (assoc semi alt-list)))
                  (semi-in-alt-list (equal? alt (assoc-ref alt-list semi)))

                  ;; 1. new acc
                  ;; 2. cancel acc: in the key, cancels an alt in alt-list
                  ;;     (semi is in alt-list but the alt does not match)
                  ;; 3. forced acc: forced with !
                  (new-acc (and (not in-the-key) (not in-alt-list)))
                  (cancel-acc (and in-the-key (not in-alt-list) semi-in-alt-list))
                  (forced-acc (equal? #t (ly:grob-property grob 'forced))))

                 (if (or new-acc cancel-acc forced-acc)
                     (cn-redo-acc-sign grob alt (cn-magnification grob context))
                     ;; 4. is an acc but not a new one in this measure
                     ;; 5. is not an acc and is not cancelling previous acc
                     ;; TODO: does grob-suicide affect ledger line widths?
                     (ly:grob-suicide! grob))

                 ;; add to or remove from alt-list
                 ;; add replaces any existing alt for that semi
                 (cond
                  (new-acc (set! alt-list (assoc-set! alt-list semi alt)))
                  (cancel-acc (set! alt-list (assoc-remove! alt-list semi))))
                 )))))))))


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

#(define (cn-make-keysig-posns prev pattern result x-inc)
   "Recursive function to calculate x/y positions for keysig dots."
   (if (equal? '() pattern)
       result
       (let*
        ((whole-step (equal? (car pattern) prev))
         (y-step (if whole-step 2 1))
         (x-step (if whole-step 0 x-inc))
         (last-xy (last result))
         (new-xy (cons (+ x-step (car last-xy)) (+ y-step (cdr last-xy)))))
        (cn-make-keysig-posns (car pattern) (cdr pattern)
          (append result (list new-xy)) x-inc))))

#(define (cn-make-keysig-stack mode alt-list note-space black-tonic tonic-num)
   "Create the stack of circles (and tonic oval) for the key sig."
   (let*
    ((raw-pattern (take (drop '(#t #t #t #f #f #f #f #t #t #t #f #f #f #f) mode) 7))
     (raw-first-item (list-ref raw-pattern 0))
     ;; invert raw-pattern if needed, so that the first item is
     ;; #t for black tonic and #f for white tonic
     (pattern (if (or
                   (and black-tonic (not raw-first-item))
                   (and (not black-tonic) raw-first-item))
                  (map not raw-pattern)
                  raw-pattern))
     (first-item (list-ref pattern 0))
     (x-inc (if (and (pair? alt-list) (positive? (cdr (car alt-list)))) -0.6 0.6))

     (raw-posns (cn-make-keysig-posns (car pattern) (cdr pattern) '((0 . 0)) x-inc))
     (posns-b (map (lambda (p) (cons (car p) (* (cdr p) note-space))) raw-posns))

     (posns (if (negative? x-inc)
                (map (lambda (p) (cons (+ 1.2 (car p)) (cdr p))) posns-b)
                posns-b))


     (black-dot (make-circle-stencil 0.3 0.001 #t))
     (white-dot (make-circle-stencil 0.25 0.15 #f))

     (stack-list (map (lambda (xy bw)
                        (ly:stencil-translate (if bw black-dot white-dot) xy))
                   posns pattern))

     ;; add alterations - convert alt-list to a relative basis, tonic = 0, etc.
     (relative-alt-list (map (lambda (n)
                               (cons (modulo (- (car n) tonic-num) 7) (cdr n))) alt-list))
     (full-alt-list (map (lambda (n)
                           (assoc-ref relative-alt-list n)) '(0 1 2 3 4 5 6)))

     (sharp-line (ly:stencil-translate-axis
                  (make-connected-path-stencil '((-0.7  -0.7)) 0.2 1 1 #f #f)
                  -0.2 Y))
     (flat-line (ly:stencil-translate-axis
                 (make-connected-path-stencil '((-0.7  0.7)) 0.2 1 1 #f #f)
                 0.2 Y))
     (alt-stack-list (map (lambda (stil alt xy)
                            (cond
                             ((equal? alt -1/2)
                              (ly:stencil-combine-at-edge stil X -1
                                (ly:stencil-translate flat-line xy)
                                -0.2))
                             ((equal? alt 1/2)
                              (ly:stencil-combine-at-edge stil X -1
                                (ly:stencil-translate sharp-line xy)
                                -0.2))
                             (else stil)))
                       stack-list full-alt-list posns)))
    (fold ly:stencil-add empty-stencil alt-stack-list)))

#(define (cn-draw-keysig grob context)
   "Draws Clairnote key signature stencils."
   (let*
    ((base-staff-space (ly:context-property context 'cnBaseStaffSpace))
     (tonic-pitch (ly:context-property context 'tonic))
     ;; number of the tonic (0-6) (C-B)
     (tonic-num (ly:pitch-notename tonic-pitch))
     ;; semitone of tonic (0-11) (C-B)
     (tonic-semi (modulo (ly:pitch-semitones tonic-pitch) 12))
     (mag (cn-magnification grob context))

     (alt-list (ly:grob-property grob 'alteration-alist))
     (alt-count (cn-get-keysig-alt-count alt-list))
     (major-tonic-num (cn-get-major-tonic alt-count))
     ;; number of the mode (0-6)
     (mode (modulo (- tonic-num major-tonic-num) 7))
     ;; the distance between two adjacent notes given vertical staff compression
     (note-space (* 0.5 base-staff-space))
     (black-tonic (equal? 0 (modulo tonic-semi 2)))
     (raw-stack (cn-make-keysig-stack mode alt-list note-space black-tonic tonic-num))

     ;; position the sig vertically
     (base-vert-adj (- tonic-semi 12))
     ;; adjust position for odd octave staves and clefs shifted up/down an octave, etc.
     (staff-clef-adjust (cn-get-staff-clef-adjust
                         (ly:context-property context 'cnStaffOctaves)
                         (ly:context-property context 'cnClefShift)))
     (vert-adj (* note-space (+ base-vert-adj staff-clef-adjust)))
     (stack (ly:stencil-translate-axis raw-stack vert-adj Y)))
    ;; shift the sig to the right for better spacing
    (ly:stencil-translate-axis stack 0 X))
   #!
    (if (> mode 2)
        (ly:stencil-translate-axis stack 0.35 X)
        (ly:stencil-translate-axis stack 0.9 X))
   !#
   )

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
          (key-sig-stil (cn-draw-keysig grob context))
          (mult (magstep (ly:grob-property grob 'font-size 0.0))))

         (ly:grob-set-property! grob 'stencil
           (ly:stencil-scale key-sig-stil mult mult))
         (ly:grob-set-property! grob 'X-extent
           (ly:stencil-extent (ly:grob-property grob 'stencil) 0))
         )))))))


%% CLEFS AND OTTAVA (8VA 8VB 15MA 15MB)

% see /scm/parser-clef.scm and /ly/music-functions-init.ly

% We use an engraver to convert to Clairnote clef properties
% on the fly, rather than changing the traditional properties
% themselves at the source.  This allows TradStaff to work
% fully for any clef.

#(define (trad-to-cn-clef glyph pos)
   "Takes trad clef glyph and position and returns a symbol
    indicating the Clairnote clef to use.  Separating this
    into two steps means this function is never fed Clairnote
    clef positions, only trad clef positions."
   (let*
    ((new-clef
      (cond
       ((string= "clefs.G" glyph)
        (assoc-ref
         '((-4 . cn-treble) ;; french => treble
            (-2 . cn-treble)) ;; treble => treble
         pos))

       ((string= "clefs.C" glyph)
        (assoc-ref
         '((-4 . cn-treble) ;; soprano => treble
            (-2 . cn-alto) ;; mezzosoprano => alto
            (0 . cn-alto) ;; alto => alto (settings unchanged, but needed)
            (2 . cn-alto) ;; tenor => alto
            (4 . cn-bass)) ;; baritone => bass
         pos))

       ((string= "clefs.F" glyph)
        (assoc-ref
         '((0 . cn-bass) ;; varbaritone => bass
            (2 . cn-bass) ;; bass => bass
            (4 . cn-bass)) ;; subbass => bass
         pos))

       ((string= "clefs.percussion" glyph) 'cn-percussion)
       (else #f))))

    (if new-clef
        new-clef
        (begin
         (ly:warning "clef unsupported by clairnote-code.ly, using another clef instead.")
         (cond
          ((string= "clefs.F" glyph) 'cn-bass)
          ((string= "clefs.C" glyph) 'cn-alto)
          (else 'cn-treble))))))

#(define (trad-to-cn-clef-transposition trans)
   "If trans is already a Clairnote value (...-12, 12, 24...) just return trans,
    else convert from 7 notes per octave to 12.  7-->12, 14-->24. Rounding
    means only multiples of 12 are ever returned (... -24, -12, 0, 12, 24 ...)."
   (if (= 0 (modulo trans 12))
       trans
       (* 12 (round (/ trans 7)))))

#(define (cn-get-clef-props clef-name clef-transpo staff-clef-adjust)
   "In order to have stems change direction at the vertical center
    of the staff we use different clef settings for staves with odd
    or even numbers of octaves."

   ;; To calculate the clairnote middle c position subtract the clef position
   ;; from 12 for bass clef or from -12 for treble clef (to adjust the clef
   ;; position without affecting the position of middle c or other notes)
   (let*
    ((clef-data
      ;; default for staves with two octaves
      (assoc-ref
       '((cn-treble . ("clefs.G" -5 -12))
         (cn-bass . ("clefs.F" 5 12))
         (cn-alto . ("clefs.C" 0 0))
         (cn-percussion . ("clefs.percussion" 0 0)))
       clef-name))

     ;; clef transposition
     (new-transpo (trad-to-cn-clef-transposition clef-transpo))
     (mid-c-pos (- (list-ref clef-data 2) new-transpo))

     ;; staff-clef-adjust shifts clefPosition and middleCClefPosition:
     ;; up 6 note-positions for odd octave staves
     ;; up 12 for even octave staves with 4 or more octaves
     ;; up or down 12 * Staff.cnClefShift
     (new-pos (+ (list-ref clef-data 1) staff-clef-adjust))
     (new-mid-c-pos (+ mid-c-pos staff-clef-adjust)))
    (list (list-ref clef-data 0) new-pos new-mid-c-pos new-transpo)))

#(define Cn_clef_ottava_engraver
   ;; Override clef and ottava settings. A closure stores the previous
   ;; properties in order to detect new settings (sigh). Uses listeners
   ;; to modify context properties before grobs are created.
   (lambda (context)
     (let*
      ((prev-mid-c-off '())
       ;; clefGlyph clefPosition middleCClefPosition clefTransposition
       (prev-clef '(() () () ()))
       (prev-cue '(() () () ()))
       (prev-clef-name '())
       (prev-cue-name '())
       (prev-staff-octaves #f)
       (prev-clef-shift 0))

      (make-engraver
       (listeners
        ;; TODO: confirm that rhythmic-event is best event to listen to.
        ((rhythmic-event engraver event)
         (let*
          ((clef-prop-list '(clefGlyph clefPosition middleCClefPosition clefTransposition))
           (cue-prop-list '(cueClefGlyph cueClefPosition middleCCuePosition cueClefTransposition))

           (get-context-prop (lambda (prop) (ly:context-property context prop)))
           (now-clef (map get-context-prop clef-prop-list))
           (now-cue (map get-context-prop cue-prop-list))
           (now-mid-c-off (ly:context-property context 'middleCOffset))
           (now-staff-octaves (ly:context-property context 'cnStaffOctaves))
           (now-clef-shift (ly:context-property context 'cnClefShift))

           (changed-staff (not (equal? now-staff-octaves prev-staff-octaves)))
           (changed-clef (not (equal? now-clef prev-clef)))
           (changed-cue (not (equal? now-cue prev-cue)))
           (cue-unset (equal? now-cue '(() () () ())))
           (changed-clef-shift (not (equal? now-clef-shift prev-clef-shift)))

           (set-context-props!
            (lambda (context props vals)
              (for-each
               (lambda (prop val) (ly:context-set-property! context prop val))
               props vals)
              (ly:set-middle-C! context))))

          ;; set prev values to new values and then set context properties to prev values

          (if changed-staff (set! prev-staff-octaves now-staff-octaves))

          (if changed-clef (set! prev-clef-name
                                 (trad-to-cn-clef (list-ref now-clef 0) (list-ref now-clef 1))))

          (if changed-clef-shift (set! prev-clef-shift now-clef-shift))

          (if (or changed-clef changed-staff changed-clef-shift)
              (begin
               (set! prev-clef (cn-get-clef-props
                                prev-clef-name
                                (list-ref now-clef 3)
                                (cn-get-staff-clef-adjust prev-staff-octaves prev-clef-shift)))
               (set-context-props! context clef-prop-list prev-clef)))

          ;; cue clefs
          (if changed-cue
              (if cue-unset
                  (set! prev-cue-name '())
                  (set! prev-cue-name
                        (trad-to-cn-clef (list-ref now-cue 0) (list-ref now-cue 1)))))

          (if (or changed-cue changed-staff changed-clef-shift)
              (if cue-unset
                  ;; \cueClefUnset
                  (set! prev-cue now-cue)
                  ;; else \cueClef
                  (begin
                   (set! prev-cue (cn-get-clef-props
                                   prev-cue-name
                                   (list-ref now-cue 3)
                                   (cn-get-staff-clef-adjust prev-staff-octaves prev-clef-shift)))
                   (set-context-props! context cue-prop-list prev-cue))))

          ;; new ottava? (8va 8vb etc.)
          (if (not (equal? now-mid-c-off prev-mid-c-off))
              (begin
               (set! prev-mid-c-off (* now-mid-c-off 12/7))
               (ly:context-set-property! context 'middleCOffset prev-mid-c-off)
               (ly:set-middle-C! context)
               )))))))))


%% REPEAT SIGN DOTS (BAR LINES)

% adjust the position of dots in repeat signs
% for Clairnote staff or traditional staff

#(define (cn-make-colon-bar-line grob extent)
   "A procedure that draws dots (repeat sign dots) at
    @code{dot-positions}. The coordinates are the same as
    @code{StaffSymbol.line-positions}, a dot-position of X
    is equivalent to a line-position of X."
   (let*
    ((staff-sym (ly:grob-object grob 'staff-symbol))
     (is-clairnote-staff
      (if (ly:grob? staff-sym)
          (ly:grob-property staff-sym 'cn-is-clairnote-staff)
          #t))
     (odd-octaves
      (if (ly:grob? staff-sym)
          (member -2 (ly:grob-property staff-sym 'line-positions))
          #f))
     (dot-positions
      (if is-clairnote-staff
          (if odd-octaves '(4 8) '(-2 2))
          '(-1 1)))
     (staff-space (ly:staff-symbol-staff-space grob))
     (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot")))
    (fold
     (lambda (dp prev)
       (ly:stencil-add prev
         (ly:stencil-translate-axis dot (* dp (/ staff-space 2)) Y)))
     empty-stencil
     dot-positions)))

#(add-bar-glyph-print-procedure ":" cn-make-colon-bar-line)


%% TIME SIGNATURES

#(define Cn_time_signature_engraver
   ;; "Adjust vertical position of time sig based on vertical staff scaling."
   (make-engraver
    (acknowledgers
     ((time-signature-interface engraver grob source-engraver)
      (let*
       ((context (ly:translator-context engraver))
        (base-staff-space (ly:context-property context 'cnBaseStaffSpace))
        (vscale-staff (* 12/7 base-staff-space))
        (base-y-offset (* (+ vscale-staff -0.9) -2.5))

        ;; adjust position for odd octave staves and clefs shifted up/down an octave
        ;; note-space is the distance between two adjacent notes given vertical staff compression
        (note-space (* 0.5 base-staff-space))
        (staff-clef-adjust (cn-get-staff-clef-adjust
                            (ly:context-property context 'cnStaffOctaves)
                            (ly:context-property context 'cnClefShift)))

        (y-offset (+ base-y-offset (* note-space staff-clef-adjust)))

        ;; adjustment for \magnifyStaff
        (mag (cn-magnification grob context))
        (final-y-offset (* y-offset mag)))

       (ly:grob-set-property! grob 'Y-offset final-y-offset))))))


%% STEM LENGTH AND DOUBLE STEMS

#(define Cn_stem_engraver
   ; "Lengthen all stems and give half notes double stems."
   (make-engraver
    (acknowledgers
     ((stem-interface engraver grob source-engraver)
      ;; make sure \omit is not in effect (i.e. stencil is not #f) and the stem has a
      ;; notehead (is not for a rest, rest grobs have stem grobs that have no stencil)
      (if (and (ly:grob-property-data grob 'stencil)
               (not (equal? '() (ly:grob-object grob 'note-heads))))
          (let*
           ((context (ly:translator-context engraver))
            (bss-inverse (/ 1 (ly:context-property context 'cnBaseStaffSpace))))
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
                ))))))))


%% BEAMS

#(define Cn_beam_engraver
   ; "Adjust size and spacing of beams, needed due to vertically compressed staff."
   (make-engraver
    (acknowledgers
     ((beam-interface engraver grob source-engraver)
      (let*
       ((context (ly:translator-context engraver))
        (base-staff-space-inverse
         (/ 1 (ly:context-property context 'cnBaseStaffSpace)))
        (thick (ly:grob-property-data grob 'beam-thickness))
        (len-frac (ly:grob-property-data grob 'length-fraction))
        (space (if (number? len-frac) len-frac 1)))
       (ly:grob-set-property! grob 'beam-thickness
         (* thick base-staff-space-inverse))
       ;; TODO: the 1.1 adjustment below was just visually estimated
       (ly:grob-set-property! grob 'length-fraction
         (* space 1.1 base-staff-space-inverse)))
      ))))


%% USER: EXTENDING STAVES & STAVES WITH DIFFERENT OCTAVE SPANS

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

cnStaffExtender =
#(define-music-function
  (parser location reset going-up going-down)
  (boolean? integer? integer?)
  #{
    \context Staff \applyContext
    #(lambda (context)
       (if (not (equal? 'TradStaff (ly:context-name context)))
           (let*
            ((grob-def (ly:context-grob-definition context 'StaffSymbol))
             (current-lines (ly:assoc-get 'line-positions grob-def '(-8 -4 4 8)))
             ;; base is '(-8 -4) or '(-2 2) for Clairnote
             (base-lines (ly:context-property context 'cnBaseStaffLines))
             (posns (if reset base-lines current-lines))
             (new-posns (cn-get-new-staff-positions posns base-lines going-up going-down)))
            (ly:context-pushpop-property context 'StaffSymbol 'line-positions new-posns))))
    \stopStaff
    \startStaff
  #})

cnExtendStaffUp = \cnStaffExtender ##f 1 0
cnExtendStaffDown = \cnStaffExtender ##f 0 1
cnUnextendStaffUp = \cnStaffExtender ##f -1 0
cnUnextendStaffDown = \cnStaffExtender ##f 0 -1

cnStaffOctaveSpan =
#(define-music-function (parser location octaves) (positive-integer?)
   ;; odd octaves: extended the same amount up and down (from 1)
   ;; even octaves: extended up one more than they are down
   (let*
    ((odd-octs (odd? octaves))
     (base-lines (if odd-octs '(-2 2) '(-8 -4)))
     (n (/ (1- octaves) 2))
     (upwards (if odd-octs n (ceiling n)))
     (downwards (if odd-octs n (floor n))))
    #{
      \set Staff.cnStaffOctaves = #octaves
      \set Staff.cnBaseStaffLines = #base-lines
      \override Staff.StaffSymbol.ledger-positions = #base-lines
      \cnStaffExtender ##t #upwards #downwards
    #}))

cnClefPositionShift =
#(define-music-function (parser location octaves) (integer?)
   #{
     \set Staff.cnClefShift = #octaves
   #})


%% USER: FIVE LINE STAFF

cnFiveLineStaff = {
  \cnStaffOctaveSpan 2
  \override Staff.StaffSymbol.line-positions = #'(-8 -4 0 4 8)
}


%% USER: SET STAFF COMPRESSION

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
      \set Staff.cnBaseStaffSpace = #ss
      \override Staff.StaffSymbol.staff-space = #ss
    #}))


%% USER: CUSTOMIZE NOTEADS

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

% All are Staff context properties unless otherwise noted.

% Stores the base staff-space to store the vertical compression of the
% Clairnote staff. The actual staff-space may differ with \magnifyStaff, etc.
% Stem and beam size, time sig and key sig position, etc. depend on it.
#(cn-translator-property-description 'cnBaseStaffSpace positive?)

% Stores the base staff line positions used for extending the staff
% up or down. See cnExtendStaff function.
#(cn-translator-property-description 'cnBaseStaffLines list?)

% For double stems for half notes.
#(cn-translator-property-description 'cnDoubleStemSpacing number?)
#(cn-translator-property-description 'cnDoubleStemWidthScale non-zero?)

% For note head style and scaling note head width
#(cn-translator-property-description 'cnNoteheadStyle string?)
#(cn-translator-property-description 'cnNoteheadWidthScale non-zero?)

% Indicates number of octaves the staff spans, lets us use
% different clef settings so stems always flip at center of staff
#(cn-translator-property-description 'cnStaffOctaves positive-integer?)

% For shifting clef position up or down an octave
#(cn-translator-property-description 'cnClefShift integer?)


%% CUSTOM GROB PROPERTIES

% function from "scm/define-grob-properties.scm" (modified)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% StaffSymbol.cn-is-clairnote-staff is used for repeat sign dots.
#(cn-define-grob-property 'cn-is-clairnote-staff boolean?)



%% LEGACY SUPPORT FOR LILYPOND 2.18.2 ETC.

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


%% CHORDS

% cn-shift-notehead, copied and modified from the LilyPond Snippet
% Repository, snippet 861, "Re-positioning note heads on the
% opposite side of the stem," http://lsr.di.unimi.it/LSR/Item?id=861
% Thanks to David Nalesnik and Thomas Morley for work on that snippet.
% Use that snippet for any manual adjustments of note head positions.

#(if
  (cn-check-ly-version < '(2 19 34))
  (begin

   (define (cn-shift-notehead nh nh-dir stem-dir)
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

   (define (cn-chords-loop note-heads first-semi stem-dir note-col)
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

   (define (cn-note-column-callback grob)
     ; "For notes in chords or harmonic intervals that are 2 semitones
     ; apart or less, automatically position them on opposite sides of the stem.
     ; (See Stem::calc_positioning_done in LilyPond source code lily/stem.cc)"
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
            (cn-chords-loop (cdr nhs-cooked) first-semi stem-dir grob)))))
   ))

%% END LEGACY SUPPORT SECTION


%% STAFF CONTEXT DEFINITION

\layout {
  % copy Staff context with its standard settings to
  % a custom staff context called TradStaff
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
    % prevents barlines at start of single (2-octave) system from being shown
    \override SystemStartBar.collapse-height = #9
  }

  % customize Staff context to make it a Clairnote staff
  \context {
    \Staff

    % context property settings
    % traditional clef settings are immediately converted to
    % clairnote settings by custom clef engraver
    clefGlyph = "clefs.G"
    clefPosition = -2
    middleCClefPosition = -6
    clefTransposition = 0
    middleCPosition = -12
    staffLineLayoutFunction = #ly:pitch-semitones
    printKeyCancellation = ##f
    \numericTimeSignature

    % custom context properties
    cnBaseStaffSpace = #0.75
    cnBaseStaffLines = #'(-8 -4)
    cnDoubleStemSpacing = #4.5
    cnDoubleStemWidthScale = #1.5
    cnNoteheadStyle = "lilypond"
    cnNoteheadWidthScale = #1
    cnStaffOctaves = #2
    cnClefShift = #0

    % grob property overrides
    \override StaffSymbol.line-positions = #'(-8 -4 4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)
    \override Stem.no-stem-extend = ##t
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()
    \override KeySignature.horizontal-skylines = #'()
    % TODO: whole note ledger lines are a bit too wide
    \override LedgerLineSpanner.length-fraction = 0.45
    \override LedgerLineSpanner.minimum-length-fraction = 0.35

    #(if (cn-check-ly-version >= '(2 19 34))
         #{
           \override Stem.note-collision-threshold = 2
           \override NoteCollision.note-collision-threshold = 2
         #}
         ;; an empty else clause is needed for 2.18 compatibility
         #{ #})

    #(if (cn-check-ly-version >= '(2 19 36))
         #{
           \override StaffSymbol.ledger-extra = 2
         #}
         ;; an empty else clause is needed for 2.18 compatibility
         #{ #})

    % NoteColumn override doesn't work as an engraver for some reason,
    % crashes with manual beams on chords.
    #(if (cn-check-ly-version < '(2 19 34))
         #{ \with {
           \override NoteColumn.before-line-breaking = #cn-note-column-callback
         } #})

    % staff-space reflects vertical compression of Clairnote staff.
    % Default of 0.75 makes the Clairnote octave 1.28571428571429
    % times the size of the traditional octave (3/4 * 12/7 = 9/7).
    % Adjacent note heads overlap by 0.625 (5/8).
    \override StaffSymbol.staff-space = #0.75

    % custom engravers
    \consists \Cn_clef_ottava_engraver
    \consists \Cn_key_signature_engraver
    \consists \Cn_time_signature_engraver
    \consists \Cn_note_heads_engraver
    \consists \Cn_stem_engraver
    \consists \Cn_beam_engraver

    #(if (cn-check-ly-version < '(2 19 18))
         #{ \with { \consists \Cn_dots_engraver } #})

    % Put all engravers before Cn_accidental_engraver or we may get a segfault crash.
    % Probably because it does ly:grob-suicide! on some accidental grobs –
    % that seems to be the source of the problem.
    \consists \Cn_accidental_engraver
  }
}

% allow parent contexts to accept \TradStaff in midi output too
\midi {
  \context {
    \Staff
    cnBaseStaffLines = #'(-8 -4)
  }
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
