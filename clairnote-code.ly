%    This file "clairnote-code.ly" is a LilyPond include file for producing
%    sheet music in Clairnote music notation (http://clairnote.org).
%    Version: 20161214
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


%--- UTILITY FUNCTIONS ----------------

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

#(define (cn-magnification2 grob)
   "Return the current magnification (from magnifyStaff, etc.)
    as the ratio of actual staff-space over cn-base-staff-space."
   (/ (ly:staff-symbol-staff-space grob)
     (ly:grob-property grob 'cn-base-staff-space)))

#(define (cn-get-staff-clef-adjust staff-octaves clef-octave-shift)
   "Calculate the amount to vertically adjust the position of the clef,
    key signature, and time signature, in note-spaces / half-staff-spaces."
   (+
    (* 12 clef-octave-shift)
    (if (odd? staff-octaves)
        6
        (if (> staff-octaves 2) 12 0))))

#(define (cn-note-heads-from-grob grob default)
   "Takes a grob like a Stem and returns a list of NoteHead grobs or default."
   (let* ((heads-array (ly:grob-object grob 'note-heads))
          (heads-list (if (ly:grob-array? heads-array)
                          (ly:grob-array->list heads-array)
                          ;; should never/rarely? happen:
                          default)))
     heads-list))

#(if (not (defined? 'grob::name))
     ;; TODO: Delete this after we stop supporting LilyPond 2.18
     (define (grob::name grob)
       "Return the name of the grob @var{grob} as a symbol."
       (assq-ref (ly:grob-property grob 'meta) 'name)))


%--- LILYPOND VERSION CHECKING ----------------

% borrowed from scm/lily-library.scm (added in LilyPond 2.19.57)
% TODO: not needed after we drop support for LilyPond 2.18

#(if (not (defined? 'lexicographic-list-compare?))
     (define (lexicographic-list-compare? op a b)
       "Lexicographically compare two lists @var{a} and @var{b} using
        the operator @var{op}. The types of the list elements have to
        be comparable with @var{op}. If the lists are of different length
        the trailing elements of the longer list are ignored."
       (let* ((ca (car a))
              (iseql (op ca ca)))
         (let loop ((ca ca) (cb (car b)) (a (cdr a)) (b (cdr b)))
           (let ((axb (op ca cb)))
             (if (and (pair? a) (pair? b)
                      (eq? axb iseql (op cb ca)))
                 (loop (car a) (car b) (cdr a) (cdr b))
                 axb))))))

#(if (not (defined? 'ly:version?))
     (define (ly:version? op ver)
       "Using the operator @var{op} compare the currently executed LilyPond
        version with a given version @var{ver} which is passed as a list of
        numbers."
       (lexicographic-list-compare? op (ly:version) ver)))


%--- NOTE HEADS AND STEM ATTACHMENT ----------------

%% use make-path-stencil when we drop 2.18 support, see v.20150410

#(define (cn-whole-note-stencil grob context black-note)
   "default Clairnote whole notes"
   (let*
    ((black-path
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

     ;; white-path is the black path with the center hole added
     (white-path
      (append
       black-path
       '((moveto 1.06033904 -0.36566768)
         (curveto 1.24701856 -0.36566768 1.32542384 -0.2688184 1.32542384 -0.09707328)
         (curveto 1.32542384 0.19788 1.10140848 0.36566752 0.80645504 0.36566752)
         (curveto 0.61977552 0.36566752 0.545104 0.26881824 0.545104 0.09707312)
         (curveto 0.545104 -0.19788016 0.7653856 -0.36566768 1.06033904 -0.36566768)
         (closepath))))

     (path-to-use (if black-note black-path white-path))
     (mag (cn-magnification grob context)))

    (ly:stencil-scale
     (grob-interpret-markup grob
       (markup (#:override '(filled . #t) (#:path 0.0001 path-to-use))))
     mag mag)))

#(define (cn-default-note-stencil grob context black-note dur-log)
   "Default Clairnote note heads.  Shapes for half-note and
    quarter-note glyphs are modified versions (rotated -4 degrees then
    scaled vertically by 0.9299) of these glyphs from the
    Bravura font, licensed under the SIL Open Font License (OFL), see:
    http://scripts.sil.org/OFL
    http://www.smufl.org/fonts/
    http://blog.steinberg.net/2013/05/introducing-bravura-music-font/"
   (let*
    ((black-path
      '((moveto 1.0161518991984164 0.5004939160736768)
        (curveto 1.1900858991984165 0.45804726744838686 1.3000056991984164 0.36006297281891986 1.3267185991984165 0.22365338254444356)
        (curveto 1.3472530991984164 0.11878494731492373 1.3090816991984164 -0.03242382812749062 1.2346937991984164 -0.14100554722801906)
        (curveto 1.1037044991984164 -0.3321698541497182 0.8786728091984164 -0.46440803197455877 0.6176073691984164 -0.5036333230878486)
        (curveto 0.5243774691984164 -0.517643902935715 0.36590771919841636 -0.5120649140876494 0.28844503919841635 -0.49205136745873423)
        (curveto -0.029717590801583628 -0.40984214143367353 -0.09642331080158363 -0.0917693764989545 0.14752670919841637 0.17990713903061328)
        (curveto 0.2984087391984164 0.347937968940766 0.5439097091984164 0.4755783490866303 0.7757855691984165 0.5065506656056886)
        (curveto 0.8399878691984165 0.5151219432426919 0.9695811491984164 0.5118635199715458 1.0161502991984164 0.5005020382431211)
        (closepath)))

     (white-path
      '((moveto 1.026468864255086 0.4998680875655276)
        (curveto 1.215249864255086 0.4618436649454002 1.337174464255086 0.35147108531050375 1.354920364255086 0.20252749405141746)
        (curveto 1.369178964255086 0.08282868839604651 1.312372764255086 -0.07672001395465605 1.209350364255086 -0.20633856802981299)
        (curveto 1.077365164255086 -0.37239062345611024 0.889153024255086 -0.47211463127579484 0.6458905642550861 -0.5048878796193299)
        (curveto 0.585101844255086 -0.5130801213612108 0.548868934255086 -0.5134163330622651 0.443309034255086 -0.5067845101638356)
        (curveto 0.32885581425508603 -0.49958868733223255 0.30882433425508604 -0.4965974421400958 0.260617494255086 -0.47979947287536673)
        (curveto 0.118058624255086 -0.4300386473948317 0.024819864255086005 -0.335419029253747 0.0042339542550860025 -0.21961971038330325)
        (curveto -0.015404825744913999 -0.1091556900709823 0.035236334255086 0.05025573233647185 0.132092634255086 0.18283290751856218)
        (curveto 0.268217284255086 0.3691712451565947 0.47658985425508604 0.48176186299022195 0.730680684255086 0.5062696961187646)
        (curveto 0.823563584255086 0.5152225290660725 0.965453244255086 0.5121589666760266 1.026469764255086 0.4998666604401908)
        (closepath)
        (moveto 0.8920403042550861 0.32723653716982337)
        (curveto 0.801899114255086 0.305937547790631 0.674353834255086 0.25092305532124815 0.517242874255086 0.16559336664623436)
        (curveto 0.199745884255086 -0.006853856240945699 0.109727534255086 -0.09774589911519554 0.151265174255086 -0.2039339094078499)
        (curveto 0.168776074255086 -0.24869436361851288 0.191705974255086 -0.27755407274963595 0.226470474255086 -0.2985602160096806)
        (curveto 0.309656374255086 -0.34884402584120455 0.42197617425508605 -0.33748020734960626 0.634757234255086 -0.25724484236248213)
        (curveto 0.9774722042550861 -0.1280260070658748 1.216026564255086 0.03390026706789495 1.2240259642550861 0.14273918170232358)
        (curveto 1.2287459642550862 0.20700273076812625 1.184881964255086 0.28132959706261473 1.121983764255086 0.3156476039275703)
        (curveto 1.083730764255086 0.33652340307350437 1.077348764255086 0.3379303583723863 1.015085564255086 0.3392592023444909)
        (curveto 0.969948864255086 0.34025914149726677 0.9307790642550859 0.33638021483136094 0.892038904255086 0.32724518554936555)
        (closepath)))

     (path-to-use (if black-note black-path white-path))
     (mag (cn-magnification grob context)))

    (if (< dur-log 1)
        ;; whole note
        (cn-whole-note-stencil grob context black-note)
        ;; not whole note
        (ly:stencil-scale
         (grob-interpret-markup grob
           (markup (#:override '(filled . #t) (#:path 0.0001 path-to-use))))
         mag mag))))

#(define (cn-lilypond-note-stencil grob context black-note dur-log)
   "lilypond style note heads (Emmentaler font)"
   (if (< dur-log 1)
       ;; whole note
       (cn-whole-note-stencil grob context black-note)
       ;; not whole note
       (if black-note
           (ly:font-get-glyph (ly:grob-default-font grob) "noteheads.s2")
           ;; white notes are scaled horizontally to match black ones
           (ly:stencil-scale
            (ly:font-get-glyph (ly:grob-default-font grob) "noteheads.s1")
            0.945 1)
           )))

#(define (cn-funksol-note-stencil grob context black-note dur-log)
   "funksol style note heads"
   (if (< dur-log 1)
       ;; whole note
       (cn-whole-note-stencil grob context black-note)
       ;; not whole note
       (if black-note
           (ly:font-get-glyph (ly:grob-default-font grob) "noteheads.s2solFunk")
           (ly:font-get-glyph (ly:grob-default-font grob) "noteheads.s1solFunk"))))

#(define Cn_note_heads_engraver
   ;; Customizes stencil, stencil-width/height, stem-attachment, rotation.
   (make-engraver
    (acknowledgers
     ((note-head-interface engraver grob source-engraver)
      ;; make sure \omit is not in effect (i.e. stencil is not #f)
      ;; and do nothing for certain notehead styles
      (if
       (and
        (ly:grob-property-data grob 'stencil)
        (not (memq (ly:grob-property-data grob 'style)
               '(harmonic harmonic-black harmonic-mixed
                  diamond cross xcircle triangle slash))))
       ;; TODO: better handling of various notehead styles
       ;; http://lilypond.org/doc/v2.18/Documentation/notation/note-head-styles
       ;; output-lib.scm
       (let*
        ((context (ly:translator-context engraver))
         (black-note (= 0 (modulo (cn-notehead-semitone grob) 2)))
         (dur-log (ly:grob-property grob 'duration-log))
         (stil-proc (ly:context-property context 'cnNoteheadStencilProcedure))
         (width-scale (ly:context-property context 'cnNoteheadWidthScale 1))
         (height-scale (ly:context-property context 'cnNoteheadHeightScale 1))
         (rotn (ly:context-property context 'cnNoteheadRotation #f))
         (stem-attach (ly:context-property context 'cnStemAttachment #f)))

        (ly:grob-set-property! grob 'stencil
          (stil-proc grob context black-note dur-log))

        ;; if not a whole note (or longer)
        ;; dur-logs: 0 = whole, 1 = half, 2 = quarter and shorter
        (if (>= dur-log 1)
            (begin

             ;; width and height
             (if (not (and (= 1 width-scale) (= 1 height-scale)))
                 (ly:grob-set-property! grob 'stencil
                   (ly:stencil-scale
                    (ly:grob-property grob 'stencil)
                    width-scale height-scale)))

             ;; rotation
             (if rotn
                 (ly:grob-set-property! grob 'rotation (list rotn 0 0)))

             ;; stem attachment
             (if stem-attach
                 (ly:grob-set-property! grob 'stem-attachment
                   (if black-note
                       (car stem-attach)
                       (cdr stem-attach))))
             ))))))))


%--- ACCIDENTAL SIGNS ----------------

#(define cn-acc-sign-stils
   ;; associative list of accidental sign stencils
   (let*
    ((draw-acc-sign
      (lambda (is-sharp)
        "Return a sharp or flat sign stencil."
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
    (if (ly:version? <= '(2 19 0))
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
                     (if (ly:version? >= '(2 19 7))
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


%--- KEY SIGNATURES ----------------

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
     (x-inc (if (and (pair? alt-list) (positive? (cdr (car alt-list)))) -0.8 0.8))

     (raw-posns (cn-make-keysig-posns (car pattern) (cdr pattern) '((0 . 0)) x-inc))
     (posns-b (map (lambda (p) (cons (car p) (* (cdr p) note-space))) raw-posns))

     (posns (if (negative? x-inc)
                (map (lambda (p) (cons (+ 1.2 (car p)) (cdr p))) posns-b)
                posns-b))

     (black-dot (make-oval-stencil 0.34 0.34 0.14 #t))
     (white-dot (make-oval-stencil 0.34 0.34 0.15 #f))

     (stack-list (map (lambda (xy bw)
                        (ly:stencil-translate (if bw black-dot white-dot) xy))
                   posns pattern))

     ;; add alterations - convert alt-list to a relative basis, tonic = 0, etc.
     (relative-alt-list (map (lambda (n)
                               (cons (modulo (- (car n) tonic-num) 7) (cdr n))) alt-list))
     (full-alt-list (map (lambda (n)
                           (assoc-ref relative-alt-list n)) '(0 1 2 3 4 5 6)))

     (sharp-line (ly:stencil-translate-axis
                  (make-connected-path-stencil '((-0.7  -0.7)) 0.22 1 1 #f #f)
                  -0.2 Y))
     (flat-line (ly:stencil-translate-axis
                 (make-connected-path-stencil '((-0.7  0.7)) 0.22 1 1 #f #f)
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
                       stack-list full-alt-list posns))
     (combined-stack (fold ly:stencil-add empty-stencil alt-stack-list))
     ;; horizontal position adjustment
     (extent (ly:stencil-extent combined-stack 0))
     (positioned-stack
      (ly:stencil-translate-axis combined-stack (- (car extent)) X)))
    positioned-stack))

#(define (cn-draw-keysig grob)
   "Draws Clairnote key signature stencils."
   (let*
    ((base-staff-space (ly:grob-property grob 'cn-base-staff-space))
     (tonic-pitch (ly:grob-property grob 'cn-tonic))
     ;; number of the tonic (0-6) (C-B)
     (tonic-num (ly:pitch-notename tonic-pitch))
     ;; semitone of tonic (0-11) (C-B)
     (tonic-semi (modulo (ly:pitch-semitones tonic-pitch) 12))
     (mag (cn-magnification2 grob))

     (alt-list (ly:grob-property grob 'alteration-alist))
     (alt-count (cn-get-keysig-alt-count alt-list))
     (major-tonic-num (cn-get-major-tonic alt-count))
     ;; number of the mode (0-6)
     (mode (modulo (- tonic-num major-tonic-num) 7))
     ;; the distance between two adjacent notes given vertical staff compression
     (note-space (* 0.5 base-staff-space))
     (black-tonic (equal? 0 (modulo tonic-semi 2)))
     (raw-stack (cn-make-keysig-stack mode alt-list note-space black-tonic tonic-num))

     ;; position the sig vertically, C-tonic keys stay in place, the rest are moved down.
     (base-vert-adj (if (= tonic-semi 0) tonic-semi (- tonic-semi 12)))

     ;; adjust position for odd octave staves and clefs shifted up/down an octave, etc.
     (staff-clef-adjust (cn-get-staff-clef-adjust
                         (ly:grob-property grob 'cn-staff-octaves)
                         (ly:grob-property grob 'cn-clef-shift)))
     (vert-adj (* note-space (+ base-vert-adj staff-clef-adjust)))
     (stack (ly:stencil-translate-axis raw-stack vert-adj Y)))

    ;; TODO: is this horizontal adjustment needed?
    ;; shift the sig to the right for better spacing
    ;; (ly:stencil-translate-axis stack 0 X)
    ;; (if (> mode 2)
    ;;    (ly:stencil-translate-axis stack 0.35 X)
    ;;    (ly:stencil-translate-axis stack 0.9 X))

    stack))

#(define (cn-customize-key-signature grob)
   (let* ((stil (cn-draw-keysig grob))
          (mult (magstep (ly:grob-property grob 'font-size 0.0)))
          (stil-mult (ly:stencil-scale stil mult mult)))

     (ly:grob-set-property! grob 'stencil stil-mult)
     ))

#(define (cn-key-signature-grob-callback grob)
   "Clairnote's staff definition has printKeyCancellation = ##f, which
    prevents key cancellations, except for changing to C major or
    A minor. So here we prevent even those key cancellations."
   (cond
    ;; key cancellation?
    ((grob::has-interface grob 'key-cancellation-interface)
     (ly:grob-set-property! grob 'stencil #f))

    ;; omitted?
    ((not (ly:grob-property-data grob 'stencil)) #f)

    (else (cn-customize-key-signature grob))
    ))

#(define (Cn_key_signature_engraver context)
   "Sets the tonic for the key on key signature grobs."
   ;; Spare parts: (ly:context-property context 'printKeyCancellation)
   (make-engraver
    (acknowledgers
     ((key-signature-interface engraver grob source-engraver)
      (ly:grob-set-property! grob 'cn-tonic
        (ly:context-property context 'tonic))
      ))))


%--- CLEFS AND OTTAVA (8VA 8VB 15MA 15MB) ----------------

%% see /scm/parser-clef.scm and /ly/music-functions-init.ly

%% We use an engraver to convert to Clairnote clef properties
%% on the fly, rather than changing the traditional properties
%% themselves at the source.  This allows TradStaff to work
%% fully for any clef.

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
          ((clef-prop-list
            '(clefGlyph clefPosition middleCClefPosition clefTransposition))
           (cue-prop-list
            '(cueClefGlyph cueClefPosition middleCCuePosition cueClefTransposition))

           (get-context-prop (lambda (prop) (ly:context-property context prop)))
           (now-clef (map get-context-prop clef-prop-list))
           (now-cue (map get-context-prop cue-prop-list))
           (now-mid-c-off (get-context-prop 'middleCOffset))
           (now-staff-octaves (get-context-prop 'cnStaffOctaves))
           (now-clef-shift (get-context-prop 'cnClefShift))

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
                                 (trad-to-cn-clef
                                  (list-ref now-clef 0) (list-ref now-clef 1))))

          (if changed-clef-shift (set! prev-clef-shift now-clef-shift))

          (if (or changed-clef changed-staff changed-clef-shift)
              (begin
               (set! prev-clef (cn-get-clef-props
                                prev-clef-name
                                (list-ref now-clef 3)
                                (cn-get-staff-clef-adjust
                                 prev-staff-octaves prev-clef-shift)))

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
                                   (cn-get-staff-clef-adjust
                                    prev-staff-octaves prev-clef-shift)))

                   (set-context-props! context cue-prop-list prev-cue))))

          ;; new ottava? (8va 8vb etc.)
          (if (not (equal? now-mid-c-off prev-mid-c-off))
              (begin
               (set! prev-mid-c-off (* now-mid-c-off 12/7))
               (ly:context-set-property! context 'middleCOffset prev-mid-c-off)
               (ly:set-middle-C! context)
               )))))

       ;; copy clefTransposition context property to a custom Clef grob property
       (acknowledgers
        ((clef-interface engraver clef-grob source-engraver)
         (ly:grob-set-property! clef-grob 'cn-clef-transposition
           (ly:context-property context 'clefTransposition))))
       ))))


%--- CLEF GLYPHS

#(define cn-clef-curves
   ;; treble
   '(("clefs.G" .
       ((moveto 1.5506 4.76844)
        (curveto 1.5376 4.76844 1.5066 4.75114 1.5136 4.73384)
        (lineto 1.7544 4.17292)
        (curveto 1.8234 3.97367 1.8444 3.88334 1.8444 3.66416)
        (curveto 1.8444 3.16204 1.5635 2.76967 1.2174 2.38312)
        (lineto 1.0789 2.2278)
        (curveto 0.5727 1.68982 0 1.16441 0 0.45906)
        (curveto 0 -0.36713 0.6414 -1.05 1.4549 -1.05)
        (curveto 1.5319 -1.05 1.6984 -1.0492 1.8799 -1.0372)
        (curveto 2.0139 -1.0282 2.1594 -0.9969 2.2732 -0.9744)
        (curveto 2.3771 -0.9538 2.5752 -0.8757 2.5752 -0.8757)
        (curveto 2.7512 -0.8152 2.6612 -0.62915 2.5442 -0.6835)
        (curveto 2.5442 -0.6835 2.3481 -0.7626 2.2449 -0.7822)
        (curveto 2.1355 -0.803 1.9939 -0.8319 1.8645 -0.8382)
        (curveto 1.6935 -0.8462 1.5257 -0.8402 1.4569 -0.8352)
        (curveto 1.1541 -0.8139 0.8667 -0.67432 0.6558 -0.48763)
        (curveto 0.5148 -0.36284 0.3782 -0.17408 0.3582 0.12709)
        (curveto 0.3582 0.76471 0.792 1.23147 1.255 1.71365)
        (lineto 1.3978 1.86523)
        (curveto 1.8046 2.29959 2.185 2.75829 2.185 3.32815)
        (curveto 2.185 3.77846 1.9185 4.42204 1.6113 4.75678)
        (curveto 1.5983 4.76858 1.5713 4.77188 1.5513 4.76828)
        (closepath)))

     ;; bass
     ("clefs.F" .
       ((moveto 0.2656 0.78107)
        (curveto 0.3775 0.79547 0.4351 0.84567 0.7003 0.85587)
        (curveto 0.9459 0.86587 1.0531 0.85987 1.1805 0.83797)
        (curveto 1.6967 0.74937 2.1173 0.13032 2.1173 -0.64059)
        (curveto 2.1173 -2.10531 0.9987 -3.04975 0.019 -3.8078)
        (curveto 0 -3.8345 0 -3.846 0 -3.8652)
        (curveto 0 -3.9101 0.022 -3.94 0.056 -3.94)
        (curveto 0.071 -3.94 0.079 -3.93904 0.107 -3.9231)
        (curveto 1.3341 -3.23572 2.6095 -2.2656 2.6095 -0.57604)
        (curveto 2.6095 0.4711 2.0006 1.05061 1.1664 1.05061)
        (curveto 0.9058 1.05561 0.7658 1.05861 0.5568 1.02591)
        (curveto 0.4588 1.01061 0.248 0.97281 0.219 0.92831)
        (curveto 0.165 0.89151 0.162 0.77308 0.266 0.78129)
        (closepath)))

     ;; alto
     ("clefs.C" .
       ((moveto 1.0406 2.93878)
        (curveto 0.9606 2.93578 0.8881 2.93178 0.8237 2.92878)
        (lineto 0.8237 2.92846)
        (curveto 0.6586 2.92046 0.4659 2.89806 0.3697 2.87906)
        (curveto 0.1409 2.83386 0.0236 2.78916 0 2.75937)
        (curveto -0.018 2.73927 -0.015 2.71087 0 2.69037)
        (curveto 0.023 2.64587 0.145 2.67017 0.4188 2.72887)
        (curveto 0.5108 2.74867 0.6924 2.76597 0.8607 2.77257)
        (curveto 1.0868 2.78157 1.2883 2.70417 1.3194 2.69167)
        (curveto 1.7053 2.53668 2.0444 2.24033 2.0444 1.46855)
        (curveto 2.0444 0.8488 1.8942 0.04261 1.4629 0.04261)
        (curveto 1.4489 0.04061 1.4419 0.03861 1.4289 0.02891)
        (curveto 1.4149 0.01311 1.4179 0.00091 1.4169 -0.01179)
        (curveto 1.4169 -0.01193 1.4169 -0.01195 1.4169 -0.01211)
        (curveto 1.4169 -0.01225 1.4169 -0.01227 1.4169 -0.01243)
        (curveto 1.4169 -0.02513 1.4169 -0.03723 1.4289 -0.05313)
        (curveto 1.4389 -0.06213 1.4479 -0.06493 1.4629 -0.06683)
        (curveto 1.8942 -0.06683 2.0444 -0.87302 2.0444 -1.49278)
        (curveto 2.0444 -2.26455 1.7053 -2.56059 1.3194 -2.71559)
        (curveto 1.2884 -2.72799 1.0868 -2.80579 0.8607 -2.79679)
        (curveto 0.6924 -2.78979 0.5113 -2.77259 0.4188 -2.75279)
        (curveto 0.145 -2.69409 0.0231 -2.66979 0 -2.71429)
        (curveto -0.011 -2.73479 -0.014 -2.76349 0 -2.78359)
        (curveto 0.024 -2.81339 0.1409 -2.85799 0.3697 -2.90328)
        (curveto 0.4657 -2.92228 0.6586 -2.94468 0.8237 -2.95268)
        (lineto 0.8237 -2.953)
        (curveto 0.9525 -2.958 1.1126 -2.9714 1.305 -2.96)
        (curveto 1.9479 -2.916 2.5587 -2.47655 2.5587 -1.48844)
        (curveto 2.5587 -0.89409 2.1807 -0.20184 1.7065 -0.01218)
        (curveto 2.1807 0.17748 2.5587 0.86972 2.5587 1.46406)
        (curveto 2.5587 2.45218 1.9479 2.89194 1.305 2.93594)
        (curveto 1.209 2.94194 1.1207 2.94094 1.0406 2.93794)
        (closepath)))
     ))

#(define (cn-number-stencil grob number)
   "Returned stencils are centered horizontally, number must be 0-9."
   (ly:stencil-aligned-to
    (ly:font-get-glyph (ly:grob-default-font grob)
      (list-ref '("zero" "one" "two" "three" "four"
                   "five" "six" "seven" "eight" "nine") number))
    X 0))

#(define (cn-clef-stencil-callback clef-grob)

   ;; (glyph-name (ly:grob-property clef-grob 'glyph-name))
   ;; glyph-name is e.g. "clefs.G_change"
   ;; for if we ever want to deal with clef changes vs initial clefs

   (let* ((glyph (ly:grob-property clef-grob 'glyph))
          (curve-path (assoc-ref cn-clef-curves glyph)))
     (if curve-path
         (let*
          ((curve-stil (grob-interpret-markup clef-grob
                         (markup (#:override '(filled . #t) (#:path 0.0001 curve-path)))))
           (mult (magstep (ly:grob-property clef-grob 'font-size 0.0)))
           (scaled-curve (ly:stencil-scale curve-stil mult mult))

           (transpo (ly:grob-property clef-grob 'cn-clef-transposition))
           ;; bass clef default octave is 3, treble and alto are 4
           (default-octave (if (string=? "clefs.F" glyph) 3 4))
           (octave (+ default-octave (/ transpo 12)))
           (number-shift (cond
                          ((string=? glyph "clefs.G") (case octave
                                                        ((4) '(1.5 . -0.63))
                                                        ((6) '(1.6 . -0.63))
                                                        (else '(1.7 . -0.63))))
                          ((string=? glyph "clefs.F") '(1.0 . -1.33))
                          ((string=? glyph "clefs.C") '(0.9 . 0.48))
                          (else '(0 . 0))))
           (scale 0.9)
           (number-stil
            (ly:stencil-translate
             (ly:stencil-scale (cn-number-stencil clef-grob octave) scale scale)
             (cons
              (* mult (car number-shift))
              (* mult (cdr number-shift))))))

          (if (string=? glyph "clefs.C")
              (ly:stencil-add scaled-curve number-stil
                (ly:stencil-translate
                 (ly:stencil-scale
                  (cn-number-stencil clef-grob (1- octave)) scale scale)
                 (cons (* mult 0.9) (* mult -2.46))))

              (ly:stencil-add scaled-curve number-stil)))

         ;; else other clef e.g. percussion clef
         (ly:clef::print clef-grob))))


%--- REPEAT SIGN DOTS (BAR LINES) ----------------

%% adjust the position of dots in repeat signs
%% for Clairnote staff or traditional staff

#(add-bar-glyph-print-procedure ":"
   (lambda (grob extent)
     "A procedure that draws repeat sign dots at
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
       dot-positions))))


%--- TIME SIGNATURES ----------------

#(define (cn-time-signature-grob-callback grob)
   ;; "Adjust vertical position of time sig based on vertical staff scaling."
   (let*
    ((base-staff-space (ly:grob-property grob 'cn-base-staff-space))
     (vscale-staff (* 12/7 base-staff-space))
     (base-y-offset (* (+ vscale-staff -0.9) -2.5))

     ;; adjust position for odd octave staves and clefs shifted up/down an octave
     ;; note-space is the distance between two adjacent notes given vertical staff compression
     (note-space (* 0.5 base-staff-space))
     (staff-clef-adjust (cn-get-staff-clef-adjust
                         (ly:grob-property grob 'cn-staff-octaves)
                         (ly:grob-property grob 'cn-clef-shift)))

     (y-offset (+ base-y-offset (* note-space staff-clef-adjust)))

     ;; adjustment for \magnifyStaff
     (mag (cn-magnification2 grob))
     (final-y-offset (* y-offset mag)))

    (ly:grob-set-property! grob 'Y-offset final-y-offset)))


%--- STEM LENGTH AND DOUBLE STEMS ----------------

#(define (cn-grob-edge grob positive)
   "Takes a grob and returns the edge of the grob in positive
    or negative direction (up or down), positive arg is boolean."
   (let* ((offset (ly:grob-property grob 'Y-offset))
          (extent (ly:grob-property grob 'Y-extent))
          (extent-dir (if positive (cdr extent) (car extent))))
     (+ offset extent-dir)))

#(define (cn-grobs-edge grobs positive)
   "Takes a list of grobs and positive, a boolean of whether the
    direction we care about is positive/up or not/down, and returns
    the furthest edge of the grobs in that direction."
   (let* ((comparator (if positive > <))
          (final-edge
           (fold (lambda (g prev-edge)
                   (let ((this-edge (cn-grob-edge g positive)))
                     (if (comparator this-edge prev-edge)
                         this-edge
                         prev-edge)))
             (cn-grob-edge (car grobs) positive)
             (cdr grobs))))
     final-edge))

#(define (cn-double-stem grob)
   (let*
    ((stem-stil (ly:stem::print grob))
     (dir (ly:grob-property grob 'direction))
     (up-stem (= dir 1))

     ;; --- X / width / spacing ---

     (stem-x-extent (ly:grob-property grob 'X-extent))
     (stem-width (abs (- (car stem-x-extent) (cdr stem-x-extent))))

     ;; by default second stem is 1.5 times as thick as standard stem
     (width-scale (ly:grob-property grob 'cn-double-stem-width-scale 1.5))
     (stem2-width (* stem-width width-scale))

     ;; amount to move the edges outward to achieve 2nd stem width
     (width-shift (/ (abs (- stem-width stem2-width)) 2))

     ;; spacing is 3.5 times stem-width by default
     (spacing-scale (ly:grob-property grob 'cn-double-stem-spacing 3.5))
     (spacing-shift (* dir spacing-scale stem-width))

     (stem-left-edge (car stem-x-extent))
     (stem-right-edge (cdr stem-x-extent))

     (stem2-left-edge (+ spacing-shift stem-left-edge (* -1 width-shift)))
     (stem2-right-edge (+ spacing-shift stem-right-edge width-shift))

     ;; Note that SVG output needs extents pairs to be in proper ascending order
     (stem2-x-extent (cons stem2-left-edge stem2-right-edge))

     ;; --- Y / length ---

     (stem-y-extent (ly:grob-property grob 'Y-extent))
     (note-heads (cn-note-heads-from-grob grob '()))
     (heads-edge (cn-grobs-edge note-heads up-stem))
     (stem-tip (if up-stem (cdr stem-y-extent) (car stem-y-extent)))

     (stem2-y-extent (if up-stem
                         (cons heads-edge stem-tip)
                         (cons stem-tip heads-edge)))

     (blot (ly:output-def-lookup (ly:grob-layout grob) 'blot-diameter))

     (stem2-stil (ly:round-filled-box stem2-x-extent stem2-y-extent blot)))

    (ly:grob-set-property! grob 'stencil
      (ly:stencil-add stem-stil stem2-stil))
    ;; X-extent needs to be set here because its usual callback
    ;; ly:stem::width doesn't take the actual stencil width into account
    (ly:grob-set-property! grob 'X-extent
      (ly:stencil-extent (ly:grob-property grob 'stencil) 0))
    ))

#(define (cn-multiply-details details multiplier skip-list)
   "multiplies each of the values of a details property
    (e.g. of the stem grob) by multiplier, except for
    skip-list, a list of symbols, e.g. '(stem-shorten) "
   (define multiply-by (lambda (x) (* x multiplier)))
   (map
    (lambda (dt)
      (let ((head (car dt))
            (vals (cdr dt)))
        (cons head (if (memq head skip-list)
                       vals
                       (map multiply-by vals)))))
    details))

#(define (cn-customize-stem grob)
   "Lengthen all stems to undo staff compression side effects,
    and give half notes double stems."
   (let* ((bss-inverse (/ 1 (ly:grob-property grob 'cn-base-staff-space)))
          (deets (ly:grob-property grob 'details))
          (deets2 (cn-multiply-details deets bss-inverse '(stem-shorten))))

     (ly:grob-set-property! grob 'details deets2)

     ;; double stems for half notes
     (if (= 1 (ly:grob-property grob 'duration-log))
         (cn-double-stem grob)
         )))

#(define (cn-stem-grob-callback grob)
   "Make sure omit is not in effect (i.e. stencil is not #f)
    and the stem has a notehead (i.e. is not for a rest,
    rest grobs have stem grobs that have no stencil)"
   (if (and (ly:grob-property-data grob 'stencil)
            (not (null? (ly:grob-object grob 'note-heads))))
       (cn-customize-stem grob)
       ))

%--- CROSS-STAFF STEMS ----------------

% needed for cross-staff stems for double stemmed half notes
% code modified from music-functions.scm -- used with \crossStaff function
% procedures have been encapsulated inside cn-make-stem-spans!

#(define (cn-make-stem-spans! ctx stems trans)
   "Create stem spans for cross-staff stems"

   (define (close-enough? x y)
     "Values are close enough to ignore the difference"
     (< (abs (- x y)) 0.0001))

   (define (extent-combine extents)
     "Combine a list of extents"
     (if (pair? (cdr extents))
         (interval-union (car extents) (extent-combine (cdr extents)))
         (car extents)))

   (define ((stem-connectable? ref root) stem)
     "Check if the stem is connectable to the root"
     ;; The root is always connectable to itself
     (or (eq? root stem)
         (and
          ;; Horizontal positions of the stems must be almost the same
          (close-enough? (car (ly:grob-extent root ref X))
            (car (ly:grob-extent stem ref X)))
          ;; The stem must be in the direction away from the root's notehead
          (positive? (* (ly:grob-property root 'direction)
                       (- (car (ly:grob-extent stem ref Y))
                         (car (ly:grob-extent root ref Y))))))))

   (define (stem-span-stencil span)
     "Connect stems if we have at least one stem connectable to the root"
     (let* ((system (ly:grob-system span))
            (root (ly:grob-parent span X))
            (stems (filter (stem-connectable? system root)
                           (ly:grob-object span 'stems))))
       (if (<= 2 (length stems))
           (let* ((yextents (map (lambda (st)
                                   (ly:grob-extent st system Y)) stems))
                  (yextent (extent-combine yextents))
                  (layout (ly:grob-layout root))
                  (blot (ly:output-def-lookup layout 'blot-diameter))

                  ;; START CLAIRNOTE EDITS

                  ;; we just get the 1st notehead, does it matter which?
                  (note-head (list-ref (cn-note-heads-from-grob root '()) 0)))

             ;; if is half note...
             (if (and note-head (= 1 (ly:grob-property note-head 'duration-log)))
                 ;; TODO: better cross staff double stems for half notes,
                 ;; and un-hardcode the following stem X extent, works
                 ;; fine with 'set-global-staff-size' but ugly with 'magnifyStaff'
                 (ly:round-filled-box '(-0.065 . 0.065) yextent blot)

                 (begin
                  ;; Hide spanned stems
                  (for-each (lambda (st)
                              (set! (ly:grob-property st 'stencil) #f))
                    stems)
                  ;; Draw a nice looking stem with rounded corners
                  (ly:round-filled-box (ly:grob-extent root root X) yextent blot))))

           ;; END CLAIRNOTE EDITS

           ;; Nothing to connect, don't draw the span
           #f)))

   (define ((make-stem-span! stems trans) root)
     "Create a stem span as a child of the cross-staff stem (the root)"
     (let ((span (ly:engraver-make-grob trans 'Stem '())))
       (ly:grob-set-parent! span X root)
       (set! (ly:grob-object span 'stems) stems)
       ;; Suppress positioning, the stem code is confused by this weird stem
       (set! (ly:grob-property span 'X-offset) 0)
       (set! (ly:grob-property span 'stencil) stem-span-stencil)))

   (define (stem-is-root? stem)
     "Check if automatic connecting of the stem was requested.  Stems connected
      to cross-staff beams are cross-staff, but they should not be connected to
      other stems just because of that."
     (eq? cross-staff-connect (ly:grob-property-data stem 'cross-staff)))

   ;; make-stem-spans! continued..

   ;; Cannot do extensive checks here, just make sure there are at least
   ;; two stems at this musical moment
   (if (<= 2 (length stems))
       (let ((roots (filter stem-is-root? stems)))
         (for-each (make-stem-span! stems trans) roots))))

% overwrites the default engraver in order to call custom cn-make-stem-spans!
#(define (Span_stem_engraver ctx)
   "Connect cross-staff stems to the stems above in the system"
   (let ((stems '()))
     (make-engraver
      ;; Record all stems for the given moment
      (acknowledgers
       ((stem-interface trans grob source)
        (set! stems (cons grob stems))))
      ;; Process stems and reset the stem list to empty
      ((process-acknowledged trans)
       (cn-make-stem-spans! ctx stems trans)
       (set! stems '())))))


%--- DOTS ON DOTTED NOTES ----------------

#(define (cn-highest-semitone note-heads)
   (reduce (lambda (a b) (if (> a b) a b))
     0
     (map cn-notehead-semitone note-heads)))

#(define (cn-dots-callback dots-grob)
   "Avoid collision between double-stem and dots by shifting right the dots on
    double-stemmed half notes but only when they are on a staff line, have an
    up stem, and are the highest note in their column.
    We use a callback here so that the stem width is already set.
    Returns a pair of numbers or #f for the Dots.extra-offset grob property."
   (let*
    ((parent (ly:grob-parent dots-grob Y))
     ;; parent is a Rest grob or a NoteHead grob
     (note-head (and (not (grob::has-interface parent 'rest-interface)) parent))
     (semi '())
     (stem '()))
    (and note-head

         ;; is half note?
         (= 1 (ly:grob-property note-head 'duration-log))

         ;; is line note?
         (begin (set! semi (cn-notehead-semitone note-head))
           (= 0 (modulo semi 4)))

         ;; is up-stem?
         (begin (set! stem (ly:grob-object (ly:grob-parent note-head X) 'stem))
           stem)
         (not (null? stem))
         (= 1 (ly:grob-property stem 'direction))

         ;; is highest note?
         (let* ((note-heads (cn-note-heads-from-grob stem '())))
           (or (= 1 (length note-heads))
               (= semi (cn-highest-semitone note-heads))))

         ;; return a pair for Dots.extra-offset
         ;; maybe a better calculation is possible based on custom
         ;; double stem Staff context properties, but that would take
         ;; an engraver to access it, and Dots engraver is in Voice context
         (let* ((stem-extent (ly:grob-property stem 'X-extent))
                (stem-width (- (cdr stem-extent) (car stem-extent)))
                (x-offset (* 0.75 stem-width)))
           (cons x-offset 0))
         )))


%--- BEAMS ----------------

#(define (cn-beam-grob-callback grob)
   "Adjust size and spacing of beams. Needed due to vertically compressed staff."
   (let* ((bss-inverse (/ 1 (ly:grob-property grob 'cn-base-staff-space)))
          (thick (ly:grob-property grob 'beam-thickness))
          (len-frac (ly:grob-property grob 'length-fraction))
          (space (if (number? len-frac) len-frac 1)))

     ;; TODO: the 1.1 adjustment below was just visually estimated
     (ly:grob-set-property! grob 'length-fraction (* space 1.1 bss-inverse))
     (ly:grob-set-property! grob 'beam-thickness (* thick bss-inverse))
     ))

%--- LEDGER LINES ----------------

%% default, gradual, conservative
#(define cn-ledgers-gradual '(2 2 2 5))

%% jumps to two ledger lines immediately,
%% omits c ledger line quickly
#(define cn-ledgers-less-gradual '(2 2 5 2))

%% no ledger-extra, solid notes in center of spaces don't get
%% a ledger beyond them, but float in space.

#(define (cn-ledger-pattern dist staff-symbol)
   "Produces the ledger line pattern for a given note."
   ;; dist is distance of note from the closest staff line
   ;; extra-4, extra-8, extra-12 work like LilyPond's ledger-extra property
   ;; but tied to their specific ledger lines. They determine when ledgers
   ;; start to appear beyond the note (default is 2, other good values are 1, 5, 6).
   ;; hide-4 determines when 4-position lines (C) start to disappear
   ;; (2 or 5 make sense, 0-7 are possible)
   ;; TODO? provide a way to show all C ledgers?
   (let*
    ((recipe (ly:grob-property staff-symbol 'cn-ledger-recipe cn-ledgers-gradual))
     (extra-4 (list-ref recipe 0))
     (extra-8 (list-ref recipe 1))
     (extra-12 (list-ref recipe 2))
     (hide-4 (list-ref recipe 3))
     (rem (remainder dist 12))
     (base (* 12 (quotient dist 12)))

     ;; list of positions from 0 to base, cycling at 8 and 12 (E and G#/Ab)
     (lrs (reverse (merge
                    (iota (quotient base 12) 12 12)
                    (iota (quotient (+ 4 base) 12) 8 12)
                    <)))

     (get-ledger (lambda (pos extra rem base)
                   (if (<= (- pos extra) rem)
                       (list (+ pos base))
                       '())))

     (lr12 (get-ledger 12 extra-12 rem base))
     (lr8 (get-ledger 8 extra-8 rem base))

     (lr4 (if (<= rem (+ 4 hide-4))
              (get-ledger 4 extra-4 rem base)
              '()))

     (result (append lr12 lr8 lr4 lrs)))
    result))

#(define cn-ledger-positions
   ;; A function that takes a StaffSymbol grob and a vertical
   ;; position of a note head and returns a list of ledger line positions,
   ;; based on StaffSymbol.cn-ledger-recipe."
   '(lambda (staff-symbol pos)
      (let*
       ((lines (ly:grob-property staff-symbol 'line-positions '(-8 -4 4 8)))

        (nearest-line
         (fold (lambda (line prev)
                 (if (< (abs (- line pos)) (abs (- prev pos)))
                     line
                     prev))
           (car lines)
           (cdr lines)))

        (diff (- pos nearest-line))
        (dist (abs diff))
        (dir (if (negative? diff) -1 1))

        ;; get generic ledger positions and then transform them so
        ;; they are relative to nearest-line and in the right direction
        (ledgers0 (cn-ledger-pattern dist staff-symbol))
        (ledgers1 (map (lambda (n) (+ nearest-line (* dir n)))
                    ledgers0))

        ;; remove any ledgers that would fall on staff lines
        (ledgers2 (filter (lambda (n) (not (member n lines)))
                          ledgers1)))
       ledgers2)))


%--- USER: EXTENDING STAVES & DIFFERENT OCTAVE SPANS ----------------

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

#(define cnStaffExtender
   (define-music-function
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
    #}))

#(define cnExtendStaffUp #{ \cnStaffExtender ##f 1 0 #})
#(define cnExtendStaffDown #{ \cnStaffExtender ##f 0 1 #})
#(define cnUnextendStaffUp #{ \cnStaffExtender ##f -1 0 #})
#(define cnUnextendStaffDown #{ \cnStaffExtender ##f 0 -1 #})

#(define cnStaffOctaveSpan
   (define-music-function (parser location octaves) (positive-integer?)
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
        \override Staff.KeySignature.cn-staff-octaves = #octaves
        \override Staff.TimeSignature.cn-staff-octaves = #octaves
        \set Staff.cnBaseStaffLines = #base-lines
        \override Staff.StaffSymbol.ledger-positions = #base-lines
        \cnStaffExtender ##t #upwards #downwards
      #})))

#(define cnClefPositionShift
   (define-music-function (parser location octaves) (integer?)
     #{
       \set Staff.cnClefShift = #octaves
       \override Staff.KeySignature.cn-clef-shift = #octaves
       \override Staff.TimeSignature.cn-clef-shift = #octaves
     #}))


%--- USER: ALTERNATE STAVES (EXPERIMENTAL) ----------------

#(define cnFiveLineStaff
   #{
     \cnStaffOctaveSpan 2
     \override Staff.StaffSymbol.line-positions = #'(-8 -4 0 4 8)
   #})

#(define cnFourLineStaff
   #{
     \cnStaffOctaveSpan 2
     \override Staff.StaffSymbol.line-positions = #'(-8 -4 0 4)
   #})


%--- USER: SET STAFF COMPRESSION ----------------

%% must be used before \magnifyStaff for both to work
#(define cnStaffCompression
   (define-music-function (parser location ss) (number?)
     "0.75 is the default Clairnote staff-space (ss). An ss arg of
      1 gives an uncompressed staff. 7/12 gives a staff with
      same size octave as traditional"
     (let*
      ((trad-octave (/ (round (* 10000 (exact->inexact (* 12/7 ss)))) 10000))
       (notehead-overlap (+ 0.5 (- 0.5 (/ ss 2)))))
      (ly:message
       "Clairnote: custom staff compression of ~a will produce octaves ~a times the size of octaves in traditional notation; adjacent note heads (a semitone apart) will overlap by about ~a of their height."
       ss trad-octave notehead-overlap)
      #{
        \set Staff.cnBaseStaffSpace = #ss
        \override Staff.Beam.cn-base-staff-space = #ss
        \override Staff.KeySignature.cn-base-staff-space = #ss
        \override Staff.Stem.cn-base-staff-space = #ss
        \override Staff.StaffSymbol.staff-space = #ss
      #})))


%--- USER: CUSTOMIZE NOTEADS ----------------

#(define cnNoteheadStyle
   (define-music-function (parser location style) (string?)
     (cond

      ((equal? style "funksol")
       #{
         \set Staff.cnNoteheadStencilProcedure = #cn-funksol-note-stencil
         % 1.4 results in approx. width of standard LilyPond noteheads.
         \set Staff.cnNoteheadWidthScale = #1.35
         \set Staff.cnNoteheadHeightScale = #1
         \set Staff.cnNoteheadRotation = ##f
         % \set Staff.cnStemAttachment = ##f
         \set Staff.cnStemAttachment = #'((1 . 0.2) . (1 . 0.2))
       #})

      ((equal? style "lilypond")
       #{
         \set Staff.cnNoteheadStencilProcedure = #cn-lilypond-note-stencil
         \set Staff.cnNoteheadWidthScale = #1
         \set Staff.cnNoteheadHeightScale = #1
         % black notes can be rotated as far as -27,
         % but -18 also works for white notes, currently -9
         \set Staff.cnNoteheadRotation = #-9
         \set Staff.cnStemAttachment = #'((1.04 . 0.3) . (1.06 . 0.3))
       #})

      (else
       (if (not (equal? style "default"))
           (ly:warning
            "unrecognized style ~s used with \\cnNoteheadStyle, using default instead."
            style))
       #{
         \set Staff.cnNoteheadStencilProcedure = #cn-default-note-stencil
         \set Staff.cnNoteheadWidthScale = #1
         \set Staff.cnNoteheadHeightScale = #1
         \set Staff.cnNoteheadRotation = #0
         \set Staff.cnStemAttachment = ##f
       #})
      )))


%--- CUSTOM CONTEXT PROPERTIES ----------------

#(let*
  ;; translator-property-description function
  ;; from "scm/define-context-properties.scm" (modified)
  ((add-prop
    (lambda (symbol type?)
      (set-object-property! symbol 'translation-type? type?)
      (set-object-property! symbol 'translation-doc "custom context property")
      (set! all-translation-properties (cons symbol all-translation-properties))
      symbol)))

  ;; All are Staff context properties unless otherwise noted.

  ;; Stores the base staff-space to store the vertical compression of the
  ;; Clairnote staff. The actual staff-space may differ with \magnifyStaff, etc.
  ;; Stem and beam size, time sig and key sig position, etc. depend on it.
  (add-prop 'cnBaseStaffSpace positive?)

  ;; Stores the base staff line positions used for extending the staff
  ;; up or down. See cnExtendStaff function.
  (add-prop 'cnBaseStaffLines list?)

  ;; For note head styles
  (add-prop 'cnNoteheadStencilProcedure procedure?)
  (add-prop 'cnNoteheadWidthScale non-zero?)
  (add-prop 'cnNoteheadHeightScale non-zero?)
  (add-prop 'cnNoteheadRotation number?)

  ;; cnStemAttachment should be a pair of pairs: '((1 . 2) . (3 . 4))
  ;; the first for black notes, the second for white notes
  (add-prop 'cnStemAttachment scheme?)

  ;; Indicates number of octaves the staff spans, lets us use
  ;; different clef settings so stems always flip at center of staff
  (add-prop 'cnStaffOctaves positive-integer?)

  ;; For shifting clef position up or down an octave
  (add-prop 'cnClefShift integer?))


%--- CUSTOM GROB PROPERTIES ----------------

#(let*
  ;; define-grob-property function
  ;; from "scm/define-grob-properties.scm" (modified)
  ((add-grob-prop
    (lambda (symbol type?)
      (set-object-property! symbol 'backend-type? type?)
      (set-object-property! symbol 'backend-doc "custom grob property")
      symbol)))

  ;; StaffSymbol.cn-is-clairnote-staff is used for repeat sign dots.
  (add-grob-prop 'cn-is-clairnote-staff boolean?)

  ;; KeySignature.cn-tonic tonic note of the key.
  ;; The 'tonic context property turned into a grob property.
  (add-grob-prop 'cn-tonic ly:pitch?)

  ;; For double stems for half notes.
  (add-grob-prop 'cn-double-stem-spacing number?)
  (add-grob-prop 'cn-double-stem-width-scale non-zero?)

  ;; StaffSymbol.cn-ledger-recipe used to produce ledger line pattern.
  (add-grob-prop 'cn-ledger-recipe number-list?)

  ;; Clef.cn-clef-transposition makes clef transposition accessible from clef grobs
  (add-grob-prop 'cn-clef-transposition integer?)

  ;; The base staff-space for the vertical compression of the Clairnote staff.
  ;; The actual staff-space may differ with \magnifyStaff, etc.
  ;; Stem and beam size, time sig and key sig position, etc. depend on it.
  (add-grob-prop 'cn-base-staff-space positive?)

  ;; Indicates number of octaves the staff spans, lets us use
  ;; different clef settings so stems always flip at center of staff
  (add-grob-prop 'cn-staff-octaves positive-integer?)

  ;; For shifting clef position up or down an octave
  (add-grob-prop 'cn-clef-shift integer?))


%--- LEGACY SUPPORT FOR LILYPOND 2.18.2 ETC. ----------------

%--- LEGACY DOTS ON DOTTED NOTES ----------------

#(if (ly:version? < '(2 19 18))
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


%--- LEGACY CHORDS ----------------

%% cn-shift-notehead, copied and modified from the LilyPond Snippet
%% Repository, snippet 861, "Re-positioning note heads on the
%% opposite side of the stem," http://lsr.di.unimi.it/LSR/Item?id=861
%% Thanks to David Nalesnik and Thomas Morley for work on that snippet.
%% Use that snippet for any manual adjustments of note head positions.

#(if
  (ly:version? < '(2 19 34))
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
     (let* ((nhs-raw (cn-note-heads-from-grob
                      grob
                      ;; when rests, no note heads in NoteColumn grob
                      '(0))))
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

%--- END LEGACY SUPPORT SECTION ----------------


%--- STAFF CONTEXT DEFINITION ----------------

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
    cnNoteheadStencilProcedure = #cn-default-note-stencil
    cnStaffOctaves = #2
    cnClefShift = #0

    % grob property overrides
    \override Stem.cn-base-staff-space = #0.75
    \override Beam.cn-base-staff-space = #0.75
    \override KeySignature.cn-base-staff-space = #0.75
    \override TimeSignature.cn-base-staff-space = #0.75

    \override KeySignature.cn-staff-octaves = #2
    \override TimeSignature.cn-staff-octaves = #2

    \override KeySignature.cn-clef-shift = #0
    \override TimeSignature.cn-clef-shift = #0

    \override StaffSymbol.line-positions = #'(-8 -4 4 8)
    \override StaffSymbol.ledger-positions = #'(-8 -4 0 4 8)

    \override Stem.no-stem-extend = ##t
    \override Stem.cn-double-stem-spacing = #3.5
    \override Stem.cn-double-stem-width-scale = #1.5
    \override Stem.before-line-breaking = #cn-stem-grob-callback

    \override Beam.before-line-breaking = #cn-beam-grob-callback
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()

    \override KeySignature.horizontal-skylines = #'()
    \override KeySignature.before-line-breaking = #cn-key-signature-grob-callback
    \override KeyCancellation.horizontal-skylines = #'()
    \override KeyCancellation.before-line-breaking =#cn-key-signature-grob-callback

    \override TimeSignature.before-line-breaking = #cn-time-signature-grob-callback

    % TODO: whole note ledger lines are a bit too wide
    \override LedgerLineSpanner.length-fraction = 0.45
    \override LedgerLineSpanner.minimum-length-fraction = 0.35

    \override Clef.stencil = #cn-clef-stencil-callback
    \override CueClef.stencil = #cn-clef-stencil-callback
    \override CueEndClef.stencil = #cn-clef-stencil-callback
    \override ClefModifier.stencil = ##f

    % empty else clauses are needed for 2.18 compatibility
    #(if (ly:version? >= '(2 19 34))
         #{
           \override Stem.note-collision-threshold = 2
           \override NoteCollision.note-collision-threshold = 2
         #}
         #{ #})

    % LilyPond bug with "ledger-extra = 2" before 2.19.36
    #(if (ly:version? >= '(2 19 36))
         #{
           \override StaffSymbol.ledger-extra = 2
         #}
         #{
           \override StaffSymbol.ledger-extra = 1
         #})

    #(if (ly:version? >= '(2 19 42))
         #{
           \override StaffSymbol.ledger-positions-function = #cn-ledger-positions
           \override StaffSymbol.cn-ledger-recipe = #cn-ledgers-gradual
         #}
         #{ #})

    % NoteColumn override doesn't work as an engraver for some reason,
    % crashes with manual beams on chords.
    #(if (ly:version? < '(2 19 34))
         #{
           \override NoteColumn.before-line-breaking = #cn-note-column-callback
         #})

    % staff-space reflects vertical compression of Clairnote staff.
    % Default of 0.75 makes the Clairnote octave 1.28571428571429
    % times the size of the traditional octave (3/4 * 12/7 = 9/7).
    % Adjacent note heads overlap by 0.625 (5/8).
    \override StaffSymbol.staff-space = #0.75

    % adjust x-axis dots position to not collide with double-stemmed half notes
    \override Dots.extra-offset = #cn-dots-callback

    % custom engravers
    \consists \Cn_clef_ottava_engraver
    \consists \Cn_key_signature_engraver
    \consists \Cn_note_heads_engraver

    #(if (ly:version? < '(2 19 18))
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
