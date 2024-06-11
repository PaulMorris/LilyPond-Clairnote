(define-module (clairnote ledger))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (lily)
  (clairnote utils)
  (clairnote staff)
  (clairnote region)
  (clairnote stencil))

(define (cn-ledger-pattern dist staff-symbol)
  ;; Produces the ledger line pattern for a given note.
  ;; dist is distance of note from closest staff line.

  ;; A cycle is generally an octave, so cycle-size is
  ;; 12 (staff positions per octave) and cycle-count
  ;; is the number of octaves we are dealing with.
  (let* ((recipe (ly:grob-property staff-symbol 'cn-ledger-recipe '(12 ())))
         (cycle-size (list-ref recipe 0))
         (configs (list-ref recipe 1))
         (cycle-count (+ 1 (quotient dist cycle-size)))
         (config-to-ledgers
           (lambda (config)
             (let*
               ((ledger-posn (list-ref config 0))
                 (further-add (list-ref config 1))
                 (nearer-drop (list-ref config 2))
                 (furthest (+ dist further-add))
                 (nearest (if nearer-drop (- dist nearer-drop) 0))
                 ;; Generate the list of ledgers in descending order.
                 (ledgers (reverse (iota cycle-count ledger-posn cycle-size)))
                 (in-range (lambda (l) (and (<= l furthest)
                                        (>= l nearest)))))
               (filter in-range ledgers))))
         (ledger-lists (map config-to-ledgers configs))

         ;; When there is an accidental sign on a note, LilyPond may
         ;; shorten the furthest ledger, the first in the list.
         ;; So we merge into one list, keeping descending order
         (ledger-list (fold (lambda (lst result) (merge lst result >))
                       '()
                       ledger-lists))
         ;; But if the note is on a ledger, move that ledger to the
         ;; front of the list, so that ledger will be shortened.
         (ledger-list2 (if (memv dist ledger-list)
                        (cons dist (delv dist ledger-list))
                        ledger-list)))
    ledger-list2))

(define (cn-ledger-positions staff-symbol pos)
  ;; A function that takes a StaffSymbol grob and a vertical
  ;; position of a note head and returns a list of ledger line positions,
  ;; based on StaffSymbol.cn-ledger-recipe."
  (let* ((lines (ly:grob-property staff-symbol 'line-positions '(-8 -4 4 8)))
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
    ledgers2))

(define-public (NoteHead-ledger-positions grob)
  (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
         (ctx (ly:grob-property staff-symbol 'cn-internal-staff-cached))
         (coord-base (assq-ref ctx #:coord-base))
         (x-loc (grob-coord grob coord-base X))
         (y-loc (ly:grob-property grob 'staff-position))
         (staff-lines
           (region-y-profile
             (intersect
               (assq-ref ctx #:avoid-union)
               (region-box x-loc x-loc -inf.0 +inf.0))))

         (closest-dist +inf.0)
         (closest *unspecified*)
         (closest-dir *unspecified*)
         (no-ledger #f))

    (dolist! ((y1 . y2) staff-lines)
      (when (<= y1 y-loc y2)
        (set! no-ledger #t))
      (let ((d (- y1 y-loc)))
        (when (< 0 d closest-dist)
          (set! closest-dist d)
          (set! closest y1)
          (set! closest-dir DOWN)))
      (let ((d (- y-loc y2)))
        (when (< 0 d closest-dist)
          (set! closest-dist d)
          (set! closest y2)
          (set! closest-dir UP))))

    (if (or no-ledger (null? staff-lines))
      '()
      (let* ((ledgers0 (cn-ledger-pattern closest-dist staff-symbol))
             (ledgers1 (map (lambda (n) (+ closest (* closest-dir n))) ledgers0)))

        ;; remove any ledgers that would fall on staff lines
        (map car
          (range-subtract
            (range-simplify (map (lambda (x) (cons x x)) ledgers1))
            staff-lines))))))

;; (define-public (LedgerLineSpanner-stencil grob)
;;   empty-stencil)

(define-public (script-y-offset-callback-unpure grob default-value)
  (match-let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
               (ctx (ly:grob-property staff-symbol 'cn-internal-staff-cached))
               (coord-base (assq-ref ctx #:coord-base))

               (half-staff-space (/ (ly:grob-property staff-symbol 'staff-space 1) 2))

               (direction (ly:grob-property grob 'direction))

               ((left-f . right-f) (grob-extent grob coord-base X))

               ((y1 . y2) (grob-extent grob grob Y))

               (left (ensure-exact-or-inf left-f 1/100))
               (right (ensure-exact-or-inf right-f 1/100))

               (vert-reg (region-box left right -inf.0 +inf.0))

               (avoid-y-range (region-y-profile
                               (intersect
                                 (assq-ref ctx #:avoid-with-real-ycoord)
                                 vert-reg))))

    (set! avoid-y-range
      (range-simplify
        (map-list ((a . b) avoid-y-range)
          (cons
            (- (- a (* 1/2 half-staff-space)) y2)
            (- (+ b (* 1/2 half-staff-space)) y1)))))

    (dolist! ((x . y) avoid-y-range)
      (when (< x default-value y)
        (if (< direction 0)
          ;; move at most 1 half-staff-space
          (if (close? default-value x half-staff-space)
            (set! default-value x))
          (if (close? default-value y half-staff-space)
            (set! default-value y)))))

    default-value))

(define-public script-y-offset-callback
  ;; TODO: maybe use grob transformer instead?
  (grob::compose-function
    (ly:make-unpure-pure-container
      script-y-offset-callback-unpure
      (lambda (grob start end v)
        v))
    side-position-interface::y-aligned-side))
