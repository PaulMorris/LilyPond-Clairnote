(define-module (clairnote staff))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (srfi srfi-8)
  (srfi srfi-17)
  (srfi srfi-43)
  (lily)
  (clairnote utils)
  (clairnote region)
  (clairnote stencil))

(grob-prop 'cn-staff-range
  (lambda (x)
    (and (pair? x)
      (exact? (car x))
      (exact? (cdr x)))))
(grob-prop 'cn-internal-staff-range-engraved pair?)

(define-public (StaffSymbol-cn-staff-range grob)
  (ly:grob-property grob 'cn-internal-staff-range-engraved '(-8 . 8)))

(context-prop 'cnInternalStaffRange pair?)

;; this shifts staff-position of notes by some integer amount
;; currently only exposed through \cnExperimentalYshift
;; imo this is bad since it makes 'staff-position of stuff different from what the user expects
(context-prop 'cnInternalYshift integer?)
(grob-prop 'cn-internal-yshift integer?)

(define-public (internal-extend-staff context reset going-up going-down)
  (match-let* (((a . b) (ly:context-property context 'cnInternalStaffRange '(-8 . 8)))
               (newa (- (if reset -8 a) (* 12 going-down)))
               (newb (+ (if reset 8 b) (* 12 going-up))))
    (set! (ly:context-property context 'cnInternalStaffRange) (cons newa newb))))

(define-public (internal-set-yshift context val)
  (set! (ly:context-property context 'cnInternalYshift) val))

(grob-prop 'cn-internal-staff-cached scheme?)

(define-public (StaffSymbol-line-positions grob)
  (match-let* ((shift (ly:grob-property-data grob 'cn-internal-yshift))
               ((x . y) (ly:grob-property grob 'cn-staff-range '(-8 . 8)))
               (l (+ x shift))
               (u (+ y shift)))
    (if (< l u)
      (list l u)
      (if (eqv? l u)
        (list l)
        '()))))

(define-public (compute-staff-regions staff-symbol)
  (let* ((vert-group (ly:grob-object staff-symbol 'axis-group-parent-Y))
         (ledger-line-spanner #f)
         (note-heads '())
         (length-fraction 0.25)
         (staff-space (ly:grob-property staff-symbol 'staff-space 1))
         (yshift (ly:grob-property-data staff-symbol 'cn-internal-yshift))
         (region-as-real-ycoord
           (lambda (region)
             (region-mapbox region
               (lambda (x1 x2 y1 y2)
                 (region-box x1 x2
                   (* (+ y1 yshift) staff-space 1/2)
                   (* (+ y2 yshift) staff-space 1/2)))))))

    (dolist! (e (grob-array->list-safe (ly:grob-object vert-group 'elements)))
      (when (grob::has-interface e 'ledger-line-spanner-interface)
        (when (eq? (ly:grob-object e 'staff-symbol) staff-symbol)
          (assert! (not ledger-line-spanner))
          (set! ledger-line-spanner e))))

    (when ledger-line-spanner
      (set! length-fraction
        (ly:grob-property ledger-line-spanner 'length-fraction 0.25))

      (set! note-heads
        (grob-array->list-safe
          (ly:grob-object ledger-line-spanner 'note-heads))))

    (let ((ctx `((#:cn-internal-yshift . ,yshift)
                 (#:staff-symbol . ,staff-symbol)
                 (#:coord-base . ,staff-symbol)
                 (#:note-heads . ,note-heads)
                 (#:debug-regions . ,(list))
                 (#:LedgerLineSpanner-length-fraction . ,length-fraction)
                 (#:region-as-real-ycoord . ,region-as-real-ycoord)
                 (#:avoid . ,(list))
                 (#:draw . ,(list))))
          (staff-fn (ly:grob-property-data staff-symbol 'cn-staff)))

      ;; (benchmark
      ;;   (staff-fn ctx))
      (staff-fn ctx)

      (assq-setf! ctx #:avoid-union
        (apply union (assq-ref ctx #:avoid)))

      ;; (select-remainder-y-range (assq-ref ctx #:avoid-union) 12 4 8)
      ;; (region-debug ctx
      ;;   (select-remainder-y (shift-y (assq-ref ctx #:avoid-union) -4 0) 4 12))
      ;; (region-debug ctx (assq-ref ctx #:avoid-union))

      (assq-setf! ctx #:avoid-with-real-ycoord
        (region-as-real-ycoord (assq-ref ctx #:avoid-union)))

      ;; (dbg (assq-ref ctx #:avoid))
      ;; (dbg (assq-ref ctx #:avoid-with-real-ycoord))

      ctx)))

(define-public (StaffSymbol-stencil staff-symbol)

  (let* ((ctx (ly:grob-property staff-symbol 'cn-internal-staff-cached))
         (yshift (assq-ref ctx #:cn-internal-yshift))
         (staff-space (ly:grob-property staff-symbol 'staff-space 1))
         (thickness
           (* (ly:grob-property staff-symbol 'thickness 1.0)
             (ly:output-def-lookup (ly:grob-layout staff-symbol) 'line-thickness)))
         (tmp-ledger-thickness (ly:grob-property staff-symbol 'ledger-line-thickness '(1.0 . 0.1)))
         (ledger-thickness (+
                            (* (car tmp-ledger-thickness) thickness)
                            (* (cdr tmp-ledger-thickness) staff-space)))
         (inherit-props `((staff
                           (#:thickness . ,thickness))
                          (ledger
                           (#:thickness . ,ledger-thickness))))
         (res-stencil empty-stencil)
         (region-as-real-ycoord (assq-ref ctx #:region-as-real-ycoord)))

    (for-each! (reverse (assq-ref ctx #:draw)) spec
      (let* ((region (region-as-real-ycoord (assq-ref spec #:region)))
             (inherit (assq-ref inherit-props (assq-ref spec #:inherit)))
             (color (or (assq-ref spec #:color) (assq-ref inherit #:color)))
             (thickness (or (assq-ref spec #:thickness) (assq-ref inherit #:thickness)))
             (stencil (assq-ref spec #:stencil)))
        (set! res-stencil
          (ly:stencil-add
            res-stencil
            (if (eq? stencil 'box)
              (rg-stencil-box region #:color color)
              (rg-stencil-hline region #:thickness thickness #:color color))))))

    (map
      (lambda (region color)
        (set! region (region-as-real-ycoord region))
        (set! res-stencil
          (ly:stencil-add
            res-stencil
            (rg-stencil-auto region #:color color))))
      (assq-ref ctx #:debug-regions)
      '("blue" "red" "green"))

    res-stencil))

(define (rg-noteheads-impl ctx)
  (let ((yshift (assq-ref ctx #:cn-internal-yshift))
        (note-heads (assq-ref ctx #:note-heads))
        (coord-base (assq-ref ctx #:coord-base))
        (length-fraction (assq-ref ctx #:LedgerLineSpanner-length-fraction)))

    ;; (length-fraction (ly:grob-property grob 'length-fraction 0.25))

    (apply union
      (map-list (nh note-heads)
        (let* ((nh-extent (grob-extent nh coord-base X))
               (x-extent (interval-widen nh-extent (* length-fraction (interval-length nh-extent))))
               (y-p (- (ensure-exact-or-inf (ly:grob-property nh 'staff-position) 1/20)
                     yshift)))
          (region-box
            (ensure-exact-or-inf (car x-extent) 1/10)
            (ensure-exact-or-inf (cdr x-extent) 1/10)
            y-p
            y-p))))))

(define-public (rg-noteheads ctx)
  (region-cache-with ctx #:rg-noteheads-cached
    (lambda ()
      (rg-noteheads-impl ctx))))

;; TODO: this impl is not complete
(define (staff-line-range staff-symbol coord-base)
  (let ((l-b (ly:spanner-bound staff-symbol LEFT))
        (r-b (ly:spanner-bound staff-symbol RIGHT)))
    ;; (ly:item-break-dir r-b)

    (values
      (grob-coord l-b coord-base X)
      ;; when extending staff as the same time as changing clef
      ;; we extend more to the right.
      (max
        (cdr (grob-extent r-b coord-base X))
        (grob-coord r-b coord-base X)))))

(define (rg-staff-hbound-impl ctx)
  (let* ((staff-symbol (assq-ref ctx #:staff-symbol))
         (coord-base (assq-ref ctx #:coord-base)))
    (receive (a b) (staff-line-range staff-symbol coord-base)
      (region-box
        (inexact->exact a)
        (inexact->exact b)
        -inf.0
        +inf.0))))

(define-public (rg-staff-hbound ctx)
  (region-cache-with ctx #:rg-staff-hbound-cached
    (lambda ()
      (rg-staff-hbound-impl ctx))))

(define (rg-staff-impl ctx)
  (match-let* ((staff-symbol (assq-ref ctx #:staff-symbol))
               ((a . b) (ly:grob-property staff-symbol 'cn-staff-range '(-8 . 8))))
    (intersect
      (rg-staff-hbound ctx)
      (region-box
        -inf.0
        +inf.0
        a
        b))))

(define-public (rg-staff ctx)
  (region-cache-with ctx #:rg-staff-cached
    (lambda ()
      (rg-staff-impl ctx))))

(define (remove-small-gaps vals)
  (let ((changed #t))
    (while changed
      (set! changed #f)
      (let ((cur-start #f)
            (cur-end #f)
            (ans '()))
        (for-each
          (lambda (x)
            (if (and cur-start (< (* 5 (- (car x) cur-end)) (- (cdr x) cur-start)))
              (begin
                (set! changed #t)
                (set! cur-end (cdr x)))
              (begin
                (when cur-start
                  (set! ans (cons (cons cur-start cur-end) ans)))
                (set! cur-start (car x))
                (set! cur-end (cdr x))))
            x)
          vals)
        (set! ans (cons (cons cur-start cur-end) ans))
        (set! vals (reverse ans))))
    vals))

(define-public (remove-small-gaps-x region)
  (region-map-rows region remove-small-gaps))

(define-public (mirror-to-height ctx height lb ub)
  (let* ((r (rg-noteheads ctx)))
    (set! r (shift-y r (- height ub) (- height lb)))
    (set! r (select-remainder-y r height 12))
    r))
