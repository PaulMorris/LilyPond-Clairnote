(define-module (clairnote region))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (srfi srfi-8)
  (srfi srfi-17)
  (srfi srfi-43)
  ((lily) #:prefix lily:)
  (clairnote utils))

;; a rectangle is
;; #(x1 x2 y1 y2)
;; which represents (x, y) such that x1<=x<=x2 && y1<=y<=y2

;; a region is expressed as a union of rectangles in a vector

(define-public empty-region #())

(define-public (region-into-val v)
  ;; the reason for using exact is that otherwise we have
  ;; (<= 0 0.0) => #t
  ;; (>= 0 0.0) => #t
  ;; (eqv? 0 0.0) => #f
  ;; which breaks some assumptions that is made
  (if (or (exact? v) (inf? v))
    v
    (rationalize (inexact->exact v) 1/1000)))

(define-public (region-box x1 x2 y1 y2)
  (if (and (<= x1 x2) (<= y1 y2)
       (not
         (or
           (eqv? x1 +inf.0)
           (eqv? x2 -inf.0)
           (eqv? y1 +inf.0)
           (eqv? y2 -inf.0))))

    `#(#(,(region-into-val x1)
         ,(region-into-val x2)
         ,(region-into-val y1)
         ,(region-into-val y2)))

    empty-region))

(define (sort-list-and-remove-dups list-x)
  (set! list-x (sort list-x <))
  (let ((prev #f)
        (ans '()))
    (dolist! (x list-x)
      (unless (eqv? x prev)
        (push x ans))
      (set! prev x))
    (reverse-list->vector ans)))

(define (cons< x y)
  (if (eqv? (car x) (car y))
    (< (cdr x) (cdr y))
    (< (car x) (car y))))

(define-public (range-simplify range)
  (set! range (sort-list range cons<))
  (let ((cur-start #f)
        (cur-end #f)
        (ans '()))

    (dolist! ((x . y) range)
      (when (<= x y)
        (if (and cur-start (<= x cur-end))
          (when (<= cur-end y)
            (set! cur-end y))

          (begin
            (when cur-start
              (push (cons cur-start cur-end) ans))
            (set! cur-start x)
            (set! cur-end y)))))

    (when cur-start
      (push (cons cur-start cur-end) ans))
    (reverse! ans)))

(define (min2 x y)
  ;; preservers whether x and y is exact
  (if (> x y)
    y
    x))

(define (max2 x y)
  ;; preservers whether x and y is exact
  (if (> x y)
    x
    y))

(define-public (range-intersect r1 r2)
  (if (or (null? r1) (null? r2))
    '()
    (let* ((r2-l #f)
           (r2-u #f)
           (advance-r2
             (lambda ()
               (set! r2-l (caar r2))
               (set! r2-u (cdar r2))
               (set! r2 (cdr r2))
               (if (null? r2)
                 (set! r2 '((+inf.0 . +inf.0))))))
           (ans '()))

      (advance-r2)

      (dolist! ((x . y) r1)
        (while (< r2-u x)
          (advance-r2))

        (while (< r2-u y)
          (push (cons (max2 r2-l x) r2-u) ans)
          (set! x r2-u)
          (advance-r2))

        (when (<= r2-l y)
          (push (cons (max2 r2-l x) y) ans)))

      (reverse! ans))))

(define-public (range-subtract r1 r2)
  (if (null? r2)
    r1
    (let* ((r2-l #f)
           (r2-u #f)
           (advance-r2
             (lambda ()
               (set! r2-l (caar r2))
               (set! r2-u (cdar r2))
               (set! r2 (cdr r2))
               (if (null? r2)
                 (set! r2 '((+inf.0 . +inf.0))))))
           (ans '()))

      (advance-r2)

      (dolist! ((x . y) r1)
        (let ((do-include-x #t))
          (while (< r2-u x)
            (advance-r2))

          (while (< r2-u y)
            (when (< x r2-l)
              (push (cons x r2-l) ans))
            (set! x r2-u)
            (set! do-include-x #f)
            (advance-r2))

          (when (and (< x r2-l) (or do-include-x (< x (min2 r2-l y))))
            (push (cons x (min2 r2-l y)) ans))))

      (reverse! ans))))

(define (rect-less v1 v2)
  (< (vector-ref v1 0) (vector-ref v2 0)))

(define-public (regions-foreach-columns-full regions f)
  (let* ((x-coords (list -inf.0 +inf.0))
         (k (length regions))
         (n #f)
         (ranges-all '())
         (nowidth-ranges-all '()))

    (dolist! (r regions)
      (for-vec (#(x1 x2 y1 y2) r)
        (push x1 x-coords)
        (push x2 x-coords)))

    (set! x-coords
      (sort-list-and-remove-dups x-coords))
    (set! n (vector-length x-coords))

    (dolist! (r regions)
      (set! r (sort r rect-less))
      (let* ((i 0)
             (nowidth-ranges (make-vector n '()))
             (ranges (make-vector n '())))

        (for-vec (#(x1 x2 y1 y2) r)
          (while (not (eqv? (vector-ref x-coords i) x1))
            (inc i))

          (let ((j i))
            (while (not (eqv? (vector-ref x-coords j) x2))
              (push (cons y1 y2) (vector-ref nowidth-ranges j))
              (push (cons y1 y2) (vector-ref ranges j))
              (inc j))
            (push (cons y1 y2) (vector-ref nowidth-ranges j))))

        (push ranges ranges-all)
        (push nowidth-ranges nowidth-ranges-all)))

    (set! ranges-all (reverse! ranges-all))
    (set! nowidth-ranges-all (reverse! nowidth-ranges-all))

    ;; (dbg
    ;;   x-coords
    ;;   ranges-all
    ;;   nowidth-ranges-all)

    (for (i 0 (1- n))
      (let ((rs (map-list (ranges ranges-all)
                 (range-simplify (vector-ref ranges i)))))
        (when (any (lambda (x) (not (null? x))) rs)
          (f rs
            (vector-ref x-coords i)
            (vector-ref x-coords (1+ i))))))

    (for (i 0 n)
      (let ((rs (map-list (ranges nowidth-ranges-all)
                 (range-simplify (vector-ref ranges i)))))
        (when (any (lambda (x) (not (null? x))) rs)
          (f rs
            (vector-ref x-coords i)
            (vector-ref x-coords i)))))))

(define-public (region-foreach-columns r f)
  (set! r (sort r rect-less))
  (let ((x-coords (list -inf.0 +inf.0)))
    (for-vec (#(x1 x2 y1 y2) r)
      (push x1 x-coords)
      (push x2 x-coords))

    (set! x-coords
      (sort-list-and-remove-dups x-coords))

    (let* ((i 0)
           (n (vector-length x-coords))
           (nowidth-ranges (make-vector n '()))
           (ranges (make-vector n '())))

      (for-vec (#(x1 x2 y1 y2) r)
        (while (not (eqv? (vector-ref x-coords i) x1))
          (inc i))

        (if (eqv? x1 x2)
          (push (cons y1 y2) (vector-ref nowidth-ranges i))
          (let ((j i))
            (while (not (eqv? (vector-ref x-coords j) x2))
              (push (cons y1 y2) (vector-ref ranges j))
              (inc j)))))

      (for (i 0 n)
        (set! (vector-ref ranges i)
          (range-simplify (vector-ref ranges i))))

      (for (i 1 n)
        (set! (vector-ref nowidth-ranges i)
          (range-subtract
            (range-subtract
              (range-simplify (vector-ref nowidth-ranges i))
              (vector-ref ranges (1- i)))
            (vector-ref ranges i))))

      ;; (dbg
      ;;   r
      ;;   x-coords
      ;;   ranges
      ;;   nowidth-ranges)

      (let* ((cur-s -inf.0)
             (cur-e -inf.0)
             (cur-rg '())
             (handle-range
               (lambda (rg x1 x2)
                 (unless (and (null? rg) (eqv? x1 x2))
                   (if (equal? rg cur-rg)
                     (set! cur-e x2)
                     (begin
                       (unless (null? cur-rg)
                         (f cur-rg cur-s cur-e))
                       (set! cur-s x1)
                       (set! cur-e x2)
                       (set! cur-rg rg)))))))

        (for (i 0 (1- n))
          (let ((x1 (vector-ref x-coords i))
                (x2 (vector-ref x-coords (1+ i)))
                (rg1 (vector-ref nowidth-ranges i))
                (rg2 (vector-ref ranges i)))
            (handle-range rg1 x1 x1)
            (handle-range rg2 x1 x2)))

        (unless (null? cur-rg)
          (f cur-rg cur-s cur-e))))))

(define-public (region-transpose region)
  (vector-map
    (lambda (i v)
      (match-let ((#(x1 x2 y1 y2) v))
        (vector y1 y2 x1 x2)))
    region))

(define-public (region-foreach-rows region f)
  (region-foreach-columns (region-transpose region) f))

(define-public (region-simplify r)
  (let ((res-boxes '()))
    (region-foreach-columns r
      (lambda (range x1 x2)
        (dolist! ((y1 . y2) range)
          (push (region-box x1 x2 y1 y2) res-boxes))))
    (apply vector-append (reverse! res-boxes))))

(define-public (region-simplify-horizontal r)
  (region-transpose (region-simplify (region-transpose r))))

(define-public (region-map-columns r f)
  (let ((res-boxes '()))
    (regions-foreach-columns-full (list r)
      (lambda (range x1 x2)
        (dolist! ((y1 . y2) (range-simplify (f (first range))))
          (push (region-box x1 x2 y1 y2) res-boxes))))
    (region-simplify (apply vector-append (reverse! res-boxes)))))

(define-public (region-map-rows r f)
  (region-transpose (region-map-columns (region-transpose r) f)))

(define-public (union . regions)
  (apply vector-append regions))

(define-public (regions-map-columns rs f)
  (let ((res-boxes '()))
    (regions-foreach-columns-full rs
      (lambda (ranges x1 x2)
        (dolist! ((y1 . y2) (range-simplify (f ranges x1 x2)))
          (push (region-box x1 x2 y1 y2) res-boxes))))
    (region-simplify (apply vector-append (reverse! res-boxes)))))

(define (intersect2 r1 r2)
  (regions-map-columns (list r1 r2)
    (lambda (rs x1 x2)
      (match-let (((rg1 rg2) rs))
        (range-intersect rg1 rg2)))))

(define-public (intersect r1 r2)
  (intersect2 r1 r2))

(define-public (subtract r1 r2)
  (regions-map-columns (list r1 r2)
    (lambda (rs x1 x2)
      (match-let (((rg1 rg2) rs))
        (range-subtract rg1 rg2)))))

(define-public (region-mapbox region f)
  (let ((res-boxes '()))
    (for-vec (#(x1 x2 y1 y2) region)
      (push (f x1 x2 y1 y2) res-boxes))
    (apply vector-append (reverse! res-boxes))))

(define-public (region-linear-transform-y region scale translate)
  (region-mapbox region
    (lambda (x1 x2 y1 y2)
      (region-box x1 x2
        (+ (* y1 scale) translate)
        (+ (* y2 scale) translate)))))

(define-public (range-shift range lb ub)
  (map-list ((a . b) range)
    (cons (+ a lb) (+ b ub))))

(define-public (shift-x region lb ub)
  (region-mapbox region
    (lambda (x1 x2 y1 y2)
      (region-box (+ x1 lb) (+ x2 ub) y1 y2))))

(define-public (shift-y region lb ub)
  (region-mapbox region
    (lambda (x1 x2 y1 y2)
      (region-box x1 x2 (+ y1 lb) (+ y2 ub)))))

(define (range-fill range)
  (list
    (cons
      (car (first range))
      (cdr (last range)))))

(define-public (fill-y region)
  (region-map-columns region (lambda (r) (range-fill r))))

(define-public (range-select-floor-remainder range r1 r2 div)
  (let ((ans '()))
    (dolist! ((s . e) range)
      (assert! (not (eqv? s -inf.0)))
      (assert! (not (eqv? e +inf.0)))

      ;; s <= x * div + r1, x * div + r2 <= e
      ;; (s - r1) / div <= x <= (e - r2) / div
      (let ((a (floor/ (- s r1) div))
            (b (ceiling/ (- e r2) div)))
        (while (<= a b)
          (let ((v1 (+ (* a div) r1))
                (v2 (+ (* a div) r2)))
            (when (<= s v1 v2 e)
              (push (cons v1 v2) ans)))
          (inc a))))
    ans))

(define-public (select-remainder-y region remainder div)
  ;; (dbg remainder)
  (region-map-columns region
    (lambda (r)
      (if (list? remainder)
        (apply append
          (map-list (rv remainder)
            (range-select-floor-remainder r rv rv div)))
        (range-select-floor-remainder r remainder remainder div)))))

(define-public (select-remainder-y-range region r1 r2 div)
  (region-map-columns region (lambda (r) (range-select-floor-remainder r r1 r2 div))))

(export widen-x)
(define* (widen-x region mul #:optional add)
  (set! mul (rationalize (inexact->exact mul) 1/100))
  (region-map-rows region
    (lambda (r)
      (map-list ((x . y) r)
        (let ((m (* (+ x y) 1/2))
              (l (* (+ (* mul (- y x)) (or add 0)) 1/2)))
          (cons (- m l) (+ m l)))))))

(define-public (region-y-profile region)
  (let ((ans '()))
    (region-foreach-rows region
      (lambda (r y1 y2)
        (push (cons y1 y2) ans)
        '()))
    (reverse! ans)))

(define-public (region-debug ctx region)
  (assq-set!
    ctx
    #:debug-regions
    (cons
      region
      (assq-ref ctx #:debug-regions)))
  region)

(define-public (region-cache-with ctx key regionfn)
  (let ((cached (assq-ref ctx key)))
    (if cached
      cached
      (begin
        (set! cached (regionfn))
        (assq-set! ctx key cached)
        cached))))

(export region-draw-box)
(define* (region-draw-box ctx region #:key (color #f))
  (assq-set!
    ctx
    #:draw
    (cons
      `((#:region . ,region)
        (#:stencil . box)
        (#:color . ,color))
      (assq-ref ctx #:draw))))

(export region-draw-hline)
(define* (region-draw-hline ctx region #:key (color #f) (thickness #f) (inherit 'staff) (avoid #t))
  (assq-set!
    ctx
    #:draw
    (cons
      `((#:region . ,region)
        (#:stencil . hline)
        (#:color . ,color)
        (#:thickness . ,thickness)
        (#:inherit . ,inherit))
      (assq-ref ctx #:draw)))
  (when avoid
    (assq-set! ctx #:avoid
      (cons region (assq-ref ctx #:avoid)))))
