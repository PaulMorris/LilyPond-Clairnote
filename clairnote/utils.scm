(define-module (clairnote utils))

(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (lily))

(define (format-loc loc)
  (format #f "~a:~a"
    (assq-ref loc 'filename)
    (assq-ref loc 'line)))

(define (dbg-impl args-q args loc)
  (format (current-error-port)
    "~%~a~%"
    (format-loc loc))

  (map
    (lambda (a b)
      (format (current-error-port) "~a: ~a~%" a b))
    args-q
    args)

  (last args))

(define-syntax-public dbg
  (syntax-rules ()
    ((_ . expr)
      (dbg-impl
        (quote expr)
        (list . expr)
        (current-source-location)))))

(define-public (assert-impl val expr loc)
  (unless val
    (let ((msg (format #f "~%assertion failed (~a): ~a~%" (format-loc loc) expr)))
      (error msg))))

(define-syntax-public assert!
  (syntax-rules ()
    ((_ expr)
      (assert-impl
        expr
        (quote expr)
        (current-source-location)))))

(define-public (benchmark-impl start-t val end-t expr loc)
  (format (current-error-port)
    "~%~a~%~a: ~a seconds~%"
    (format-loc loc)
    expr
    (/ (exact->inexact (- end-t start-t)) internal-time-units-per-second))
  val)

(define-syntax-public benchmark
  (syntax-rules ()
    ((_ expr)
      (benchmark-impl
        (get-internal-real-time)
        expr
        (get-internal-real-time)
        (quote expr)
        (current-source-location)))))

(export for-each!)
(define-macro for-each!
  (lambda* (val binding #:rest body)
    `(for-each
      (lambda (x)
       (match-let ((,binding x))
        ,@body))
      ,val)))

(export dolist!)
(define-macro dolist!
  (lambda* (spec #:rest body)
    (match-let (((var v) spec))
      `(let ((tmp--list ,v))
        (while (not (null? tmp--list))
         (match-let ((,var (car tmp--list)))
          ,@body)
         (set! tmp--list (cdr tmp--list)))))))

(export push)
(define-macro (push x y)
  `(set! ,y (cons ,x ,y)))

(export pop)
(define-macro (pop x)
  `(let ((tmp--ans (car ,x)))
    (set! ,x (cdr ,x))
    tmp--ans))

(export inc)
(define-macro (inc x)
  `(set! ,x (1+ ,x)))

(export assq-setf!)
(define-macro (assq-setf! alist key val)
  `(set! ,alist (assq-set! ,alist ,key ,val)))

(export for)
(define-macro for
  (lambda* (spec #:rest body)
    (match-let (((var lb ub) spec))
      `(do ((,var ,lb (1+ ,var))) ((>= ,var ,ub))
        ,@body))))

(export for-vec)
(define-macro for-vec
  (lambda* (spec #:rest body)
    (match-let (((var v) spec))
      `(let ((tmp--vec ,v))
        (for (tmp--i 0 (vector-length tmp--vec))
         (match-let ((,var (vector-ref tmp--vec tmp--i)))
          ,@body))))))

(export map-list)
(define-macro map-list
  (lambda* (spec #:rest body)
    (match-let (((var v) spec))
      `(map
        (lambda (x)
         (match-let ((,var x))
          ,@body))
        ,v))))

(define-public (close? x y tolerance)
  (< (abs (- x y)) tolerance))

(define-public (odd?-safe x)
  (if (integer? x)
    (odd? x)
    #f))

(define-public (ensure-exact-or-inf x tolerance)
  (if (inf? x)
    x
    (rationalize (inexact->exact x) tolerance)))

(define-public (context-prop symbol type?)
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc "custom context property")
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol)

(define-public (grob-prop symbol type?)
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc "custom grob property")
  symbol)

(define-public (grob-maybe-pure-prop grob sym pure)
  (if pure
    (ly:grob-pure-property grob sym 0 0)
    (ly:grob-property grob sym)))

(define-public (index-cell cell dir)
  (if (equal? dir RIGHT)
    (cdr cell)
    (car cell)))

(define-public (index-set-cell! x dir val)
  (case dir
    ((-1) (set-car! x val))
    ((1) (set-cdr! x val))))

(define-public (grob-extent grob base dir)
  (let* ((common (ly:grob-common-refpoint grob base dir))
         (ext (ly:grob-extent grob common dir))
         (base-coord (ly:grob-relative-coordinate base common dir)))
    (cons (- (car ext) base-coord) (- (cdr ext) base-coord))))

(define-public (grob-coord grob base dir)
  (let* ((common (ly:grob-common-refpoint grob base dir))
         (grob-coord (ly:grob-relative-coordinate grob common dir))
         (base-coord (ly:grob-relative-coordinate base common dir)))
    (- grob-coord base-coord)))

(define-public (grob-array->list-safe grob-array)
  (if (and grob-array (not (null? grob-array)))
    (ly:grob-array->list grob-array)
    '()))
