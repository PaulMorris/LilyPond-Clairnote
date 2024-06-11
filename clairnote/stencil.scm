(define-module (clairnote stencil))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (srfi srfi-8)
  (srfi srfi-43)
  (lily)
  (clairnote region)
  (clairnote utils))

(define-public (stencil-in-color! stencil color)
  (when color
    (if (pair? color)
      (set! stencil
        (ly:stencil-in-color stencil
          (first color)
          (second color)
          (third color)))
      (set! stencil
        (ly:stencil-in-color stencil
          color))))
  stencil)

(export rg-stencil-hline)
(define* (rg-stencil-hline region #:key (color #f) (thickness 1))

  (let ((ans empty-stencil))
    (region-foreach-rows region
      (lambda (rg y1 y2)
        (assert (eqv? y1 y2))
        (for-each! rg (x1 . x2)
          (set! ans
            (ly:stencil-add
              ans
              (make-line-stencil thickness
                (+ x1 (/ thickness 2))
                y1
                (- x2 (/ thickness 2))
                y1))))
        '()))

    (set! ans (stencil-in-color! ans color))
    ans))

(export rg-stencil-box)
(define* (rg-stencil-box region #:key (color #f))

  (let ((ans empty-stencil))
    (region-foreach-columns region
      (lambda (rg x1 x2)
        (for-each! rg (y1 . y2)
          (when (and (< x1 x2) (< y1 y2))
            (set! ans
              (ly:stencil-add
                ans
                (make-filled-box-stencil (cons x1 x2) (cons y1 y2))))))
        '()))
    (set! ans (stencil-in-color! ans color))
    ans))

(export rg-stencil-auto)
(define* (rg-stencil-auto region #:key (color #f))
  (let ((ans empty-stencil))

    (for-vec (#(x1 x2 y1 y2) (region-simplify region))
      (if (or (eqv? x1 x2) (eqv? y1 y2))
        (set! ans
          (ly:stencil-add
            ans
            (make-line-stencil 0.4 x1 y1 x2 y2)))
        (set! ans
          (ly:stencil-add
            ans
            (make-filled-box-stencil (cons x1 x2) (cons y1 y2))))))

    (set! ans (stencil-in-color! ans color))
    ans))
