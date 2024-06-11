(define-module (clairnote config))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (srfi srfi-19)
  (lily)
  (clairnote utils)
  (clairnote staff)
  (clairnote region)
  (clairnote ledger))

(grob-prop 'cn-staff list?)

(define-public (staff-background-from-bounds ctx bounds)
  (intersect
    (rg-staff-hbound ctx)
    (fill-y
      (remove-small-gaps-x
        (fill-y
          (widen-x bounds 4))))))

(define-public (staff-background-default ctx)
  (region-cache-with ctx #:cn--cached-staff-background-default
    (lambda ()
      (staff-background-from-bounds ctx
        (union
          (rg-staff ctx)
          ;; TODO: maybe look at clefs, rests, etc?
          (mirror-to-height ctx 0 -2 2)
          (mirror-to-height ctx 4 2 10)
          (mirror-to-height ctx 8 2 10))))))

(define-public (staff-background-uneven ctx)
  (region-cache-with ctx #:cn--cached-staff-background-uneven
    (lambda ()
      (staff-background-from-bounds ctx
        (union
          (rg-staff ctx)
          (mirror-to-height ctx 0 -2 2)
          (mirror-to-height ctx 4 2 6)
          (mirror-to-height ctx 8 6 10))))))

(define-public ((staff-dn-with-bg background-fn) ctx)
  (let ((background (background-fn ctx))
        (ledgers (mirror-to-height ctx 0 -2 2)))
    (region-draw-hline ctx (select-remainder-y background '(4 8) 12))
    (region-draw-hline ctx ledgers #:inherit 'ledger)))

(define-public staff-dn (staff-dn-with-bg staff-background-default))
(define-public staff-dn-uneven (staff-dn-with-bg staff-background-uneven))

(define-public ((staff-sn-with-bg background-fn) ctx)
  (let ((background (staff-background-default ctx))
        (ledgers (union
                  (mirror-to-height ctx 0 -2 2)
                  (mirror-to-height ctx 2 2 2)
                  (mirror-to-height ctx 6 6 6)
                  (mirror-to-height ctx 10 10 10))))
    (region-draw-hline ctx (select-remainder-y background 4 12))
    (region-draw-hline ctx (select-remainder-y background 8 12))
    (region-draw-hline ctx ledgers #:inherit 'ledger)))
(define-public staff-sn (staff-sn-with-bg staff-background-default))
(define-public staff-sn-uneven (staff-sn-with-bg staff-background-uneven))

(define (get-legacy-ledgers-helper ctx)
  (let* ((notes (rg-noteheads ctx))
         (staff (rg-staff ctx))
         (staff-bigger (shift-y staff -1/100 1/100)))

    (lambda (height add-down add-up nearer-drop)
      (let* ((notes-added (shift-y notes (- add-down) add-up))
             (notes-reg (fill-y (union staff notes-added)))

             (notes-valid-reg
               (if nearer-drop
                 (intersect notes-reg (shift-y notes (- nearer-drop) nearer-drop))
                 (union notes-added (subtract notes-reg staff-bigger)))))

        (select-remainder-y notes-valid-reg height 12)))))

(define-public (get-legacy-ledgers ctx)
  (let* ((staff-symbol (assq-ref ctx #:staff-symbol))
         (cn-ledger-recipe (ly:grob-property staff-symbol 'cn-ledger-recipe))
         (get-ledgers (get-legacy-ledgers-helper ctx))
         (style-lower '())
         (style-upper '())
         (ans '()))
    ;; (dbg (assq-ref ctx #:LedgerLineSpanner-length-fraction))
    (assert! (eqv? (car cn-ledger-recipe) 12))
    (assert! (eqv? (length cn-ledger-recipe) 2))

    ;; (dbg cn-ledger-recipe)
    (dolist! ((a b c) (cadr cn-ledger-recipe))
      (push (list (modulo (+ 8 a) 12) b c) style-upper)
      (push (list (modulo (- -8 a) 12) b c) style-lower))

    (set! style-lower (sort! style-lower car<))
    (set! style-upper (sort! style-upper car<))
    (map
      (lambda (l u)
        (match-let (((a1 b1 c1) l)
                    ((a2 b2 c2) u))
          (assert! (eqv? a1 a2))
          (assert! (eqv? c1 c2))
          (push (get-ledgers a1 b1 b2 c1) ans)))
      style-lower
      style-upper)

    (apply union ans)))

(define-public (staff-with-legacy-ledgers ctx)
  (let* ((lines (select-remainder-y (rg-staff ctx) '(4 8) 12))
         (ledgers (subtract (get-legacy-ledgers ctx) lines)))
    (region-draw-hline ctx lines)
    (region-draw-hline ctx ledgers #:inherit 'ledger)))
