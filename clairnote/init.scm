(use-modules
  (srfi srfi-1)
  (srfi srfi-13)
  (lily))

;; for me (on guix) %compile-fallback-path is set to a non writable directory.
;; change it so that it is writable
;; TODO: other platforms
(unless (and (access? %compile-fallback-path W_OK) (file-is-directory? %compile-fallback-path))
  (set! %compile-fallback-path
    (string-append (getenv "HOME")
      "/.cache/guile/ccache/lilypond/")))

;; compiled code is faster and have debug info
(set! %load-should-auto-compile #t)
(ly:set-option 'compile-scheme-code #t)

;; backtraces are nice
(debug-enable 'backtrace)

(let ((guile-user-mod (resolve-module '(guile-user))))

  (unless (module-variable guile-user-mod 'cn:clairnote-module-list)
    (module-define! guile-user-mod
      'cn:clairnote-module-list
      (map (lambda (x)
            (resolve-interface x #:prefix 'cn:))
        '((clairnote utils)
          (clairnote staff)
          (clairnote ledger)
          (clairnote region)
          (clairnote config)
          (clairnote stencil)))))

  (map
    (lambda (x)
      ;; different from module-add!
      ;; module-add! allows a var to be used by a module
      ;; ly:module-copy makes vars available in anything that imports a module
      ;; so this allows
      ;; 1) cn:xyz be used in a scope such as \paper
      ;; 2) lilypond a.ly b.ly to work with clairnote
      (ly:module-copy (current-module) x))
    (module-ref guile-user-mod 'cn:clairnote-module-list)))
